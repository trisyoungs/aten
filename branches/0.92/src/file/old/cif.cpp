/*
// IUC CIF I/O
*/

using namespace std;
#include "model/model.h"
#include "file/format.h"
#include "file/parse.h"
#include "file/cif.h"
#include "base/prefs.h"
#include "base/elemap.h"
#include <string>

// Mini CIF dictionary (from 2.3.1 standard)
const char *CD_keywords[CD_NITEMS] = { "_NULL_", "loop_", "_chemical_name_common",
	"_cell_length_a", "_cell_length_b", "_cell_length_c",
	"_cell_angle_alpha", "_cell_angle_beta", "_cell_angle_gamma",
	"_atom_site_type_symbol", "_atom_site_fract_x", "_atom_site_fract_y", "_atom_site_fract_z",
	"_space_group_symop_operation_xyz", "_symmetry_equiv_pos_as_xyz" };
	
cifdic CD_from_text(const char *s)
	{ return (cifdic) enum_search("NULL",CD_NITEMS,CD_keywords,s); };

bool fileio::read_cif(const char *filename)
{
	dbg_begin(DM_CALLS,"fileio::read_cif");
	// Read in an IUC cif coordinate file
	int n, success;
	int foundcell, foundatoms;
	vec3<double> celllengths, cellangles;
	cifdic keyword;
	atom *i;
	bool done;
	format ciffmt;
	string fmt;
	symmop *so;
	msg(DM_NONE,"Type   : IUC cif coordinates\n");
	// Find the loop with '_atom_site_' data in it and create a format to read in the data
	foundcell = 0;
	foundatoms = 0;
	ifstream ciffile(filename,ios::in);
	while (!ciffile.eof())
	{
		success = parser.get_args(&ciffile,PO_DEFAULTS);
		if (success == -1)
		{
			// End of file - did we get the data we needed?
			if (foundatoms == 0)
			{
				msg(DM_NONE,"Found no atoms in cif file.\n");
				dbg_end(DM_CALLS,"fileio::read_cif");
				return FALSE;
			}
			else if (foundcell < 6)
			{
				msg(DM_NONE,"Found incomplete unit cell spec in cif file.\n");
				dbg_end(DM_CALLS,"fileio::read_cif");
				return FALSE;
			}
			done = TRUE;
		}
		else if (success == 1)
		{
			msg(DM_NONE,"Error reading cif file.\n");
			dbg_end(DM_CALLS,"fileio::read_cif");
			return FALSE;
		}
		// Search file for 'loop_' commands and several other bits and pieces we're interested in
		keyword = CD_from_text(parser.argc(0));
		switch (keyword)
		{
			case (CD_OTHER):	break;
			case (CD_NAME):		targetmodel->set_name(parser.argc(1)); break;
			case (CD_CELLA):	celllengths.x = parser.argd(1); foundcell ++; break;
			case (CD_CELLB):	celllengths.y = parser.argd(1); foundcell ++; break;
			case (CD_CELLC):	celllengths.z = parser.argd(1); foundcell ++; break;
			case (CD_CELLALPHA):	cellangles.x = parser.argd(1); foundcell ++; break;
			case (CD_CELLBETA):	cellangles.y = parser.argd(1); foundcell ++; break;
			case (CD_CELLGAMMA):	cellangles.z = parser.argd(1); foundcell ++; break;
			case (CD_LOOP):
				// Is this the '_atom_site_' loop, the '_symmetry_' loop, or something else?
				success = parser.get_args(&ciffile,PO_DEFAULTS);
				if (success != 0)
				{
					msg(DM_NONE,"Error reading 'loop_' items in cif file.\n");
					dbg_end(DM_CALLS,"fileio::read_cif");
					return FALSE;
				}
				if (strncmp(parser.argc(0),"_atom_site_aniso_",17) == 0) break;
				/*
				// Atom data loop
				*/
				else if (strncmp(parser.argc(0),"_atom_site_",11) == 0)
				{
					// Parse lines to retrieve order of data items, until we find the atom data itself
					ciffmt.clear();
					fmt = "";
					while (parser.argc(0)[0] == '_')
					{
						keyword = CD_from_text(parser.argc(0));
						switch (keyword)
						{
							case (CD_ELEMENT):	fmt += text_from_FN(FN_ELEMENT); break;
							case (CD_FRACX):	fmt += text_from_FN(FN_POSX); break;
							case (CD_FRACY):	fmt += text_from_FN(FN_POSY); break;
							case (CD_FRACZ):	fmt += text_from_FN(FN_POSZ); break;
							default:		fmt += text_from_FN(FN_DISCARD); break;
						}
						fmt += " ";
						success = parser.get_args(&ciffile,PO_DEFAULTS);
						if (success != 0)
						{
							msg(DM_NONE,"Error reading atomic 'loop_' items in cif file.\n");
							dbg_end(DM_CALLS,"fileio::read_cif");
							return FALSE;
						}
					}
					// Now read in the atom data with a format created from the string assembled above.
					// First line of atom data has already been read in
					ciffmt.create(fmt.c_str());
					while (parser.argc(0)[0] != '\0')
					{
						ciffmt.parse_line(PO_DEFAULTS);
						i = targetmodel->add_atom(ciffmt.get_el());
						i->set_coords(ciffmt.get_r());
						foundatoms ++;
						success = parser.get_args(&ciffile,PO_DEFAULTS);
						if (success != 0)
						{
							msg(DM_NONE,"Error reading atom data in cif file.\n");
							dbg_end(DM_CALLS,"fileio::read_cif");
							return FALSE;
						}
					}
				}
				/*
				// Symmetry operators group
				*/
				else if ((strncmp(parser.argc(0),"_symmetry_equiv_",16) == 0) || (parser.argc(0),"_space_group_",13) == 0)
				{
					// Parse lines to retrieve order of data items, until we find the atom data itself
					ciffmt.clear();
					fmt = "";
					while (parser.argc(0)[0] == '_')
					{
						keyword = CD_from_text(parser.argc(0));
						switch (keyword)
						{
							case (CD_SYMOPERATOR):	fmt += text_from_FN(FN_STRING1); break;
							case (CD_SYMEQUIVPOS):	fmt += text_from_FN(FN_STRING1); break;
							default:		fmt += text_from_FN(FN_DISCARD); break;
						}
						fmt += " ";
						success = parser.get_args(&ciffile,PO_USEQUOTES);
						if (success != 0)
						{
							msg(DM_NONE,"Error reading symmetry 'loop_' items in cif file.\n");
							dbg_end(DM_CALLS,"fileio::read_cif");
							return FALSE;
						}
					}
					// Now read in the symop data with a format created from the string assembled above.
					// First operation has already been read in
					ciffmt.create(fmt.c_str());
					while (parser.argc(0)[0] != '\0')
					{
						ciffmt.parse_line(PO_USEQUOTES);
						so = targetmodel->add_symmop();
						so->set_from_xyz(ciffmt.get_s(0));
						success = parser.get_args(&ciffile,PO_DEFAULTS);
						if (success != 0)
						{
							msg(DM_NONE,"Error reading atom data in cif file.\n");
							dbg_end(DM_CALLS,"fileio::read_cif");
							return FALSE;
						}
					}
				}
				break;
		}
		// 
	}
	// Store cell data
	// TODO set cell type from spacegroup
	targetmodel->cell.set(CT_PARALLELEPIPED,celllengths,cellangles);
	// Apply symmetry operations
	if (prefs.get_pack_on_load()) targetmodel->apply_model_symmops(NULL);
	// Convert stored fractional atomic coordinates into model coordinates
	targetmodel->frac_coords_to_real();
	if (prefs.get_fold_on_load()) targetmodel->fold_all_atoms();
	finalise_model(MF_CIF,filename);
	dbg_end(DM_CALLS,"fileio::read_cif");
	return TRUE;
}
