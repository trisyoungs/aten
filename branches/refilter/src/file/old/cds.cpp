/*
// Cambridge FDAT Import
*/

using namespace std;
#include <string>
#include "model/model.h"
#include "file/format.h"
#include "file/parse.h"
#include "base/sysfunc.h"
#include "base/prefs.h"
#include "base/master.h"

bool fileio::read_cds(const char *filename)
{
	dbg_begin(DM_CALLS,"fileio::read_cds");
	// Read in a Protein Databank / Brookhaven file
	atom *i, *newi;
	bool repeat = TRUE;
	int n, m, s, success, offset, nsymm, numatoms, numsymmatoms, cellpresent, bondatoms[2];
	int atfor, centsymm, spacegroup, nformula, nconn, nremark;
	vec3<double> celllengths, cellangles, cellvector, tempv;
	string line, temp;
	msg(DM_NONE,"Type   : Cambridge Database FDAT\n");
	// Create the string formats
	format cds_cell, cds_atom;
	cds_cell.create("cx6 cy6 cz6 ca6 cb6 cc6 s6 *18 i3 t8 j3");
	cds_atom.create("e5 x7 y7 z7");
	// Open file and begin
	ifstream cdsfile(filename,ios::in);
	// Record 1 - Directory Information
	// Do this without using format chunks, since it's full of data and v.complex
	getline(cdsfile,line);
	do
	{
		targetmodel->set_name(line.substr(1,8).c_str());
		// Crystal system == atoi(line.substr(9,1).c_str());
		// Total number of characters in rfactor, remark, disorder and error fields (record 3)
		nremark = atoi(line.substr(26,3).c_str()) + atoi(line.substr(29,3).c_str());
		nremark += atoi(line.substr(32,3).c_str()) + atoi(line.substr(35,3).c_str());
		nsymm = atoi(line.substr(38,3).c_str());
		numatoms = atoi(line.substr(44,3).c_str());
		numsymmatoms = atoi(line.substr(47,3).c_str());
		cellpresent = atoi(line.substr(56,1).c_str());
		nconn = atoi(line.substr(53,3).c_str());
		atfor = atoi(line.substr(58,1).c_str());
		centsymm = atoi(line.substr(59,1).c_str());
		// Print out some useful data on the file...
		msg(DM_NONE,"Title  : %s\n",targetmodel->get_name());
		msg(DM_NONE,"Symm   : %i symmetry-unique atoms in file.\n",numatoms);
		msg(DM_NONE,"Symm   : %i symmetry-related atoms in file.\n",numsymmatoms);
		msg(DM_NONE,"Symm   : %i symmetry operations given in file.\n",nsymm);
		msg(DM_NONE,"Remark : Text information totals %i characters (spans %i lines).\n",nremark,(nremark/80)+1);
		msg(DM_NONE,"Bond   : %i bonds specified in file.\n",nconn);
		nremark = nremark/80 + 1;
		// Record 2 - Unit cell parameters
		spacegroup = 0;
		if (cellpresent == 1)
		{
			// Precision digits (6) are contained in the string object of the format...
			if (cds_cell.parse_file(&cdsfile,PO_DEFAULTS) != 0)
			{
				msg(DM_NONE,"Error reading unit cell data (record 2) from file.\n");
				dbg_end(DM_CALLS,"fileio::read_cds");
				return FALSE;
			}
			celllengths = cds_cell.get_lengths();
			cellangles = cds_cell.get_angles();
			temp = cds_cell.get_s(0);
			// Convert cell parameters using precision digits
			celllengths.x /= pow(10.0,atoi(temp.substr(0,1).c_str()));
			celllengths.y /= pow(10.0,atoi(temp.substr(1,1).c_str()));
			celllengths.z /= pow(10.0,atoi(temp.substr(2,1).c_str()));
			cellangles.x /= pow(10.0,atoi(temp.substr(3,1).c_str()));
			cellangles.y /= pow(10.0,atoi(temp.substr(4,1).c_str()));
			cellangles.z /= pow(10.0,atoi(temp.substr(5,1).c_str()));
			// Get spacegroup number and number of formula units per cell
			spacegroup = cds_cell.get_i();
			nformula = cds_cell.get_j();
			targetmodel->set_spacegroup(spacegroup);
			msg(DM_NONE,"Cell   : Spacegroup is %s (%i).\n",spacegroups.get_name(spacegroup),spacegroup);
			targetmodel->cell.set(spacegroups.get_cell_type(spacegroup),celllengths,cellangles);
		}
		// Record 3 - Text information (skip)
		for (n=0; n<nremark; n++)
		{
			if (parser.get_line(&cdsfile,PO_DEFAULTS) != 0)
			{
				msg(DM_NONE,"Error reading text information (record 3) from file.\n");
				dbg_end(DM_CALLS,"fileio::read_cds");
				return FALSE;
			}
			msg(DM_NONE,"Remark : %s\n",parser.get_line());
		}
		// Record 4 - Equivalent positions of space group
		if (nsymm > 1)
		{
			offset = 0;
			getline(cdsfile,line);
			for (n=0; n<nsymm-1; n++)
			{
				temp = line.substr(n*20,15);
				//printf(" SymmOp %i '%s'\n",n,temp.c_str());
				// Add symmetry operations to model's own list
				//symmop *sym = targetmodel->add_symmop();
				//sym->set_from_cds(temp);
				offset += 20;
			}
		}
		// Record 5 - Atomic radii (skip)
		if (numatoms > 0)
			if (parser.skip_lines(&cdsfile,1) != 0)
			{
				msg(DM_NONE,"Error reading atomic radius data (record 5) from file.\n");
				dbg_end(DM_CALLS,"fileio::read_cds");
				return FALSE;
			}
		// Record 6 - Atomic positions (basic and symmetry equivalent atoms...)
		// Each line contains three atoms (except potentially the last)
		if (atfor == 2)
		{
			offset = 0;
			// Atomic positions
			for (n=0; n<numatoms; n++)
			{
				// If offset%3 == 0 then we need to read in a new line...
				if (offset%3 == 0)
				{
					offset = 0;
					getline(cdsfile,line);
				}
				// Grab part of the string and parse it
				temp = line.substr(offset*27,26);
				//printf("Atom %3i line = %s\n",n,temp.c_str());
				cds_atom.parse_string(temp.c_str(),PO_DEFAULTS);
				newi = targetmodel->add_atom(cds_atom.get_el());
				// cds_atom->r /= 100000.0;
				newi->set_coords(cds_atom.get_r() / 100000.0);
				//printf("Atom xyz %8.4f %8.4f %8.4f\n",newi->r.x,newi->r.y,newi->r.z);
				offset ++;
			}
			// Symmetry-related atoms 
			for (n=0; n<numsymmatoms; n++)
			{
				// If offset%3 == 0 then we need to read in a new line...
				if (offset%3 == 0)
				{
					offset = 0;
					getline(cdsfile,line);
				}
				// Grab part of the string and parse it
				temp = line.substr(offset*27,26);
				cds_atom.parse_string(temp.c_str(),PO_DEFAULTS);
				newi = targetmodel->add_atom(cds_atom.get_el());
				// cds_atom->r /= 100000.0;
				newi->set_coords(cds_atom.get_r() / 100000.0);
				//printf("SymmAtom xyz %8.4f %8.4f %8.4f\n",newi->r.x,newi->r.y,newi->r.z);
				offset ++;
			}
		}
		// Record 7 - Bonding
		if (nconn > 0)
		{
			// 'offset' contains the number of items read in from the current line
			offset = 100;
			// Two possible formats - natoms+nsymmatoms < 100 ? 40i2 : 26i3,2x
			(numatoms + numsymmatoms) < 100 ? s = 2 : s = 3;
			// First 'numatoms+numsymmatoms' records have implied first atom - id 'n'
			// Our loop works on single integers being read in, so adjust limit (decrease) to account for pairs of atoms
			for (n=0; n<(nconn - (nconn-(numatoms + numsymmatoms))/2); n++)
			{
				// If n >= (natoms + nsymmatoms) then read in atom pairs to bond instead of implying bond partner 'n'
				// Set bondatoms[1] to 'n+1' anyway. It will be reduced by 1 later on...
				bondatoms[1] = n + 1;
				for (m=0; m<(n >= (numatoms + numsymmatoms) ? 2 : 1); m++)
				{
					// If offset is high enough then we need to read in a new line...
					if (offset >= ((numatoms + numsymmatoms) < 100 ? 40 : 26))
					{
						offset = 0;
						getline(cdsfile,line);
					}
					// Grab part of the string and parse it
					bondatoms[m] = atoi(line.substr(offset*s,s).c_str());
					offset ++;
				}
				//printf(" %i - bonding atoms %i and %i\n",n,bondatoms[0],bondatoms[1]);
				// Check for bondatoms[0] = 0 - no more bonds to specify for atom id 'n'
				if (bondatoms[0] != 0) targetmodel->bond_atoms(bondatoms[1]-1,bondatoms[0]-1,BT_SINGLE);
			}
		}
		// Perform post-load operations
		if (prefs.get_pack_on_load())
		{
			atom *i = targetmodel->find_atom(numatoms-1);
			targetmodel->apply_spacegroup_symmops(i);
		}
		if (prefs.get_bond_on_load() == PS_YES) targetmodel->calculate_bonding();
		// Convert fractional coordinates that we've stored into real coordinates
		targetmodel->frac_coords_to_real();
		if (prefs.get_fold_on_load()) targetmodel->fold_all_atoms();
		finalise_model(MF_UNKNOWN,filename);
		// Is there another crystal after this one?
		getline(cdsfile,line);
		if (!cdsfile.eof())
		{
			// Create new model and carry on
			targetmodel = master.add_model();
		}
		else repeat = FALSE;
	} while (repeat);
	cdsfile.close();
	dbg_end(DM_CALLS,"fileio::read_cds");
	return TRUE;
}
