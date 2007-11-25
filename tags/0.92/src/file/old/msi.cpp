/*
// MSI file IO routines.
*/

using namespace std;
#include <vector>
#include "classes/forcefield.h"
#include "model/model.h"
#include "file/parse.h"
#include "base/elemap.h"
#include "base/sysfunc.h"

bool fileio::read_msi(const char *filename)
{
	// Read in the data from an MSI file.
	dbg_begin(DM_CALLS,"fileio::read_msi");
	atom *i;
	vector <int> bondi, bondj;
	bool gotcoords, gotelement;
	mat3<double> newcell;
	int success, nbonds, n, temp_id, gotbonds;
	cell_type ct = CT_NONE;
	char keywd[100];
	msg(DM_NONE,"Type   : MSI (Cerius2) Structure\n"); 
	ifstream msifile(filename,ios::in);
	// Awkward filing (for us) - use a temporary array of bond data until all atoms have been read in.
	nbonds = 0;
	do
	{
		success = parser.get_args(&msifile,PO_STRIPBRACKETS);
		if (success == -1) break;
		if (success == 1)
		{
			msg(DM_NONE,"Error reading msi file.\n");
			dbg_end(DM_CALLS,"fileio::read_msi");
			return FALSE;
		}
		// Check for cell definition
		strcpy(keywd,parser.argc(2));
		if (strncmp(keywd,"PeriodicType",12) == 0)
		{	// Set the cell type
			// TODO Other cell types in this format!
			if (strncmp(parser.argc(3),"100",3) == 0) ct = CT_CUBIC;
		}
		else if (strncmp(keywd,"A3",2) == 0) newcell.set(MATX,parser.argd(3),parser.argd(4),parser.argd(5));
		else if (strncmp(keywd,"B3",2) == 0) newcell.set(MATY,parser.argd(3),parser.argd(4),parser.argd(5));
		else if (strncmp(keywd,"C3",2) == 0) newcell.set(MATZ,parser.argd(3),parser.argd(4),parser.argd(5));
		// Atom / Bond keywords
		strcpy(keywd,parser.argc(1));
		if (strncmp(keywd,"Atom",4) == 0)
		{
			// We've found an atom, so read in its data.
			gotcoords = FALSE;
			gotelement = FALSE;
			i = targetmodel->add_atom(0);
			i->tempi = parser.argi(0);
			while ( !(gotcoords*gotelement) )
			{
				if (parser.get_args(&msifile,PO_STRIPBRACKETS) != 0)
				{
					msg(DM_NONE,"Error reading msi atom data.\n");
					dbg_end(DM_CALLS,"fileio::read_msi");
					return FALSE;
				}
				if (strncmp(parser.argc(2),"XYZ",3) == 0)
				{	// Atomic coordinates (in args 4-6)
					i->set_coords(parser.argd(3),parser.argd(4),parser.argd(5));
					gotcoords = TRUE;
				}
				else if (strncmp(parser.argc(2),"ACL",3) == 0)
				{	// Element
					i->set_element(elements.find(parser.argc(4)));
					gotelement = TRUE;
				}
			}
		}
		else if (strncmp(keywd,"Bond",4) == 0)
		{	// We've found a bond, so read it in.
			nbonds ++;
			gotbonds = 0;
			while (gotbonds != 2)
			{
				if (parser.get_args(&msifile,PO_STRIPBRACKETS) != 0)
				{
					msg(DM_NONE,"Error reading msi bond data.\n");
					dbg_end(DM_CALLS,"fileio::read_msi");
					return FALSE;
				}
				if (strncmp(parser.argc(2),"Atom1",5) == 0)
				{
					gotbonds ++;
					bondi.push_back(parser.argi(3));
				}
				else if (strncmp(parser.argc(2),"Atom2",5) == 0)
				{
					gotbonds ++;
					bondj.push_back(parser.argi(3));
				}
			}
		}
	} while (success == 0);
	msifile.close();
	// Set the model's cell axes to those read in earlier
	targetmodel->cell.set(ct,newcell);
	// Now we must set the atom pointers in the bond array.
	for (n=0; n<nbonds; n++)
	{
		// Get the atoms in the atomlist with tempi's the same as those in the msibond node and bond them.
		targetmodel->bond_atoms(targetmodel->find_atom_by_tempi(bondi[n]),targetmodel->find_atom_by_tempi(bondj[n]),BT_SINGLE);
	}
	finalise_model(MF_MSI,filename);
	dbg_end(DM_CALLS,"fileio::read_msi");
	return TRUE;
}
