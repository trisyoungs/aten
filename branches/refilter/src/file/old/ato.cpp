/*
// EPSR 'ato' file filter.
*/

using namespace std;
#include <string>
#include <vector>
#include "model/model.h"
#include "classes/config.h"
#include "classes/pattern.h"
#include "classes/forcefield.h"
#include "file/parse.h"
#include "file/filter.h"
#include "base/elemap.h"

bool fileio::read_epsrato(const char *filename)
{
	// Read in an EPSR ato coordinate file
	dbg_begin(DM_CALLS,"fileimp::read_epsrato");
	atom *i;
	int nmols,m,n,natoms,nres,nrot,r,count,el;
	double tempd;
	vec3<double> com, atompos;
	bool done;
	vector <string> names;
	msg(DM_NONE,"Type   : EPSR ATO Configuration\n"); 
	ifstream atofile(filename,ios::in);
	count = 0;
	// File header:
	// 1 : nmols, box length, temperature
	if (parser.get_args(&atofile,PO_DEFAULTS) != 0)
	{
		msg(DM_NONE,"Error reading line from ato file.\n");
		dbg_end(DM_CALLS,"fileimp::read_epsrato");
		return FALSE;
	}
	nmols = parser.argi(0);
	tempd = parser.argd(1);
	mat3<double> newcell;
	newcell.set(MATX,tempd,0.0,0.0);
	newcell.set(MATY,0.0,tempd,0.0);
	newcell.set(MATZ,0.0,0.0,tempd);
	targetmodel->cell.set(CT_CUBIC,newcell);
	// 2 : step sizes etc. (ignore)
	if (parser.get_args(&atofile,PO_DEFAULTS) != 0)
	{
		msg(DM_NONE,"Error reading line from ato file.\n");
		dbg_end(DM_CALLS,"fileimp::read_epsrato");
		return FALSE;
	}
	// Molecule/atom specifications are in the form:
	// n  : natoms, comx, comy, comz, phisx, phiy, phiz
	// n+1: atom name 1
	// n+2: x,y,z (offsets from com)
	// n+3: nrestraints, res1, res2... (number of distance restraints, 5 per line)
	// n+4: ...resN-1, resN
	// n+5: nrot (number of defined molecular rotations)
	// n+6: atom1, atom2 (bonds of rotation 'axis')
	// n+7: list of headgroup atoms that are rotated
	for (m=0; m<nmols; m++)
	{
		if (parser.get_args(&atofile,PO_DEFAULTS) != 0)
		{
			msg(DM_NONE,"Error reading line from ato file.\n");
			dbg_end(DM_CALLS,"fileimp::read_epsrato");
			return FALSE;
		}
		natoms = parser.argi(0);
		count += natoms;
		names.resize(count,"");
		com.set(parser.argd(1),parser.argd(2),parser.argd(3));
		for (n=0; n<natoms; n++)
		{
			if (parser.get_args(&atofile,PO_DEFAULTS) != 0)
			{
				msg(DM_NONE,"Error reading atom name %i in molecule %i.\n",n+1,m+1);
				dbg_end(DM_CALLS,"fileimp::read_epsrato");
				return FALSE;
			}
			names[(count-natoms)+n] = parser.argc(0);
			// We will set elements at the end
			if (parser.get_args(&atofile,PO_DEFAULTS) != 0)
			{
				msg(DM_NONE,"Error reading coordinates for atom %i in molecule %i.\n",n+1,m+1);
				dbg_end(DM_CALLS,"fileimp::read_epsrato");
				return FALSE;
			}
			i = targetmodel->add_atom(0);
			atompos.set(parser.argd(0),parser.argd(1),parser.argd(2));
			atompos += com;
			i->set_coords(atompos);
			if (parser.get_args(&atofile,PO_DEFAULTS) != 0)
			{
				msg(DM_NONE,"Error reading number of restraints for atom %i in molecule %i.\n",n+1,m+1);
				dbg_end(DM_CALLS,"fileimp::read_epsrato");
				return FALSE;
			}
			nres = parser.argi(0);
			nres = (nres-1) / 5;
			for (r=0; r<nres; r++)
			{
				if (parser.get_args(&atofile,PO_DEFAULTS) != 0)
				{
					msg(DM_NONE,"Error reading restraint %i on atom %i in molecule %i.\n",r+1,n+1,m+1);
					dbg_end(DM_CALLS,"fileimp::read_epsrato");
					return -1;
				}
			}
		}
		// Discard molecular rotations
		if (parser.get_args(&atofile,PO_DEFAULTS) != 0)
		{
			msg(DM_NONE,"Error reading number of rotations for molecule %i.\n",m+1);
			dbg_end(DM_CALLS,"fileimp::read_epsrato");
			return -1;
		}
		nrot = parser.argi(0);
		for (r=0; r<nrot*3; r++)
		{
			if (parser.get_args(&atofile,PO_DEFAULTS) != 0)
			{
				msg(DM_NONE,"Error reading rotation %i for molecule %i.\n",r+1,m+1);
				dbg_end(DM_CALLS,"fileimp::read_epsrato");
				return -1;
			}
		}
	}
	// Forcefield specification follows, including name to element mappings.
	// Read in until we find don't find an element symbol (i.e. a number).	
	// Convert the names vector as we go.
	done = FALSE;
	do
	{
		if (parser.get_args(&atofile,PO_DEFAULTS) != 0)
		{
			msg(DM_NONE,"Error reading line from ato file.\n");
			dbg_end(DM_CALLS,"fileimp::read_epsrato");
			return -1;
		}
		// Format is : name, symbol, 0
		el = elements.find(parser.argc(1));
		if (el == 0) break;
		msg(DM_NONE,"       : Mapping atom name %s to element %i.\n",parser.argc(0),el);
		for (n=0; n<count; n++) if (names[n] == parser.argc(0)) names[n] == parser.argc(1);
		if (parser.get_args(&atofile,PO_DEFAULTS) != 0)
		{
			msg(DM_NONE,"Error reading line from ato file.\n");
			dbg_end(DM_CALLS,"fileimp::read_epsrato");
			return FALSE;
		}
	} while (!done);
	atofile.close();
	// Set the elements of the atoms
	i = targetmodel->get_atoms();
	count = 0;
	while (i != NULL)
	{
		i->set_element(elements.find(names[count].c_str()));
		count ++;
		i = i->next;
	}
	if (prefs.get_bond_on_load() != PS_NO) targetmodel->calculate_bonding();
	if (prefs.get_fold_on_load()) targetmodel->fold_all_atoms();
	finalise_model(MF_EPSRATO,filename);
	dbg_end(DM_CALLS,"fileimp::read_epsrato");
	return TRUE;
}

bool fileio::write_epsrato(const char *filename, model *sourcemodel)
{
	// Write out an EPSR ato coordinate file
	dbg_begin(DM_CALLS,"fileimp::write_epsrato");
	atom *i, *j;
	char s[129];
	vec3<double> mim_i, com;
	model *mol;
	int nmols,n,m,m1,a1,a2,nrot,r,count,nres;
	double rij;
	atompair *ap;
	bool done;
	forcefield *xff;
	vector <string> names;
	string hg;

	// Use the pattern to define restraints, and forcefield types to assign atom names
	// Use the forcefield types to assign atom type names
	sourcemodel->autocreate_patterns();
	sourcemodel->type_all();
	// Set up a temporary xcfg to hold the atomic coordinates
	config *cfg = new config(sourcemodel,NULL,FALSE,-1);
	sourcemodel->copy_to_config(cfg,FALSE,CFG_R+CFG_Z);
	ofstream atofile(filename,ios::out);
	// Line 1 : nmols, box dimension, temperature
	nmols = 0;	
	pattern *pn = sourcemodel->get_patterns();
	while (pn != NULL)
	{
		nmols += pn->get_nmols();
		pn = pn->next;
	}
	sprintf(s,"%i   %13.6e  %13.6e\n",nmols,sourcemodel->cell.get_lengths().x,480.0);
	atofile << s;
	// Line 2 : Tol, step sizes (intra trans, headgroup rot, mol rot, mol trans), vibrational temp
	sprintf(s,"  %10.3e  %10.3e  %10.3e  %10.3e  %10.3e  %10.3e\n",0.0,2.83,1.0,1.0,0.5,65.0);
	atofile << s;
	// Atom/molecule loop Begins
	i = sourcemodel->get_atoms();
	pn = sourcemodel->get_patterns();
	while (pn != NULL)
	{
		mol = &pn->molecule;
		// Create list of headgroup rotations to write out
		msg(DM_NONE,"Creating headgroup rotations for pattern '%s'...\n",pn->get_name());
		nrot = mol->epsr_rotations.get_nitems();
		if (nrot == 0) hg = "0\n";
		else
		{
			hg = itoa(nrot);
			hg += "\n";
			for (ap = mol->epsr_rotations.get_first(); ap != NULL; ap = ap->next)
			{
				// Deselect all atoms in molecule, select the first atom of the rotation, then
				// perform treeselect on the second atom (first selected atom will act as a barrier)
				mol->deselect_all();
				mol->select_atom(ap->get_i());
				mol->tree_select(ap->get_j());
				mol->deselect_atom(ap->get_i());
				mol->deselect_atom(ap->get_j());
				// Selection now contains just the atoms that form the rotation list, so construct string
				hg += " ROT\n";
				sprintf(s,"%3i%3i\n ",ap->get_i()->get_id()+1,ap->get_j()->get_id()+1);
				hg += s;
				sprintf(s,"%-4i",mol->get_nselected());
				hg += s;
				for (j = mol->get_atoms(); j != NULL; j = j->next)
				{
					if (j->is_selected())
					{
						sprintf(s,"%-4i",j->get_id()+1);
						hg += s;
					}
					j = j->next;
				}
				hg += "\n";
				//printf("nselected = %i\n",mol->get_nselected());
			}
		}
		msg(DM_NONE,"Writing molecules...\n");
		xff = pn->get_ff();
		if (xff == NULL) xff = sourcemodel->get_forcefield();
		if (xff == NULL) printf("No forcefield associated to model - using element names instead of type names...\n");
		int pnatoms = pn->get_natoms();
		for (m1=0; m1<pn->get_nmols(); m1++)
		{
			com = pn->calculate_com(cfg,m1);
			sprintf(s," %5i  %13.6e  %13.6e  %13.6e  %6.4f  %6.4f  %6.4f\n",pnatoms,com.x,com.y,com.z,0.0,0.0,0.0);
			atofile << s;
			for (a1=0; a1<pnatoms; a1++)
			{
				if (xff != NULL) atofile << (i->get_fftype() <= 0 ? " NUL" : xff->get_name(i->get_fftype())) << "\n";
				else atofile << elements.name(i) << "\n";
				// Get atomic coordinates relative to COM.
				mim_i = sourcemodel->cell.mim(i,&com);
				mim_i -= com;
				sprintf(s," %13.6e  %13.6e  %13.6e \n",mim_i.x,mim_i.y,mim_i.z);
				atofile << s;
				// Write list of restraints.
				/* OLD VERSION - Just use mim distances between atoms.
				if (pnatoms > 1)
				{
					atofile << pnatoms;
					for (a2=0; a2<pnatoms; a2++)
					{
						if ((a2%5 == 0) && (a2 != 0)) atofile << "\n";
						mim_i = sourcemodel->cell.mimd(i,&cfg->r[count+a2]);
						rij = mim_i.magnitude();
						sprintf(s," %3i  %13.6e ",a2+1,rij);
						atofile << s;
					}
				}
				else atofile << " 0 ";
				*/
				// NEW VERSION - Use restraint list in pattern model
				nres = mol->get_nrestraints_by_id(a1);
				if (nres == 0) atofile << " 0 ";
				else
				{
					atofile << nres;
					ap = mol->epsr_restraints.get_first();
					count = 0;
					while (ap != NULL)
					{
						// Check if this restraint involves the current atom (a1)
						if ((a1 == ap->get_i()->get_id()) || (a1 == ap->get_j()->get_id()))
						{
							a1 == ap->get_i()->get_id() ? a2 = ap->get_j()->get_id() : a2 = ap->get_i()->get_id();
							sprintf(s," %3i  %13.6e ",a2+1,ap->get_data());
							atofile << s;
							count ++;
							if ((count%5 == 0) && (count != 0)) atofile << "\n";
						}
						ap = ap->next;
					}
				}
				atofile << "\n";
				i = i->next;
			}
			// Write out headgroup rotation data
			atofile << hg;
		}
		pn = pn->next;
	}
	// Write the forcefield info
	msg(DM_NONE,"Writing forcefield data...\n");
	count = 0;
	i = sourcemodel->get_atoms();
	pn = sourcemodel->get_patterns();
	while (pn != NULL)
	{
		xff = pn->get_ff();
		if (xff == NULL) xff = sourcemodel->get_forcefield();
		int pnatoms = pn->get_natoms();
		for (a1=0; a1<pnatoms; a1++)
		{
			printf("Atom %i\n",a1);
			// Check vector list 'names' to see if this ff type has been written out already
			done = FALSE;
			for (n=0; n<count; n++)
				if (names[n] == xff->get_name(i)) done = TRUE;
			if (!done)
			{
				count ++;
				names.resize(count,"");
				names[count-1] = xff->get_name(i);
				sprintf(s," %3s %3s %1i\n",names[count-1].c_str(),elements.symbol(i),0);
				atofile << s;
				sprintf(s," %8.4f  %8.4f  %8.4f  %8.4f  %8.4f\n",xff->get_vdw_param(i,VF_LJ_EPS),xff->get_vdw_param(i,VF_LJ_SIGMA),
					elements.mass(i),0.0,0.0);
				atofile << s;
			}
			i = i->next;
		}
		// Skip the rest of the molecule's atoms (neet to maintain correct i*)
		for (m1=1; m1<pn->get_nmols(); m1++)
			for (a1=0; a1<pnatoms; a1++)
				i = i->next;
		pn = pn->next;
	}
	dbg_end(DM_CALLS,"fileimp::write_epsrato");
	return TRUE;
}
