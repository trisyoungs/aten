/*
	*** Energy expression 
	*** src/energy/expression.cpp
	Copyright T. Youngs 2007
	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "classes/pattern.h"
#include "model/model.h"
#include "classes/forcefield.h"
#include "base/prefs.h"
#include "base/elements.h"

bool model::create_expression()
{
	// This routine should be called before any operation (or series of operations) requiring calculation of energy / forces. Here, we check the validity / existence of an energy expression for the specified model, and create / recreate if necessary.
	dbg_begin(DM_CALLS,"model::create_expression");
	// 0) If the expression is already valid, return
	if (expression_is_valid())
	{
		dbg_end(DM_CALLS,"model::create_expression");
		return TRUE;
	}
	// Reset some variables
	prefs.valid_ewaldauto = FALSE;
	uniquetypes.clear();
	msg(DM_NONE,"Creating expression for model %s...\n",name.get());
	// 1) Assign internal atom type data (hybridisations). [type_all also performs create_pattern()]
	if (!type_all())
	{
		msg(DM_NONE,"Couldn't type atoms.\n");
		dbg_end(DM_CALLS,"model::create_expression");
		return FALSE;
	}
	// 2) Remove old expression data and create new
	pattern *p = patterns.first();
	while (p != NULL)
	{
		p->delete_expression();
		p->init_expression(this);
		if (!p->fill_expression(this)) return FALSE;
		p->create_conmat();
		p = p->next;
	}
	// 3) Check the electrostatic setup for the model
	if (prefs.calc_elec())
	{
		elec_method emodel = prefs.get_electrostatics();
		switch (emodel)
		{
			case (EM_OFF):
				msg(DM_NONE,"Electrostatics are off.\n");
				break;
			case (EM_COULOMB):
				if (cell.get_type() != CT_NONE) msg(DM_NONE,"!!! Coulomb sum requested for periodic model.\n");
				break;
			default: // Ewald - issue warnings, but don't return FALSE
				if (cell.get_type() == CT_NONE)
				{
					msg(DM_NONE,"!!! Ewald sum cannot be used for a non-periodic model.\n");
					//dbg_end(DM_CALLS,"model::create_expression");
					//return FALSE;
				}
				else if (cell.get_type() != CT_CUBIC)
				{
					msg(DM_NONE,"!!! Ewald sum only implemented for cubic cells.\n");
					//dbg_end(DM_CALLS,"model::create_expression");
					//return FALSE;
				}
				break;
		}
	}
	// 4) Create list of unique atom types now in model
	// First, create a list of unique type references
	reflist<ffatom> uniqueref;
	refitem<ffatom> *ri;
	ffatom *ffa;
	for (atom *i = atoms.first(); i != NULL; i = i->next) uniqueref.add_unique(i->get_type());
	// Now, populate the uniquetypes list with copies of these atom types
	for (ri = uniqueref.first(); ri != NULL; ri = ri->next)
	{
		ffa = uniquetypes.add();
		ffa->copy(ri->item);
	}
	expression_point = logs[LOG_STRUCTURE];
	dbg_end(DM_CALLS,"model::create_expression");
	return TRUE;
}

void pattern::init_expression(model *xmodel)
{
	// Create arrays for storage of FF data for atoms, bonds, angles etc.
	// NBonds can be calculated through a loop over all atoms
	// NAngles can be calculated from atomic nbonds data.
	// NTorsions can be calculated from the bond list and atomic nbonds data.
	dbg_begin(DM_CALLS,"pattern::init_expression");
	atom *i, *j;
	refitem<bond> *bref;
	int n, atomid, nbonds, nangles, ntorsions;
	nbonds = 0;
	nangles = 0;
	ntorsions = 0;
	i = xmodel->get_atoms();
	while (i != NULL)
	{
		atomid = i->get_id();
		if ((atomid >= startatom) && (atomid <= endatom))
		{
			// Bond counter
			nbonds += i->get_nbonds();
			// Angle counter
			for (n=i->get_nbonds()-1; n>0; n--) nangles += n;
			// Torsion counter
			// Slightly more complicated - need a second loop of bound atoms
			bref = i->get_bonds();
			while (bref != NULL)
			{
				ntorsions += (i->get_nbonds() - 1) * (bref->item->get_partner(i)->get_nbonds() - 1);
				bref = bref->next;
			}
		}
		i = i->next;
	}
	// Some totals are double counted, so...
	nbonds /= 2;
	ntorsions /= 2;
	msg(DM_NONE,"Expression for pattern contains %i bonds, %i angles, and %i torsions.\n",nbonds,nangles,ntorsions);
	atoms.create_empty(natoms);
	bonds.create_empty(nbonds);
	angles.create_empty(nangles);
	torsions.create_empty(ntorsions);
	if (conmat != NULL) msg(DM_NONE,"pattern::init_expression : Error - connectivity matrix already allocated.\n");
	else
	{
		conmat = new int*[natoms];
		for (n=0; n<natoms; n++) conmat[n] = new int[natoms];
	}
	dbg_end(DM_CALLS,"pattern::init_expression");
}

bool pattern::fill_expression(model *xmodel)
{
	// Fill the energy expression for the pattern.
	// The structure that we create will include a static array of pointers
	// to the original atomic elements, to ease the generation of the expression.
	dbg_begin(DM_CALLS,"pattern::fill_expression");
	atom *ai, *aj, *ak, *al;
	refitem<bond> *bref;
	ffbound *ffb;
	ffparams params;
	patatom *pa;
	patbound *pb;
	forcefield *xff;
	// Counters for incomplete aspects of the expression
	int iatoms = 0, ibonds = 0, iangles = 0, itorsions = 0;
	incomplete = FALSE;
	// Temp vars for type storage
	ffatom *ti, *tj, *tk, *tl;
	// Lists of unique bound atoms (used by angle and torsion generation routines)
	int bonding[natoms][7];
	int count, ii, jj, kk, ll;
	// If there is no specified pattern forcefield, use the parent model's instead
	ff == NULL ? xff = xmodel->get_ff() : xff = ff;
	msg(DM_NONE,"Fleshing out expression for %i atoms in model %s...\n",totalatoms,xmodel->get_name());
	msg(DM_NONE,"... Using forcefield '%s'...\n",xff->get_name());
	// Construct the atom list.
	// If any atom has not been assigned a type, we *still* include it in the list
	ai = firstatom;
	count = 0;
	for (pa = atoms.first(); pa != NULL; pa = pa->next)
	{
		count ++;
		pa->set_atom(ai);
		pa->set_data(ai->get_type());
		if (ai->get_type() == 0)
		{
			msg(DM_NONE,"... No FF definition for atom %i (%s).\n",count+1,elements.symbol(ai));
			incomplete = TRUE;
			iatoms ++;
		}
		// If the forcefield is rule-based, generate the required parameters first
		if (xff->get_rules() != FFR_NORULES) xff->generate_vdw(ai);
		// Point to the data
		//pa->data = pa->type->get_params()->get_data();
		ai = ai->next;
	}
	// Construct the bond list.
	// Use the atomic bond lists and convert them, filling in the forcefield data as we go.
	// Add only bonds where id(i) > id(j) to prevent double counting of bonds
	// Also, create the lists of bound atoms here for use by the angle and torsion functions.
	// Again, only add bonds involving atoms in the first molecule of the pattern.
	for (count=0; count<natoms; count++) bonding[count][0] = 0;
	ai = firstatom;
	count = 0;
	for (ii=0; ii<natoms; ii++)
	{
		// Go through the list of bonds to this atom
		bref = ai->get_bonds();
		while (bref != NULL)
		{
			// Get relative IDs and check if i > j
			aj = bref->item->get_partner(ai);
			ti = ai->get_type();
			tj = aj->get_type();
			jj = aj->get_id() - startatom;
			// Quick check to ensure the bond is within the same molecule...
			if (jj > endatom)
			{
				msg(DM_NONE,"!!! Found bond between molecules. Check pattern.\n");
				dbg_end(DM_CALLS,"pattern::fill_expression");
				return FALSE;
			}
			if (jj > ii)
			{
				bonds[count]->set_atomid(0,ii);
				bonds[count]->set_atomid(1,jj);
				// Search for the bond data. If its a rule-based FF and we don't find any matching data,
				// generate it. If its a normal forcefield, flag the incomplete marker.
				ffb = xff->find_bond(ti,tj);
				// If we found a match, point to it
				if (ffb != NULL) bonds[count]->set_data(ffb);
				else
				{
					// If not a rule-based FF, nullify pointer
					if (xff->get_rules() == FFR_NORULES) bonds[count]->set_data(NULL);
					else
					{
						// Generate the new parameters required
						ffb = xff->generate_bond(ai,aj);
						bonds[count]->set_data(ffb);
					}
				}
				// Check ffb - if it's still NULL we couldn't find a definition
				if (ffb == NULL)
				{
					msg(DM_NONE,"!!! No FF definition for bond %s-%s.\n",ti->get_equiv(),tj->get_equiv());
					incomplete = TRUE;
					ibonds ++;
				}
				else
				{
					params = bonds[count]->get_data()->get_params();
					msg(DM_VERBOSE,"Bond %s-%s data : %f %f %f %f\n",ti->get_equiv(), tj->get_equiv(), params.data[0], params.data[1], params.data[2], params.data[3]);
				}
				// Update the bonding array counters
				bonding[ii][0] ++;
				bonding[jj][0] ++;
				// Add the bond partner to each of the atom's own lists
				bonding[ii][bonding[ii][0]] = jj;
				bonding[jj][bonding[jj][0]] = ii;
				count ++;
			}
			bref = bref->next;
		}
		ai = ai->next;
	}
	if (bonds.size() != count)
	{
		msg(DM_NONE,"...INTERNAL ERROR: expected %i bonds, found %i\n",bonds.size(),count);
		incomplete = TRUE;
	}
	else if (ibonds == 0) msg(DM_NONE,"... Found parameters for %i bonds.\n",bonds.size());
	else msg(DM_NONE,"... Missing parameters for %i of %i bonds.\n",ibonds,bonds.size());
	// Construct the angle list.
	// Use the list of bound atoms in the bonding[][] array generated above
	count = 0;
	// Loop over central atoms 'jj'
	for (jj=0; jj<natoms; jj++)
	{
		for (ii=1; ii<=bonding[jj][0]; ii++)
		{
			for (kk=ii+1; kk<=bonding[jj][0]; kk++)
			{
				ai = atoms[bonding[jj][ii]]->get_atom();
				aj = atoms[jj]->get_atom();
				ak = atoms[bonding[jj][kk]]->get_atom();
				ti = ai->get_type();
				tj = aj->get_type();
				tk = ak->get_type();
				angles[count]->set_atomid(0,bonding[jj][ii]);
				angles[count]->set_atomid(1,jj);
				angles[count]->set_atomid(2,bonding[jj][kk]);
				// Search for the bond data. If its a rule-based FF and we don't find any matching data,
				// generate it. If its a normal forcefield, flag the incomplete marker.
				ffb = xff->find_angle(ti,tj,tk);
				if (ffb != NULL) angles[count]->set_data(ffb);
				else
				{
					// If not a rule-based FF, nullify pointer
					if (xff->get_rules() == FFR_NORULES) angles[count]->set_data(NULL);
					else
					{
						// Generate the new parameters required
						ffb = xff->generate_angle(ai,aj,ak);
						angles[count]->set_data(ffb);
					}
				}
				// Check ffa and raise warning if NULL
				if (ffb == NULL)
				{
					msg(DM_NONE,"!!! No FF definition for angle %s-%s-%s.\n", ti->get_equiv(), tj->get_equiv(), tk->get_equiv());
					incomplete = TRUE;
					iangles ++;
				}
				else
				{
					params = angles[count]->get_data()->get_params();
					msg(DM_VERBOSE,"Angle %s-%s-%s data : %f %f %f %f\n", ti->get_equiv(), tj->get_equiv(), tk->get_equiv(), params.data[0], params.data[1], params.data[2], params.data[3]);
				}
				count ++;
			}
		}
	}
	if (angles.size() != count)
	{
		msg(DM_NONE,"...INTERNAL ERROR: expected %i angles, found %i\n",angles.size(),count);
		incomplete = TRUE;
	}
	else if (iangles == 0) msg(DM_NONE,"... Found parameters for %i angles.\n",angles.size());
	else msg(DM_NONE,"... Missing parameters for %i of %i angles.\n",iangles,angles.size());
	// Construct the torsion list.
	// Loop over the bond list and add permutations of the bonding atoms listed for either atom j and k
	count = 0;
	// Loop over the bonds in the molecule as the basis, then we can never count the same torsion twice.
	for (pb = bonds.first(); pb != NULL; pb = pb->next)
	{
		jj = pb->get_atomid(0);
		kk = pb->get_atomid(1);
		// Loop over list of atoms bound to jj
		for (ii=1; ii<=bonding[jj][0]; ii++)
		{
			// Skip atom kk
			if (bonding[jj][ii] == kk) continue;
			// Loop over list of atoms bound to kk
			for (ll=1; ll<=bonding[kk][0]; ll++)
			{
				// Skip atom jj
				if (bonding[kk][ll] == jj) continue;

				ai = atoms[bonding[jj][ii]]->get_atom();
				aj = atoms[jj]->get_atom();
				ak = atoms[kk]->get_atom();
				al = atoms[bonding[kk][ll]]->get_atom();
				ti = ai->get_type();
				tj = aj->get_type();
				tk = ak->get_type();
				tl = al->get_type();
				torsions[count]->set_atomid(0,bonding[jj][ii]);
				torsions[count]->set_atomid(1,jj);
				torsions[count]->set_atomid(2,kk);
				torsions[count]->set_atomid(3,bonding[kk][ll]);

				// Search for the bond data. If its a rule-based FF and we don't find any matching data,
				// generate it. If its a normal forcefield, flag the incomplete marker.
				ffb = xff->find_torsion(ti,tj,tk,tl);
				if (ffb != NULL) torsions[count]->set_data(ffb);
				else
				{
					// If not a rule-based FF, nullify pointer
					if (xff->get_rules() == FFR_NORULES) torsions[count]->set_data(NULL);
					else
					{
						// Generate the new parameters required
						ffb = xff->generate_torsion(ai,aj,ak,al);
						torsions[count]->set_data(ffb);
					}
				}
				// Check fft and raise warning if NULL
				if (ffb == NULL)
				{
					msg(DM_NONE,"!!! No FF definition for torsion %s-%s-%s-%s.\n", ti->get_equiv(), tj->get_equiv(), tk->get_equiv(), tl->get_equiv());
					incomplete = TRUE;
					itorsions ++;
				}
				else
				{
					params = torsions[count]->get_data()->get_params();
					msg(DM_VERBOSE,"Torsion %s-%s-%s-%s data : %f %f %f %f\n", ti->get_equiv(), tj->get_equiv(), tk->get_equiv(), tl->get_equiv(), params.data[0], params.data[1], params.data[2], params.data[3]);
				}
				count ++;
			}
		}
	}
	if (torsions.size() != count)
	{
		msg(DM_NONE,"...INTERNAL ERROR: expected %i torsions, found %i\n",torsions.size(),count);
		incomplete = TRUE;
	}
	else if (itorsions == 0) msg(DM_NONE,"... Found parameters for %i torsions.\n",torsions.size());
	else msg(DM_NONE,"... Missing parameters for %i of %i torsions.\n",itorsions,torsions.size());
	// Print out a warning if the expression is incomplete.
	if (incomplete) msg(DM_NONE,"!!! Expression is incomplete.\n");
	dbg_end(DM_CALLS,"pattern::fill_expression");
	return (incomplete ? FALSE : TRUE);
}
