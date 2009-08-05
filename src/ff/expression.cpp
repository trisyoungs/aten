/*
	*** Expression creation
	*** src/ff/expression.cpp
	Copyright T. Youngs 2007-2009

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

#include "model/model.h"
#include "main/aten.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "classes/forcefieldbound.h"
#include "base/pattern.h"

// Initialise expression for pattern
void Pattern::initExpression(bool vdwOnly)
{
	// Create arrays for storage of FF data for atoms, bonds, angles etc.
	// NBonds can be calculated through a loop over all atoms
	// NAngles can be calculated from atomic nBonds data.
	// NTorsions can be calculated from the bond list and atomic nBonds data.
	msg.enter("Pattern::initExpression");
	Atom *i;
	Refitem<Bond,int> *bref;
	int n, atomId, nBonds, nAngles, nTorsions;
	nBonds = 0;
	nAngles = 0;
	nTorsions = 0;
	// We always create the atom array.
	atoms_.createEmpty(nAtoms_);
	if (vdwOnly)
	{
		noIntramolecular_ = TRUE;
		msg.print("Expression for pattern '%s' contains Atomtype terms only.\n", name_.get());
	}
	else
	{
		for (i = parent_->atoms(); i != NULL; i = i->next)
		{
			atomId = i->id();
			if ((atomId >= startAtom_) && (atomId <= endAtom_))
			{
				// Bond counter
				nBonds += i->nBonds();
				// Angle counter
				for (n=i->nBonds()-1; n>0; n--) nAngles += n;
				// Torsion counter slightly more complicated - need a second loop of bound atoms
				for (bref = i->bonds(); bref != NULL; bref = bref->next)
					nTorsions += (i->nBonds() - 1) * (bref->item->partner(i)->nBonds() - 1);
			}
		}
		// Some totals are double counted, so...
		nBonds /= 2;
		nTorsions /= 2;
		msg.print("Expression for pattern '%s' contains %i bonds, %i angles, and %i torsions.\n", name_.get(), nBonds, nAngles, nTorsions);
		bonds_.createEmpty(nBonds);
		angles_.createEmpty(nAngles);
		torsions_.createEmpty(nTorsions);
	}
	if (conMatrix_ != NULL) msg.print("Pattern::initExpression : Warning - connectivity matrix was already allocated.\n");
	conMatrix_ = new int*[nAtoms_];
	for (n=0; n<nAtoms_; n++) conMatrix_[n] = new int[nAtoms_];
	if (vdwScaleMatrix_ != NULL) msg.print("Pattern::initExpression : Warning - VDW scaling matrix was already allocated.\n");
	vdwScaleMatrix_ = new double*[nAtoms_];
	for (n=0; n<nAtoms_; n++) vdwScaleMatrix_[n] = new double[nAtoms_];
	if (elecScaleMatrix_ != NULL) msg.print("Pattern::initExpression : Warning - electrostatic scaling matrix was already allocated.\n");
	elecScaleMatrix_ = new double*[nAtoms_];
	for (n=0; n<nAtoms_; n++) elecScaleMatrix_[n] = new double[nAtoms_];
	msg.exit("Pattern::initExpression");
}

bool Pattern::fillExpression()
{
	// Fill the energy expression for the pattern.
	// The structure that we create will include a static array of pointers
	// to the original atomic elements, to ease the generation of the expression.
	msg.enter("Pattern::fillExpression");
	Atom *ai, *aj, *ak, *al;
	Refitem<Bond,int> *bref;
	ForcefieldBound *ffb;
	PatternAtom *pa;
	PatternBound *pb;
	Forcefield *ff;
	// Counters for incomplete aspects of the expression
	int iatoms = 0, ibonds = 0, iangles = 0, itorsions = 0;
	incomplete_ = FALSE;
	// Temp vars for type storage
	ForcefieldAtom *ti, *tj, *tk, *tl;
	int count, ii, jj, kk, ll;
	List< ListItem<int> > *bonding;
	bonding = new List< ListItem<int> >[nAtoms_];
	// Clear old unique terms lists
	forcefieldBonds_.clear();
	forcefieldAngles_.clear();
	forcefieldTorsions_.clear();
	forcefieldTypes_.clear();
	// Get forcefield to use - we should be guaranteed to find one at this point, but check anyway...
	ff = (forcefield_ == NULL ? parent_->forcefield() : forcefield_);
	if (ff == NULL) ff = aten.defaultForcefield();
	if (ff == NULL)
	{	
		msg.print("Can't complete expression for pattern '%s' - no forcefield associated to pattern or model, and no default set.\n", name_.get());
		msg.exit("Pattern::fillExpression");
		return FALSE;
	}
	msg.print("Fleshing out expression for %i atoms in pattern '%s'...\n", totalAtoms_, name_.get());
	msg.print("... Using forcefield '%s'...\n", ff->name());
	// Construct the atom list.
	// If any atom has not been assigned a type, we *still* include it in the list
	ai = firstAtom_;
	for (count = 0; count<nAtoms_; ++count)
	{
		if (ai == NULL)
		{
			msg.print("Fatal Error: Fell off end of atom list while assigning types - can't complete expression for pattern '%s'.\n", name_.get());
			msg.exit("Pattern::fillExpression");
			return FALSE;
		}
		if (ai->type() == NULL)
		{
			msg.print("... No FF definition for atom %i (%s).\n", count+1, elements().symbol(ai));
			incomplete_ = TRUE;
			iatoms ++;
		}
		// Set data
		setAtomData(count, ai, ai->type());
		// If the forcefield is rule-based, generate the required parameters first
		if (ff->rules() != Rules::None) ff->generateVdw(ai);
		ai = ai->next;
	}
	// Generate intramolecular terms (if not disabled)
	if (!noIntramolecular_)
	{
		// Construct the bond list.
		// Use the atomic bond lists and convert them, filling in the forcefield data as we go.
		// Add only bonds where id(i) > id(j) to prevent double counting of bonds
		// Also, create the lists of bound atoms here for use by the angle and torsion functions.
		// Again, only add bonds involving atoms in the first molecule of the pattern.
		//for (count=0; count<nAtoms_; count++) bonding[count][0] = 0;
		ai = firstAtom_;
		count = 0;
		for (ii=0; ii<nAtoms_; ii++)
		{
			// Go through the list of bonds to this atom
			bref = ai->bonds();
			while (bref != NULL)
			{
				// Get relative IDs and check if i > j
				aj = bref->item->partner(ai);
				ti = ai->type();
				tj = aj->type();
				jj = aj->id() - startAtom_;
				// Quick check to ensure the bond is within the same molecule...
				if (jj > endAtom_)
				{
					msg.print("!!! Found bond between molecules. Check pattern.\n");
					msg.exit("Pattern::fillExpression");
					return FALSE;
				}
				if (jj > ii)
				{
					bonds_[count]->setAtomId(0,ii);
					bonds_[count]->setAtomId(1,jj);
					// Search for the bond data. If its a rule-based FF and we don't find any matching data,
					// generate it. If its a normal forcefield, flag the incomplete marker.
					ffb = ff->findBond(ti,tj);
					// If we found a match, point to it
					if (ffb != NULL) setBondData(count, ffb);
					else
					{
						// If not a rule-based FF, nullify pointer
						if (ff->rules() == Rules::None) setBondData(count, NULL);
						else
						{
							// Generate the new parameters required
							ffb = ff->generateBond(ai,aj);
							setBondData(count, ffb);
						}
					}
					// Check ffb - if it's still NULL we couldn't find a definition
					if (ffb == NULL)
					{
						msg.print("!!! No FF definition for bond %s-%s.\n", ti->equivalent(), tj->equivalent());
						incomplete_ = TRUE;
						ibonds ++;
					}
					else
					{
						msg.print(Messenger::Verbose,"Bond %s-%s data : %f %f %f %f\n",ti->equivalent(), tj->equivalent(), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3));
					}
					// Update the bonding array counters
					bonding[ii].add()->data = jj;
					bonding[jj].add()->data = ii;
					//bonding[ii][0] ++;
					//bonding[jj][0] ++;
					// Add the bond partner to each of the atom's own lists
					//bonding[ii][bonding[ii][0]] = jj;
					//bonding[jj][bonding[jj][0]] = ii;
					count ++;
				}
				bref = bref->next;
			}
			ai = ai->next;
		}
		if (bonds_.nItems() != count)
		{
			msg.print("...INTERNAL ERROR: expected %i bonds, found %i\n", bonds_.nItems(), count);
			incomplete_ = TRUE;
		}
		else if (ibonds == 0) msg.print("... Found parameters for %i bonds.\n", bonds_.nItems());
		else msg.print("... Missing parameters for %i of %i bonds.\n", ibonds, bonds_.nItems());
		// Construct the angle list.
		// Use the list of bound atoms in the bonding[][] array generated above
		count = 0;
		// Loop over central atoms 'jj'
		for (jj=0; jj<nAtoms_; jj++)
		{
			for (ii=0; ii<bonding[jj].nItems(); ii++)
			{
				for (kk=ii+1; kk<bonding[jj].nItems(); kk++)
				{
					ai = atoms_[bonding[jj][ii]->data]->atom();
					aj = atoms_[jj]->atom();
					ak = atoms_[bonding[jj][kk]->data]->atom();
					ti = ai->type();
					tj = aj->type();
					tk = ak->type();
					angles_[count]->setAtomId(0,bonding[jj][ii]->data);
					angles_[count]->setAtomId(1,jj);
					angles_[count]->setAtomId(2,bonding[jj][kk]->data);
					// Search for the bond data. If its a rule-based FF and we don't find any matching data,
					// generate it. If its a normal forcefield, flag the incomplete marker.
					ffb = ff->findAngle(ti,tj,tk);
					if (ffb != NULL) setAngleData(count, ffb);
					else
					{
						// If not a rule-based FF, nullify pointer
						if (ff->rules() == Rules::None) setAngleData(count, NULL);
						else
						{
							// Generate the new parameters required
							ffb = ff->generateAngle(ai,aj,ak);
							setAngleData(count, ffb);
						}
					}
					// Check ffa and raise warning if NULL
					if (ffb == NULL)
					{
						msg.print("!!! No FF definition for angle %s-%s-%s.\n", ti->equivalent(), tj->equivalent(), tk->equivalent());
						incomplete_ = TRUE;
						iangles ++;
					}
					else
					{
						msg.print(Messenger::Verbose,"Angle %s-%s-%s data : %f %f %f %f\n", ti->equivalent(), tj->equivalent(), tk->equivalent(), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3));
					}
					count ++;
				}
			}
		}
		if (angles_.nItems() != count)
		{
			msg.print("...INTERNAL ERROR: expected %i angles, found %i\n", angles_.nItems(), count);
			incomplete_ = TRUE;
		}
		else if (iangles == 0) msg.print("... Found parameters for %i angles.\n", angles_.nItems());
		else msg.print("... Missing parameters for %i of %i angles.\n", iangles, angles_.nItems());
		// Construct the torsion list.
		// Loop over the bond list and add permutations of the bonding atoms listed for either atom j and k
		count = 0;
		// Loop over the bonds in the molecule as the basis, then we can never count the same torsion twice.
		for (pb = bonds_.first(); pb != NULL; pb = pb->next)
		{
			jj = pb->atomId(0);
			kk = pb->atomId(1);
			// Loop over list of atoms bound to jj
			for (ii=0; ii<bonding[jj].nItems(); ii++)
			{
				// Skip atom kk
				if (bonding[jj][ii]->data == kk) continue;
				// Loop over list of atoms bound to kk
				for (ll=0; ll<bonding[kk].nItems(); ll++)
				{
					// Skip atom jj
					if (bonding[kk][ll]->data == jj) continue;
	
					ai = atoms_[bonding[jj][ii]->data]->atom();
					aj = atoms_[jj]->atom();
					ak = atoms_[kk]->atom();
					al = atoms_[bonding[kk][ll]->data]->atom();
					ti = ai->type();
					tj = aj->type();
					tk = ak->type();
					tl = al->type();
					torsions_[count]->setAtomId(0,bonding[jj][ii]->data);
					torsions_[count]->setAtomId(1,jj);
					torsions_[count]->setAtomId(2,kk);
					torsions_[count]->setAtomId(3,bonding[kk][ll]->data);
	
					// Search for the bond data. If its a rule-based FF and we don't find any matching data,
					// generate it. If its a normal forcefield, flag the incomplete marker.
					ffb = ff->findTorsion(ti,tj,tk,tl);
					if (ffb != NULL) setTorsionData(count, ffb);
					else
					{
						// If not a rule-based FF, nullify pointer
						if (ff->rules() == Rules::None) setTorsionData(count, NULL);
						else
						{
							// Generate the new parameters required
							ffb = ff->generateTorsion(ai,aj,ak,al);
							setTorsionData(count, ffb);
						}
					}
					// Check fft and raise warning if NULL
					if (ffb == NULL)
					{
						msg.print("!!! No FF definition for torsion %s-%s-%s-%s.\n", ti->equivalent(), tj->equivalent(), tk->equivalent(), tl->equivalent());
						incomplete_ = TRUE;
						itorsions ++;
					}
					else
					{
						msg.print(Messenger::Verbose,"Torsion %s-%s-%s-%s data : %f %f %f %f\n", ti->equivalent(), tj->equivalent(), tk->equivalent(), tl->equivalent(), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3));
					}
					count ++;
				}
			}
		}
		if (torsions_.nItems() != count)
		{
			msg.print("...INTERNAL ERROR: expected %i torsions, found %i\n", torsions_.nItems(), count);
			incomplete_ = TRUE;
		}
		else if (itorsions == 0) msg.print("... Found parameters for %i torsions.\n", torsions_.nItems());
		else msg.print("... Missing parameters for %i of %i torsions.\n", itorsions, torsions_.nItems());
	}
	delete bonding;
	// Print out a warning if the expression is incomplete.
	if (incomplete_) msg.print("!!! Expression is incomplete.\n");
	msg.exit("Pattern::fillExpression");
	return (incomplete_ ? FALSE : TRUE);
}

