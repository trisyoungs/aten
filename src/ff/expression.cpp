/*
	*** Expression creation
	*** src/ff/expression.cpp
	Copyright T. Youngs 2007-2010

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

// Create forcefield expression for pattern
bool Pattern::createExpression(bool vdwOnly, bool allowDummy)
{
	// Create arrays for storage of FF data for atoms, bonds, angles etc.
	// NBonds can be calculated through a loop over all atoms
	// NAngles can be calculated from atomic nBonds data.
	// NTorsions can be calculated from the bond list and atomic nBonds data.
	msg.enter("Pattern::createExpression");
	Atom *i;
	Refitem<Bond,int> *bref;
	int atomId, nBonds = 0, nAngles = 0, nTorsions = 0, nImpropers = 0, nUreyBradleys = 0, nDummy;
	Atom *ai, *aj, *ak, *al;
	ForcefieldBound *ffb;
	PatternAtom *ipa[4];
	PatternBound *pb;
	// Counters for incomplete aspects of the expression
	int iatoms = 0, ibonds = 0, iangles = 0, itorsions = 0;
	incomplete_ = FALSE;
	// Temp vars for type storage
	ForcefieldAtom *ti, *tj, *tk, *tl;
	int ii, jj, kk, ll, n, m;
	List< ListItem<int> > *bonding;
	bonding = new List< ListItem<int> >[nAtoms_];
	// Clear old arrays
	atoms_.clear();
	bonds_.clear();
	angles_.clear();
	torsions_.clear();
	// Clear old unique terms lists
	forcefieldBonds_.clear();
	forcefieldAngles_.clear();
	forcefieldTorsions_.clear();
	uniqueForcefieldTypes_.clear();
	// Get forcefield to use - we should be guaranteed to find one at this point, but check anyway...
	Forcefield *ff = (forcefield_ == NULL ? parent_->forcefield() : forcefield_);
	if (ff == NULL) ff = aten.defaultForcefield();
	if (ff == NULL)
	{
		msg.print("Can't complete expression for pattern '%s' - no forcefield associated to pattern or model, and no default set.\n", name_.get());
		msg.exit("Pattern::createExpression");
		return FALSE;
	}
	if (vdwOnly)
	{
		noIntramolecular_ = TRUE;
		msg.print("Expression for pattern '%s' contains Atomtype terms only.\n", name_.get());
	}
	else
	{
		noIntramolecular_ = FALSE;
		// Determine the number of bonds, angles, and torsions to expect in the pattern
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
		msg.print("Basic pattern '%s' contains %i bonds, %i angles, and %i torsions. Impropers and Urey-Bradley terms (if any) will be added later.\n", name_.get(), nBonds, nAngles, nTorsions);
	}
	// Fill the energy expression for the pattern.
	// The structure that we create will include a static array of pointers
	// to the original atomic elements, to ease the generation of the expression.
	msg.print("Fleshing out expression for %i atoms in pattern '%s'...\n", totalAtoms_, name_.get());
	msg.print("... Using forcefield '%s'...\n", ff->name());
	// Construct the atom list.
	// If any atom has not been assigned a type, we *still* include it in the list
	ai = firstAtom_;
	for (n = 0; n<nAtoms_; ++n)
	{
		if (ai == NULL)
		{
			msg.print("Fatal Error: Fell off end of atom list while assigning types - can't complete expression for pattern '%s'.\n", name_.get());
			msg.exit("Pattern::fillExpression");
			return FALSE;
		}
		if (ai->type() == NULL)
		{
			msg.print("!!! No FF type definition for atom %i (%s).\n", n+1, elements().symbol(ai));
			incomplete_ = TRUE;
			iatoms ++;
		}
		// Set data
		addAtomData(ai, ai->type());
		// If the type of vdw interaction is None, attempt to generate it
		if ((ai->type()->vdwForm() == VdwFunctions::None) && (ff->vdwGenerator() != NULL)) ff->generateVdw(ai);
		// Make another check to see that we now have vdw parameters
		if (ai->type()->vdwForm() ==  VdwFunctions::None)
		{
			msg.print("!!! No FF type definition for atom %i (%s).\n", n+1, elements().symbol(ai));
			incomplete_ = TRUE;
			iatoms ++;
		}
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
		ai = firstAtom_;
		nDummy = 0;
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
					msg.exit("Pattern::createExpression");
					return FALSE;
				}
				if (jj > ii)
				{
					// Search for the bond data. If its a rule-based FF and we don't find any matching data,
					// generate it. If its a normal forcefield, flag the incomplete marker.
					ffb = ff->findBond(ti,tj);
					// If we didn't find a match in the forcefield, attempt generation and dummy term addition
					if (ffb == NULL)
					{
						if (ff->bondGenerator() != NULL) ffb = ff->generateBond(ai,aj);
						else if (addDummyTerms_ || allowDummy)
						{
							ffb = createDummyBond(ti,tj);
							++nDummy;
						}
					}
					addBondData(ffb, ii, jj);
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
					bonding[ii].add()->setValue(jj);
					bonding[jj].add()->setValue(ii);
				}
				bref = bref->next;
			}
			ai = ai->next;
		}
		if (bonds_.nItems() != nBonds)
		{
			msg.print("...INTERNAL ERROR: expected %i bonds, found %i\n", nBonds, bonds_.nItems());
			incomplete_ = TRUE;
		}
		else if (bonds_.nItems() == 0) msg.print("... No bonds in model.\n");
		else if (ibonds == 0)
		{
			if (nDummy == 0) msg.print("... Found parameters for %i bonds.\n", bonds_.nItems());
			else msg.print("... Found parameters for %i bonds (%i dummy terms).\n", bonds_.nItems(), nDummy);
		}
		else msg.print("... Missing parameters for %i of %i bonds.\n", ibonds, bonds_.nItems());
		// Construct the angle list.
		// Use the list of bound atoms in the bonding[][] array generated above
		// Loop over central atoms 'jj'
		nDummy = 0;
		for (jj=0; jj<nAtoms_; jj++)
		{
			for (ii=0; ii<bonding[jj].nItems(); ii++)
			{
				for (kk=ii+1; kk<bonding[jj].nItems(); kk++)
				{
					ai = atoms_[bonding[jj][ii]->value()]->atom();
					aj = atoms_[jj]->atom();
					ak = atoms_[bonding[jj][kk]->value()]->atom();
					ti = ai->type();
					tj = aj->type();
					tk = ak->type();
					// Search for the angle data. If its a rule-based FF and we don't find any matching data,
					// generate it. If its a normal forcefield, flag the incomplete marker.
					ffb = ff->findAngle(ti,tj,tk);
					// If we didn't find a match in the forcefield, attempt generation and dummy term addition
					if (ffb == NULL)
					{
						if (ff->angleGenerator() != NULL) ffb = ff->generateAngle(ai,aj,ak);
						else if (addDummyTerms_ || allowDummy)
						{
							ffb = createDummyAngle(ti,tj,tk);
							++nDummy;
						}
					}
					addAngleData(ffb, bonding[jj][ii]->value(), jj, bonding[jj][kk]->value());
					// Check ffb and raise warning if NULL
					if (ffb == NULL)
					{
						msg.print("!!! No FF definition for angle %s-%s-%s.\n", ti->equivalent(), tj->equivalent(), tk->equivalent());
						incomplete_ = TRUE;
						iangles ++;
					}
					else
					{
						msg.print(Messenger::Verbose,"Angle %s-%s-%s data : %f %f %f %f\n", ti->equivalent(), tj->equivalent(), tk->equivalent(), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3));
						// Check here for Urey-Bradley definition involving the same atoms.
						// We don't mind if there isn't one
						ffb = ff->findUreyBradley(ti,tj,tk);
						if (ffb != NULL)
						{
							++nUreyBradleys;
							addBondData(ffb, bonding[jj][ii]->value(), bonding[jj][kk]->value());
						}
					}
				}
			}
		}
		if (angles_.nItems() != nAngles)
		{
			msg.print("...INTERNAL ERROR: expected %i angles, found %i\n", nAngles, angles_.nItems());
			incomplete_ = TRUE;
		}
		else if (angles_.nItems() == 0) msg.print("... No angles in model.\n");
		else if (iangles == 0)
		{
			if (nUreyBradleys == 0)
			{
				if (nDummy == 0) msg.print("... Found parameters for %i angles.\n", angles_.nItems());
				else msg.print("... Found parameters for %i angles (%i dummy terms).\n", bonds_.nItems(), nDummy);
			}
			else if (nDummy == 0) msg.print("... Found parameters for %i angles with %i corresponding Urey-Bradley definitions.\n", nUreyBradleys);
			else msg.print("... Found parameters for %i angles (%i dummy terms) with %i corresponding Urey-Bradley definitions.\n", nDummy, nUreyBradleys);
		}
		else msg.print("... Missing parameters for %i of %i angles.\n", iangles, angles_.nItems());
		// Construct the torsion list.
		// Loop over the bond list and add permutations of the bonding atoms listed for either atom j and k
		// Loop over the bonds in the molecule as the basis, then we can never count the same torsion twice.
		nDummy = 0;
		for (pb = bonds_.first(); pb != NULL; pb = pb->next)
		{
			jj = pb->atomId(0);
			kk = pb->atomId(1);
			// Loop over list of atoms bound to jj
			for (ii=0; ii<bonding[jj].nItems(); ii++)
			{
				// Skip atom kk
				if (bonding[jj][ii]->value() == kk) continue;
				// Loop over list of atoms bound to kk
				for (ll=0; ll<bonding[kk].nItems(); ll++)
				{
					// Skip atom jj
					if (bonding[kk][ll]->value() == jj) continue;
	
					ai = atoms_[bonding[jj][ii]->value()]->atom();
					aj = atoms_[jj]->atom();
					ak = atoms_[kk]->atom();
					al = atoms_[bonding[kk][ll]->value()]->atom();

					// Check for ii == ll (caused by three-membered rings)
					if (ai->id() == al->id())
					{
						msg.print("... Excluded torsion %i-%i-%i-%i because terminal atoms are the same (three-membered ring?) - expected nTorsions reduced from %i to %i...\n", ai->id()+1, aj->id()+1, ak->id()+1, al->id()+1, nTorsions, nTorsions-1);
						nTorsions --;
						continue;
					}

					ti = ai->type();
					tj = aj->type();
					tk = ak->type();
					tl = al->type();
	
					// Search for the torsion data. If its a rule-based FF and we don't find any matching data,
					// generate it. If its a normal forcefield, flag the incomplete marker.
					ffb = ff->findTorsion(ti,tj,tk,tl);
					// If we didn't find a match in the forcefield, attempt generation and dummy term addition
					if (ffb == NULL)
					{
						if (ff->torsionGenerator() != NULL) ffb = ff->generateTorsion(ai,aj,ak,al);
						else if (addDummyTerms_ || allowDummy)
						{
							ffb = createDummyTorsion(ti,tj,tk,tl);
							++nDummy;
						}
					}
					addTorsionData(ffb, bonding[jj][ii]->value(), jj, kk, bonding[kk][ll]->value());
					// Check ffb and raise warning if NULL
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
				}
			}
		}
		if (torsions_.nItems() != nTorsions)
		{
			msg.print("...INTERNAL ERROR: expected %i torsions, found %i\n", nTorsions, torsions_.nItems());
			incomplete_ = TRUE;
		}
		else if (torsions_.nItems() == 0) msg.print("... No torsions in model.\n");
		else if (itorsions == 0)
		{
			if (nDummy == 0) msg.print("... Found parameters for %i torsions.\n", torsions_.nItems());
			else msg.print("... Found parameters for %i torsions (%i dummy terms).\n", torsions_.nItems(), nDummy);
		}
		else msg.print("... Missing parameters for %i of %i torsions.\n", itorsions, torsions_.nItems());
		// Construct improper torsions list
		// Cycle over impropers defined in forcefield and see if the pattern contains those atoms within a certain distance
		nImpropers = 0;
		for (ffb = ff->impropers(); ffb != NULL; ffb = ffb->next)
		{
			// Loop over four atoms in improper definition in turn
			ii = 0;
			for (n=0; n<4; ++n)
			{
				for (ipa[n] = atoms_.first(); ipa[n] != NULL; ipa[n] = ipa[n]->next)
				{
					// Atom cannot have been used before in this improper...
					for (m=0; m<n; ++m) if (ipa[n] == ipa[m]) break;
					if (m != n) continue;
					if (strcmp(ipa[n]->atom()->type()->equivalent(), ffb->typeName(n)) == 0) break;
				}
				// If no match is found, no atoms match this improper so exit
				if (ipa[n] == NULL) break;
				// The atom contained in 'pa' is a match for the typename in the improper, so check
				// its distance from the previous atom.
				if (n > 0)
				{
					double dist = parent_->distance(ipa[n]->atom(), ipa[n-1]->atom());
					if (dist > prefs.maxImproperDist())
					{
						msg.print(Messenger::Verbose, "Atom %i of improper is too far from previous atom (%f A, limit is %f).\n", n+1, dist, prefs.maxImproperDist());
						break;
					}
				}
				ii++;
			}
			// Did we match all four atoms of the improper?
			if (ii != 4) continue;

			// If we get here, then we did, so add this improper to the torsion array
			nImpropers++;
			addTorsionData(ffb, ipa[0]->atom()->id()-startAtom_, ipa[1]->atom()->id()-startAtom_, ipa[2]->atom()->id()-startAtom_, ipa[3]->atom()->id()-startAtom_);
			msg.print(Messenger::Verbose,"Improper %s-%s-%s-%s data : %f %f %f %f\n", ipa[0]->atom()->type()->equivalent(), ipa[1]->atom()->type()->equivalent(), ipa[2]->atom()->type()->equivalent(), ipa[3]->atom()->type()->equivalent(), ffb->parameter(0), ffb->parameter(1), ffb->parameter(2), ffb->parameter(3));

		}
		if (nImpropers > 0) msg.print("... Found parameters for %i impropers.\n", nImpropers);
	}
	delete[] bonding;
	// Print out a warning if the expression is incomplete.
	if (incomplete_) msg.print("!!! Expression is incomplete.\n");
	msg.exit("Pattern::createExpression");
	return (incomplete_ ? FALSE : TRUE);
}

