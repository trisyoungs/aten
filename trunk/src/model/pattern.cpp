/*
	*** Model pattern functions
	*** src/model/pattern.cpp
	Copyright T. Youngs 2007,2008

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
#include "classes/pattern.h"
#include "classes/atomaddress.h"
#include "classes/clipboard.h"
#include "base/master.h"
#include "base/elements.h"

// Number of nodes in pattern
int Model::nPatterns()
{
	return patterns_.nItems();
}

// Return the first pattern node of the model
Pattern *Model::patterns()
{
	return patterns_.first();
}

// Return the last pattern node of the model
Pattern *Model::lastPattern()
{
	return patterns_.last();
}

// Return whether the patterns are valid
bool Model::arePatternsValid()
{
	return (patternsPoint_ == logs_[LOG_STRUCTURE] ? TRUE : FALSE);
}

// Return n'th pattern node
Pattern *Model::pattern(int id)
{
	return patterns_[id];
}

// Add Pattern Node
Pattern *Model::addPattern(int mols, int numatoms, const char *patname)
{
	dbgBegin(DM_CALLS,"Model::addPattern");
	// Determine starting atom...
	Pattern *lastp = patterns_.last();
	int start = (lastp == NULL ? 0 : lastp->startAtom() + lastp->nMols() * lastp->nAtoms());
	Pattern *newpnode = patterns_.add();
	newpnode->setParent(this);
	newpnode->setName(patname);
	newpnode->initialise(patterns_.nItems()-1,start,mols,numatoms);
	msg(DM_VERBOSE,"New pattern '%s' added - startatom %i, %i mols, %i atoms per mol.\n",patname,start,mols,numatoms);
	if ((start + mols*numatoms) == atoms_.nItems())
	{
		msg(DM_NONE,"Pattern description completed (spans %i atoms).\n",atoms_.nItems());
		energy.resize(patterns_.nItems());
		// Create representative molecules
		msg(DM_NONE,"Creating representative molecules...");
		createPatternMolecules();
		msg(DM_NONE,"Done.\n");
		// Patterns depend only on the properties / relation of the atoms, and not the positions..
		patternsPoint_ = logs_[LOG_STRUCTURE];
	}
	else if ((start + mols*numatoms) > atoms_.nItems()) msg(DM_NONE,"New pattern '%s' extends %i atoms past number of atoms in owner model.\n",patname,(start + mols*numatoms) - atoms_.nItems());
	dbgEnd(DM_CALLS,"Model::addPattern");
	return newpnode;
}

// Cut pattern
void Model::cutPattern(Pattern *source)
{
	patterns_.cut(source);
}

// Own pattern
void Model::ownPattern(Pattern *source, bool own)
{
	dbgBegin(DM_CALLS,"Model::ownPattern");
	// Set the starting atom from the last pattern in the model's list
	Pattern *p = patterns_.last();
	int start = (p == NULL ? 0 : p->startAtom() + p->nMols() * p->nAtoms());
	// Add the pattern onto the end of the current list
	patterns_.own(source);
	energy.resize(patterns_.nItems());
	// Set startatom and endatom to be coherent with the models current list
	source->setContents(start,-1,-1);
	source->setId(patterns_.nItems()-1);
	//source->set_id(patterns_.nItems()-1);
	if (own) source->setParent(this);
	dbgEnd(DM_CALLS,"Model::ownPattern");
}

// Set 'fixed' propety of patterns
void Model::setPatternsFixed(int upto)
{
	// Set all patterns in the current model to be 'fixed'
	dbgBegin(DM_CALLS,"Model::setPatternsFixed");
	Pattern *p = patterns_.first();
	int count = 0;
	while (p != NULL)
	{
		if (count == upto) break;
		p->setFixed(TRUE);
		p = p->next;
		count ++;
	}
	dbgEnd(DM_CALLS,"Model::setPatternsFixed");
}

// Determine the locality of the supplied atom
Atomaddress Model::locateAtom(Atom *i)
{
	dbgBegin(DM_CALLS,"Model::locateAtom");
	int n, patternno, molno, atomno, id;
	Pattern *p;
	Atomaddress result;
	if (!autocreatePatterns())
	{
		msg(DM_NONE,"Model::locateAtom : No valid pattern available for model.\n");
		dbgEnd(DM_CALLS,"Model::locateAtom");
		return result;
	}
	id = i->id();
	// First, find the pattern the atom is covered by
	patternno = -1;
	p = patterns_.first();
	while (p != NULL)
	{
		if ((id >= p->startAtom()) && (id <= p->endAtom()))
		{
			patternno = p->id();
			break;
		}
		p = p->next;
	}
	if (patternno == -1)
	{
		printf("Fatal error - could not find owner pattern for atom!\n");
		dbgEnd(DM_CALLS,"Model::locateAtom");
		return result;
	}
	// Next, find its molecule id
	id -= p->startAtom();
	molno = id / p->nAtoms();
	// Finally, get the atom offset
	atomno = id % p->nAtoms();
	// Store values, and return
	result.setPattern(p);
	result.setMolecule(molno);
	result.setOffset(atomno);
	dbgEnd(DM_CALLS,"Model::locateAtom");
	return result;
}

// Clar patterns
void Model::clearPatterns()
{
	dbgBegin(DM_CALLS,"Model::clearPatterns");
	patterns_.clear();
	patternsPoint_ = -1;
	expressionPoint_ = -1;
	msg(DM_NONE,"Pattern list cleared for model '%s'.\n",name_.get());
	dbgEnd(DM_CALLS,"Model::clearPatterns");
}

// Autocreate patterns
bool Model::autocreatePatterns()
{
	// Determine the pattern (molecule) layout of the model
	dbgBegin(DM_CALLS,"Model::autocreatePatterns");
	int n, atomid, nsel2, nmols, idi, idj, idoff;
	bool same;
	static Dnchar emp;
	Clipboard patclip;
	emp.createEmpty(1024);
	Pattern *p;
	Refitem<Bond,int> *rb;
	Atom *i, *isel, *clipi;
	// Check current pattern first...
	if (arePatternsValid())
	{
		dbgEnd(DM_CALLS,"Model::autocreatePatterns");
		return TRUE;
	}
	// Delete all old nodes first.
	msg(DM_NONE,"Autodetecting patterns for model '%s'..\n",name_.get());
	patterns_.clear();
	// If there are no atoms in the molecule, exit here.
	if (atoms_.nItems() == 0)
	{
		msg(DM_NONE,"No patterns defined for model '%s' - no atoms present.\n",name_.get());
		patternsPoint_ = logs_[LOG_STRUCTURE];
		dbgEnd(DM_CALLS,"Model::autocreatePatterns");
		return TRUE;
	}
	// To autodetect, we start off at atoms_head in the model, tree-select this atom and copy the selection to the clipboard. Use the clipboard to check subsequent selections, and if its the same just increase the nmols counter by one. If it's different, assume its the start of a new type of molecule and reset the counters.
	atomid = 0;
	nmols = 0;
	i = atoms_.first();
	while (atomid != atoms_.nItems())
	{
		selectNone();
		// Select molecule starting at atom 'i' and calculate fingerprint
		selectTree(i);
		// We insist that the molecule consists of consecutively ordered atoms, otherwise we can't proceed, so count the number of selected
		// atoms in those that we now skip (if != nselected then we must force a 1*N pattern)
		nsel2 = 0;
		atomid += nSelected_;
		//selectionGetEmpirical(emp);
		for (n=0; n<nSelected_; n++)
		{
			if (i->isSelected()) nsel2 ++;
			i = i->next;
		}
		if (nsel2 != nSelected_)
		{
			msg(DM_NONE,"Warning - model cannot be divided into molecules because of non-ordered atoms_.\nPattern for model will be 1*N.\n");
			// Remove any patterns added so far and set values so we create a generic 1*N pattern instead
			patterns_.clear();
			nmols = 0;
			selectAll();
			selectionEmpirical(emp);
			msg(DM_NONE,"Added default pattern: %s\n",emp.get());
			p = addPattern(1,atoms_.nItems(),emp.get());
			break;
		}
		// If this is the first pass (molecule), copy the selection. If not, compare it
		if (nmols == 0)
		{
			patclip.copySelection(this);
			selectionEmpirical(emp);
			nmols = 1;
		}
		else
		{
			// Compare clipboard contents with current selection
			same = TRUE;
			// Check number of atoms first....
			if (nSelected_ != patclip.nAtoms()) same = FALSE;
			else
			{
				/*
				// Atoms
				*/
				clipi = patclip.atoms();
				for (isel = firstSelected(); isel != NULL; isel = isel->nextSelected())
				{
					// Element check
					if (clipi->element() != isel->element())
					{
						same = FALSE;
						break;
					}
					// Fixed forcefield type check
					if (clipi->hasFixedType() != isel->hasFixedType())
					{
						same = FALSE;
						break;
					}
					else if (clipi->hasFixedType())
					{
						// Both have fixed type - make sure types are the same
						if (clipi->type() != isel->type())
						{
							same = FALSE;
							break;
						}
					}
					clipi = clipi->next;
				}
				// Bonding between atoms_...
				idoff = firstSelected()->id();
				if (same) for (isel = firstSelected(); isel != NULL; isel = isel->nextSelected())
				{
					// Convert IDs so they start at zero (i.e. subtract ID of current atom 'i')
					idi = isel->id() - idoff;
					for (rb = isel->bonds(); rb != NULL; rb = rb->next)
					{
						idj = rb->item->partner(isel)->id() - idoff;
						if (idi < idj) continue;
						if (!patclip.hasBond(idi,idj))
						{
							same = FALSE;
							break;
						}
					}
					if (!same) break;
				}
			}
			// If we get to here with same == TRUE then we increase nmols. Otherwise, we create a new pattern.
			if (same) nmols ++;
			else
			{
				// Not the same as the last stored pattern, so store old data and start a new one
				msg(DM_NONE,"New pattern found: %s\n",emp.get());
				p = addPattern(nmols,patclip.nAtoms(),emp.get());
				patclip.copySelection(this);
				selectionEmpirical(emp);
				nmols = 1;
			}
		}
	}
	// Store last pattern data 
	if (nmols != 0)
	{
		msg(DM_NONE,"New pattern found: %s\n",emp.get());
		p = addPattern(nmols,patclip.nAtoms(),emp.get());
	}

	// Deselect all atoms
	selectNone();

	// Patterns depend only on the properties / relation of the atoms, and not the positions..
	patternsPoint_ = logs_[LOG_STRUCTURE];

	dbgEnd(DM_CALLS,"Model::autocreatePatterns");
	return TRUE;
}

// Create representative molecules for patterns
void Model::createPatternMolecules()
{
	dbgBegin(DM_CALLS,"Model::createPatternMolecules");
	Clipboard clip;
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		// Just select the first molecule in the pattern, and copy-paste to the model
		selectNone();
		Atom *i = p->firstAtom();
		for (int n=0; n<p->nAtoms(); n++)
		{
			selectAtom(i);
			i = i->next;
		}
		// Copy selection which now represents one molecule of this pattern
		clip.copySelection(this);
		// Clear the pattern's representative molecule
		p->molecule->clear();
		clip.pasteToModel(p->molecule);
		p->molecule->setName(p->name());
		p->molecule->centre(0.0,0.0,0.0);
		p->molecule->selectNone();
	}
	selectNone();
	dbgEnd(DM_CALLS,"Model::createPatternMolecules");
}

// Get empirical formula of selection
void Model::selectionEmpirical(Dnchar &target)
{
	dbgBegin(DM_CALLS,"Model::selectionEmpirical");
	int elcount[elements.nElements()];
	//string result;
	target.clear();
	int n;
	// Reset element counters
	for (n=0; n<elements.nElements(); n++) elcount[n] = 0;
	Atom *i = atoms_.first();
	while (i != NULL)
	{
		if (i->isSelected()) elcount[i->element()] ++;
		i = i->next;
	}
	// Construct element string
	for (n=elements.nElements()-1; n>0; n--)
		if (elcount[n] != 0) 
		{
			target.cat(elements.symbol(n));
			if (elcount[n] > 1) target.cat(itoa(elcount[n]));
		}
	dbgEnd(DM_CALLS,"Model::selectionEmpirical");
}

// Get atom fingerprint of current selection
void Model::selectionAtomFingerprint(Dnchar &target)
{
	dbgBegin(DM_CALLS,"Model::selectionAtomFingerprint");
	target.clear();
	Atom *i = firstSelected();
	if (i == NULL)
	{
		dbgEnd(DM_CALLS,"Model::selectionAtomFingerprint");
		return;
	}
	int lastel = i->element(), newel;
	int count = 1;
	for (i = i->next; i != NULL; i = i->nextSelected())
	{
		// Check this element against the last. If the last element is the same, increase the counter. If different, append to the string
		newel = i->element();
		if (newel == lastel) count ++;
		else
		{
			target.cat(elements.symbol(i));
			target.cat(itoa(count));
			lastel = newel;
			count = 0;
		}
	}
	// Check for last element chunk
	if (count != 0)
	{
		target.cat(elements.symbol(lastel));
		target.cat(itoa(count));
	}
	dbgEnd(DM_CALLS,"Model::selectionAtomFingerprint");
}

// Get bond fingerprint of current selection
void Model::selectionBondFingerprint(Dnchar &target)
{
	dbgBegin(DM_CALLS,"Model::selectionBondFingerprint");
	target.clear();
	int count = 0, diff;
	Refitem<Bond,int> *ri;
	Atom *i = atoms_.first();
	Atom *j;
	while (i != NULL)
	{
		if (i->isSelected()) 
		{
			ri = i->bonds();
			while (ri != NULL)
			{
				j = ri->item->partner(i);
				diff = j->id() - i->id();
				if (diff > 0)
				{
					target.cat(itoa(count));
					target += '-';
					target.cat(itoa(count + diff));
					target += ' ';
				}
				ri = ri->next;
			}
			count ++;
		}
		i = i->next;
	}
	dbgEnd(DM_CALLS,"Model::selectionBondFingerprint");
}

// Find pattern by name
Pattern *Model::findPattern(const char *s)
{
	dbgBegin(DM_CALLS,"Model::findPattern");
	Pattern *p = NULL;
	for (p = patterns_.first(); p != NULL; p = p->next)
		if (strcmp(p->name(),s) == 0) break;
	if (p == NULL) msg(DM_NONE,"Pattern '%s' does not exist in model '%s'.\n",s,name_.get());
	dbgEnd(DM_CALLS,"Model::findPattern");
	return p;
}

// Charge pattern atom
void Model::chargePatternAtom(Pattern *p, int id, double q)
{
	dbgBegin(DM_CALLS,"Model::chargePatternAtom");
	int n,m,pnatoms;
	Atom *i = p->firstAtom();
	pnatoms = p->nAtoms();
	// Skip on the 'i' pointer so it points to the atom 'id' in the first molecule of the pattern
	for (n=0; n<id; n++) i = i->next;
	i->setCharge(q);
	// Loop over molecules-1, setting charge and then skipping on pnatoms
	for (n=1; n<p->nMols(); n++)
	{
		for (m=0; m<pnatoms; m++) i = i->next;
		i->setCharge(q);
	}
	dbgEnd(DM_CALLS,"Model::chargePatternAtom");
}

// Find pattern for given atom
Pattern *Model::pattern(Atom *i)
{
	dbgBegin(DM_CALLS,"Model::pattern[atom]");
	int id = i->id();
	Pattern *p;
	for (p = patterns_.first(); p != NULL; p = p->next)
		if ((id >= p->startAtom()) && (id <= p->endAtom())) break;
	dbgEnd(DM_CALLS,"Model::pattern[atom]");
	return p;
}

// Print patterns
void Model::printPatterns()
{
	dbgBegin(DM_CALLS,"Model::printPatterns");
	Pattern *p = patterns_.first();
	if (p == NULL) msg(DM_NONE,"No patterns defined for model '%s'.\n",name_.get());
	else
	{
		msg(DM_NONE,"Pattern info for model '%s':\n", name_.get());
		msg(DM_NONE,"  ID  NMols  Starti  Endi    Name            Forcefield\n");
		while (p != NULL)
		{
			msg(DM_NONE,"  %2i  %5i  %6i  %6i  %-16s  %s\n", p->id(), p->nMols(), p->startAtom(), p->endAtom(), p->name(), p->forcefield() ? p->forcefield()->name() : "< Inherited >");
			p = p->next;
		}
	}
	dbgEnd(DM_CALLS,"Model::printPatterns");
}

