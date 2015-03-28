/*
	*** Model pattern functions
	*** src/model/pattern.cpp
	Copyright T. Youngs 2007-2015

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
#include "base/pattern.h"
#include "base/atomaddress.h"
#include "model/clipboard.h"
#include "ff/forcefield.h"

ATEN_USING_NAMESPACE

// Number of nodes in pattern
int Model::nPatterns() const
{
	return patterns_.nItems();
}

// Return the first pattern node of the model
Pattern* Model::patterns() const
{
	return patterns_.first();
}

// Return the last pattern node of the model
Pattern* Model::lastPattern() const
{
	return patterns_.last();
}

// Return whether the patterns are valid
bool Model::arePatternsValid() const
{
	return (patternsPoint_ == log(Log::Structure) ? TRUE : FALSE);
}

// Return n'th pattern node
Pattern* Model::pattern(int id)
{
	return patterns_[id];
}

// Add Pattern Node
Pattern* Model::addPattern(QString patternName, int nMols, int nAtomsPerMol)
{
	Messenger::enter("Model::addPattern");
	// Determine starting atom...
	Pattern* lastp = patterns_.last();
	int start = (lastp == NULL ? 0 : lastp->startAtom() + lastp->nMolecules() * lastp->nAtoms());
	Pattern* newpnode = patterns_.add();
	newpnode->setParent(this);
	newpnode->setName(patternName);
	newpnode->initialise(patterns_.nItems()-1, start, nMols, nAtomsPerMol);
	Messenger::print("New pattern '%s' added - startatom %i, %i mols, %i atoms per mol.", qPrintable(patternName) , start+1, nMols, nAtomsPerMol);
	if ((start + nMols*nAtomsPerMol) == atoms_.nItems())
	{
		Messenger::print("Pattern description completed (spans %i atoms).", atoms_.nItems());
		energy.resize(patterns_.nItems());
		Messenger::print("Done.");
		// Patterns depend only on the properties / relation of the atoms, and not the positions..
		patternsPoint_ = log(Log::Structure);
	}
	else if ((start + nMols*nAtomsPerMol) > atoms_.nItems())
	{
		Messenger::print("New pattern '%s' extends %i atoms past number of atoms in owner model.", qPrintable(patternName), (start + nMols*nAtomsPerMol) - atoms_.nItems());
		Messenger::print("Not added.");
		patterns_.remove(newpnode);
		newpnode = NULL;
	}
	Messenger::exit("Model::addPattern");
	return newpnode;
}

// Cut pattern
void Model::cutPattern(Pattern* source)
{
	patterns_.cut(source);
}

// Own pattern
void Model::ownPattern(Pattern* source, bool own)
{
	Messenger::enter("Model::ownPattern");
	// Set the starting atom from the last pattern in the model's list
	Pattern* p = patterns_.last();
	int start = (p == NULL ? 0 : p->startAtom() + p->nMolecules() * p->nAtoms());
	// Add the pattern onto the end of the current list
	patterns_.own(source);
	energy.resize(patterns_.nItems());
	// Set startatom and endatom to be coherent with the models current list
	source->setContents(start,-1,-1);
	source->setId(patterns_.nItems()-1);
	//source->set_id(patterns_.nItems()-1);
	if (own) source->setParent(this);
	Messenger::exit("Model::ownPattern");
}

// Set 'fixed' propety of patterns
void Model::setPatternsFixed(int upto)
{
	// Set all patterns in the current model to be 'fixed'
	Messenger::enter("Model::setPatternsFixed");
	Pattern* p = patterns_.first();
	int count = 0;
	while (p != NULL)
	{
		if (count == upto) break;
		p->setFixed(TRUE);
		p = p->next;
		count ++;
	}
	Messenger::exit("Model::setPatternsFixed");
}

// Determine the locality of the supplied atom
AtomAddress Model::locateAtom(Atom* i)
{
	Messenger::enter("Model::locateAtom");
	int patternno, molno, atomno, id;
	Pattern* p;
	AtomAddress result;
	if (!createPatterns())
	{
		Messenger::print("Model::locateAtom : No valid pattern available for model.");
		Messenger::exit("Model::locateAtom");
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
		Messenger::exit("Model::locateAtom");
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
	Messenger::exit("Model::locateAtom");
	return result;
}

// Clar patterns
void Model::clearPatterns()
{
	Messenger::enter("Model::clearPatterns");
	patterns_.clear();
	patternsPoint_ = -1;
	expressionPoint_ = -1;
	Messenger::print("Pattern list cleared for model '%s'.", qPrintable(name_));
	Messenger::exit("Model::clearPatterns");
}

// Autocreate patterns
bool Model::createPatterns()
{
	// Determine the pattern (molecule) layout of the model
	Messenger::enter("Model::createPatterns");
	int n, atomid, nsel2, nmols, idi, idj, idoff, count;
	bool same;
	QString empirical;
	Clipboard patclip;
	Pattern* p;
	Refitem<Bond,int>* rb;
	Atom* i, *selectSource;
	ClipAtom* clipi;
	Refitem<Atom,int>* isel;
	
	// Check current pattern first...
	if (arePatternsValid())
	{
		Messenger::exit("Model::createPatterns");
		return TRUE;
	}
	
	// Delete all old nodes first.
	Messenger::print("Autodetecting patterns for model '%s'..", qPrintable(name_));
	patterns_.clear();
	// If there are no atoms in the molecule, exit here.
	if (atoms_.nItems() == 0)
	{
		Messenger::print("No patterns defined for model '%s' - no atoms present.", qPrintable(name_));
		patternsPoint_ = log(Log::Structure);
		Messenger::exit("Model::createPatterns");
		return TRUE;
	}
	
	// To autodetect, we start off at the first atom in the model, tree-select this atom and copy the selection to the clipboard. Use the clipboard to check subsequent selections, and if its the same just increase the nmols counter by one. If it's different, assume its the start of a new type of molecule and reset the counters.
	atomid = 0;
	nmols = 0;
	i = atoms_.first();
	while (atomid != atoms_.nItems())
	{
		selectNone(TRUE);
		
		// Select molecule starting at atom 'i' and calculate fingerprint
		selectTree(i, TRUE);
		selectSource = i;
		
		// We insist that the molecule consists of consecutively ordered atoms, otherwise we can't proceed, so count the number of selected
		// atoms in those that we now skip (if != nselected then we must force a 1*N pattern)
		nsel2 = 0;
		atomid += marked_.nItems();
		//selectionGetEmpirical(emp);
		for (n=0; n<marked_.nItems(); n++)
		{
			if (i->isSelected(TRUE)) nsel2 ++;
			i = i->next;
		}
		if (nsel2 != marked_.nItems())
		{
			Messenger::print("Pattern creation failed because of bad atom ordering or the presence of additional bonds.");
			Messenger::print("Problem occurred in pattern %i whilst selecting from atom %i.", patterns_.nItems()+1, selectSource->id()+1);

			// Remove any patterns added so far
			patterns_.clear();
			nmols = 0;

			Messenger::exit("Model::createPatterns");
			return FALSE;
		}
		// If this is the first pass (molecule), copy the selection. If not, compare it
		if (nmols == 0)
		{
			patclip.copyMarked(this);
			empirical = selectionEmpirical(TRUE);
			nmols = 1;
		}
		else
		{
			// Compare clipboard contents with current selection
			same = TRUE;
			// Check number of atoms first....
			if (marked_.nItems() != patclip.nAtoms()) same = FALSE;
			else
			{
				// Atoms
				clipi = patclip.atoms();
				for (isel = marked_.first(); isel != NULL; isel = isel->next)
				{
					count++;
					
					// Element check
					if (clipi->atom().element() != isel->item->element())
					{
						same = FALSE;
						break;
					}
					
					// Fixed position
					if (clipi->atom().isPositionFixed() != isel->item->isPositionFixed())
					{
						same = FALSE;
						break;
					}

					// Fixed forcefield type check
					if (clipi->atom().hasFixedType() != isel->item->hasFixedType())
					{
						same = FALSE;
						break;
					}
					else if (clipi->atom().hasFixedType())
					{
						// Both have fixed type - make sure types are the same
						if (clipi->atom().type() != isel->item->type())
						{
							same = FALSE;
							break;
						}
					}
					clipi = clipi->next;
				}
				
				// Bonding between atoms (but only if atoms themselves check out)...
				if (same)
				{
					idoff = selection(TRUE)->item->id();
					count = 0;
					for (isel = marked_.first(); isel != NULL; isel = isel->next)
					{
						// Convert IDs so they start at zero (i.e. subtract ID of current atom 'i')
						idi = isel->item->id() - idoff;
						for (rb = isel->item->bonds(); rb != NULL; rb = rb->next)
						{
							idj = rb->item->partner(isel->item)->id() - idoff;
							if (idi < idj) continue;
							++count;
							if (!patclip.hasBond(idi,idj))
							{
								same = FALSE;
								break;
							}
						}
						if (!same) break;
					}
					// Check for difference between number of bonds between marked atoms and clipboard atoms
					if (count != patclip.nBonds()) same = FALSE;
				}
			}

			// If we get to here with same == TRUE then we increase nmols. Otherwise, we create a new pattern.
			if (same) ++nmols;
			else
			{
				// Not the same as the last stored pattern, so store old data and start a new one
				Messenger::print(Messenger::Verbose, "New pattern found: %s", qPrintable(empirical));
				p = addPattern(qPrintable(empirical), nmols, patclip.nAtoms());
				patclip.copyMarked(this);
				empirical = selectionEmpirical(TRUE);
				nmols = 1;
			}
		}
	}
	// Store last pattern data 
	if (nmols != 0)
	{
		Messenger::print(Messenger::Verbose, "New pattern found: %s", qPrintable(empirical));
		p = addPattern(qPrintable(empirical), nmols, patclip.nAtoms());
	}

	// Describe the atoms / rings in the patterns
	describeAtoms();

	// Patterns depend only on the properties / relation of the atoms, and not the positions..
	patternsPoint_ = log(Log::Structure);

	Messenger::exit("Model::createPatterns");
	return TRUE;
}

// Create default pattern
Pattern* Model::createDefaultPattern()
{
	clearPatterns();
	Pattern* p = addPattern("Default", 1, atoms_.nItems());
	return p;
}

// Find pattern by name
Pattern* Model::findPattern(QString name) const
{
	Messenger::enter("Model::findPattern");
	Pattern* p = NULL;
	for (p = patterns_.first(); p != NULL; p = p->next) if (p->name() == name) break;
	if (p == NULL) Messenger::print("Pattern '%s' does not exist in model '%s'.", qPrintable(name), qPrintable(name_));
	Messenger::exit("Model::findPattern");
	return p;
}

// Charge pattern atom
void Model::chargePatternAtom(Pattern* p, int id, double q)
{
	Messenger::enter("Model::chargePatternAtom");
	int n,m,pnatoms;
	Atom* i = p->firstAtom();
	pnatoms = p->nAtoms();
	// Skip on the 'i' pointer so it points to the atom 'id' in the first molecule of the pattern
	for (n=0; n<id; n++) i = i->next;
	atomSetCharge(i, q);
	// Loop over molecules-1, setting charge and then skipping on pnatoms
	for (n=1; n<p->nMolecules(); n++)
	{
		for (m=0; m<pnatoms; m++) i = i->next;
		atomSetCharge(i, q);
	}
	Messenger::exit("Model::chargePatternAtom");
}

// Find pattern for given atom
Pattern* Model::pattern(Atom* i)
{
	Messenger::enter("Model::pattern[atom]");
	int id = i->id();
	Pattern* p;
	for (p = patterns_.first(); p != NULL; p = p->next)
		if ((id >= p->startAtom()) && (id <= p->endAtom())) break;
	Messenger::exit("Model::pattern[atom]");
	return p;
}

// Print patterns
void Model::printPatterns() const
{
	Messenger::enter("Model::printPatterns");
	Pattern* p = patterns_.first();
	if (p == NULL) Messenger::print("No patterns defined for model '%s'.", qPrintable(name_));
	else
	{
		Messenger::print("Pattern info for model '%s':", qPrintable(name_));
		Messenger::print("  ID  NMols  StartId EndId   Name              Forcefield");
		while (p != NULL)
		{
			Messenger::print("  %2i  %-5i  %-6i  %-6i  %-16s  %s", p->id(), p->nMolecules(), p->startAtom(), p->endAtom(), qPrintable(p->name()), p->forcefield() ? qPrintable(p->forcefield()->name()) : "< Inherited >");
			p = p->next;
		}
	}
	Messenger::exit("Model::printPatterns");
}

// Validate current pattern definition in model
bool Model::validatePatterns()
{
	Messenger::enter("Model::validatePatterns");
	// Cycle over patterns, checking atom and bond fingerprints of molecules against the first in each
	// TODO
	Messenger::exit("Model::validatePatterns");
	return TRUE;
}
