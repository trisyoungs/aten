/*
	*** Model atom functions
	*** src/model/atom.cpp
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
#include "model/undostate.h"
#include "model/undoevent.h"
#include "base/atom.h"
#include "base/pattern.h"
#include "base/elements.h"

// Return the start of the atom list
Atom *Model::atoms() const
{
	return atoms_.first();
}

// Return the number of atoms in the model
int Model::nAtoms() const
{
	return atoms_.nItems();
}

// Add atom
Atom *Model::addAtom(short int newel, Vec3<double> pos, int targetid)
{
	msg.enter("Model::addAtom");
	Atom *newatom;
	if (targetid == -1) newatom = atoms_.add();
	else newatom = atoms_.insert(targetid == 0 ? NULL : atoms_[targetid-1]);
	newatom->setParent(this);
	newatom->setElement(newel);
	newatom->setId(targetid == -1 ? atoms_.nItems() - 1 : targetid);
	if (targetid != -1) renumberAtoms(newatom);
	newatom->r() = pos;
	increaseMass(newel);
	changeLog.add(Log::Structure);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		AtomEvent *newchange = new AtomEvent;
		newchange->set(TRUE, newatom);
		recordingState_->addEvent(newchange);
	}
	msg.exit("Model::addAtom");
	return newatom;
}

// Create a new atom at the Model's current pen position
Atom *Model::addAtomAtPen(short int el, int targetid)
{
	return addAtom(el, penPosition_, targetid);
}

// Add atom copy
Atom *Model::addCopy(Atom *source)
{
	msg.enter("Model::addCopy");
	Atom *newatom = atoms_.add();
	newatom->copy(source);
	newatom->setParent(this);
	newatom->setId(atoms_.nItems() - 1);
	changeLog.add(Log::Structure);
	increaseMass(newatom->element());
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		AtomEvent *newchange = new AtomEvent;
		newchange->set(TRUE, newatom);
		recordingState_->addEvent(newchange);
	}
	msg.exit("Model::addCopy");
	return newatom;
}

// Add atom copy at specified position in list
Atom *Model::addCopy(Atom *afterthis, Atom *source)
{
	msg.enter("Model::addCopy");
	Atom *newatom = atoms_.insert(afterthis);
	//printf("Adding copy after... %li %li\n",afterthis,source);
	newatom->copy(source);
	renumberAtoms(afterthis);
	changeLog.add(Log::Structure);
	increaseMass(newatom->element());
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		AtomEvent *newchange = new AtomEvent;
		newchange->set(TRUE,newatom);
		recordingState_->addEvent(newchange);
	}
	msg.exit("Model::addCopy");
	return newatom;
}

// Remove atom
void Model::removeAtom(Atom *xatom, bool noupdate)
{
	msg.enter("Model::removeAtom");
	// Delete a specific atom (passed as xatom)
	reduceMass(xatom->element());
// 	if (!noupdate) calculateDensity();
	// Renumber the ids of all atoms in the list after this one
	if (!noupdate) for (Atom *i = xatom->next; i != NULL; i = i->next) i->decreaseId();
	if (xatom->isSelected()) deselectAtom(xatom);
	changeLog.add(Log::Structure);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		AtomEvent *newchange = new AtomEvent;
		newchange->set(FALSE,xatom);
		recordingState_->addEvent(newchange);
	}
	atoms_.remove(xatom);
	msg.exit("Model::removeAtom");
}

// Delete Atom
void Model::deleteAtom(Atom *xatom, bool noupdate)
{
	msg.enter("Model::deleteAtom");
	// The atom may be present in other, unassociated lists (e.g. measurements), so we must
	// also check those lists for this atom and remove it.
	if (xatom == NULL) msg.print("No atom to delete.\n");
	else
	{
		// Remove measurements
		removeMeasurements(xatom);
		// Delete All Bonds To Specific Atom
		Refitem<Bond,int> *bref = xatom->bonds();
		while (bref != NULL)
		{
			// Need to detach the bond from both atoms involved
			Bond *b = bref->item;
			Atom *j = b->partner(xatom);
			unbondAtoms(xatom,j,b);
			bref = xatom->bonds();
		}
		// Finally, delete the atom
		removeAtom(xatom, noupdate);
	}
	msg.exit("Model::deleteAtom");
}

// Transmute atom
void Model::transmuteAtom(Atom *i, short int el)
{
	msg.enter("Model::transmuteAtom");
	if (i == NULL) msg.print("No atom to transmute.\n");
	else
	{
		short int oldel = i->element();
		if (oldel != el)
		{
			reduceMass(i->element());
			i->setElement(el);
			increaseMass(i->element());
			changeLog.add(Log::Structure);
			// Add the change to the undo state (if there is one)
			if (recordingState_ != NULL)
			{
				TransmuteEvent *newchange = new TransmuteEvent;
				newchange->set(i->id(),oldel,el);
				recordingState_->addEvent(newchange);
			}
		}
	}
	msg.exit("Model::transmuteAtom");
}

// Return (and autocreate if necessary) the static atoms array
Atom **Model::atomArray()
{
	return atoms_.array();
}

// Clear atoms
void Model::clearAtoms()
{
	msg.enter("Model::clearAtoms");
	Atom *i = atoms_.first();
	while (i != NULL)
	{
		deleteAtom(i);
		i = atoms_.first();
	}
	msg.exit("Model::clearAtoms");
}

// Find atom
Atom *Model::findAtom(int id)
{
	// Find an atom according to its internal id (useful when atom ids may have been set differently by import filters)
	Atom *i;
	for (i = atoms_.first(); i != NULL; i = i->next) if (id == i->id()) break;
	if (i == NULL) msg.print("Warning - No atom with internal id %i exists in the current model.\n", id);
	return i;
}

// Find atom by tempi
Atom *Model::findAtomByTempi(int tempi)
{
	// Find an atom according to its tempi value
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->tempi == tempi) return i;
	return NULL;
}

// Return the list index of the specified atom
int Model::atomIndex(Atom *i) const
{
	return atoms_.indexOf(i);
}

// Renumber Atoms
void Model::renumberAtoms(Atom *from)
{
	msg.enter("Model::renumberAtoms");
	static int count;
	static Atom *i;
	if (from == NULL)
	{
		count = 0;
		i = atoms_.first();
	}
	else
	{
		count = from->id() + 1;
		i = from->next;
	}
	for (i = i; i != NULL; i = i->next)
	{
		i->setId(count);
		count ++;
	}
	msg.exit("Model::renumberAtoms");
}

// Return atom 'n' in the model
Atom *Model::atom(int n)
{
	msg.enter("Model::atom");
	// Check range first
	if ((n < 0) || (n >= atoms_.nItems()))
	{
		msg.print("Atom id '%i' is out of range for model '%s'.\n", n+1, name_.get());
		msg.exit("Model::atom");
		return NULL;
	}
	msg.exit("Model::atom");
	return atoms_[n];
}

// Reset forces on all atoms
void Model::zeroForces()
{
	msg.enter("Model::zeroForces");
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) i->f().zero();
	msg.exit("Model::zeroForces");
}

// Reset forces on all fixed atoms
void Model::zeroForcesFixed()
{
	msg.enter("Model::zeroForcesFixed");
	Atom *i;
	// First, apply pattern-designated fixes
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next) if (p->areAtomsFixed())
	{
		i = p->firstAtom();
		for (int n=0; n<p->totalAtoms(); ++n) { i->f().zero(); i = i->next; }
	}
	// Next, apply specific atom fixes
	for (i = atoms_.first(); i != NULL; i = i->next) if (i->isPositionFixed()) i->f().zero();
	msg.exit("Model::zeroForcesFixed");
}

// Set visibility of specified atom
void Model::setHidden(Atom *i, bool hidden)
{
	if (i->isHidden() != hidden)
	{
		i->setHidden(hidden);
		changeLog.add(Log::Visual);
		// Add the change to the undo state (if there is one)
		if (recordingState_ != NULL)
		{
			HideEvent *newchange = new HideEvent;
			newchange->set(hidden, i->id());
			recordingState_->addEvent(newchange);
		}
	}
}

// Normalise forces
void Model::normaliseForces(double norm)
{
	// 'Normalise' the forces in linecfg such that the largest force is equal to the maximum cartesian step size
	msg.enter("Model::normaliseForces");
	double maxfrc;
	static Vec3<double> f;
	Atom **modelatoms = atomArray();
	int i;
	// Find the largest force
	maxfrc = 0.0;
	for (i=0; i<atoms_.nItems(); i++)
	{
		f = modelatoms[i]->f();
		if (fabs(f.x) > maxfrc) maxfrc = fabs(f.x);
		if (fabs(f.y) > maxfrc) maxfrc = fabs(f.y);
		if (fabs(f.z) > maxfrc) maxfrc = fabs(f.z);
	}
	// Normalise with respect to this force
	maxfrc *= norm;
	for (i=0; i<atoms_.nItems(); i++) modelatoms[i]->f() /= maxfrc;
	msg.exit("Model::normaliseForces");
}

// Move specified atom (channel for undo/redo)
void Model::translateAtom(Atom *target, Vec3<double> delta)
{
	target->r() += delta;
	changeLog.add(Log::Coordinates);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		TranslateEvent *newchange = new TranslateEvent;
		newchange->set(target->id(), delta);
		recordingState_->addEvent(newchange);
	}
}

// Position specified atom (channel for undo/redo)
void Model::positionAtom(Atom *target, Vec3<double> newr)
{
	static Vec3<double> delta;
	delta = newr - target->r();
	target->r() = newr;
	changeLog.add(Log::Coordinates);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		TranslateEvent *newchange = new TranslateEvent;
		newchange->set(target->id(), delta);
		recordingState_->addEvent(newchange);
	}
}

// Set charge of specified atom
void Model::chargeAtom(Atom *target, double q)
{
	double oldcharge = target->charge();
	target->setCharge(q);
	changeLog.add(Log::Coordinates);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		ChargeEvent *newchange = new ChargeEvent;
		newchange->set(target->id(), oldcharge, q);
		recordingState_->addEvent(newchange);
	}
}

// Return total bond order penalty of atoms in the model
int Model::totalBondOrderPenalty() const
{
	int result = 0;
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) result += elements().bondOrderPenalty(i, i->totalBondOrder()/2);
	return result;
}

// Count bonds of specific type
int Model::countBondsToAtom(Atom *i, Bond::BondType type)
{
	msg.enter("Model::countBondsToAtom");
	int count = 0;
	for (Refitem<Bond,int> *bref = i->bonds(); bref != NULL; bref = bref->next)
		if (bref->item->order() == type) count ++;
	msg.exit("Model::countBondsToAtom");
	return count;
}

// Set style of individual atom
void Model::styleAtom(Atom *i, Atom::DrawStyle ds)
{
	// Sets all atoms currently selected to have the drawing style specified
	Atom::DrawStyle oldstyle = i->style();
	i->setStyle(ds);
	changeLog.add(Log::Visual);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		StyleEvent *newchange = new StyleEvent;
		newchange->set(i->id(), oldstyle, ds);
		recordingState_->addEvent(newchange);
	}
}

// Set selection style
void Model::styleSelection(Atom::DrawStyle ds)
{
	// Sets all atoms currently selected to have the drawing style specified
	for (Atom *i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) styleAtom(i, ds);
}

// Return the total mass of atoms
double Model::mass() const
{
	return mass_;
}

// Reduce the mass (and unknown element count) of the model
void Model::reduceMass(int element)
{
	mass_ -= elements().atomicMass(element);
	if (mass_ < 0.0) mass_ = 0.0;
	if (element == 0) --nUnknownAtoms_;
	// Recalculate density since mass has changed
	calculateDensity();
}

// Increasethe mass (and unknown element count) of the model
void Model::increaseMass(int element)
{
	mass_ += elements().atomicMass(element);
	if (element == 0) ++nUnknownAtoms_;
	// Recalculate density since mass has changed
	calculateDensity();
}

// Calculate mass
void Model::calculateMass()
{
	// Recalculate the mass of the atoms in the model.
	msg.enter("Model::calculateMass");
	mass_ = 0.0;
	nUnknownAtoms_ = 0;
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		mass_ += elements().atomicMass(i);
		if (i->element() == 0) ++nUnknownAtoms_;
	}
	calculateDensity();
	msg.exit("Model::calculateMass");
}

// Return number of unknown atoms in the model
int Model::nUnknownAtoms()
{
	return nUnknownAtoms_;
}
