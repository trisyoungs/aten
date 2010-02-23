/*
	*** Model measurement functions
	*** src/model/measure.cpp
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
#include "base/measurement.h"
#include "model/undoevent.h"
#include "model/undostate.h"

// Return number of angle measurements
int Model::nAngles()
{
	return angles_.nItems();
}

// Return first angle measurement in the list
Measurement *Model::angles()
{
	return angles_.first();
}

// Return nth angle measurement in the list
Measurement *Model::angle(int index)
{
	return angles_[index];
}

// Return number of distance measurements
int Model::nDistances()
{
	return distances_.nItems();
}

// Return first distance measurement in the list
Measurement *Model::distances()
{
	return distances_.first();
}

// Return nth distance measurement in the list
Measurement *Model::distance(int index)
{
	return distances_[index];
}

// Return number of torsion measurements
int Model::nTorsions()
{
	return torsions_.nItems();
}

// Return first torsion measurement in the list
Measurement *Model::torsions()
{
	return torsions_.first();
}

// Return nth torsion measurement in the list
Measurement *Model::torsion(int index)
{
	return torsions_[index];
}

// Clear all measurements
void Model::clearMeasurements()
{
	angles_.clear();
	distances_.clear();
	torsions_.clear();
}

// Add distance measurement
double Model::measureDistance(Atom *i, Atom *j)
{
	// Measure distances between atoms
	msg.enter("Model::measureDistance");
	Measurement *newdist = findDistance(i,j);
	// If this distance isn't currently in the list, add it. Otherwise, delete it
	if (newdist == NULL) addMeasurement(Measurement::Distance,i,j);
	else removeMeasurement(newdist);
	msg.exit("Model::measureDistance");
	return (newdist == NULL ? 0.0 : newdist->value());
}

// Add angle measurement (atom ids)
double Model::measureDistance(int i, int j)
{
	return measureDistance(atom(i), atom(j));
}

// Add angle measurement
double Model::measureAngle(Atom *i, Atom *j, Atom *k)
{
	// Measure angles between atoms
	msg.enter("Model::measureAngle");
	Measurement *newangle = findAngle(i,j,k);
	// Check that this angle isn't already in the list. If it is, delete it
	if (newangle == NULL) addMeasurement(Measurement::Angle,i,j,k);
	else removeMeasurement(newangle);
	msg.exit("Model::measureAngle");
	return (newangle == NULL ? 0.0 : newangle->value());
}

// Add angle measurement (atom ids)
double Model::measureAngle(int i, int j, int k)
{
	return measureAngle(atom(i), atom(j), atom(k));
}

// Add torsion measurement
double Model::measureTorsion(Atom *i, Atom *j, Atom *k, Atom *l)
{
	// Measure torsions between atoms
	msg.enter("Model::measureTorsion");
	Measurement *newtorsion = findTorsion(i,j,k,l);
	// If this torsion isn't in the list, add it. Otherwise, delete it.
	if (newtorsion == NULL) newtorsion = addMeasurement(Measurement::Torsion,i,j,k,l);
	else removeMeasurement(newtorsion);
	msg.exit("Model::measureTorsion");
	return (newtorsion == NULL ? 0.0 : newtorsion->value());
}

// Add torsion measurement (atom ids)
double Model::measureTorsion(int i, int j, int k, int l)
{
	return measureTorsion(atom(i), atom(j), atom(k), atom(l));
}

// Remove specific measurement
void Model::removeMeasurement(Measurement *me)
{
	msg.enter("Model::removeMeasurement");
	Measurement::MeasurementType type = me->type();
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		MeasurementEvent *newchange = new MeasurementEvent;
		Atom **atoms = me->atoms();
		switch (type)
		{
			case (Measurement::Distance):
				newchange->set(FALSE, type, atoms[0]->id(), atoms[1]->id());
				break;
			case (Measurement::Angle):
				newchange->set(FALSE, type, atoms[0]->id(), atoms[1]->id(), atoms[2]->id());
				break;
			case (Measurement::Torsion):
				newchange->set(FALSE, type, atoms[0]->id(), atoms[1]->id(), atoms[2]->id(), atoms[3]->id());
				break;
		}
		recordingState_->addEvent(newchange);
	}
	switch (type)
	{
		case (Measurement::Distance):
			distances_.remove(me);
			break;
		case (Measurement::Angle):
			angles_.remove(me);
			break;
		case (Measurement::Torsion):
			torsions_.remove(me);
			break;
	}
	msg.exit("Model::removeMeasurement");
}

// Clear measurements of specific type
void Model::removeMeasurements(Measurement::MeasurementType gt)
{
	msg.enter("Model::removeMeasurements");
	if (gt == Measurement::Distance) distances_.clear();
	else if (gt == Measurement::Angle) angles_.clear();
	else if (gt == Measurement::Torsion) torsions_.clear();
	msg.exit("Model::removeMeasurements");
}

// Delete measurements involving specific atom
void Model::removeMeasurements(Atom *xatom)
{
	// Search the lists of measurements for the supplied atom, and remove any that use it
	msg.enter("Model::removeMeasurements[atom]");
	Measurement *prevm, *m;
	m = distances_.last();
	while (m != NULL)
	{
		prevm = m->prev;
		if (m->involvesAtom(xatom)) removeMeasurement(m);
		m = prevm;
	}
	m = angles_.last();
	while (m != NULL)
	{
		prevm = m->prev;
		if (m->involvesAtom(xatom)) removeMeasurement(m);
		m = prevm;
	}
	m = torsions_.last();
	while (m != NULL)
	{
		prevm = m->prev;
		if (m->involvesAtom(xatom)) removeMeasurement(m);
		m = prevm;
	}
	msg.exit("Model::removeMeasurements[atom]");
}

// Add Measurement
Measurement *Model::addMeasurement(Measurement::MeasurementType gt, ...)
{
	msg.enter("Model::addMeasurement");
	Atom *i, *atoms[4];
	// Get remaining atoms_...
	int n, nexpected = Measurement::nMeasurementAtoms(gt);
	va_list vars;
	va_start(vars,gt);
	for (n=0; n<nexpected; n++)
	{
		// Get argument from list and check it...
		i = va_arg(vars, Atom*);
		atoms[n] = i;
		if (i == NULL)
		{
			printf("Model::addMeasurement <<<< Not enough atoms supplied - needed %i, got %i >>>>\n",nexpected,n);
			msg.exit("Model::addMeasurement");
			return NULL;
		}
	}
	va_end(vars);
	Measurement *newm;
	if (gt == Measurement::Distance) newm = distances_.add();
	else if (gt == Measurement::Angle) newm = angles_.add();
	else if (gt == Measurement::Torsion) newm = torsions_.add();
	for (n=0; n<nexpected; ++n) newm->setAtom(n, atoms[n]);
	newm->setType(gt);
	newm->calculate(&cell_);
	newm->print();
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		MeasurementEvent *newchange = new MeasurementEvent;
		switch (gt)
		{
			case (Measurement::Distance):
				newchange->set(TRUE, gt, atoms[0]->id(), atoms[1]->id());
				break;
			case (Measurement::Angle):
				newchange->set(TRUE, gt, atoms[0]->id(), atoms[1]->id(), atoms[2]->id());
				break;
			case (Measurement::Torsion):
				newchange->set(TRUE, gt, atoms[0]->id(), atoms[1]->id(), atoms[2]->id(), atoms[3]->id());
				break;
		}
		recordingState_->addEvent(newchange);
	}
	msg.exit("Model::addMeasurement");
	return newm;
}

// Add measurements in selection
void Model::addMeasurementsInSelection(Measurement::MeasurementType gt)
{
	msg.enter("Model::addMeasurementsInSelection");
	Atom *i, *j, *k, *l;
	Refitem<Atom,int> *ri;
	Refitem<Bond,int> *b1, *b2, *b3;
	switch (gt)
	{
		case (Measurement::Distance):
			for (ri = selection(); ri != NULL; ri = ri->next)
			{
				i = ri->item;
				for (b1 = i->bonds(); b1 != NULL; b1 = b1->next)
				{
					j = b1->item->partner(i);
					if (!j->isSelected()) continue;
					if (findDistance(i,j) != NULL) continue;
					if (i->id() < j->id()) addMeasurement(gt,i,j);
				}
			}
			break;
		case (Measurement::Angle):
			for (ri = selection(); ri != NULL; ri = ri->next)
			{
				j = ri->item;
				// Get bonds to this atom and loop over them again
				for (b1 = j->bonds(); b1 != NULL; b1 = b1->next)
				{
					i = b1->item->partner(j);
					if (!i->isSelected()) continue;
					for (b2 = b1->next; b2 != NULL; b2 = b2->next)
					{
						k = b2->item->partner(j);
						if (k->isSelected() && (findAngle(i,j,k) == NULL)) addMeasurement(gt,i,j,k);
					}
				}
			}
			break;
		case (Measurement::Torsion):
			// Find bond j-k where both are selected
			for (ri = selection(); ri != NULL; ri = ri->next)
			{
				j = ri->item;
				for (b1 = j->bonds(); b1 != NULL; b1 = b1->next)
				{
					k = b1->item->partner(j);
					if (k->isSelected() && (k > j))
					{
						// b1 = j-k. Loop over bonds on j (b2) and on k (b3) and find selected pairs
						for (b2 = j->bonds(); b2 != NULL; b2 = b2->next)
						{
							// Find selected atom i in torsion i-j-k-l
							i = b2->item->partner(j);
							if (i->isSelected() && (b2->item != b1->item))
							{
								// Find selected atom l in torsion i-j-k-l
								b3 = k->bonds();
								while (b3 != NULL)
								{
									l = b3->item->partner(k);
									if (l->isSelected() && (b3->item != b1->item))
									{
										// Found four selected atoms forming a torsion
										if (findTorsion(i,j,k,l) == NULL) addMeasurement(gt,i,j,k,l);
									}
									b3 = b3->next;
								}
							}
						}
					}
				}
			}
			break;
	}
	msg.exit("Model::addMeasurementsInSelection");
}

// Find specific distance
Measurement *Model::findDistance(Atom *i, Atom *j)
{
	Measurement *result = NULL;
	for (result = distances_.first(); result != NULL; result = result->next)
	{
		if ((result->atom(0) == i) && (result->atom(1) == j)) break;
		if ((result->atom(0) == j) && (result->atom(1) == i)) break;
	}
	return result;
}

// Find specific angle
Measurement *Model::findAngle(Atom *i, Atom *j, Atom *k)
{
	Measurement *result = NULL;
	for (result = angles_.first(); result != NULL; result = result->next)
	{
		if (result->atom(1) != j) continue;
		if ((result->atom(0) == i) && (result->atom(2) == k)) break;
		if ((result->atom(0) == k) && (result->atom(2) == i)) break;
	}
	return result;
}

// Find specific torsion
Measurement *Model::findTorsion(Atom *i, Atom *j, Atom *k, Atom *l)
{
	Measurement *result = NULL;
	for (result = torsions_.first(); result != NULL; result = result->next)
	{
		if ((result->atom(0) == i) && (result->atom(3) == l))
		{
			if ((result->atom(1) == j) && (result->atom(2) == k)) break;
		}
		if ((result->atom(0) == l) && (result->atom(3) == i))
		{
			if ((result->atom(1) == k) && (result->atom(2) == j)) break;
		}
	}
	return result;
}

// Calculate distance
double Model::distance(Atom *i, Atom *j)
{
	return cell_.distance(i,j);
}

// Calculate distance
double Model::distance(int i, int j)
{
	// Make sure we have a staticatoms array
	Atom **modelatoms = atomArray();
	return cell_.distance(modelatoms[i], modelatoms[j]);
}

// Calculate angle
double Model::angle(Atom *i, Atom *j, Atom *k)
{
	return cell_.angle(i,j,k);
}

// Calculate angle (radians)
double Model::angle(int i, int j, int k)
{
	// Make sure we have a staticatoms array
	Atom **modelatoms = atomArray();
	return cell_.angle(modelatoms[i], modelatoms[j], modelatoms[k]);
}

// Calculate torsion
double Model::torsion(Atom *i, Atom *j, Atom *k, Atom *l)
{
	return cell_.torsion(i,j,k,l);
}

// Calculate torsion (radians)
double Model::torsion(int i, int j, int k, int l)
{
	// Make sure we have a staticatoms array
	Atom **modelatoms = atomArray();
	return cell_.torsion(modelatoms[i], modelatoms[j], modelatoms[k], modelatoms[l]);
}

// Update measurements
void Model::updateMeasurements()
{
	msg.enter("Model::updateMeasurements");
	Measurement *m;
	for (m = distances_.first(); m != NULL; m = m->next) m->calculate(&cell_);
	for (m = angles_.first(); m != NULL; m = m->next) m->calculate(&cell_);
	for (m = torsions_.first(); m != NULL; m = m->next) m->calculate(&cell_);
	msg.exit("Model::updateMeasurements");
}

// List measurements
void Model::listMeasurements()
{
	msg.enter("Model::listMeasurements");
	Measurement *m;
	for (m = distances_.first(); m != NULL; m = m->next) m->print();
	for (m = angles_.first(); m != NULL; m = m->next) m->print();
	for (m = torsions_.first(); m != NULL; m = m->next) m->print();
	msg.exit("Model::listMeasurements");
}
