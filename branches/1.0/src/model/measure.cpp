/*
	*** Model measurement functions
	*** src/model/measure.cpp
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
#include "base/measurement.h"
#include "model/undoevent.h"
#include "model/undostate.h"

// Return first measurement in the list
Measurement *Model::measurements()
{
	return measurements_.first();
}

// Clear all measurements
void Model::clearMeasurements()
{
	measurements_.clear();
}

// Add distance measurement
double Model::measureDistance(Atom *i, Atom *j)
{
	// Measure distances between atoms
	msg.enter("Model::measureDistance");
	Measurement *newdist = findMeasurement(Measurement::Distance,i,j);
	// If this distance isn't currently in the list, add it. Otherwise, delete it
	if (newdist == NULL) addMeasurement(Measurement::Distance,i,j);
	else removeMeasurement(newdist);
	msg.exit("Model::measureDistance");
	return (newdist == NULL ? 0.0 : newdist->value());
}

// Add angle measurement (atom ids)
double Model::measureDistance(int i, int j)
{
	// Measure torsions between atom ids
	msg.enter("Model::measureDistance");
	Measurement *newdist = findMeasurement(Measurement::Distance, atom(i), atom(j));
	// If this torsion isn't in the list, add it. Otherwise, delete it.
	if (newdist == NULL) addMeasurement(Measurement::Distance, atom(i), atom(j));
	else removeMeasurement(newdist);
	msg.exit("Model::measureDistance");
	return (newdist == NULL ? 0.0 : newdist->value());
}

// Add angle measurement
double Model::measureAngle(Atom *i, Atom *j, Atom *k)
{
	// Measure angles between atoms
	msg.enter("Model::measureAngle");
	Measurement *newangle = findMeasurement(Measurement::Angle,i,j,k);
	// Check that this angle isn't already in the list. If it is, delete it
	if (newangle == NULL) addMeasurement(Measurement::Angle,i,j,k);
	else removeMeasurement(newangle);
	msg.exit("Model::measureAngle");
	return (newangle == NULL ? 0.0 : newangle->value());
}

// Add angle measurement (atom ids)
double Model::measureAngle(int i, int j, int k)
{
	// Measure torsions between atom ids
	msg.enter("Model::measureAngle");
	Measurement *newangle = findMeasurement(Measurement::Angle, atom(i), atom(j), atom(k));
	// If this torsion isn't in the list, add it. Otherwise, delete it.
	if (newangle == NULL) addMeasurement(Measurement::Angle, atom(i), atom(j), atom(k));
	else removeMeasurement(newangle);
	msg.exit("Model::measureAngle");
	return (newangle == NULL ? 0.0 : newangle->value());
}

// Add torsion measurement
double Model::measureTorsion(Atom *i, Atom *j, Atom *k, Atom *l)
{
	// Measure torsions between atoms
	msg.enter("Model::measureTorsion");
	Measurement *newtorsion = findMeasurement(Measurement::Torsion,i,j,k,l);
	// If this torsion isn't in the list, add it. Otherwise, delete it.
	if (newtorsion == NULL) newtorsion = addMeasurement(Measurement::Torsion,i,j,k,l);
	else removeMeasurement(newtorsion);
	msg.exit("Model::measureTorsion");
	return (newtorsion == NULL ? 0.0 : newtorsion->value());
}

// Add torsion measurement (atom ids)
double Model::measureTorsion(int i, int j, int k, int l)
{
	// Measure torsions between atom ids
	msg.enter("Model::measureTorsion");
	Measurement *newtorsion = findMeasurement(Measurement::Torsion, atom(i), atom(j), atom(k), atom(l));
	// If this torsion isn't in the list, add it. Otherwise, delete it.
	if (newtorsion == NULL) newtorsion = addMeasurement(Measurement::Torsion, atom(i), atom(j), atom(k), atom(l));
	else removeMeasurement(newtorsion);
	msg.exit("Model::measureTorsion");
	return (newtorsion == NULL ? 0.0 : newtorsion->value());
}

// Remove specific measurement
void Model::removeMeasurement(Measurement *me)
{
	msg.enter("Model::removeMeasurement");
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		MeasurementEvent *newchange = new MeasurementEvent;
		Atom **atoms = me->atoms();
		Measurement::MeasurementType type = me->type();
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
	measurements_.remove(me);
	msg.exit("Model::removeMeasurement");
}

// Clear measurements of specific type
void Model::removeMeasurements(Measurement::MeasurementType gt)
{
	msg.enter("Model::removeMeasurements");
	Measurement *me = measurements_.first(), *meNext;
	while (me != NULL)
	{
		if (me->type() != gt) me = me->next;
		else
		{
			// Store pointer to measurement after this one...
			meNext = me->next;
			removeMeasurement(me);
			me = meNext;
		}
	}
	msg.exit("Model::removeMeasurements");
}

// Delete measurements involving specific atom
void Model::removeMeasurements(Atom *xatom)
{
	// Search the lists of measurements for the supplied atom, and remove any that use it
	msg.enter("Model::removeMeasurements[atom]");
	int n;
	bool remove;
	Measurement *nextm, *m;
	Atom **atoms;
	m = measurements_.first();
	while (m != NULL)
	{
		remove = FALSE;
		atoms = m->atoms();
		for (n=0; n<Measurement::nMeasurementAtoms(m->type()); n++) if (atoms[n] == xatom) remove = TRUE;
		if (remove)
		{
			nextm = m->next;
			removeMeasurement(m);
			m = nextm;
		}
		else m = m->next;
	}
	msg.exit("Model::removeMeasurements[atom]");
}

// Add Measurement
Measurement *Model::addMeasurement(Measurement::MeasurementType gt, Atom *first, ...)
{
	msg.enter("Model::addMeasurement");
	Atom *i, **atoms;
	Measurement *newm = measurements_.add();
	newm->setType(gt);
	newm->setAtom(0, first);
	// Get remaining atoms_...
	int nexpected = Measurement::nMeasurementAtoms(gt);
	va_list vars;
	va_start(vars,first);
	for (int n=1; n<nexpected; n++)
	{
		// Get argument from list and check it...
		i = va_arg(vars, Atom*);
		newm->setAtom(n, i);
		if (i == NULL)
		{
			printf("Model::addMeasurement <<<< Not enough atoms supplied - needed %i, got %i >>>>\n",nexpected,n);
			measurements_.remove(newm);
			newm = NULL;
			break;
		}
	}
	va_end(vars);
	// If we succeeded in adding all the atoms we required, set the value of the measurement
	if (newm != NULL)
	{
		newm->calculate(&cell_);
		newm->print();
		// Add the change to the undo state (if there is one)
		if (recordingState_ != NULL)
		{
			MeasurementEvent *newchange = new MeasurementEvent;
			atoms = newm->atoms();
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
	}
	msg.exit("Model::addMeasurement");
	return newm;
}

// Add measurements in selection
void Model::addMeasurementsInSelection(Measurement::MeasurementType gt)
{
	msg.enter("Model::addMeasurementsInSelection");
	Atom *i, *j, *k, *l;
	Refitem<Bond,int> *b1, *b2, *b3;
	switch (gt)
	{
		case (Measurement::Distance):
			i = firstSelected();
			while (i != NULL)
			{
				b1 = i->bonds();
				while (b1 != NULL)
				{
					j = b1->item->partner(i);
					if (j->isSelected())
						if (findMeasurement(gt,i,j) == NULL)
							if (i->id() < j->id()) addMeasurement(gt,i,j);
					b1 = b1->next;
				}
				i = i->nextSelected();
			}
			break;
		case (Measurement::Angle):
			j = firstSelected();
			while (j != NULL)
			{
				// Get bonds to this atom and loop over them again
				b1 = j->bonds();
				while (b1 != NULL)
				{
					i = b1->item->partner(j);
					if (i->isSelected())
					{
						b2 = b1->next;
						while (b2 != NULL)
						{
							k = b2->item->partner(j);
							if (k->isSelected() && (findMeasurement(gt,i,j,k) == NULL)) addMeasurement(gt,i,j,k);
							b2 = b2->next;
						}
					}
					b1 = b1->next;
				}
				j = j->nextSelected();
			}
			break;
		case (Measurement::Torsion):
			// Find bond j-k where both are selected
			j = firstSelected();
			while (j != NULL)
			{
				b1 = j->bonds();
				while (b1 != NULL)
				{
					k = b1->item->partner(j);
					if (k->isSelected() && (k > j))
					{
						// b1 = j-k. Loop over bonds on j (b2) and on k (b3) and find selected pairs
						b2 = j->bonds();
						while (b2 != NULL)
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
										if (findMeasurement(gt,i,j,k,l) == NULL) addMeasurement(gt,i,j,k,l);
									}
									b3 = b3->next;
								}
							}
							b2 = b2->next;
						}
					}
					b1 = b1->next;
				}
				j = j->nextSelected();
			}
			break;
	}
	msg.exit("Model::addMeasurementsInSelection");
}

// Find Measurement
Measurement *Model::findMeasurement(Measurement::MeasurementType gt,  Atom *first, ...)
{
	msg.enter("Model::findMeasurement");
	Measurement *result, *m;
	int n, matched1, matched2;
	bool proceed;
	Atom *searchatoms[4], **matoms;
	searchatoms[0] = first;
	int nexpected = Measurement::nMeasurementAtoms(gt);
	va_list vars;
	va_start(vars,first);
	proceed = TRUE;
	for (n=1; n<nexpected; n++)
	{
		// Get argument from list and check it...
		searchatoms[n] = va_arg(vars,Atom*);
		if (searchatoms[n] == NULL)
		{
			printf("findMeasurement : Not enough atoms supplied for measurement - needed %i, got %i.\n",nexpected,n);
			proceed = FALSE;
			break;
		}
	}
	va_end(vars);
	// Continue to search for measurement in list if we have enough atoms
	result = NULL;
	if (proceed)
	{
		for (m = measurements_.first(); m != NULL; m = m->next)
		{
			if (gt == m->type())
			{
				matoms = m->atoms();
				// Check atoms supplied (forward and back)
				matched1 = 0;
				matched2 = 0;
				for (n=0; n<nexpected; n++)
				{
					if (searchatoms[n] == matoms[n]) matched1 ++;
					if (searchatoms[nexpected-n] == matoms[n]) matched2 ++;
				}
				if ((matched1 == nexpected) || (matched2 == nexpected))
				{
					result = m;
					break;
				}
			}
			if (result != NULL) break;
		}
	}
	msg.exit("Model::findMeasurement");
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
	for (Measurement *m = measurements_.first(); m != NULL; m = m->next) m->calculate(&cell_);
	msg.exit("Model::updateMeasurements");
}

// List measurements
void Model::listMeasurements()
{
	msg.enter("Model::listMeasurements");
	Atom **matoms;
	for (int mt = Measurement::Distance; mt < Measurement::nMeasurementTypes; mt++)
	{
// 		switch (mt)
// 		{
// 			case (Measurement::Distance):
// 				msg.print("Distances:");
// 				break;
// 			case (Measurement::Angle):
// 				msg.print("Angles:");
// 				break;
// 			case (Measurement::Torsion):
// 				msg.print("Torsions:");
// 				break;
// 		}
		for (Measurement *m = measurements_.first(); m != NULL; m = m->next)
		{
			if (mt != m->type()) continue;
			m->print();
		}
	}
	msg.exit("Model::listMeasurements");
}
