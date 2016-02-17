/*
	*** Model measurement functions
	*** src/model/measure.cpp
	Copyright T. Youngs 2007-2016

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

ATEN_USING_NAMESPACE

// Return number of angle measurements
int Model::nAngleMeasurements() const
{
	return angleMeasurements_.nItems();
}

// Return first angle measurement in the list
Measurement* Model::angleMeasurements() const
{
	return angleMeasurements_.first();
}

// Return nth angle measurement in the list
Measurement* Model::angleMeasurement(int index)
{
	return angleMeasurements_[index];
}

// Return number of distance measurements
int Model::nDistanceMeasurements() const
{
	return distanceMeasurements_.nItems();
}

// Return first distance measurement in the list
Measurement* Model::distanceMeasurements() const
{
	return distanceMeasurements_.first();
}

// Return nth distance measurement in the list
Measurement* Model::distanceMeasurement(int index)
{
	return distanceMeasurements_[index];
}

// Return number of torsion measurements
int Model::nTorsionMeasurements() const
{
	return torsionMeasurements_.nItems();
}

// Return first torsion measurement in the list
Measurement* Model::torsionMeasurements() const
{
	return torsionMeasurements_.first();
}

// Return nth torsion measurement in the list
Measurement* Model::torsionMeasurement(int index)
{
	return torsionMeasurements_[index];
}

// Clear all measurements
void Model::clearMeasurements()
{
	angleMeasurements_.clear();
	distanceMeasurements_.clear();
	torsionMeasurements_.clear();
	logChange(Log::Labels);
}

// Add distance measurement
double Model::addDistanceMeasurement(Atom* i, Atom* j, bool quiet)
{
	// Measure distances between atoms
	Messenger::enter("Model::addDistanceMeasurement");
	double result = 0.0;
	Measurement* newdist = findDistanceMeasurement(i,j);

	// If this distance isn't currently in the list, add it. Otherwise, delete it
	if (newdist == NULL)
	{
		newdist = addMeasurement(Measurement::DistanceMeasurement,i,j);
		if (!quiet) newdist->print();
		result = newdist->value();
	}
	else removeMeasurement(newdist);

	Messenger::exit("Model::addDistanceMeasurement");
	return result;
}

// Add angle measurement (atom ids)
double Model::addDistanceMeasurement(int i, int j, bool quiet)
{
	return addDistanceMeasurement(atom(i), atom(j), quiet);
}

// Add angle measurement
double Model::addAngleMeasurement(Atom* i, Atom* j, Atom* k, bool quiet)
{
	// Measure angles between atoms
	Messenger::enter("Model::addAngleMeasurement");
	double result = 0.0;
	Measurement* newangle = findAngleMeasurement(i,j,k);

	// Check that this angle isn't already in the list. If it is, delete it
	if (newangle == NULL)
	{
		newangle = addMeasurement(Measurement::AngleMeasurement,i,j,k);
		if (!quiet) newangle->print();
		result = newangle->value();
	}
	else removeMeasurement(newangle);

	Messenger::exit("Model::addAngleMeasurement");
	return result;
}

// Add angle measurement (atom ids)
double Model::addAngleMeasurement(int i, int j, int k, bool quiet)
{
	return addAngleMeasurement(atom(i), atom(j), atom(k), quiet);
}

// Add torsion measurement
double Model::addTorsionMeasurement(Atom* i, Atom* j, Atom* k, Atom* l, bool quiet)
{
	// Measure torsions between atoms
	Messenger::enter("Model::addTorsionMeasurement");
	double result = 0.0;
	Measurement* newtorsion = findTorsionMeasurement(i,j,k,l);

	// If this torsion isn't in the list, add it. Otherwise, delete it.
	if (newtorsion == NULL)
	{
		newtorsion = addMeasurement(Measurement::TorsionMeasurement,i,j,k,l);
		if (!quiet) newtorsion->print();
		result = newtorsion->value();
	}
	else removeMeasurement(newtorsion);

	Messenger::exit("Model::addTorsionMeasurement");
	return result;
}

// Add torsion measurement (atom ids)
double Model::addTorsionMeasurement(int i, int j, int k, int l, bool quiet)
{
	return addTorsionMeasurement(atom(i), atom(j), atom(k), atom(l), quiet);
}

// Remove specific measurement
void Model::removeMeasurement(Measurement* me)
{
	Messenger::enter("Model::removeMeasurement");
	Measurement::MeasurementType type = me->type();

	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		MeasurementEvent* newchange = new MeasurementEvent;
		Atom** atoms = me->atoms();
		switch (type)
		{
			case (Measurement::DistanceMeasurement):
				newchange->set(false, type, atoms[0]->id(), atoms[1]->id());
				break;
			case (Measurement::AngleMeasurement):
				newchange->set(false, type, atoms[0]->id(), atoms[1]->id(), atoms[2]->id());
				break;
			case (Measurement::TorsionMeasurement):
				newchange->set(false, type, atoms[0]->id(), atoms[1]->id(), atoms[2]->id(), atoms[3]->id());
				break;
			default:
				break;
		}
		recordingState_->addEvent(newchange);
	}

	switch (type)
	{
		case (Measurement::DistanceMeasurement):
			distanceMeasurements_.remove(me);
			break;
		case (Measurement::AngleMeasurement):
			angleMeasurements_.remove(me);
			break;
		case (Measurement::TorsionMeasurement):
			torsionMeasurements_.remove(me);
			break;
		default:
			break;
	}

	logChange(Log::Labels);

	Messenger::exit("Model::removeMeasurement");
}

// Clear measurements of specific type
void Model::removeMeasurements(Measurement::MeasurementType gt)
{
	Messenger::enter("Model::removeMeasurements");

	if (gt == Measurement::DistanceMeasurement) distanceMeasurements_.clear();
	else if (gt == Measurement::AngleMeasurement) angleMeasurements_.clear();
	else if (gt == Measurement::TorsionMeasurement) torsionMeasurements_.clear();

	logChange(Log::Labels);

	Messenger::exit("Model::removeMeasurements");
}

// Delete measurements involving specific atom
void Model::removeMeasurements(Atom* xatom)
{
	// Search the lists of measurements for the supplied atom, and remove any that use it
	Messenger::enter("Model::removeMeasurements[atom]");
	Measurement* prevm, *m;
	m = distanceMeasurements_.last();
	while (m != NULL)
	{
		prevm = m->prev;
		if (m->involvesAtom(xatom)) removeMeasurement(m);
		m = prevm;
	}
	m = angleMeasurements_.last();
	while (m != NULL)
	{
		prevm = m->prev;
		if (m->involvesAtom(xatom)) removeMeasurement(m);
		m = prevm;
	}
	m = torsionMeasurements_.last();
	while (m != NULL)
	{
		prevm = m->prev;
		if (m->involvesAtom(xatom)) removeMeasurement(m);
		m = prevm;
	}

	logChange(Log::Labels);

	Messenger::exit("Model::removeMeasurements[atom]");
}

// Add Measurement
Measurement* Model::addMeasurement(Measurement::MeasurementType gt, ...)
{
	Messenger::enter("Model::addMeasurement");
	Atom* i, *atoms[4];

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
			Messenger::exit("Model::addMeasurement");
			return NULL;
		}
	}
	va_end(vars);
	Measurement* newm = NULL;
	if (gt == Measurement::DistanceMeasurement) newm = distanceMeasurements_.add();
	else if (gt == Measurement::AngleMeasurement) newm = angleMeasurements_.add();
	else if (gt == Measurement::TorsionMeasurement) newm = torsionMeasurements_.add();
	for (n=0; n<nexpected; ++n) newm->setAtom(n, atoms[n]);
	newm->setType(gt);
	newm->calculate(&cell_);

	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		MeasurementEvent* newchange = new MeasurementEvent;
		switch (gt)
		{
			case (Measurement::DistanceMeasurement):
				newchange->set(true, gt, atoms[0]->id(), atoms[1]->id());
				break;
			case (Measurement::AngleMeasurement):
				newchange->set(true, gt, atoms[0]->id(), atoms[1]->id(), atoms[2]->id());
				break;
			case (Measurement::TorsionMeasurement):
				newchange->set(true, gt, atoms[0]->id(), atoms[1]->id(), atoms[2]->id(), atoms[3]->id());
				break;
			default:
				break;
		}
		recordingState_->addEvent(newchange);
	}

	logChange(Log::Labels);

	Messenger::exit("Model::addMeasurement");
	return newm;
}

// Add measurements in selection
void Model::addMeasurementsInSelection(Measurement::MeasurementType gt)
{
	Messenger::enter("Model::addMeasurementsInSelection");
	Atom* i, *j, *k, *l;
	RefListItem<Atom,int>* ri;
	RefListItem<Bond,int>* b1, *b2, *b3;
	switch (gt)
	{
		case (Measurement::DistanceMeasurement):
			for (ri = selection(); ri != NULL; ri = ri->next)
			{
				i = ri->item;
				for (b1 = i->bonds(); b1 != NULL; b1 = b1->next)
				{
					j = b1->item->partner(i);
					if (!j->isSelected()) continue;
					if (findDistanceMeasurement(i,j) != NULL) continue;
					if (i->id() < j->id()) addMeasurement(gt,i,j);
				}
			}
			break;
		case (Measurement::AngleMeasurement):
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
						if (k->isSelected() && (findAngleMeasurement(i,j,k) == NULL)) addMeasurement(gt,i,j,k);
					}
				}
			}
			break;
		case (Measurement::TorsionMeasurement):
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
										if (findTorsionMeasurement(i,j,k,l) == NULL) addMeasurement(gt,i,j,k,l);
									}
									b3 = b3->next;
								}
							}
						}
					}
				}
			}
			break;
		default:
			break;
	}
	logChange(Log::Labels);
	Messenger::exit("Model::addMeasurementsInSelection");
}

// Find specific distance
Measurement* Model::findDistanceMeasurement(Atom* i, Atom* j) const
{
	Measurement* result = NULL;
	for (result = distanceMeasurements_.first(); result != NULL; result = result->next)
	{
		if ((result->atom(0) == i) && (result->atom(1) == j)) break;
		if ((result->atom(0) == j) && (result->atom(1) == i)) break;
	}
	return result;
}

// Find specific angle
Measurement* Model::findAngleMeasurement(Atom* i, Atom* j, Atom* k) const
{
	Measurement* result = NULL;
	for (result = angleMeasurements_.first(); result != NULL; result = result->next)
	{
		if (result->atom(1) != j) continue;
		if ((result->atom(0) == i) && (result->atom(2) == k)) break;
		if ((result->atom(0) == k) && (result->atom(2) == i)) break;
	}
	return result;
}

// Find specific torsion
Measurement* Model::findTorsionMeasurement(Atom* i, Atom* j, Atom* k, Atom* l) const
{
	Measurement* result = NULL;
	for (result = torsionMeasurements_.first(); result != NULL; result = result->next)
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
double Model::distance(Atom* i, Atom* j)
{
	return cell_.distance(i,j);
}

// Calculate distance
double Model::distance(int i, int j)
{
	// Make sure we have a staticatoms array
	Atom** modelatoms = atomArray();
	return cell_.distance(modelatoms[i], modelatoms[j]);
}

// Calculate angle
double Model::angle(Atom* i, Atom* j, Atom* k)
{
	return cell_.angle(i,j,k);
}

// Calculate angle
double Model::angle(int i, int j, int k)
{
	// Make sure we have a staticatoms array
	Atom** modelatoms = atomArray();
	return cell_.angle(modelatoms[i], modelatoms[j], modelatoms[k]);
}

// Calculate torsion
double Model::torsion(Atom* i, Atom* j, Atom* k, Atom* l)
{
	return cell_.torsion(i,j,k,l);
}

// Calculate torsion (radians)
double Model::torsion(int i, int j, int k, int l)
{
	// Make sure we have a staticatoms array
	Atom** modelatoms = atomArray();
	return cell_.torsion(modelatoms[i], modelatoms[j], modelatoms[k], modelatoms[l]);
}

// Update measurements
void Model::updateMeasurements()
{
	Messenger::enter("Model::updateMeasurements");
	Measurement* m;
	for (m = distanceMeasurements_.first(); m != NULL; m = m->next) m->calculate(&cell_);
	for (m = angleMeasurements_.first(); m != NULL; m = m->next) m->calculate(&cell_);
	for (m = torsionMeasurements_.first(); m != NULL; m = m->next) m->calculate(&cell_);
	Messenger::exit("Model::updateMeasurements");
}

// List measurements
void Model::listMeasurements() const
{
	Messenger::enter("Model::listMeasurements");
	Measurement* m;
	for (m = distanceMeasurements_.first(); m != NULL; m = m->next) m->print();
	for (m = angleMeasurements_.first(); m != NULL; m = m->next) m->print();
	for (m = torsionMeasurements_.first(); m != NULL; m = m->next) m->print();
	Messenger::exit("Model::listMeasurements");
}
