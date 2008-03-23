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
#include "classes/measurement.h"
#include "classes/bond.h"
//#include <stdarg.h>

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
void Model::measureDistance(Atom *i, Atom *j)
{
	// Measure distances between atoms
	dbgBegin(DM_CALLS,"Model::measureDistance");
	Measurement *newdist = findMeasurement(GT_DISTANCE,i,j);
	// If this distance isn't currently in the list, add it. Otherwise, delete it
	if (newdist == NULL) addMeasurement(GT_DISTANCE,i,j);
	else removeMeasurement(newdist);
	dbgEnd(DM_CALLS,"Model::measureDistance");
}

// Add angle measurement
void Model::measureAngle(Atom *i, Atom *j, Atom *k)
{
	// Measure angles between atoms
	dbgBegin(DM_CALLS,"Model::measureAngle");
	Measurement *newangle = findMeasurement(GT_ANGLE,i,j,k);
	// Check that this angle isn't already in the list. If it is, delete it
	if (newangle == NULL) addMeasurement(GT_ANGLE,i,j,k);
	else removeMeasurement(newangle);
	dbgEnd(DM_CALLS,"Model::measureAngle");
}

// Add torsion measurement
void Model::measureTorsion(Atom *i, Atom *j, Atom *k, Atom *l)
{
	// Measure torsions between atoms
	dbgBegin(DM_CALLS,"Model::measureTorsion");
	Measurement *newtorsion = findMeasurement(GT_TORSION,i,j,k,l);
	// If this torsion isn't in the list, add it. Otherwise, delete it.
	if (newtorsion == NULL) addMeasurement(GT_TORSION,i,j,k,l);
	else removeMeasurement(newtorsion);
	dbgEnd(DM_CALLS,"Model::measureTorsion");
}

// Remove specific measurement
void Model::removeMeasurement(Measurement *me)
{
	dbgBegin(DM_CALLS,"Model::removeMeasurement");
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		Change *newchange = recordingState_->addChange();
		Atom **atoms = me->atoms();
		GeometryType type = me->type();
		switch (type)
		{
			case (GT_DISTANCE):
				newchange->set(-UE_MEASUREMENT, type, atoms[0]->id(), atoms[1]->id());
				break;
			case (GT_ANGLE):
				newchange->set(-UE_MEASUREMENT, type, atoms[0]->id(), atoms[1]->id(), atoms[2]->id());
				break;
			case (GT_TORSION):
				newchange->set(-UE_MEASUREMENT, type, atoms[0]->id(), atoms[1]->id(), atoms[2]->id(), atoms[3]->id());
				break;
		}
	}
	measurements_.remove(me);
	dbgEnd(DM_CALLS,"Model::removeMeasurement");
}

// Clear measurements of specific type
void Model::removeMeasurements(GeometryType gt)
{
	dbgBegin(DM_CALLS,"Model::removeMeasurements");
	printf("Model::removeMeasurements by type is not done.\n");
	dbgEnd(DM_CALLS,"Model::removeMeasurements");
}

// Delete measurements involving specific atom
void Model::removeMeasurements(Atom *xatom)
{
	// Search the lists of measurements for the supplied atom, and remove any that use it
	dbgBegin(DM_CALLS,"Model::removeMeasurements[atom]");
	int n;
	bool remove;
	Measurement *nextm, *m;
	Atom **atoms;
	m = measurements_.first();
	while (m != NULL)
	{
		remove = FALSE;
		atoms = m->atoms();
		for (n=0; n<natoms_from_GT(m->type()); n++) if (atoms[n] == xatom) remove = TRUE;
		if (remove)
		{
			nextm = m->next;
			removeMeasurement(m);
			m = nextm;
		}
		else m = m->next;
	}
	dbgEnd(DM_CALLS,"Model::removeMeasurements[atom]");
}

// Add Measurement
void Model::addMeasurement(GeometryType gt, Atom *first, ...)
{
	dbgBegin(DM_CALLS,"Model::addMeasurement");
	Atom *i, **atoms;
	Measurement *newm = measurements_.add();
	newm->setType(gt);
	newm->setAtom(0, first);
	// Get remaining atoms_...
	int nexpected = natoms_from_GT(gt);
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
		// Add the change to the undo state (if there is one)
		if (recordingState_ != NULL)
		{
			Change *newchange = recordingState_->addChange();
			atoms = newm->atoms();
			switch (gt)
			{
				case (GT_DISTANCE):
					newchange->set(UE_MEASUREMENT, gt, atoms[0]->id(), atoms[1]->id());
					break;
				case (GT_ANGLE):
					newchange->set(UE_MEASUREMENT, gt, atoms[0]->id(), atoms[1]->id(), atoms[2]->id());
					break;
				case (GT_TORSION):
					newchange->set(UE_MEASUREMENT, gt, atoms[0]->id(), atoms[1]->id(), atoms[2]->id(), atoms[3]->id());
					break;
			}
		}
	}
	dbgEnd(DM_CALLS,"Model::addMeasurement");
}

// Add measurements in selection
void Model::addMeasurementsInSelection(GeometryType gt)
{
	dbgBegin(DM_CALLS,"Model::addMeasurementsInSelection");
	Atom *i, *j, *k, *l;
	Refitem<Bond,int> *b1, *b2, *b3;
	switch (gt)
	{
		case (GT_DISTANCE):
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
		case (GT_ANGLE):
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
		case (GT_TORSION):
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
	dbgEnd(DM_CALLS,"Model::addMeasurementsInSelection");
}

// Find Measurement
Measurement *Model::findMeasurement(GeometryType gt,  Atom *first, ...)
{
	dbgBegin(DM_CALLS,"Model::findMeasurement");
	Measurement *result, *m;
	int n, matched1, matched2;
	bool proceed;
	Atom *searchatoms[4], **matoms;
	searchatoms[0] = first;
	int nexpected = natoms_from_GT(gt);
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
	dbgEnd(DM_CALLS,"Model::findMeasurement");
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
	dbgBegin(DM_CALLS,"Model::updateMeasurements");
	for (Measurement *m = measurements_.first(); m != NULL; m = m->next) m->calculate(&cell_);
	dbgEnd(DM_CALLS,"Model::updateMeasurements");
}
