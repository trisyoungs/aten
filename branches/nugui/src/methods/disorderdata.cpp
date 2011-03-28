/*
	*** Disorder Data
	*** src/methods/disorderdata.cpp
	Copyright T. Youngs 2007-2011

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

#include "methods/disorderdata.h"
#include "methods/partition.h"

// Constructor
DisorderData::DisorderData()
{
	// Public variables
	prev = NULL;
	next = NULL;

	// Private variables
	nAdded_ = 0;
	nFailed_ = 0;
	nDeleted_ = 0;
	partitionData_ = NULL;
	scaleFactor_ = 1.5;
}

/*
// Model / Partition Data
*/

// Initialise structure
bool DisorderData::initialise(Model *sourceModel, PartitionData *partitionData)
{
	if (sourceModel == NULL)
	{
		msg.print("Error: DisorderData::initialise() - NULL source model pointer passed.\n");
		return FALSE;
	}
	if (sourceModel->nAtoms() == 0)
	{
		msg.print("Error: DisorderData::initialise() - source model contains no atoms!\n");
		return FALSE;
	}
	partitionData_ = partitionData;
	if (partitionData_ == NULL)
	{
		msg.print("Error: DisorderData::initialise() - NULL partition data pointer passed.\n");
		return FALSE;
	}
	
	// Make copy of source model and centre it at zero, remove an labels, measurements etc., and leave selected
	sourceModel_.copy(sourceModel);
	sourceModel_.selectAll();
	sourceModel_.centre(0.0, 0.0, 0.0);
	sourceModel_.clearAllLabels();
	nAdded_ = 0;
	nFailed_ = 0;
	nDeleted_ = 0;
	return TRUE;
}


// Return insertion policy of source model
Model::InsertionPolicy DisorderData::insertionPolicy()
{
	return sourceModel_.componentInsertionPolicy();
}

// Return requested component population
int DisorderData::requestedPopulation()
{
	return sourceModel_.componentPopulation();
}

// Return requested component density
double DisorderData::requestedDensity()
{
	return sourceModel_.componentDensity();
}

// Return name of sourcemodel
const char *DisorderData::modelName()
{
	return sourceModel_.name();
}

// Return mass of sourcemodel
double DisorderData::modelMass()
{
	return sourceModel_.mass();
}

// Return name of target partition
const char *DisorderData::partitionName()
{
	return partitionData_->name();
}

// Return density of target partition
double DisorderData::partitionDensity()
{
	return partitionData_->density();
}

// Copy contents of component across to specified model
void DisorderData::copyTo(Model *target)
{
	clipboard_.copyAll(&targetModel_);
	clipboard_.pasteToModel(target);
}

/*
// Component Addition
*/

// Prepare copy of sourcemodel in random position (and orientation) in assigned partition
void DisorderData::prepareCandidate(const Matrix& volumeElement)
{
	static Matrix rotation;
	// First, select a random cell from the list in the PartitionData
	int *ijk = partitionData_->randomCell();
	// Now, select a random unit position from within this cell, and add the two
	Vec3<double> pos(AtenMath::random(), AtenMath::random(), AtenMath::random());
	pos.add(ijk[0], ijk[1], ijk[2]);
	// Now, convert this position into real cell coordinates by multiplying by the volume element
	pos = volumeElement * pos;
	// Are we rotating the model as well?
	if (sourceModel_.componentRotatable())
	{
		// Move the model back to the origin, and then apply a random rotation
		sourceModel_.centre(0.0,0.0,0.0);
		rotation.createRotationXY(AtenMath::random()*360.0, AtenMath::random()*360.0);
		for (Atom *i = sourceModel_.atoms(); i != NULL; i = i->next) i->r() = rotation * i->r();
	}
	// Centre the sourceModel_ at the random position, and we're done
	sourceModel_.centre(pos);
}

// Calculate overlap penalty of candidate with supplied model
bool DisorderData::modelOverlapPenalty(Model *other, UnitCell *globalCell)
{
	double rij, ri, rj;
	Atom **ii = other->atomArray(), **jj = sourceModel_.atomArray();
	int i, j;
	// Perform double loop over candidate molecule atoms and supplied model atoms
	for (i = 0; i < other->nAtoms(); ++i)
	{
		ri = scaleFactor_*elements().atomicRadius(ii[i]);
		for (j = 0; j < sourceModel_.nAtoms(); ++j)
		{
			// Determine distance using supplied cell conditions
			rij = globalCell->distance(ii[i], jj[j], TRUE);
			// Simple penalty function - subtract off some multiple of the atomic radius of each atom...
			rij -= ri + scaleFactor_*elements().atomicRadius(jj[j]);
			if (rij < 0.0) return TRUE;
		}
	}
	return FALSE;
}

// Calculate overlap penalty of candidate with rest of population
bool DisorderData::selfOverlapPenalty(UnitCell *globalCell)
{
	return modelOverlapPenalty(&targetModel_, globalCell);
}

// Calculate overlap penalty of candidate with all other insertion models
bool DisorderData::otherOverlapPenalty(DisorderData *first, UnitCell *globalCell)
{
	// Go through list of DisorderedData
	for (DisorderData *dd = first; dd != NULL; dd = dd->next)
	{
		if (dd == this) continue;
		if (modelOverlapPenalty(&dd->targetModel_, globalCell)) return TRUE;
	}
	return FALSE;
}

// Accept candidate model into rest of population
void DisorderData::acceptCandidate()
{
	// Copy sourcemodel to targetModel_
	clipboard_.copyAll(&sourceModel_);
	clipboard_.pasteToModel(&targetModel_, FALSE);
	// Update density of target partition
	partitionData_->adjustReducedMass(&sourceModel_);
	++nAdded_;
}

// Reject candidate model
void DisorderData::rejectCandidate()
{
	++nFailed_;
}

// Return number of copies added
int DisorderData::nAdded()
{
	return nAdded_;
}

// Adjust radius scale factor
double DisorderData::adjustScaleFactor(double multiplier, double minimumValue)
{
	// Multiply the scale factor, but never let it go below the supplied minimumValue
	if (multiplier*scaleFactor_ > minimumValue) scaleFactor_ *= multiplier;
}

// Return current radius scale factor
double DisorderData::scaleFactor()
{
	return scaleFactor_;
}