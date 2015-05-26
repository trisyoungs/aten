/*
	*** Disorder Data
	*** src/methods/disorderdata.cpp
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

#include "methods/disorderdata.h"
#include "methods/partitiondata.h"
#include "methods/partitioningscheme.h"
#include "methods/mc.h"

ATEN_USING_NAMESPACE

// Constructor
DisorderData::DisorderData() : ListItem<DisorderData>()
{
	// Private variables
	nAdded_ = 0;
	nFailed_ = 0;
	nDeleted_ = 0;
	partitionData_ = NULL;
	scaleFactor_ = mc.disorderMaximumScaleFactor();
	moleculeId_ = -1;
}

/*
// Model / Partition Data
*/

// Initialise structure
bool DisorderData::initialise(Model* sourceModel, PartitionData* partitionData)
{
	if (sourceModel == NULL)
	{
		Messenger::print("Error: DisorderData::initialise() - NULL source model pointer passed.");
		return false;
	}
	if (sourceModel->nAtoms() == 0)
	{
		Messenger::print("Error: DisorderData::initialise() - source model contains no atoms!");
		return false;
	}
	partitionData_ = partitionData;
	if (partitionData_ == NULL)
	{
		Messenger::print("Error: DisorderData::initialise() - NULL partition data pointer passed.");
		return false;
	}
	
	// Make copy of source model and centre it at zero, remove any labels, measurements etc., and leave selected
	sourceModel_.copy(sourceModel);
	sourceModel_.selectAll();
	sourceModel_.centre(0.0, 0.0, 0.0);
	sourceModel_.clearAllLabels();
	nAdded_ = 0;
	nFailed_ = 0;
	nDeleted_ = 0;
	return true;
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
QString DisorderData::modelName()
{
	return sourceModel_.name();
}

// Return reference to source model
Model& DisorderData::sourceModel()
{
	return sourceModel_;
}

// Return name of target partition
QString DisorderData::partitionName()
{
	return partitionData_->name();
}

// Return density of target partition
double DisorderData::partitionDensity()
{
	return partitionData_->density();
}

// Return partition pointer
PartitionData* DisorderData::partition()
{
	return partitionData_;
}

// Copy contents of component across to specified model
void DisorderData::copyTo(Model* target)
{
	clipboard_.copyAll(&targetModel_, true);
	clipboard_.pasteToModel(target, false);
}

/*
 * Component Addition
 */

// Prepare copy of sourcemodel in random position (and orientation) in assigned partition
void DisorderData::prepareCandidate(const Matrix& volumeElement)
{
	static Matrix rotation;
	moleculeId_ = -1;
	// First, select a random cell from the list in the PartitionData
	int *ijk = partitionData_->randomCell();
	// Now, select a random unit position from within this cell, and add the two
	Vec3<double> pos(AtenMath::random(), AtenMath::random(), AtenMath::random());
	pos.add(ijk[0], ijk[1], ijk[2]);
	// Now, convert this position into real cell coordinates by multiplying by the volume element
	pos = volumeElement*  pos;
	// Are we rotating the model as well?
	if (sourceModel_.componentRotatable())
	{
		// Move the model back to the origin, and then apply a random rotation
		sourceModel_.centre(0.0,0.0,0.0);
		rotation.createRotationXY(AtenMath::random()*360.0, AtenMath::random()*360.0);
		for (Atom* i = sourceModel_.atoms(); i != NULL; i = i->next) i->r() = rotation * i->r();
	}
	// Centre the sourceModel_ at the random position, and we're done
	sourceModel_.centre(pos);
}

// Accept candidate model into rest of population
void DisorderData::acceptCandidate()
{
	if (moleculeId_ == -1)
	{
		// Copy sourcemodel to targetModel_
		clipboard_.copyAll(&sourceModel_, true);
		clipboard_.pasteToModel(&targetModel_, false);
		// Update density of target partition
		partitionData_->adjustReducedMass(&sourceModel_);
		++nAdded_;
		// Reset failures counter
		nFailed_ = 0;
	}
	else
	{
		// Overwrite old atomic coordinates with those currently in sourceModel_
		for (int i=0; i<sourceModel_.nAtoms(); ++i) targetModel_.atom(moleculeId_*sourceModel_.nAtoms()+i)->r() = sourceModel_.atom(i)->r();
	}
}

// Reject candidate model
void DisorderData::rejectCandidate()
{
	++nFailed_;
}

// Select a random molecule from the current ensemble, and place in sourceModel_
bool DisorderData::selectCandidate(int id)
{
	if (nAdded_ == 0)
	{
		moleculeId_ = -1;
		return false;
	}
	// If an id was supplied, select it. Otherwise choose random molecule
	if (id != -1)
	{
		if (id < 0 || id >= nAdded_)
		{
			printf("Internal Error: Molecule id %i is out of range in DisorderData::selectCandidate().\n", id);
			moleculeId_ = -1;
			return false;
		}
		moleculeId_ = id;
	}
	else moleculeId_ = AtenMath::randomi(nAdded_);
	
	// Select all atoms in target molecule and copy/paste them
	targetModel_.selectNone();
	for (int i=moleculeId_*sourceModel_.nAtoms(); i < (moleculeId_+1)*sourceModel_.nAtoms(); ++i) targetModel_.selectAtom(i);
	clipboard_.copySelection(&targetModel_, true);
	sourceModel_.clear();
	clipboard_.pasteToModel(&sourceModel_);
	return true;
}

// Delete selected candidate
void DisorderData::deleteCandidate()
{
	if (moleculeId_ == -1) printf("Internal Error: No candidate to delete in DisorderData.\n");
	else
	{
		targetModel_.selectNone();
		for (int i=moleculeId_*sourceModel_.nAtoms(); i < (moleculeId_+1)*sourceModel_.nAtoms(); ++i) targetModel_.selectAtom(i);
		targetModel_.selectionDelete();
		partitionData_->adjustReducedMass(&sourceModel_, true);
		--nAdded_;
		moleculeId_ = -1;
	}
}

// Tweak molecule position / rotation, and place in sourceModel_
void DisorderData::tweakCandidate(double maxDistance, double maxAngle, PartitioningScheme* scheme)
{
	// Are we rotating the model as well?
	static Matrix rotation;
	Vec3<double> shift, cog;
	if (sourceModel_.componentRotatable())
	{
		// Get current centre of molecule
		Vec3<double> oldCentre = sourceModel_.selectionCentreOfGeometry();
		sourceModel_.centre(0.0,0.0,0.0);
		rotation.createRotationXY(AtenMath::random()*maxAngle, AtenMath::random()*maxAngle);
		for (Atom* i = sourceModel_.atoms(); i != NULL; i = i->next) i->r() = rotation * i->r();
		sourceModel_.centre(oldCentre);
	}
	// Apply a random shift to the position, but ensure that the centre of geometry stays in the same region
	for (int i=0; i<5; ++i)
	{
		shift.randomUnit();
		shift *= maxDistance;
		sourceModel_.translateSelectionLocal(shift);
		cog = sourceModel_.cell().realToFrac(sourceModel_.selectionCentreOfGeometry());
		if (scheme->partitionId(cog.x, cog.y, cog.z) == partitionData_->id()) return;
		sourceModel_.translateSelectionLocal(-shift);
	}
}

// Determine whether candidate molecule overlaps with supplied model
bool DisorderData::modelOverlaps(Model* other, UnitCell& globalCell)
{
	double rij, ri;
	Atom** ii = other->atomArray(), **jj = sourceModel_.atomArray();
	int i, j;
	// Perform double loop over candidate molecule atoms and supplied model atoms
	for (i = 0; i < other->nAtoms(); ++i)
	{
		// Disregard current selected molecule, if 'other == sourceModel_'
		if ((other == &targetModel_) && (i >= moleculeId_*sourceModel_.nAtoms()) && (i < (moleculeId_+1)*sourceModel_.nAtoms())) continue;
		
		ri = scaleFactor_*Elements().atomicRadius(ii[i]);
		for (j = 0; j < sourceModel_.nAtoms(); ++j)
		{
			// Determine distance using supplied cell conditions
			rij = globalCell.distance(ii[i], jj[j], true);
			// Simple penalty function - subtract off some multiple of the atomic radius of each atom...
			rij -= ri + scaleFactor_*Elements().atomicRadius(jj[j]);
			if (rij < 0.0) return true;
		}
	}
	return false;
}

// Determine whether candidate molecule overlaps rest of population
bool DisorderData::selfOverlaps(UnitCell& globalCell)
{
	return modelOverlaps(&targetModel_, globalCell);
}

// Determine whether candidate molecule overlaps with all other insertion models
bool DisorderData::otherOverlaps(DisorderData* first, UnitCell& globalCell)
{
	// Go through list of DisorderedData
	for (DisorderData *dd = first; dd != NULL; dd = dd->next)
	{
		if (dd == this) continue;
		if (modelOverlaps(&dd->targetModel_, globalCell)) return true;
	}
	return false;
}

// Return number of copies added
int DisorderData::nAdded()
{
	return nAdded_;
}

// Return number of successive failures since last successful insertion
int DisorderData::nFailed()
{
	return nFailed_;
}

// Adjust radius scale factor
void DisorderData::adjustScaleFactor(double multiplier, double minimumValue, double maximumValue)
{
	// Multiply the scale factor, but never let it go outside the range supplied
	scaleFactor_ *= multiplier;
	if (scaleFactor_ < minimumValue) scaleFactor_ = minimumValue;
	else if (scaleFactor_ > maximumValue) scaleFactor_ = maximumValue;
	nFailed_ = 0;
}

// Return current radius scale factor
double DisorderData::scaleFactor()
{
	return scaleFactor_;
}

// Reset counting variable
void DisorderData::resetCount()
{
	count_ = 0;
}

// Increase counting variable
void DisorderData::increaseCount(int delta)
{
	count_ += delta;
}

// Return counting variable
int DisorderData::count()
{
	return count_;
}
