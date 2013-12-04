/*
	*** Disorder Data
	*** src/methods/disorderdata.h
	Copyright T. Youngs 2007-2013

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

#ifndef ATEN_DISORDERDATA_H
#define ATEN_DISORDERDATA_H

#include "model/model.h"
#include "model/clipboard.h"

// Forward declarations
class PartitionData;
class PartitioningScheme;

// Insertion Data Class for Disorder builder
class DisorderData
{
	public:
	// Constructor
	DisorderData();
	// List pointers
	DisorderData *prev, *next;
	
	
	/*
	// Model / Partition Data
	*/
	private:
	// Source component model (copied from supplied target in initialise())
	Model sourceModel_;
	// Insertion model containing copies of source model
	Model targetModel_;
	// Target partition data
	PartitionData *partitionData_;
	
	public:
	// Initialise structure
	bool initialise(Model *sourceModel, PartitionData *partitionData);
	// Return insertion policy of source model
	Model::InsertionPolicy insertionPolicy();
	// Return requested component population
	int requestedPopulation();
	// Return requested component density
	double requestedDensity();
	// Return name of sourcemodel
	const char *modelName();
	// Returnreference to sourcemodel
	Model &sourceModel();
	// Return name of target partition
	const char *partitionName();
	// Return density of target partition
	double partitionDensity();
	// Return partition pointer
	PartitionData *partition();
	// Copy contents of component across to specified model
	void copyTo(Model *target);


	/*
	// Component Addition
	*/
	private:
	// Number of copies added
	int nAdded_;
	// Number of insertion attempts failed since last successful insertion
	int nFailed_;
	// Number of copies deleted
	int nDeleted_;
	// Local clipboard for copy events
	Clipboard clipboard_;
	// Scaling factor for atomic radii in penalty function
	double scaleFactor_;
	// Id of molecule selected from current ensemble (if any)
	int moleculeId_;
	// Counting variable
	int count_;
	
	public:
	// Prepare copy of sourcemodel in random position (and orientation) in assigned partition
	void prepareCandidate(const Matrix &volumeElement);
	// Accept candidate model into rest of population
	void acceptCandidate();
	// Reject candidate model
	void rejectCandidate();
	// Select a random (or specific, if supplied) molecule from the current ensemble, and place in sourceModel_
	bool selectCandidate(int id = -1);
	// Delete selected candidate
	void deleteCandidate();
	// Tweak molecule position / rotation, and place in sourceModel_
	void tweakCandidate(double maxDistance, double maxAngle, PartitioningScheme *scheme);
	// Determine whether candidate molecule overlaps with supplied model
	bool modelOverlaps(Model *other, UnitCell *globalCell);
	// Determine whether candidate molecule overlaps rest of population
	bool selfOverlaps(UnitCell *globalCell);
	// Determine whether candidate molecule overlaps with all other insertion models
	bool otherOverlaps(DisorderData *first, UnitCell *globalCell);
	// Return number of copies added
	int nAdded();
	// Return number of successive failures since last successful insertion
	int nFailed();
	// Adjust radius scale factor
	void adjustScaleFactor(double multiplier, double minimumValue, double maximumValue = 100.0);
	// Return current radius scale factor
	double scaleFactor();
	// Reset counting variable
	void resetCount();
	// Increase counting variable
	void increaseCount(int delta = 1);
	// Return counting variable
	int count();
};

#endif
