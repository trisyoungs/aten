/*
	*** Disorder Data
	*** src/methods/disorderdata.h
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

#ifndef ATEN_DISORDERDATA_H
#define ATEN_DISORDERDATA_H

#include "model/model.h"
#include "model/clipboard.h"

// Forward declarations
class PartitionData;

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
	// Return mass of sourcemodel
	double modelMass();
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
	
	public:
	// Prepare copy of sourcemodel in random position (and orientation) in assigned partition
	void prepareCandidate(const Matrix &volumeElement);
	// Accept candidate model into rest of population
	void acceptCandidate();
	// Reject candidate model
	void rejectCandidate();
	// Select a random molecule from the current ensemble, and place in sourceModel_
	bool selectCandidate();
	// Delete selected candidate
	void deleteCandidate();
	// Tweak molecule position / rotation, and place in sourceModel_
	void tweakCandidate(double maxDistance, double maxAngle);
	// Calculate overlap penalty of candidate with supplied model
	bool modelOverlapPenalty(Model *other, UnitCell *globalCell);
	// Calculate overlap penalty of candidate with rest of population
	bool selfOverlapPenalty(UnitCell *globalCell);
	// Calculate overlap penalty of candidate with all other insertion models
	bool otherOverlapPenalty(DisorderData *first, UnitCell *globalCell);
	// Return number of copies added
	int nAdded();
	// Return number of successive failures since last successful insertion
	int nFailed();
	// Adjust radius scale factor
	double adjustScaleFactor(double multiplier, double minimumValue);
	// Return current radius scale factor
	double scaleFactor();
};

#endif
