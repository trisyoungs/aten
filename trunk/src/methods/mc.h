/*
	*** Monte Carlo methods
	*** src/methods/mc.h
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

#ifndef ATEN_MONTECARLO_H
#define ATEN_MONTECARLO_H

#include "templates/list.h"
#include "base/dnchar.h"

// Forward declarations
class Model;
class Pattern;
class PartitioningScheme;

// Monte Carlo
class MonteCarlo
{
	public:
	// Constructor / Destructor
	MonteCarlo();
	~MonteCarlo();
	// Monte Carlo move types
	enum MoveType { Translate, Rotate, ZMatrix, Insert, Delete, nMoveTypes };
	static const char *moveTypeKeyword(MoveType);
	static MoveType moveType(const char *name, bool reportError = 0);


	/*
	// Subroutines
	*/
	public:
	// Create acceptance ratio array
	void createRatioArray(int);


	/*
	// MC Move Probabilities, step sizes, trial numbers
	*/
	private:
	// Maximum size of allowed move (units depend on move type)
	double maxStep_[nMoveTypes];
	// Number of times to attempt move types
	int nTrials_[nMoveTypes];
	// Turn on/off move types
	bool moveAllowed_[nMoveTypes];
	// Acceptance ratio counts per pattern
	double **acceptanceRatio_;
	// Size of acceptratio array
	int acceptanceRatioSize_;
	// Number of cycles to perform in MC method
	int nCycles_;
	// Energy differences below which to accept moves
	double acceptanceEnergy_[nMoveTypes];
	// Temperature for Boltzmann probabilities
	double temperature_;

	public:
	// Set maximum stepsize for MC move
	void setMaxStep(MoveType m, double d);
	// Get maximum stepsize for MC move
	double maxStep(MoveType m) const;
	// Set ntrials for MC move
	void setNTrials(MoveType m, int i);
	// Get ntrials for MC move
	int nTrials(MoveType m) const;
	// Set allowed flag for MC move
	void setMoveAllowed(MoveType m, bool b);
	// Get allowed flag for MC move
	bool isMoveAllowed(MoveType m) const;
	// Set eaccept limit for MC move
	void setAcceptanceEnergy(MoveType m, double d);
	// Get eaccept limit for MC move
	double acceptanceEnergy(MoveType m) const;
	// Set number of MC cycles to perform
	void setNCycles(int i);
	// Get ntrials for MC move
	int nCycles() const;
	// Set MC temperature for Boltzmann probabilities
	void setTemperature(double t);
	// Return current MC temperature
	double temperature() const;


	/*
	// Disorder Builder Variables
	*/
	private:
	// Maximum number of disorder cycles to perform
	int disorderMaxCycles_;
	// Percentage error we will allow in actual vs requested densities
	double disorderAccuracy_;
	// Number of successive failed insertions allowed before component scale factor is reduced
	int disorderMaxFailures_;
	// Factor by which to reduce scale factors after reaching the limit of unsuccessful insertions
	double disorderReductionFactor_;
	// Minimum scale factor to allow
	double disorderMinimumScaleFactor_;
	// Maximum scale factor to allow
	double disorderMaximumScaleFactor_;
	// Number of tweaks to attempt, per component, per cycle
	int disorderNTweaks_;
	// Maximum distance to translate molecule in tweak
	double disorderDeltaDistance_;
	// Maximum angle (each around X and Y) to rotate molecule in tweak
	double disorderDeltaAngle_;
	// Maximum number of recovery cycles to perform
	int disorderRecoveryMaxCycles_;
	// Maximum number of tweaks, per molecule per component, in recovery
	int disorderRecoveryMaxTweaks_;
	// Fraction of non-overlapping component molecules required for success
	double disorderRecoveryThreshold_;

	public:
	// Set maximum number of disorder cycles to perform
	void setDisorderMaxCycles(int ncycles);
	// Return maximum number of disorder cycles to perform
	int disorderMaxCycles();
	// Set percentage error allowed in actual vs requested densities
	void setDisorderAccuracy(double accuracy);
	// Return percentage error allowed in actual vs requested densities
	double disorderAccuracy();
	// Set number of successive failed insertions allowed before component scale factor is reduced
	void setDisorderMaxFailures(int maxfails);
	// Return number of successive failed insertions allowed before component scale factor is reduced
	int disorderMaxFailures();
	// Set factor by which to reduce scale factors after reaching the limit of unsuccessful insertions
	void setDisorderReductionFactor(double factor);
	// Return factor by which to reduce scale factors after reaching the limit of unsuccessful insertions
	double disorderReductionFactor();
	// Set minimum scale factor to allow
	void setDisorderMinimumScaleFactor(double factor);
	// Return minimum scale factor to allow
	double disorderMinimumScaleFactor();
	// Set maximum scale factor to allow
	void setDisorderMaximumScaleFactor(double factor);
	// Return maximum scale factor to allow
	double disorderMaximumScaleFactor();
	// Set number of tweaks to attempt, per component, per cycle
	void setDisorderNTweaks(int ntweaks);
	// Retur number of tweaks to attempt, per component, per cycle
	int disorderNTweaks();
	// Set maximum distance to translate molecule in tweak
	void setDisorderDeltaDistance(double distance);
	// Return maximum distance to translate molecule in tweak
	double disorderDeltaDistance();
	// Set maximum angle (each around X and Y) to rotate molecule in tweak
	void setDisorderDeltaAngle(double angle);
	// Return maximum angle (each around X and Y) to rotate molecule in tweak
	double disorderDeltaAngle();
	// Set maximum number of recovery cycles to perform
	void setDisorderRecoveryMaxCycles(int ncycles);
	// Return maximum number of recovery cycles to perform
	int disorderRecoveryMaxCycles();
	// Set maximum number of tweaks, per molecule per component, in recovery
	void setDisorderRecoveryMaxTweaks(int n);
	// Return maximum number of tweaks, per molecule per component, in recovery
	int disorderRecoveryMaxTweaks();
	// Set fraction of non-overlapping component molecules required for success
	void setDisorderRecoveryThreshold(double d);
	// Return fraction of non-overlapping component molecules required for success
	double disorderRecoveryThreshold();
	
	
	/*
	// Main Routines
	*/
	public:
	// Minimise the specified model
	bool minimise(Model* target, double, double);
	// Run disordered builder
	bool disorder(Model* destmodel, PartitioningScheme* scheme, bool fixedCell);
};

// Static Singleton
extern MonteCarlo mc;

#endif
