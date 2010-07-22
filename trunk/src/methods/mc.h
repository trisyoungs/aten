/*
	*** Monte Carlo methods
	*** src/methods/mc.h
	Copyright T. Youngs 2007-2010

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
#include "base/region.h"
#include "base/dnchar.h"

// Forward declarations
class Model;
class Cell;
class Pattern;

// Monte Carlo
class MonteCarlo
{
	public:
	// Monte Carlo move types
	enum MoveType { Translate, Rotate, ZMatrix, Insert, Delete, nMoveTypes };
	static const char *moveTypeKeyword(MoveType);
	static MoveType moveType(const char *name, bool reporterror = 0);

	/*
	// Main Routines
	*/
	public:
	// Constructor / Destructor
	MonteCarlo();
	~MonteCarlo();
	// Minimise the specified model
	bool minimise(Model*, double, double);
	// Run disordered builder
	bool disorder(Model*);

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
	// Scaling factor for VDW radius in disorder method
	double vdwScale_;
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
	// Sets the vDW radius scale
	void setVdwScale(double d);
	// Return current vdw radius scale
	double vdwScale() const;
	// Set MC temperature for Boltzmann probabilities
	void setTemperature(double t);
	// Return current MC temperature
	double temperature() const;
};

// Static Singleton
extern MonteCarlo mc;

#endif
