/*
	*** Monte Carlo methods
	*** src/methods/mc.h
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

#ifndef ATEN_MONTECARLO_H
#define ATEN_MONTECARLO_H

#include "templates/list.h"
#include "classes/region.h"
#include "classes/dnchar.h"

// Monte Carlo move types
enum MonteCarloMove { MT_TRANSLATE, MT_ROTATE, MT_ZMATRIX, MT_INSERT, MT_DELETE, MT_NITEMS };
const char *text_from_MT(MonteCarloMove);
MonteCarloMove MT_from_text(const char*);

// Forward declarations
class Model;
class Cell;
class Pattern;

// Disorder Component
class Component
{
	public:
	// Constructor
	Component();
	// List pointers
	Component *prev, *next;

	/*
	// Component (for disordered builder)
	*/
	private:
	// Pointer to the Component model
	Model *model_;
	// Pointer to the Components related pattern
	Pattern *pattern_;
	// Number of requested and actual (filled) molecules of this type
	int nRequested_, nFilled_;
	// Lists which MC move types are allowed for this Component
	bool moveAllowed_[MT_NITEMS];
	// Name of the Component
	Dnchar name_;

	public:
	// Type of region the Component is limited to
	ComponentRegion area;
	// Set the Component's model
	void setModel(Model *m);
	// Return the Component's model
	Model *model();
	// Set the Component's pattern
	void setPattern(Pattern *p);
	// Return the Component's pattern
	Pattern *pattern();
	// Set the requested number of molecules
	void setNRequested(int i);
	// Return the requested number of molecules
	int nRequested();
	// Set the number of molecules filled
	void setNFilled(int i);
	// Return the number of molecules filled
	int nFilled();
	// Set a specific move type for the Component
	void setMoveAllowed(MonteCarloMove m, bool b);
	// Set whether the Component may be translated
	bool isMoveAllowed(MonteCarloMove m);
	// Set name of Component
	void setName(const char *s);
	// Get name of Component
	const char *name();
};

// Monte Carlo
class MethodMc
{
	/*
	// Main Routines
	*/
	public:
	// Constructor
	MethodMc();
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
	double maxStep_[MT_NITEMS];
	// Number of times to attempt move types
	int nTrials_[MT_NITEMS];
	// Turn on/off move types
	bool moveAllowed_[MT_NITEMS];
	// Acceptance ratio counts per pattern
	double **acceptanceRatio_;
	// Size of acceptratio array
	int acceptanceRatioSize_;
	// Number of cycles to perform in MC method
	int nCycles_;
	// Energy differences below which to accept moves
	double acceptanceEnergy_[MT_NITEMS];
	// Scaling factor for VDW radius in disorder method
	double vdwScale_;

	public:
	// Set maximum stepsize for MC move
	void setMaxStep(MonteCarloMove m, double d);
	// Get maximum stepsize for MC move
	double maxStep(MonteCarloMove m);
	// Set ntrials for MC move
	void setNTrials(MonteCarloMove m, int i);
	// Get ntrials for MC move
	int nTrials(MonteCarloMove m);
	// Set allowed flag for MC move
	void setMoveAllowed(MonteCarloMove m, bool b);
	// Get allowed flag for MC move
	bool isMoveAllowed(MonteCarloMove m);
	// Set eaccept limit for MC move
	void setAcceptanceEnergy(MonteCarloMove m, double d);
	// Get eaccept limit for MC move
	double acceptanceEnergy(MonteCarloMove m);
	// Set number of MC cycles to perform
	void setNCycles(int i);
	// Get ntrials for MC move
	int nCycles();
	// Sets the vDW radius scale
	void setVdwScale(double d);

	/*
	// Component list (for disorder builder)
	*/
	public:
	// List of Component models to use in MC insertion
	List<Component> components;
	// Return the Component with name specified
	Component *componentByName(const char*);
};

// Static Singleton
extern MethodMc mc;

#endif
