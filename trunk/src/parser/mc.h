/*
	*** Monte Carlo Variable
	*** src/parser/mc.h
	Copyright T. Youngs 2007-2015

	This file is part of Aten.

	Prefs is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Prefs is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Prefs.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef ATEN_MCVARIABLE_H
#define ATEN_MCVARIABLE_H

#include "parser/variable.h"
#include "parser/accessor.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class TreeNode;

// Monte Carlo Variable
class MonteCarloVariable : public Variable
{
	public:
	// Constructor / Destructor
	MonteCarloVariable();
	~MonteCarloVariable();


	/*
	// Access Data
	*/
	public:
	// Accessor list
	enum Accessors { DisorderAccuracy, DisorderDeltaAngle, DisorderDeltaDistance, DisorderMaxCycles, DisorderMaxFailures, DisorderMaximumScaleFactor, DisorderMinimumScaleFactor, DisorderNTweaks, DisorderRecoveryMaxCycles, DisorderRecoveryMaxTweaks, DisorderRecoveryThreshold, DisorderReductionFactor, NCycles, Temperature, nAccessors };
	// Function list
	enum Functions { AcceptanceEnergy, MaxStep, MoveAllowed, NTrials, SetAcceptanceEnergy, SetMaxStep, SetMoveAllowed, SetNTrials, nFunctions };
	// Search variable access list for provided accessor
	StepNode* findAccessor(const char* s, TreeNode* arrayIndex, TreeNode* argList = NULL);
	// Static function to search accessors
	static StepNode* accessorSearch(const char* s, TreeNode* arrayIndex, TreeNode* argList = NULL);
	// Retrieve desired value
	static bool retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex = -1);
	// Set desired value
	static bool setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex = -1);
	// Perform desired function
	static bool performFunction(int i, ReturnValue& rv, TreeNode* node);
	// Print valid accessors/functions
	static void printAccessors();
	// Accessor data
	static Accessor accessorData[nAccessors];
	// Function Accessor data
	static FunctionAccessor functionData[nFunctions];
};

ATEN_END_NAMESPACE

#endif
