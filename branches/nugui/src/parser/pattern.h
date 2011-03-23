/*
	*** Pattern Variable and Array
	*** src/parser/pattern.h
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

#ifndef ATEN_PATTERNVARIABLE_H
#define ATEN_PATTERNVARIABLE_H

#include "parser/pvariable.h"
#include "parser/accessor.h"

// Forward Declarations
class Pattern;

// Pattern Variable
class PatternVariable : public PointerVariable
{
	public:
	// Constructor / Destructor
	PatternVariable(Pattern *ptr = NULL, bool constant = FALSE);
	~PatternVariable();

	/*
	// Access Data
	*/
	public:
	// Accessor list
	enum Accessors { Angles, Atoms, Bonds, FFAngles, FFBonds, FFTorsions, FFTypes, FField, FirstAtom, FirstAtomId, Fixed, LastAtom, LastAtomId, Name, NAngles, NAtoms, NBonds, NFFAngles, NFFBonds, NFFTorsions, NFFTypes, NMolAtoms, NMols, NTorsions, Torsions, nAccessors };
	// Function list
	enum Functions { Cog, Com, nFunctions };
	// Search variable access list for provided accessor
	StepNode *findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist = NULL);
	// Static function to search accessors
	static StepNode *accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist = NULL);
	// Retrieve desired value
	static bool retrieveAccessor(int i, ReturnValue &rv, bool hasarrayindex, int arrayIndex = -1);
	// Set desired value
	static bool setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasarrayindex, int arrayIndex = -1);
	// Perform desired function
	static bool performFunction(int i, ReturnValue &rv, TreeNode *node);
	// Print valid accessors/functions
	static void printAccessors();
	// Accessor data
	static Accessor accessorData[nAccessors];
	// Function Accessor data
	static FunctionAccessor functionData[nFunctions];
};

// Pattern Array Variable
class PatternArrayVariable : public PointerArrayVariable
{
	public:
	// Constructor / Destructor
	PatternArrayVariable(TreeNode *sizeexpr, bool constant = FALSE);

	/*
	// Inherited Virtuals
	*/
	public:
	// Search variable access list for provided accessor
	StepNode *findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist = NULL);
};

#endif
