/*
	*** Atom Variable and Array
	*** src/parser/atom.h
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

#ifndef ATEN_ATOMVARIABLE_H
#define ATEN_ATOMVARIABLE_H

#include "parser/pvariable.h"
#include "parser/accessor.h"

// Forward Declarations
class Atom;

// Atom Variable
class AtomVariable : public PointerVariable
{
	public:
	// Constructor / Destructor
	AtomVariable(Atom *i = NULL, bool constant = FALSE);
	~AtomVariable();

	/*
	// Access Data
	*/
	public:
	// Accessor list
        enum Accessors { Bonds, F, Fixed, FracX, FracY, FracZ, FX, FY, FZ, Hidden, Id, Mass, Name, NBonds, Q, R, RX, RY, RZ, Selected, Symbol, Type, V, VX, VY, VZ, Z, nAccessors };
	// Function list
	enum Functions { DummyFunction, nFunctions };
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

// Atom Array Variable
class AtomArrayVariable : public PointerArrayVariable
{
	public:
	// Constructor / Destructor
	AtomArrayVariable(TreeNode *sizeexpr, bool constant = FALSE);

	/*
	// Inherited Virtuals
	*/
	public:
	// Search variable access list for provided accessor
	StepNode *findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist = NULL);
};

#endif
