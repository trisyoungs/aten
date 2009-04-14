/*
	*** Pattern Variable and Array
	*** src/parser/pattern.h
	Copyright T. Youngs 2007-2009

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

#include "parser/variable.h"
#include "parser/accessor.h"

// Forward Declarations
class Pattern;

// Pattern Variable
class PatternVariable : public Variable
{
	public:
	// Constructor / Destructor
	PatternVariable(Pattern *ptr = NULL, bool constant = FALSE);
	~PatternVariable();

	/*
	// Set / Get
	*/
	public:
	// Return value of node
	bool execute(ReturnValue &rv);
	// Set from returnvalue node
	bool set(ReturnValue &rv);
	// Reset node
	void reset();

	/*
	// Variable Data
	*/
	private:
	// Pattern data
	void *patternData_;
	// Print node contents
	void nodePrint(int offset, const char *prefix = "");

	/*
	// Access Data
	*/
	public:
	// Accessor list
	enum Accessors { Angles, Atoms, Bonds, Cog, Com, FirstAtom, FirstAtomId, FField, LastAtom, LastAtomId, Name, NAngles, NAtoms, NBonds, NMolAtoms, NMols, NTorsions, Torsions, nAccessors };
	// Search variable access list for provided accessor
	StepNode *findAccessor(const char *s, TreeNode *arrayindex);
	// Static function to search accessors
	static StepNode *accessorSearch(const char *s, TreeNode *arrayindex);
	// Retrieve desired value
	static bool retrieveAccessor(int i, ReturnValue &rv, bool hasarrayindex, int arrayIndex = -1);
	// Set desired value
	static bool setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasarrayindex, int arrayIndex = -1);
	// Accessor data
	static Accessor accessorData[nAccessors];
};

// XXX Array Variable
class XXXArrayVariable : public Variable
{
	public:
	// Constructor / Destructor
	XXXArrayVariable(TreeNode *sizeexpr, bool constant = FALSE);
	~XXXArrayVariable();

	/*
	// Set / Get
	*/
	public:
	// Return value of node
	bool execute(ReturnValue &rv);
	// Return value of node as array
	bool executeAsArray(ReturnValue &rv, int arrayindex);
	// Set from returnvalue node
	bool set(ReturnValue &rv);
	// Set from returnvalue node as array
	bool setAsArray(ReturnValue &rv, int arrayindex);
	// Reset variable
	void reset();

	/*
	// Variable Data
	*/
	private:
	// TreeNode determining array size on initialisation
	TreeNode *arraySizeExpression_;
	// Array size
	int arraySize_;
	// XXX data
	void **xxxArrayData_;
	// Print node contents
	void nodePrint(int offset, const char *prefix);

	/*
	// Inherited Virtuals
	*/
	public:
	// Initialise node (take over from Variable::initialise())
	bool initialise();
	// Search variable access list for provided accessor
	StepNode *findAccessor(const char *s, TreeNode *arrayindex);
};

#endif
