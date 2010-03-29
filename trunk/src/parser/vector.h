/*
	*** Vector Variable and Array
	*** src/parser/vector.h
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

#ifndef ATEN_VECTORVARIABLE_H
#define ATEN_VECTORVARIABLE_H

#include "parser/variable.h"
#include "parser/accessor.h"

// Forward Declarations
class TreeNode;

// Real 3-Vector Variable
class VectorVariable : public Variable
{
	public:
	// Constructor / Destructor
	VectorVariable(bool constant = FALSE);
	VectorVariable(Vec3<double> v, bool constant = FALSE);
	VectorVariable(TreeNode *x, TreeNode *y, TreeNode *z);
	~VectorVariable();
	friend class VectorArrayVariable;

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
	// Recreate data (for constant vectors) from TreeNode parts
	bool reCreate();
	// Vector data
	Vec3<double> vectorData_;
	// Node data for constant vectors
	TreeNode *constX_, *constY_, *constZ_;
	// Print node contents
	void nodePrint(int offset, const char *prefix = "");

	/*
	// Access Data
	*/
	public:
	// Accessor list
	enum Accessors { Magnitude, X, Y, Z, nAccessors };
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
	// Accessor data
	static Accessor accessorData[nAccessors];
	// Function Accessor data
	static FunctionAccessor functionData[nFunctions];
};

// Vector Array Variable
class VectorArrayVariable : public Variable
{
	public:
	// Constructor / Destructor
	VectorArrayVariable(TreeNode *sizeexpr, bool constant = FALSE);
	~VectorArrayVariable();

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
	// Vector data
	Vec3<double> *vectorArrayData_;
	// Print node contents
	void nodePrint(int offset, const char *prefix);

	/*
	// Inherited Virtuals
	*/
	public:
	// Initialise node (take over from Variable::initialise())
	bool initialise();
	// Search variable access list for provided accessor
	StepNode *findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist = NULL);
};

#endif
