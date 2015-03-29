/*
	*** Matrix Variable and Array
	*** src/parser/matrix.h
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

#ifndef ATEN_MATRIXVARIABLE_H
#define ATEN_MATRIXVARIABLE_H

#include "parser/variable.h"
#include "parser/accessor.h"
#include "math/matrix.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class TreeNode;

// Real 3x3 Matrix Variable
class MatrixVariable : public Variable
{
	public:
	// Constructor / Destructor
	MatrixVariable(bool constant = false);
	MatrixVariable(Matrix m, bool constant = false);
	MatrixVariable(TreeNode* xx, TreeNode* xy, TreeNode* xz, TreeNode* yx, TreeNode* yy, TreeNode* yz, TreeNode* zx, TreeNode* zy, TreeNode* zz);
	~MatrixVariable();
	friend class MatrixArrayVariable;


	/*
	// Set / Get
	*/
	public:
	// Return value of node
	bool execute(ReturnValue& rv);
	// Set from returnvalue node
	bool set(ReturnValue& rv);
	// Reset node
	void reset();


	/*
	// Variable Data
	*/
	private:
	// Recreate data (for constant Matrices) from TreeNode parts
	bool reCreate();
	// Matrix data
	Matrix matrixData_;
	// Node data for constant Matrices
	TreeNode* constXX_, *constXY_, *constXZ_, *constYX_, *constYY_, *constYZ_, *constZX_, *constZY_, *constZZ_;
	// Print node contents
	void nodePrint(int offset, const char* prefix = "");


	/*
	// Access Data
	*/
	public:
	// Accessor list
	enum Accessors { Determinant, XX, XY, XZ, YX, YY, YZ, ZX, ZY, ZZ, nAccessors };
	// Function list
	enum Functions { DummyFunction, nFunctions };
	// Search variable access list for provided accessor
	StepNode* findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList = NULL);
	// Static function to search accessors
	static StepNode* accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList = NULL);
	// Retrieve desired value
	static bool retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex = -1);
	// Set desired value
	static bool setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex = -1);
	// Perform desired function
	static bool performFunction(int i, ReturnValue& rv, TreeNode* node);
	// Accessor data
	static Accessor accessorData[nAccessors];
	// Function Accessor data
	static FunctionAccessor functionData[nFunctions];
};

// Matrix Array Variable
class MatrixArrayVariable : public Variable
{
	public:
	// Constructor / Destructor
	MatrixArrayVariable(TreeNode* sizeexpr, bool constant = false);
	~MatrixArrayVariable();


	/*
	// Set / Get
	*/
	public:
	// Return value of node
	bool execute(ReturnValue& rv);
	// Return value of node as array
	bool executeAsArray(ReturnValue& rv, int arrayIndex);
	// Set from returnvalue node
	bool set(ReturnValue& rv);
	// Set from returnvalue node as array
	bool setAsArray(ReturnValue& rv, int arrayIndex);
	// Reset variable
	void reset();


	/*
	// Variable Data
	*/
	private:
	// TreeNode determining array size on initialisation
	TreeNode* arraySizeExpression_;
	// Array size
	int arraySize_;
	// Matrix data
	Matrix *matrixArrayData_;
	// Print node contents
	void nodePrint(int offset, const char* prefix);


	/*
	// Inherited Virtuals
	*/
	public:
	// Initialise node (take over from Variable::initialise())
	bool initialise();
	// Search variable access list for provided accessor
	StepNode* findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList = NULL);
};

ATEN_END_NAMESPACE

#endif
