/*
	*** Vector Variable
	*** src/parser/vector.h
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

#ifndef ATEN_VECTORVARIABLE_H
#define ATEN_VECTORVARIABLE_H

#include "parser/variable.h"

// Forward Declarations
class TreeNode;

// Real 3-Vector Variable
class NuVectorVariable : public NuVariable
{
	public:
	// Constructor / Destructor
	NuVectorVariable(bool constant = FALSE);
	NuVectorVariable(Vec3<double> v, bool constant = FALSE);
	NuVectorVariable(TreeNode *x, TreeNode *y, TreeNode *z);
	~NuVectorVariable();

	/*
	// Set / Get
	*/
	public:
	// Return value of node
	bool execute(NuReturnValue &rv);
	// Set from returnvalue node
	bool set(NuReturnValue &rv);
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
	// Retrieve specified data via accessor
	static bool retrieve(int accessor, NuReturnValue &rv);
	// Set specified data
	static bool set(int accessor, NuReturnValue &rv);
	
};

#endif
