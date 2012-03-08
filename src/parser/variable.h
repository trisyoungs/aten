/*
	*** Variable
	*** src/parser/variable.h
	Copyright T. Youngs 2007-2012

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

#ifndef ATEN_VARIABLE_H
#define ATEN_VARIABLE_H

#include "parser/treenode.h"
#include "base/dnchar.h"
#include "templates/vector3.h"
#include <stdlib.h>

// Forward Declarations
class Accessor;
class FunctionAccessor;

// Variable
class Variable : public TreeNode
{
	public:
	// Constructor / Destructor
	Variable();
	virtual ~Variable();


	/*
	// Variable Character
	*/
	protected:
	// Name of the variable
	Dnchar name_;
	// Initial value of new variable
	TreeNode *initialValue_;

	public:
	// Set name of variable
	void setName(const char* s);
	// Get name of variable
	const char *name() const;
	// Set initial value expression
	bool setInitialValue(TreeNode *node);
	// Return TreeNode corresponding to initial value
	TreeNode *initialValue() const;
	// Execute as an array
	virtual bool executeAsArray(ReturnValue &rv, int arrayindex);
	// Set as an array
	virtual bool setAsArray(ReturnValue &rv, int arrayindex);
	// Reset variable
	virtual void reset() = 0;
	// Search accessors (if any) available for node
	virtual StepNode *findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist = NULL);


	/*
	// Inherited Virtuals
	*/
	public:
	// Initialise node
	bool initialise();


	/*
	// Accessor Search
	*/
	public:
	// Search accessor list provided
	static int searchAccessor(const char *s, int nAccessors, Accessor *accessors);
	// Search accessor list provided
	static int searchAccessor(const char *s, int nAccessors, FunctionAccessor *accessors);
};

// Array Variable
class ArrayVariable : public Variable
{
	public:
	// Constructor / Destructor
	ArrayVariable();
	virtual ~ArrayVariable();

	protected:
	// TreeNode determining array size on initialisation
	TreeNode *arraySizeExpression_;
	// Array size
	int arraySize_;

	public:
	// Return current array size
	int arraySize() const;
};


#endif
