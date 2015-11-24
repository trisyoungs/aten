/*
	*** Variable
	*** src/parser/variable.h
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

#ifndef ATEN_VARIABLE_H
#define ATEN_VARIABLE_H

#include "parser/treenode.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Accessor;
class FunctionAccessor;
class Aten;

// Variable
class Variable : public TreeNode
{
	public:
	// Constructor / Destructor
	Variable();
	virtual ~Variable();


	/*
	 * Pointer to Aten
	 */
	protected:
	// Pointer to Aten
	static Aten* aten_;

	public:
	// Set pointer to Aten
	static void setAten(Aten* aten);


	/*
	 * Variable Character
	 */
	protected:
	// Name of the variable
	QString name_;
	// Initial value of new variable
	TreeNode* initialValue_;

	public:
	// Set name of variable
	void setName(QString name);
	// Get name of variable
	QString name() const;
	// Set initial value expression
	bool setInitialValue(TreeNode* node);
	// Return TreeNode corresponding to initial value
	TreeNode* initialValue() const;
	// Execute as an array
	virtual bool executeAsArray(ReturnValue& rv, int arrayIndex);
	// Set as an array
	virtual bool setAsArray(ReturnValue& rv, int arrayIndex);
	// Reset variable
	virtual void reset() = 0;
	// Search accessors (if any) available for node
	virtual StepNode* findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList = NULL);


	/*
	 * Inherited Virtuals
	 */
	public:
	// Initialise node
	bool initialise();


	/*
	 * Accessor Search / Print
	 */
	public:
	// Search accessor list provided
	static int searchAccessor(QString name, int nAccessors, Accessor *accessors);
	// Search accessor list provided
	static int searchAccessor(QString name, int nAccessors, FunctionAccessor *accessors);
	// Print valid accessors
	static void printValidAccessors(int nAccessors, Accessor* accessors, int nFunctions, FunctionAccessor* functions);
	// Check array source and destination sizes
	static bool checkAccessorArrays(Accessor& accessor, const ReturnValue& newValue, bool hasArrayIndex, int arrayIndex);
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
	TreeNode* arraySizeExpression_;
	// Array size
	int arraySize_;

	public:
	// Return current array size
	int arraySize() const;
};

ATEN_END_NAMESPACE

#endif
