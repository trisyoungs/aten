/*
	*** Variable List
	*** src/parser/variablelist.h
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

#ifndef ATEN_NUVARIABLELIST_H
#define ATEN_NUVARIABLELIST_H

#include "parser/variable.h"
#include "base/constants.h"
#include "templates/list.h"

// Forward Declarations
class NuIntegerVariable;
class NuCharacterVariable;
class NuRealVariable;

// Variable list
class NuVariableList
{
	/*
	// Variable List
	*/
	public:
	// Constructor / Destructor
	NuVariableList();

	private:
	// List of variables
	List<NuVariable> variables_;
	// List of constants
	List<NuVariable> constants_;
	// Create variable of specified type
	static NuVariable *createVariable(NuVTypes::DataType type, const char *name, TreeNode *initialValue = NULL);

	public:
	// Pass a newly-created variable / constant to the list for it to take ownership of
	void take(NuVariable *v);
	// Retrieve a named variable from the list
	NuVariable *find(const char *name);
	// Create a new variable in the list
	NuVariable *create(NuVTypes::DataType type, const char *name, TreeNode *initialValue = NULL);
	// Create a new variable (static function, so we don't take ownership of it).
	NuVariable *createFree(NuVTypes::DataType type, const char *name, TreeNode *initialValue = NULL);
	// Create a new array variable in the list
	NuVariable *createArray(NuVTypes::DataType type, const char *name, TreeNode *sizeexpr, TreeNode *initialValue = NULL);
	// Return the number of variables (not constants) contained in the list
	int nVariables();
	// Return first variable in the list
	NuVariable *first();
	// Reset all variable values
	void initialise();
	// Clear all variables, expressions etc. stored in class
	void clear();
	// Print list of variables and their values
	void print();
};

#endif
