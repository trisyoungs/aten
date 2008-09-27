/*
	*** Variable list
	*** src/command/variablelist.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_VARIABLELIST_H
#define ATEN_VARIABLELIST_H

#include "variables/variable.h"
#include "variables/character.h"
#include "base/constants.h"
#include "base/dnchar.h"
#include "templates/list.h"

// Forward Declarations
class Atom;
class Bond;
class Cell;
class Pattern;
class Model;
class PatternBound;
class ForcefieldAtom;
class ExpressionVariable;

// Variable list
class VariableList
{
	/*
	// Variable List
	*/
	public:
	// Constructor / Destructor
	VariableList();

	private:
	// List of variables / references
	List<Variable> variables_;
	// Static, dummy variable '*'
	CharacterVariable dummy_;
	// List of constant values
	List<Variable> constants_;
	// List of expressions
	List<Variable> expressions_;
	// List of variable paths
	List<Variable> paths_;
	// Create variable of specified type
	Variable *createVariable(VTypes::DataType dt, int arraysize = -1);

	public:
	// Return list position (id) of Variable in list
	int variableId(Variable *v);
	// Set existing (or create new) variable (character string)
	void set(const char*, const char*, const char*);
	void set(const char *name, const char *value);
	// Set existing variable (integer)
	void set(const char*, const char*, int);
	void set(const char *name, int value);
	// Set existing variable (double)
	void set(const char*, const char*, double);
	void set(const char *name, double value);
	// Set existing variable (pointer)
	void set(const char*, const char*, void *ptr, VTypes::DataType vt);
	void set(const char *name, void *ptr, VTypes::DataType vt);
	// Retrieve a named variable from the list
	Variable *get(const char *prefix, const char *suffix);
	Variable *get(const char *name);
	// Return dummy variable
	Variable *dummy();
	// Add a constant to the list
	Variable *addConstant(const char *s, bool forcecharacter = FALSE);
	// Add an integer constant to the list
	Variable *addConstant(int i);
	// Add an expression to the list
	Variable *addExpression(const char *s);
	// Add a variable access path to the list
	Variable *addPath(const char *s);
	// Add a named variable to the list
	Variable *addVariable(const char *prefix, const char *suffix, VTypes::DataType vt, int arraysize = -1);
	Variable *addVariable(const char *name, VTypes::DataType vt, int arraysize = -1);
	Variable *addListVariable(const char *name, VTypes::DataType vt, void *ptr);
	// Add pointer to data in Bundle variable
	Variable *addBundlePointer(const char *name, VTypes::DataType vt);
	// Clear all variables, expressions etc. stored in class
	void clear();
	// Print list of variables and their values
	void print();
};

#endif
