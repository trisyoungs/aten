/*
	*** Variable list
	*** src/classes/variablelist.h
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

#include "templates/list.h"
#include "classes/dnchar.h"
#include "classes/variable.h"
#include "base/sysfunc.h"
#include "base/constants.h"

// Forward Declarations
class Atom;
class Bond;
class Cell;
class Pattern;
class Model;
class PatternBound;
class ForcefieldAtom;

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
	// List of variables
	List<Variable> vars_;
	// Static, dummy variable '*'
	Variable dummy_;
	// List of constant values
	List<Variable> constants_;
	// List of expressions
	List<Variable> expressions_;

	public:
	// Set existing (or create new) variable (VT_CHAR)
	void set(const char*, const char*, const char*);
	void set(const char *name, const char *value);
	// Set existing (or create new) variable (VT_INT)
	void set(const char*, const char*, int);
	void set(const char *name, int value);
	// Set existing (or create new) variable (VT_FLOAT)
	void set(const char*, const char*, double);
	void set(const char *name, double value);
	// Retrieve a named variable from the list
	Variable *get(const char *prefix, const char *suffix);
	Variable *get(const char *name);
	// Return dummy variable
	Variable *dummy();
	// Add an unnamed constant to the list
	Variable *addConstant(const char *s, bool forcecharacter = FALSE);
	// Add an unnamed expression to the list
	Variable *addExpression(const char *s);
	// Add a named variable to the list
	Variable *addVariable(const char *prefix, const char *suffix, Variable::VariableType vt);
	Variable *addVariable(const char *name, Variable::VariableType vt);
	// Create, but don't set, a named variable in the list
	Variable *createVariable(const char *prefix, const char *suffix, Variable::VariableType vt);
	// Reset values of all variables
	void resetAll();
	// Reset values of variable selection
	void reset(const char*, ...);
	// Print list of variables and their values
	void print();
};

#endif
