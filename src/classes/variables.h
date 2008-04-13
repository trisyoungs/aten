/*
	*** Associative variable list
	*** src/classes/Variables.h
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

#ifndef ATEN_VARIABLES_H
#define ATEN_VARIABLES_H

#include "templates/list.h"
#include "classes/dnchar.h"
#include "base/sysfunc.h"

// Forward Declarations
class Atom;
class Bond;
class Cell;
class Pattern;
class Model;
class PatternBound;
class ForcefieldAtom;

// variable
class Variable
{
	public:
	// Variable Types
	enum VariableType { CharacterVariable, IntegerVariable, FloatVariable, AtomVariable, PatternVariable, ModelVariable, BondVariable, AngleVariable, TorsionVariable, AtomtypeVariable, nVariableTypes };
	static const char *variableType(VariableType);
	static VariableType determineType(const char *s);
	// Constructor
	Variable(VariableType vt = CharacterVariable);
	// List pointers
	Variable *prev, *next;

	/*
	// variable Contents
	*/
	private:
	// Name of the variable
	Dnchar name_;
	// Value of variable
	void *ptrValue_;
	Dnchar charValue_;
	int intValue_;
	double doubleValue_;
	// Content type of variable
	VariableType type_;
	// Whether the variable is a constant
	bool constant_;

	public:
	// Print contents of variable
	void print();
	// Clears value of variable
	void reset();
	// Set name of variable
	void setName(const char* s);
	// Set to constant value
	void setAsConstant(const char*);
	// Set value of variable (char)
	void set(const char*);
	// Set value of variable (int)
	void set(int i);
	// Set value of variable (double)
	void set(double d);
	// Set value of variable (atom*)
	void set(Atom*);
	// Set value of variable (pattern*)
	void set(Pattern*);
	// Set value of variable (model*)
	void set(Model*);
	// Set value of variable (PatternBound*)
	void set(PatternBound*);
	// Set value of variable (ForcefieldAtom*)
	void set(ForcefieldAtom*);
	// Copy pointer contents of source variable
	void copyPointer(Variable *v);
	// Sets the content type of the variable
	void setType(VariableType vt);
	// Set the variable to be a constant
	void setConstant();
	// Returns content type of the variable
	VariableType type();
	// Get name of variable
	const char *name();
	// Get value of variable as character string
	const char *asCharacter();
	// Get value of variable as integer
	int asInteger();
	// Get value of variable as double
	double asDouble();
	// Get value of variable as float
	float asFloat();
	// Get value of variable as a boolean
	bool asBool();
	// Get value of variable as pointer
	void *asPointer();
	// Integer increase
	void increase(int);
	// Integer decrease
	void decrease(int);
};

// variable list
class VariableList
{
	/*
	// variable List
	*/
	private:
	// List of variables
	List<Variable> vars_;
	// Static, dummy variable '*'
	Variable dummy_;

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
	Variable *addConstant(const char* s);
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
