/*
	*** Variable
	*** src/classes/variable.h
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

#ifndef ATEN_VARIABLE_H
#define ATEN_VARIABLE_H

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
class Expression;

// Variable
class Variable
{
	public:
	// Variable Types
	enum VariableType { CharacterVariable, IntegerVariable, FloatVariable, AtomVariable, PatternVariable, ModelVariable, BondVariable, AngleVariable, TorsionVariable, AtomtypeVariable, ExpressionVariable, nVariableTypes };
	static const char *variableType(VariableType);
	static VariableType determineType(const char *s);
	// Constructor / Destructor
	Variable(VariableType vt = CharacterVariable);
	~Variable();
	// List pointers
	Variable *prev, *next;

	/*
	// Variable Contents
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

	public:
	// Print contents of variable
	void print();
	// Clears value of variable
	void reset();
	// Set name of variable
	void setName(const char* s);
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
	// Set value of variable (Expression*)
	void set(Expression*);
	// Copy pointer contents of source variable
	void copyPointer(Variable *v);
	// Sets the content type of the variable
	void setType(VariableType vt);
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

#endif
