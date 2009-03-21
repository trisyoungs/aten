/*
	*** Variable Access Step
	*** src/variables/accessstep.h
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

#ifndef ATEN_ACCESSSTEP_H
#define ATEN_ACCESSSTEP_H

#include "base/vtypes.h"
#include "templates/vector3.h"

// Forward declarations
class AccessPath;
class Variable;
class VariableList;

// Variable Access Step
class AccessStep
{
	public:
	// Constructor / Destructor
	AccessStep();
	~AccessStep();
	// List pointers
	AccessStep *prev, *next;

	private:
	// Target variable
	Variable *target_;
	// Enumerated index of variable (for class accessor variables)
	int variableId_;
	// Array index associated to variable (if any)
	AccessPath *arrayIndex_;
	// Create arrayindex path
	bool setArrayIndex(const char *path, VariableList *parentvars);

	public:
	// Set target variable
	bool setTarget(const char *var, VariableList *parentvars, VariableList *searchvars);
	// Set target as integer constant variable
	void setConstant(int i, VariableList *sourcevars);
	// Set target as expression
	bool setExpression(const char *s, VariableList *sourcevars);
	// Return target variable's name
	const char *targetName();
	// Return target variable pointer
	Variable *target();
	// Return whether the step has an array index path set
	bool hasArrayIndex();
	// Return array index as integer value
	int arrayIndex();
	// Set enumerated target variable ID
	void setVariableId(int);
	// Return variable ID
	int variableId();
	// Get target value as integer
	int asInteger();
	// Get target value as double
	double asDouble();
	// Get target value as character
	const char *asCharacter();
	// Get target value as bool
	bool asBool();
	// Get target value as pointer
	void *asPointer(VTypes::DataType dt);
	// Get target value as pointer
 	Vec3<double> asVector();
	// Get return type of step (i.e. DataType of target variable)
	VTypes::DataType type();
	// Set value of target variable from source variable
	void setTargetVariable(Variable *srcvar);
	// Step target variable by specified amount
	bool stepTargetVariable(int delta);
};

#endif
