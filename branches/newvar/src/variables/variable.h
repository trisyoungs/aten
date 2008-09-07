/*
	*** Basic Variable
	*** src/variables/variable.h
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

#include "base/dnchar.h"
#include "base/vtypes.h"

// Forward declaration
class VariableList;

// Variable
class Variable
{
	public:
	// Constructor
	Variable();
	// List pointers
	Variable *prev, *next;

	/*
	// Variable Character
	*/
	protected:
	// Name of the variable
	Dnchar name_;
	// Type of stored data
	VTypes::DataType dataType_;
	// Whether data is read-only
	bool readOnly_;
	// VariableList in which this variable exists
	VariableList *parent_;
	// Size of array (if any, not for list types)
	int arraySize_;

	public:
	// Clears value of variable
	//void reset();
	// Set name of variable
	void setName(const char* s);
	// Get name of variable
	const char *name();
	// Sets the content type of the variable
	void setType(VTypes::DataType dt);
	// Returns content type of the variable
	VTypes::DataType type();
	// Set parent variablelist
	void setParent(VariableList *vlist);
	// Set the readonly status of the variable to TRUE
	void setReadOnly();
	// Return the readonly status of the variable
	bool readOnly();

	/*
	// Set / get
	*/
	public:
	// Set size of array (only for non-list derivations)
	virtual bool setArraySize(int size);
	// Set value of variable (char)
	virtual bool set(const char*, int index = -1);
	// Set value of variable (int)
	virtual bool set(int i, int index = -1);
	// Set value of variable (double)
	virtual bool set(double d, int index = -1);
	// Set value of variable (pointer)
	virtual bool set(void *ptr, VTypes::DataType type, int index = -1);
	// Get value of variable as character string
	virtual const char *asCharacter(int index = -1);
	// Get value of variable as integer
	virtual int asInteger(int index = -1);
	// Get value of variable as double
	virtual double asDouble(int index = -1);
	// Get value of variable as float
	float asFloat(int index = -1);
	// Get value of variable as a boolean
	virtual bool asBool(int index = -1);
	// Get value of variable as pointer of specified type
	virtual void *asPointer(VTypes::DataType type, int index = -1);
	// Step variable
	virtual bool step(int delta, int index = -1);
};

#endif
