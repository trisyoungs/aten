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
	// Constructor / Destructor
	Variable();
	~Variable();
	// List pointers
	Variable *prev, *next;

	/*
	// Variable Contents
	*/
	protected:
	// Name of the variable
	Dnchar name_;
	// Type of stored data
	VTypes::DataType dataType_;
	// List type (if any)
	VTypes::ArrayType arrayType_;
	// VariableList in which this variable exists
	VariableList *parent_;
	// Array index variable (if any)
	Variable *arrayIndex_;

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
	// Sets the array type of the variable
	void setArrayType(VTypes::ArrayType at);
	// Returns array type of the variable
	VTypes::ArrayType arrayType();
	// Set parent variablelist
	void setParent(VariableList *vlist);
	// Set array index variable
	void setArrayIndex(Variable *v);
	// Return array index variable
	Variable *arrayIndex();

	// Set value of variable (char)
	virtual void set(const char*)=0;
	// Set value of variable (int)
	virtual void set(int i)=0;
	// Set value of variable (double)
	virtual void set(double d)=0;
	// Set value of variable (pointer)
	virtual void set(void *ptr, VTypes::DataType type)=0;
	// Get value of variable as character string
	virtual const char *asCharacter()=0;
	// Get value of variable as integer
	virtual int asInteger()=0;
	// Get value of variable as double
	virtual double asDouble()=0;
	// Get value of variable as float
	virtual float asFloat()=0;
	// Get value of variable as a boolean
	virtual bool asBool()=0;
	// Get value of variable as pointer of specified type
	virtual void *asPointer(VTypes::DataType type)=0;
	// Integer increase
	virtual void increase(int)=0;
	// Integer decrease
	virtual void decrease(int)=0;
};

#endif
