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
#include "base/vobject.h"

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
	private:
	// Name of the variable
	Dnchar name_;
	// Type of stored data
	VTypes::DataType dataType_;
	// List type (if any)
	VTypes::ListType listType_;

	public:
	// Clears value of variable
	//void reset();
	// Set name of variable
	void setName(const char* s);
	// Get name of variable
	const char *name();
	// Sets the content type of the variable
	void setType(VTypes::DataType vt);
	// Returns content type of the variable
	VTypes::DataType type();

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
	virtual void increase(int);
	// Integer decrease
	virtual void decrease(int);
};

#endif
