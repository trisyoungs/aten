/*
	*** Double (Real) Variable
	*** src/variables/double.h
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

#ifndef ATEN_DOUBLEVARIABLE_H
#define ATEN_DOUBLEVARIABLE_H

#include "variables/variable.h"
#include "base/vobject.h"

// Double Variable
class DoubleVariable : public Variable : public VObject<int>
{
	public:
	// Constructor
	DoubleVariable();

	/*
	// Set / Get
	*/
	public:
	// Clears value of variable
	//void reset();
	// Set name of variable
	void setName(const char* s);
	// Get name of variable
	const char *name();
	// Sets the content type of the variable
	void setType(VObject::DataType vt);
	// Returns content type of the variable
	VObject::DataType type();

	// Set value of variable (char)
	void set(const char*)=0;
	// Set value of variable (int)
	void set(int i)=0;
	// Set value of variable (double)
	void set(double d)=0;
	// Set value of variable (pointer)
	void set(void *ptr, VObject::DataType type)=0;
	// Get value of variable as character string
	const char *asCharacter()=0;
	// Get value of variable as integer
	int asDouble()=0;
	// Get value of variable as double
	double asDouble()=0;
	// Get value of variable as float
	float asFloat()=0;
	// Get value of variable as a boolean
	bool asBool()=0;
	// Get value of variable as pointer of specified type
	void *asPointer(VObject::DataType type)=0;
	// Double increase
	void increase(int);
	// Double decrease
	void decrease(int);
};

#endif
