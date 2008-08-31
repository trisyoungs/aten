/*
	*** Real (floating-point) Variable
	*** src/variables/real.h
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

#ifndef ATEN_REALVARIABLE_H
#define ATEN_REALVARIABLE_H

#include "variables/variable.h"
#include "templates/vobject.h"

// Real Variable
class RealVariable : public Variable, VObject<double>
{
	public:
	// Constructor
	RealVariable();

	/*
	// Set / Get
	*/
	public:
	// Clears value of variable
	//void reset();
	// Set value of variable (char)
	void set(const char*);
	// Set value of variable (int)
	void set(int i);
	// Set value of variable (double)
	void set(double d);
	// Set value of variable (pointer)
	void set(void *ptr, VTypes::DataType type);
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
	// Get value of variable as pointer of specified type
	void *asPointer(VTypes::DataType type);
	// Double increase
	void increase(int);
	// Double decrease
	void decrease(int);
};

#endif
