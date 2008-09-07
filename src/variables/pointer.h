/*
	*** Pointer Variable
	*** src/variables/pointer.h
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

#ifndef ATEN_POINTERVARIABLE_H
#define ATEN_POINTERVARIABLE_H

#include "variables/variable.h"

// Integer Variable
class PointerVariable : public Variable
{
	public:
	// Constructor
	PointerVariable(VTypes::DataType);

	/*
	// Set / Get
	*/
	public:
	// Clears value of variable
	//void reset();
	// Set size of array (only for VTypes::ArrayType == NormalArray)
	bool setArraySize(int size);
	// Set value of variable (char)
	bool set(const char *s, int index = -1);
	// Set value of variable (int)
	bool set(int i, int index = -1);
	// Set value of variable (double)
	bool set(double d, int index = -1);
	// Set value of variable (pointer)
	bool set(void *ptr, VTypes::DataType type, int index = -1);
	// Get value of variable as character string
	const char *asCharacter(int index = -1);
	// Get value of variable as integer
	int asInteger(int index = -1);
	// Get value of variable as double
	double asDouble(int index = -1);
	// Get value of variable as float
	float asFloat(int index = -1);
	// Get value of variable as a boolean
	bool asBool(int index = -1);
	// Get value of variable as pointer of specified type
	void *asPointer(VTypes::DataType type, int index = -1);
	// Step variable
	bool step(int delta, int index = -1);

	/*
	// Variable Data
	*/
	private:
	// Pointer data
	void *ptrData_;
	// Array data
	void **ptrArrayData_;
};

#endif
