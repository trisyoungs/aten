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

// Pointer Variable
class PointerVariable : public Variable
{
	public:
	// Constructor / Destructor
	PointerVariable(VTypes::DataType type = VTypes::NoData);
	~PointerVariable();

	/*
	// Set / Get
	*/
	public:
	// Set size of array (only for VTypes::ArrayType == NormalArray)
	bool setArraySize(int size);
	// Set value of variable (pointer)
	bool set(void *ptr, VTypes::DataType type, Variable *index = NULL);
	// Reset (non-array) variable with type and pointer provided
	bool reset(void *ptr, VTypes::DataType type);
	// Get value of variable as pointer of specified type
	void *asPointer(VTypes::DataType type, Variable *index = NULL);
	// Step variable
	bool step(int delta, Variable *index = NULL);
	// Clears value of variable
	bool reset(Variable *index = NULL);

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
