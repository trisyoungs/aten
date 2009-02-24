/*
	*** Vector Variable
	*** src/variables/vector.h
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

#ifndef ATEN_VECTORVARIABLE_H
#define ATEN_VECTORVARIABLE_H

#include "variables/variable.h"

// Vector Variable
class VectorVariable : public Variable
{
	public:
	// Constructor / Destructor
	VectorVariable();
	~VectorVariable();

	/*
	// Set / Get
	*/
	public:
	// Set size of array
	bool setArraySize(int size);
	// Set value of variable (vector)
	bool set(Vec3<double> v, Variable *index = NULL);
	// Set value of variable (pointer)
	bool set(void *ptr, VTypes::DataType type, Variable*index = NULL);
	// Set value of variable (char)
	bool set(const char *s, Variable *index = NULL);
	// Set value of variable (int)
	bool set(int i, Variable *index = NULL);
	// Set value of variable (double)
	bool set(double d, Variable *index = NULL);
	// Get value of variable as vector pointer
	void *asPointer(VTypes::DataType type, Variable *index = NULL);
	// Get value of variable as vector
	Vec3<double> asVector(Variable *index = NULL);
	// Clears value of variable
	bool reset(Variable *index = NULL);

	/*
	// Variable Data
	*/
	private:
	// Double data
	Vec3<double> vectorData_;
	// Array data
	Vec3<double> *vectorArrayData_;
};

#endif
