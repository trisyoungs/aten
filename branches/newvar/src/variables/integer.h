/*
	*** Integer Variable
	*** src/variables/integer.h
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

#ifndef ATEN_INTEGERVARIABLE_H
#define ATEN_INTEGERVARIABLE_H

#include "variables/variable.h"

// Integer Variable
class IntegerVariable : public Variable
{
	public:
	// Constructor / Destructor
	IntegerVariable();
	~IntegerVariable();

	/*
	// Set / Get
	*/
	public:
	// Clears value of variable
	//void reset();
	// Set size of array
	bool setArraySize(int size);
	// Set value of variable (char)
	bool set(const char *s, Variable *index = NULL);
	// Set value of variable (int)
	bool set(int i, Variable *index = NULL);
	// Set value of variable (double)
	bool set(double d, Variable *index = NULL);
	// Get value of variable as character string
	const char *asCharacter(Variable *index = NULL);
	// Get value of variable as integer
	int asInteger(Variable *index = NULL);
	// Get value of variable as double
	double asDouble(Variable *index = NULL);
	// Get value of variable as a boolean
	bool asBool(Variable *index = NULL);
	// Step variable
	bool step(int delta, Variable *index = NULL);

	/*
	// Variable Data
	*/
	private:
	// Double data
	int integerData_;
	// Array data
	int *integerArrayData_;
};

#endif
