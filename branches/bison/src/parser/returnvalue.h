/*
	*** Tree Return Value
	*** src/parser/returnvalue.h
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

#ifndef ATEN_NURETURNVALUE_H
#define ATEN_NURETURNVALUE_H

#include "parser/returnvalue.h"
#include "base/vtypes.h"

// Tree Return Value
class NuReturnValue
{
	public:
	// Constructor
	NuReturnValue();
	// Operator=
	void operator=(NuReturnValue &rv);

	/*
	// Data
	*/
	private:
	// Data type contained in class
	VTypes::DataType type_;
	// Variable members for returns
	int valueI_;

	public:
	// Return type of the stored data
	VTypes::DataType type();
	// Reset data
	void reset();

	/*
	// Set
	*/
	public:
	// Set from integer value
	void set(int i);

	/*
	// Get
	*/
	public:
	// Return integer value
	int asInteger();
};

#endif
