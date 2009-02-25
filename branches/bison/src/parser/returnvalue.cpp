/*
	*** Tree Return Value
	*** src/parser/treevalue.cpp
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

#include "parser/returnvalue.h"
#include "base/messenger.h"
#include "base/sysfunc.h"
#include <stdio.h>

// Constructor
NuReturnValue::NuReturnValue()
{
	// Private variables
	type_ = VTypes::NoData;
}

// Operator =
void NuReturnValue::operator=(NuReturnValue &source)
{
	// Copy datatype of source
	type_ = source.type_;
	// To reduce unnecessary object creation/destruction, only copy the contents that corresponds to the type_
	switch (type_)
	{
		case (VTypes::NoData):
			break;
		case (VTypes::IntegerData):
			valueI_ = source.valueI_;
			break;
		default:
			printf("NuReturnValue::operator= - unrecognised variable type.\n");
			break;
	}
}

// Return type of the stored data
VTypes::DataType NuReturnValue::type()
{
	return type_;
}

// Reset data
void NuReturnValue::reset()
{
	type_ = VTypes::NoData;
}

/*
// Set
*/

// Set from integer value
void NuReturnValue::set(int i)
{
	type_ = VTypes::IntegerData;
	valueI_ = i;
}

/*
// Get
*/

// Return as integer value
int NuReturnValue::asInteger()
{
	switch (type_)
	{
		case (VTypes::NoData):
			printf("No data in ReturnValue to return!\n");
			return 0;
			break;
		case (VTypes::IntegerData):
			return valueI_;
			break;
		default:
			printf("NuReturnValue::asInteger() doesn't recognise this type.\n");
			break;
	}
	return 0;
}
