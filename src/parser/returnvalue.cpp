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
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor
NuReturnValue::NuReturnValue()
{
	// Private variables
	type_ = NuVTypes::NoData;
}

// Operator =
void NuReturnValue::operator=(NuReturnValue &source)
{
	// Copy datatype of source
	type_ = source.type_;
	// To reduce unnecessary object creation/destruction, only copy the contents that corresponds to the type_
	switch (type_)
	{
		case (NuVTypes::NoData):
			break;
		case (NuVTypes::IntegerData):
			valueI_ = source.valueI_;
			break;
		case (NuVTypes::RealData):
			valueR_ = source.valueR_;
			break;
		case (NuVTypes::CharacterData):
			valueC_ = source.valueC_;
			break;
		default:
			printf("NuReturnValue::operator= - unrecognised variable type.\n");
			break;
	}
}

// Return type of the stored data
NuVTypes::DataType NuReturnValue::type()
{
	return type_;
}

// Reset data
void NuReturnValue::reset()
{
	type_ = NuVTypes::NoData;
}

// Print info on data contents
void NuReturnValue::info()
{
	printf("This returnvalue contains data of type '%s':", NuVTypes::dataType(type_));
	switch (type_)
	{
		case (NuVTypes::NoData):
			printf("<nothing>\n");
			break;
		case (NuVTypes::IntegerData):
			printf("%i\n",valueI_);
			break;
		case (NuVTypes::RealData):
			printf("%f\n",valueR_);
			break;
		case (NuVTypes::CharacterData):
			printf("%s\n",valueC_.get());
			break;
		default:
			printf("<Don't know how to print this data type>\n");
			break;
	}
}

/*
// Set
*/

// Set from integer value
void NuReturnValue::set(int i)
{
	type_ = NuVTypes::IntegerData;
	valueI_ = i;
}

// Set from double value
void NuReturnValue::set(double d)
{
	type_ = NuVTypes::RealData;
	valueR_ = d;
}

// Set from character value
void NuReturnValue::set(const char *s)
{
	type_ = NuVTypes::CharacterData;
	valueC_ = s;
}

// Set from vector value
void NuReturnValue::set(Vec3<double> &v)
{
	type_ = NuVTypes::VectorData;
	valueV_ = v;
}

// Set from individual vector data
void NuReturnValue::set(double x, double y, double z)
{
	type_ = NuVTypes::VectorData;
	valueV_.set(x,y,z);
}

// Set from single vector data
void NuReturnValue::set(int id, double xyz)
{
	type_ = NuVTypes::VectorData;
	valueV_.set(id, xyz);
}

// Set from pointer value
void NuReturnValue::set(NuVTypes::DataType ptrtype, void *ptr)
{
	type_ = ptrtype;
	valueP_ = ptr;
}

/*
// Get (with type checking)
*/

// Return as integer value
int NuReturnValue::asInteger(bool &success)
{
	success = TRUE;
	switch (type_)
	{
		case (NuVTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as an integer!\n");
			success = FALSE;
			return 0;
			break;
		case (NuVTypes::IntegerData):
			return valueI_;
			break;
		case (NuVTypes::RealData):
			return (int)valueR_;
			break;
		case (NuVTypes::CharacterData):
			return atoi(valueC_.get());
			break;
		default:
			printf("NuReturnValue::asInteger() doesn't recognise this type.\n");
			break;
	}
	success = FALSE;
	return 0;
}

// Return as real value
double NuReturnValue::asReal(bool &success)
{
	success = TRUE;
	switch (type_)
	{
		case (NuVTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as a real!\n");
			success = FALSE;
			return 0;
			break;
		case (NuVTypes::IntegerData):
			return (double)valueI_;
			break;
		case (NuVTypes::RealData):
			return valueR_;
			break;
		case (NuVTypes::CharacterData):
			return atof(valueC_.get());
			break;
		default:
			printf("NuReturnValue::asInteger() doesn't recognise this type.\n");
			break;
	}
	success = FALSE;
	return 0;
}

// Return as character value
const char *NuReturnValue::asCharacter(bool &success)
{
	success = TRUE;
	switch (type_)
	{
		case (NuVTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as a character!\n");
			success = FALSE;
			return "_NULL_";
			break;
		case (NuVTypes::IntegerData):
			return itoa(valueI_);
			break;
		case (NuVTypes::RealData):
			return ftoa(valueR_);
			break;
		case (NuVTypes::CharacterData):
			return valueC_.get();
			break;
		default:
			printf("NuReturnValue::asInteger() doesn't recognise this type.\n");
			break;
	}
	success = FALSE;
	return 0;
}

void *NuReturnValue::asPointer(NuVTypes::DataType ptrtype, bool &success)
{
	success = TRUE;
	switch (type_)
	{
		case (NuVTypes::NoData):
			printf("Error: No data in ReturnValue to return as a pointer!\n");
			success = FALSE;
			return NULL;
			break;
		case (NuVTypes::IntegerData):
		case (NuVTypes::RealData):
		case (NuVTypes::CharacterData):
			msg.print("Error: A value of type '%s' cannot be cast into a pointer of type '%s'.\n", NuVTypes::dataType(type_), NuVTypes::dataType(ptrtype));
			success = FALSE;
			return NULL;
			break;
		default:
			// Check that internal pointer type matches requested pointer type
			if (ptrtype != type_)
			{
				msg.print("Error: A pointer of type '%s' cannot be cast into a pointer of type '%s'.\n", NuVTypes::dataType(type_), NuVTypes::dataType(ptrtype));
				success = FALSE;
				return NULL;
			}
			else return valueP_;
			break;
	}
	success = FALSE;
	return 0;
}

/*
// Get (no type checking)
*/

// Return as integer value
int NuReturnValue::asInteger()
{
	static bool success;
	return asInteger(success);
}

// Return as real value
double NuReturnValue::asReal()
{
	static bool success;
	return asReal(success);
}

// Return as character value
const char *NuReturnValue::asCharacter()
{
	static bool success;
	return asCharacter(success);
}

// Return as pointer value
void *NuReturnValue::asPointer(NuVTypes::DataType type)
{
	static bool success;
	return asPointer(type, success);
}

// Return as boolean value
bool NuReturnValue::asBool()
{
	switch (type_)
	{
		case (NuVTypes::NoData):
			return FALSE;
			break;
		case (NuVTypes::IntegerData):
			return (valueI_ > 0);
			break;
		case (NuVTypes::RealData):
			return (valueR_ > 0.0);
			break;
		case (NuVTypes::CharacterData):
			return valueC_.asBool();
			break;
		default:
			printf("NuReturnValue::asInteger() doesn't recognise this type.\n");
			break;
	}
	return FALSE;
}
