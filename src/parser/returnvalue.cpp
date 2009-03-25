/*
	*** Return Value
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

// Constructors
NuReturnValue::NuReturnValue()
{
	// Private variables
	type_ = NuVTypes::NoData;
}
NuReturnValue::NuReturnValue(int i) : type_(NuVTypes::IntegerData), valueI_(i)
{
}
NuReturnValue::NuReturnValue(double d) : type_(NuVTypes::RealData), valueR_(d)
{
}
NuReturnValue::NuReturnValue(const char *s) : type_(NuVTypes::StringData), valueS_(s)
{
}
NuReturnValue::NuReturnValue(Vec3<double> v) : type_(NuVTypes::VectorData), valueV_(v)
{
}
NuReturnValue::NuReturnValue(NuVTypes::DataType ptrtype, void *ptr) : type_(ptrtype), valueP_(ptr)
{
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
		case (NuVTypes::StringData):
			valueS_ = source.valueS_;
			break;
		case (NuVTypes::VectorData):
			valueV_ = source.valueV_;
			break;
		default:
			valueP_ = source.valueP_;
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
		case (NuVTypes::StringData):
			printf("%s\n",valueS_.get());
			break;
		case (NuVTypes::VectorData):
			printf("{%f,%f,%f}\n",valueV_.x,valueV_.y,valueV_.z);
			break;
		default:
			printf("%li\n",valueP_);
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
	type_ = NuVTypes::StringData;
	valueS_ = s;
}

// Set from vector value
void NuReturnValue::set(Vec3<double> v)
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
		case (NuVTypes::StringData):
			return atoi(valueS_.get());
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
		case (NuVTypes::StringData):
			return atof(valueS_.get());
			break;
		default:
			printf("NuReturnValue::asReal() doesn't recognise this type.\n");
			break;
	}
	success = FALSE;
	return 0;
}

// Return as character string
const char *NuReturnValue::asString(bool &success)
{
	static char converted[128];
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
		case (NuVTypes::StringData):
			return valueS_.get();
			break;
		case (NuVTypes::VectorData):
			converted[0] = '\0';
			sprintf(converted, "{%f,%f,%f}", valueV_.x, valueV_.y, valueV_.z);
			tempString_ = converted;
			return tempString_.get();
			break;
		default:
			// All pointer types
			converted[0] = '\0';
			sprintf(converted, "%li", valueP_);
			tempString_ = converted;
			return tempString_.get();
			break;
	}
	success = FALSE;
	return "NULL";
}

// Return as vector value
Vec3<double> NuReturnValue::asVector(bool &success)
{
	success = TRUE;
	switch (type_)
	{
		case (NuVTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as a character!\n");
			success = FALSE;
			return Vec3<double>();
			break;
		case (NuVTypes::VectorData):
			return valueV_;
			break;
		default:
			printf("Cannot convert return value of type '%s' into a vector.\n", NuVTypes::dataType(type_));
			break;
	}
	success = FALSE;
	return Vec3<double>();
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
		case (NuVTypes::StringData):
		case (NuVTypes::VectorData):
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
const char *NuReturnValue::asString()
{
	static bool success;
	return asString(success);
}

// Return as pointer value
void *NuReturnValue::asPointer(NuVTypes::DataType type)
{
	static bool success;
	return asPointer(type, success);
}
// Return as vector value
Vec3<double> NuReturnValue::asVector()
{
	static bool success;
	return asVector(success);
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
		case (NuVTypes::StringData):
			return valueS_.asBool();
			break;
		case (NuVTypes::VectorData):
			msg.print("Can't convert an object of type 'vector' into a bool.\n");
			return FALSE;
			break;
		default:
			// All pointer types here...
			return (valueP_ != NULL);
			break;
	}
	return FALSE;
}
