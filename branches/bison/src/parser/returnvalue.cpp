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
#include "model/model.h"
#include "ff/forcefield.h"
#include "classes/grid.h"
#include "base/pattern.h"
#include <stdio.h>
#include <stdlib.h>

// Constructors
ReturnValue::ReturnValue()
{
	// Private variables
	type_ = VTypes::NoData;
}
ReturnValue::ReturnValue(int i) : type_(VTypes::IntegerData), valueI_(i)
{
}
ReturnValue::ReturnValue(double d) : type_(VTypes::DoubleData), valueD_(d)
{
}
ReturnValue::ReturnValue(const char *s) : type_(VTypes::StringData), valueS_(s)
{
}
ReturnValue::ReturnValue(Vec3<double> v) : type_(VTypes::VectorData), valueV_(v)
{
}
ReturnValue::ReturnValue(VTypes::DataType ptrtype, void *ptr) : type_(ptrtype), valueP_(ptr)
{
}

// Operator =
void ReturnValue::operator=(ReturnValue &source)
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
		case (VTypes::DoubleData):
			valueD_ = source.valueD_;
			break;
		case (VTypes::StringData):
			valueS_ = source.valueS_;
			break;
		case (VTypes::VectorData):
			valueV_ = source.valueV_;
			break;
		default:
			valueP_ = source.valueP_;
			break;
	}
}

// Return type of the stored data
VTypes::DataType ReturnValue::type()
{
	return type_;
}

// Reset data
void ReturnValue::reset()
{
	type_ = VTypes::NoData;
}

// Return string of contained data
const char *ReturnValue::info()
{
	static char result[8096];
	result[0] = '\0';
	switch (type_)
	{
		case (VTypes::NoData):
			sprintf(result,"nothing (%s)", VTypes::dataType(type_));
			break;
		case (VTypes::IntegerData):
			sprintf(result,"%i (%s)", valueI_, VTypes::dataType(type_));
			break;
		case (VTypes::DoubleData):
			sprintf(result,"%f (%s)", valueD_, VTypes::dataType(type_));
			break;
		case (VTypes::StringData):
			sprintf(result,"'%s' (%s)", valueS_.get(), VTypes::dataType(type_));
			break;
		case (VTypes::VectorData):
			sprintf(result,"{%f,%f,%f} (%s)", valueV_.x, valueV_.y, valueV_.z, VTypes::dataType(type_));
			break;
		default:
			sprintf(result,"%li (%s)", valueP_, VTypes::dataType(type_));
			break;
	}
	return result;
}

/*
// Set
*/

// Set from integer value
void ReturnValue::set(int i)
{
	type_ = VTypes::IntegerData;
	valueI_ = i;
}

// Set from double value
void ReturnValue::set(double d)
{
	type_ = VTypes::DoubleData;
	valueD_ = d;
}

// Set from character value
void ReturnValue::set(const char *s)
{
	type_ = VTypes::StringData;
	valueS_ = s;
}

// Set from vector value
void ReturnValue::set(Vec3<double> v)
{
	type_ = VTypes::VectorData;
	valueV_ = v;
}

// Set from individual vector data
void ReturnValue::set(double x, double y, double z)
{
	type_ = VTypes::VectorData;
	valueV_.set(x,y,z);
}

// Set from single vector data
void ReturnValue::set(int id, double xyz)
{
	type_ = VTypes::VectorData;
	valueV_.set(id, xyz);
}

// Set from pointer value
void ReturnValue::set(VTypes::DataType ptrtype, void *ptr)
{
	type_ = ptrtype;
	valueP_ = ptr;
}

/*
// Get (with type checking)
*/

// Return as integer value
int ReturnValue::asInteger(bool &success)
{
	success = TRUE;
	switch (type_)
	{
		case (VTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as an integer!\n");
			success = FALSE;
			return 0;
			break;
		case (VTypes::IntegerData):
			return valueI_;
			break;
		case (VTypes::DoubleData):
			return (int)valueD_;
			break;
		case (VTypes::StringData):
			return atoi(valueS_.get());
			break;
		default:
			printf("ReturnValue::asInteger() doesn't recognise this type.\n");
			break;
	}
	success = FALSE;
	return 0;
}

// Return as real value
double ReturnValue::asDouble(bool &success)
{
	success = TRUE;
	switch (type_)
	{
		case (VTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as a real!\n");
			success = FALSE;
			return 0;
			break;
		case (VTypes::IntegerData):
			return (double)valueI_;
			break;
		case (VTypes::DoubleData):
			return valueD_;
			break;
		case (VTypes::StringData):
			return atof(valueS_.get());
			break;
		default:
			printf("ReturnValue::asDouble() doesn't recognise this type.\n");
			break;
	}
	success = FALSE;
	return 0;
}

// Return as character string
const char *ReturnValue::asString(bool &success)
{
	static char converted[128];
	success = TRUE;
	switch (type_)
	{
		case (VTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as a character!\n");
			success = FALSE;
			return "_NULL_";
			break;
		case (VTypes::IntegerData):
			return itoa(valueI_);
			break;
		case (VTypes::DoubleData):
			return ftoa(valueD_);
			break;
		case (VTypes::StringData):
			return valueS_.get();
			break;
		case (VTypes::VectorData):
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
Vec3<double> ReturnValue::asVector(bool &success)
{
	success = TRUE;
	switch (type_)
	{
		case (VTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as a character!\n");
			success = FALSE;
			return Vec3<double>();
			break;
		case (VTypes::IntegerData):
			return Vec3<double>(valueI_, valueI_, valueI_);
			break;
		case (VTypes::DoubleData):
			return Vec3<double>(valueD_, valueD_, valueD_);
			break;
		case (VTypes::VectorData):
			return valueV_;
			break;
		default:
			printf("Cannot convert return value of type '%s' into a vector.\n", VTypes::dataType(type_));
			break;
	}
	success = FALSE;
	return Vec3<double>();
}

void *ReturnValue::asPointer(VTypes::DataType ptrtype, bool &success)
{
	success = TRUE;
	switch (type_)
	{
		case (VTypes::NoData):
			printf("Error: No data in ReturnValue to return as a pointer!\n");
			success = FALSE;
			return NULL;
			break;
		case (VTypes::IntegerData):
		case (VTypes::DoubleData):
		case (VTypes::StringData):
		case (VTypes::VectorData):
			msg.print("Error: A value of type '%s' cannot be cast into a pointer of type '%s'.\n", VTypes::dataType(type_), VTypes::dataType(ptrtype));
			success = FALSE;
			return NULL;
			break;
		default:
			// Check that internal pointer type matches requested pointer type
			if (ptrtype != type_)
			{
				msg.print("Error: A pointer of type '%s' cannot be cast into a pointer of type '%s'.\n", VTypes::dataType(type_), VTypes::dataType(ptrtype));
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
int ReturnValue::asInteger()
{
	static bool success;
	return asInteger(success);
}

// Return as real value
double ReturnValue::asDouble()
{
	static bool success;
	return asDouble(success);
}

// Return as character value
const char *ReturnValue::asString()
{
	static bool success;
	return asString(success);
}

// Return as pointer value
void *ReturnValue::asPointer(VTypes::DataType type)
{
	static bool success;
	return asPointer(type, success);
}
// Return as vector value
Vec3<double> ReturnValue::asVector()
{
	static bool success;
	return asVector(success);
}

// Return as boolean value
bool ReturnValue::asBool()
{
	switch (type_)
	{
		case (VTypes::NoData):
			return FALSE;
			break;
		case (VTypes::IntegerData):
			return (valueI_ > 0);
			break;
		case (VTypes::DoubleData):
			return (valueD_ > 0.0);
			break;
		case (VTypes::StringData):
			return valueS_.asBool();
			break;
		case (VTypes::VectorData):
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

/*
// In-place modify
*/

// Increase the contained variable
bool ReturnValue::increase()
{
	bool result = TRUE;
	switch (type_)
	{
		case (VTypes::NoData):
		case (VTypes::StringData):
		case (VTypes::VectorData):
		case (VTypes::AtenData):
		case (VTypes::CellData):
		case (VTypes::ElementsData):
			result = FALSE;
			break;
		case (VTypes::IntegerData):
			++valueI_;
			break;
		case (VTypes::DoubleData):
			++valueD_;
			break;
		case (VTypes::AtomData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Atom*) valueP_)->next;
			break;
		case (VTypes::BondData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Bond*) valueP_)->next;
			break;
		case (VTypes::ForcefieldData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Forcefield*) valueP_)->next;
			break;
		case (VTypes::GridData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Grid*) valueP_)->next;
			break;
		case (VTypes::ModelData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Model*) valueP_)->next;
			break;
		case (VTypes::PatternData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Pattern*) valueP_)->next;
			break;
		// TGAY What about ForcefieldAtomData, ForcefieldBoundData, PatternBoundData
		default:
			printf("Internal Error: No 'increase' has been defined for %s.\n", VTypes::aDataType(type_));
			break;
	}
	return result;
}

// Decrease the contained variable
bool ReturnValue::decrease()
{
	bool result = TRUE;
	switch (type_)
	{
		case (VTypes::NoData):
		case (VTypes::StringData):
		case (VTypes::VectorData):
		case (VTypes::AtenData):
		case (VTypes::CellData):
		case (VTypes::ElementsData):
			result = FALSE;
			break;
		case (VTypes::IntegerData):
			--valueI_;
			break;
		case (VTypes::DoubleData):
			--valueD_;
			break;
		case (VTypes::AtomData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Atom*) valueP_)->prev;
			break;
		case (VTypes::BondData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Bond*) valueP_)->prev;
			break;
		case (VTypes::ForcefieldData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Forcefield*) valueP_)->prev;
			break;
		case (VTypes::GridData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Grid*) valueP_)->prev;
			break;
		case (VTypes::ModelData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Model*) valueP_)->prev;
			break;
		case (VTypes::PatternData):
			if (valueP_ == NULL) result = FALSE;
			else valueP_ = ((Pattern*) valueP_)->prev;
			break;
		// TGAY What about ForcefieldAtomData, ForcefieldBoundData, PatternBoundData
		default:
			printf("Internal Error: No 'decrease' has been defined for %s.\n", VTypes::aDataType(type_));
			break;
	}
	return result;
}
