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
	arraySize_ = -1;
}
ReturnValue::ReturnValue(int i) : type_(VTypes::IntegerData), valueI_(i), arraySize_(-1)
{
}
ReturnValue::ReturnValue(double d) : type_(VTypes::DoubleData), valueD_(d), arraySize_(-1)
{
}
ReturnValue::ReturnValue(const char *s) : type_(VTypes::StringData), valueS_(s), arraySize_(-1)
{
}
ReturnValue::ReturnValue(Vec3<double> v) : type_(VTypes::VectorData), valueV_(v), arraySize_(-1)
{
}

// Operator =
void ReturnValue::operator=(ReturnValue &source)
{
	clearArrayData();
	// Copy datatype of source
	type_ = source.type_;
	arraySize_ = source.arraySize_;
	// To reduce unnecessary object creation/destruction, only copy the contents that corresponds to the type_
	switch (type_)
	{
		case (VTypes::NoData):
			break;
		case (VTypes::IntegerData):
			if (arraySize_ == -1) valueI_ = source.valueI_;
			else
			{
				arrayI_ = new int[arraySize_];
				for (int i=0; i<arraySize_; ++i) arrayI_[i] = source.arrayI_[i];
			}
			break;
		case (VTypes::DoubleData):
			if (arraySize_ == -1) valueD_ = source.valueD_;
			else
			{
				arrayD_ = new double[arraySize_];
				for (int i=0; i<arraySize_; ++i) arrayD_[i] = source.arrayD_[i];
			}
			break;
		case (VTypes::StringData):
			if (arraySize_ == -1) valueS_ = source.valueS_;
			else
			{
				arrayS_ = new Dnchar[arraySize_];
				for (int i=0; i<arraySize_; ++i) arrayS_[i] = source.arrayS_[i];
			}
			break;
		case (VTypes::VectorData):
			if (arraySize_ == -1) valueV_ = source.valueV_;
			else
			{
				arrayV_ = new Vec3<double>[arraySize_];
				for (int i=0; i<arraySize_; ++i) arrayV_[i] = source.arrayV_[i];
			}
			break;
		default:
			if (arraySize_ == -1) valueP_ = source.valueP_;
			else
			{
				arrayP_ = new void*[arraySize_];
				for (int i=0; i<arraySize_; ++i) arrayP_[i] = source.arrayP_[i];
			}
			break;
	}
}

// Clear any current array data
void ReturnValue::clearArrayData()
{
	if (arraySize_ == -1) return;
	if (type_ == VTypes::IntegerData) { delete[] arrayI_; arrayI_ = NULL; }
	else if (type_ == VTypes::DoubleData) { delete[] arrayD_; arrayD_ = NULL; }
	else if (type_ == VTypes::StringData) { delete[] arrayS_; arrayS_ = NULL; }
	else if (type_ == VTypes::VectorData) { delete[] arrayV_; arrayV_ = NULL; }
	else { if (arrayP_ != NULL) delete[] arrayP_; arrayP_ = NULL; }
	arraySize_ == -1;
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
	arraySize_ = -1;
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
			if (arraySize_ == -1) sprintf(result,"%i (%s)", valueI_, VTypes::dataType(type_));
			else sprintf(result,"%i elements (array of %s)", arraySize_, VTypes::dataType(type_));
			break;
		case (VTypes::DoubleData):
			if (arraySize_ == -1) sprintf(result,"%f (%s)", valueD_, VTypes::dataType(type_));
			else sprintf(result,"%i elements (array of %s)", arraySize_, VTypes::dataType(type_));
			break;
		case (VTypes::StringData):
			if (arraySize_ == -1) sprintf(result,"'%s' (%s)", valueS_.get(), VTypes::dataType(type_));
			else sprintf(result,"%i elements (array of %s)", arraySize_, VTypes::dataType(type_));
			break;
		case (VTypes::VectorData):
			if (arraySize_ == -1) sprintf(result,"{%f,%f,%f} (%s)", valueV_.x, valueV_.y, valueV_.z, VTypes::dataType(type_));
			else sprintf(result,"%i elements (array of %s)", arraySize_, VTypes::dataType(type_));
			break;
		default:
			if (arraySize_ == -1) sprintf(result,"%li (%s)", valueP_, VTypes::dataType(type_));
			else sprintf(result,"%i elements (array of %s)", arraySize_, VTypes::dataType(type_));
			break;
	}
	return result;
}

// Return unique 'pair' code based on return types
int ReturnValue::dataPair(ReturnValue &source)
{
	return VTypes::dataPair(type_, arraySize_, source.type_, source.arraySize_);
}

/*
// Set
*/

// Set from integer value
void ReturnValue::set(int i)
{
	clearArrayData();
	type_ = VTypes::IntegerData;
	valueI_ = i;
	arraySize_ = -1;
}

// Set from double value
void ReturnValue::set(double d)
{
	clearArrayData();
	type_ = VTypes::DoubleData;
	valueD_ = d;
	arraySize_ = -1;
}

// Set from character value
void ReturnValue::set(const char *s)
{
	clearArrayData();
	type_ = VTypes::StringData;
	valueS_ = s;
	arraySize_ = -1;
}

// Set from vector value
void ReturnValue::set(Vec3<double> v)
{
	clearArrayData();
	type_ = VTypes::VectorData;
	valueV_ = v;
	arraySize_ = -1;
}

// Set from individual vector data
void ReturnValue::set(double x, double y, double z)
{
	clearArrayData();
	type_ = VTypes::VectorData;
	valueV_.set(x,y,z);
	arraySize_ = -1;
}

// Set from single vector data
void ReturnValue::set(int id, double xyz)
{
	clearArrayData();
	type_ = VTypes::VectorData;
	valueV_.set(id, xyz);
	arraySize_ = -1;
}

// Set from pointer value
void ReturnValue::set(VTypes::DataType ptrtype, void *ptr)
{
	clearArrayData();
	type_ = ptrtype;
	valueP_ = ptr;
	arraySize_ = -1;
}

// Set from standard array
void ReturnValue::setArray(VTypes::DataType type, void *array, int arraysize)
{
	clearArrayData();
	type_ = type;
	arraySize_ = arraysize;
	int i;
	if (type_ == VTypes::IntegerData)
	{
		arrayI_ = new int[arraySize_];
		int *source = (int*) array;
		for (i = 0; i < arraySize_; ++i) arrayI_[i] = source[i];
	}
	else if (type_ == VTypes::DoubleData)
	{
		arrayD_ = new double[arraySize_];
		double *source = (double*) array;
		for (i = 0; i < arraySize_; ++i) arrayD_[i] = source[i];
	}
	else if (type_ == VTypes::StringData)
	{
		arrayS_ = new Dnchar[arraySize_];
		Dnchar *source = (Dnchar*) array;
		for (i = 0; i < arraySize_; ++i) arrayS_[i] = source[i];
	}
	else if (type_ == VTypes::VectorData)
	{
		arrayV_ = new Vec3<double>[arraySize_];
		Vec3<double> *source = (Vec3<double>*) array;
		for (i = 0; i < arraySize_; ++i) arrayV_[i] = source[i];
	}
	else
	{
		// Pointer-type arrays are a little different
		arrayP_ = new void*[arraySize_];
		void **source = (void**) array;
		for (i = 0; i < arraySize_; ++i) arrayP_[i] = source[i];
	}
}

// Set array element from integer value
void ReturnValue::setElement(int id, int i)
{
	if ((type_ == VTypes::IntegerData) && (id >= 0) && (id < arraySize_)) arrayI_[id] = i;
	else printf("Error: Tried to set an integer array element when none existed/wrong type.\n");
}

// Set array element from real value
void ReturnValue::setElement(int id, double d)
{
	if ((type_ == VTypes::DoubleData) && (id >= 0) && (id < arraySize_)) arrayD_[id] = d;
	else printf("Error: Tried to set a double array element when none existed/wrong type.\n");
}

// Set array element from character value
void ReturnValue::setElement(int id, const char *s)
{
	if ((type_ == VTypes::StringData) && (id >= 0) && (id < arraySize_)) arrayS_[id] = s;
	else printf("Error: Tried to set a string array element when none existed/wrong type.\n");
}

// Set array element from vector value
void ReturnValue::setElement(int id, Vec3<double> v)
{
	if ((type_ == VTypes::VectorData) && (id >= 0) && (id < arraySize_)) arrayV_[id] = v;
	else printf("Error: Tried to set a vector array element when none existed/wrong type.\n");
}

// Set array element from pointer value
void ReturnValue::setElement(int id, VTypes::DataType type, void *ptr)
{
	if ((type_ == type) && (id >= 0) && (id < arraySize_)) arrayP_[id] = ptr;
	else printf("Error: Tried to set a pointer array element when none existed/wrong type.\n");
}

/*
// Get (with type checking)
*/

// Return as integer value
int ReturnValue::asInteger(bool &success)
{
	success = TRUE;
	if (arraySize_ != -1) msg.print("Cannot return a whole array as a single integer.\n");
	else switch (type_)
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
			return valueS_.asInteger();
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
	if (arraySize_ != -1) msg.print("Cannot return a whole array as a single double.\n");
	else switch (type_)
	{
		case (VTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as a real!\n");
			success = FALSE;
			return 0.0;
			break;
		case (VTypes::IntegerData):
			return (double)valueI_;
			break;
		case (VTypes::DoubleData):
			return valueD_;
			break;
		case (VTypes::StringData):
			return valueS_.asDouble();
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
	if (arraySize_ != -1)
	{
		// Use valueS_ to store the result....
// 		msg.print("Cannot return a whole array as a single string.\n");
		valueS_.createEmpty(1024);
		valueS_.clear();
		valueS_.cat("{ ");
		for (int i=0; i<arraySize_; ++i)
		{
			if (i != 0) valueS_.cat(", ");
			switch (type_)
			{
				case (VTypes::NoData):
					printf("Internal error: No data in ReturnValue to return as a string!\n");
					success = FALSE;
					return "_NULL_";
					break;
				case (VTypes::IntegerData):
					valueS_.cat(itoa(arrayI_[i]));
					break;
				case (VTypes::DoubleData):
					valueS_.cat(ftoa(arrayD_[i]));
					break;
				case (VTypes::StringData):
					valueS_ += '"';
					valueS_.cat(arrayS_[i].get());
					valueS_ += '"';
					break;
			}
		}
		valueS_.cat(" }");
		return valueS_.get();
	}
	else switch (type_)
	{
		case (VTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as a string!\n");
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
			msg.print("Cannot return a pointer as a string.\n");
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
			if (arraySize_ == -1) return Vec3<double>(valueI_, valueI_, valueI_);
			else if (arraySize_ == 3) return Vec3<double>( arrayI_[0], arrayI_[1], arrayI_[2] );
			else msg.print("Cannot return a whole array as a single vector.\n");
			break;
		case (VTypes::DoubleData):
			if (arraySize_ == -1) return Vec3<double>(valueD_, valueD_, valueD_);
			else if (arraySize_ == 3) return Vec3<double>( arrayD_[0], arrayD_[1], arrayD_[2] );
			else msg.print("Cannot return a whole array as a single vector.\n");
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
	if (arraySize_ != -1) msg.print("Cannot return a whole array as a single pointer.\n");
	else switch (type_)
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
			if (arraySize_ == -1)
			{
				msg.print("Error: A value of type '%s' cannot be cast into a pointer of type '%s'.\n", VTypes::dataType(type_), VTypes::dataType(ptrtype));
				success = FALSE;
				return NULL;
			}
			else if (ptrtype != type_)
			{
				msg.print("Error: An array pointer of type '%s' cannot be cast into an array pointer of type '%s'.\n", VTypes::dataType(type_), VTypes::dataType(ptrtype));
				success = FALSE;
				return NULL;
			}
			else return valueP_;
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
	return NULL;
}

// Return integer element value
int ReturnValue::asInteger(int index, bool &success)
{
	success = TRUE;
	if (arraySize_ == -1)
	{
		printf("Internal Error: ReturnValue doesn't contain an array of values.\n");
		success = FALSE;
	}
	else if ((index < 0) || (index >= arraySize_))
	{
		printf("Internal Error: Index %i out of bounds for ReturnValue array.\n", index);
		success = FALSE;
	}
	else switch (type_)
	{
		case (VTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as an integer!\n");
			success = FALSE;
			return 0;
			break;
		case (VTypes::IntegerData):
			return arrayI_[index];
			break;
		case (VTypes::DoubleData):
			return (int) arrayD_[index];
			break;
		case (VTypes::StringData):
			return atoi( arrayS_[index].get() );
			break;
		default:
			printf("ReturnValue::asInteger(id) doesn't recognise this type.\n");
			break;
	}
	success = FALSE;
	return 0;
}

// Return double element value
double ReturnValue::asDouble(int index, bool &success)
{
	success = TRUE;
	if (arraySize_ == -1)
	{
		printf("Internal Error: ReturnValue doesn't contain an array of values.\n");
		success = FALSE;
	}
	else if ((index < 0) || (index >= arraySize_))
	{
		printf("Internal Error: Index %i out of bounds for ReturnValue array.\n", index);
		success = FALSE;
	}
	else switch (type_)
	{
		case (VTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as a double!\n");
			success = FALSE;
			return 0;
			break;
		case (VTypes::IntegerData):
			return (double) arrayI_[index];
			break;
		case (VTypes::DoubleData):
			return arrayD_[index];
			break;
		case (VTypes::StringData):
			return atof( arrayS_[index].get() );
			break;
		default:
			printf("ReturnValue::asDouble(id) doesn't recognise this type.\n");
			break;
	}
	success = FALSE;
	return 0;
}

// Return as character string
const char *ReturnValue::asString(int index, bool &success)
{
	static char converted[128];
	success = TRUE;
	if (arraySize_ == -1)
	{
		printf("Internal Error: ReturnValue doesn't contain an array of values.\n");
		success = FALSE;
	}
	else if ((index < 0) || (index >= arraySize_))
	{
		printf("Internal Error: Index %i out of bounds for ReturnValue array.\n", index);
		success = FALSE;
	}
	else switch (type_)
	{
		case (VTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as a character!\n");
			success = FALSE;
			return "_NULL_";
			break;
		case (VTypes::IntegerData):
			return itoa( arrayI_[index] );
			break;
		case (VTypes::DoubleData):
			return ftoa( arrayD_[index] );
			break;
		case (VTypes::StringData):
			return arrayS_[index].get();
			break;
		case (VTypes::VectorData):
			printf("Cannot convert return value of type 'vector' into a string.\n");
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
Vec3<double> ReturnValue::asVector(int index, bool &success)
{
	success = TRUE;
	double d;
	int i;
	if (arraySize_ == -1)
	{
		printf("Internal Error: ReturnValue doesn't contain an array of values.\n");
		success = FALSE;
	}
	else if ((index < 0) || (index >= arraySize_))
	{
		printf("Internal Error: Index %i out of bounds for ReturnValue array.\n", index);
		success = FALSE;
	}
	else switch (type_)
	{
		case (VTypes::NoData):
			printf("Internal error: No data in ReturnValue to return as a character!\n");
			success = FALSE;
			return Vec3<double>();
			break;
		case (VTypes::IntegerData):
			i = arrayI_[index];
			return Vec3<double>(i, i, i);
			break;
		case (VTypes::DoubleData):
			d = arrayD_[index];
			return Vec3<double>(d, d, d);
			break;
		case (VTypes::VectorData):
			return arrayV_[index];
			break;
		default:
			printf("Cannot convert return value of type '%s' into a vector.\n", VTypes::dataType(type_));
			break;
	}
	success = FALSE;
	return Vec3<double>();
}

void *ReturnValue::asPointer(int index, VTypes::DataType ptrtype, bool &success)
{
	success = TRUE;
	if (arraySize_ == -1)
	{
		printf("Internal Error: ReturnValue doesn't contain an array of values.\n");
		success = FALSE;
	}
	else if ((index < 0) || (index >= arraySize_))
	{
		printf("Internal Error: Index %i out of bounds for ReturnValue array.\n", index);
		success =FALSE;
	}
	else switch (type_)
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
			msg.print("Error: An array element of type '%s' cannot be cast into an array element of type '%s'.\n", VTypes::dataType(type_), VTypes::dataType(ptrtype));
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
			else return arrayP_[index];
			break;
	}
	success = FALSE;
	return NULL;
}

// Return array size of data
int ReturnValue::arraySize()
{
	return arraySize_;
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
	static Dnchar booltest;
	if (arraySize_ > 0) return TRUE;
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
	// No arrays....
	if (arraySize_ != -1) return FALSE;
	switch (type_)
	{
		case (VTypes::NoData):
		case (VTypes::StringData):
		case (VTypes::VectorData):
		case (VTypes::AtenData):
		case (VTypes::CellData):
		case (VTypes::ElementData):
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
	if (arraySize_ != -1) return FALSE;
	switch (type_)
	{
		case (VTypes::NoData):
		case (VTypes::StringData):
		case (VTypes::VectorData):
		case (VTypes::AtenData):
		case (VTypes::CellData):
		case (VTypes::ElementData):
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
