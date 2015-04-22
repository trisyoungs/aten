/*
	*** Return Value
	*** src/parser/returnvalue.cpp
	Copyright T. Youngs 2007-2015

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
#include "base/elementmap.h"
#include "base/sysfunc.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "base/grid.h"
#include "base/forcefieldatom.h"
#include "base/forcefieldbound.h"
#include "base/pattern.h"
#include "templates/reflist.h"

ATEN_USING_NAMESPACE

// Constructors
ReturnValue::ReturnValue() : ListItem<ReturnValue>()
{
	// Private variables
	type_ = VTypes::NoData;
	valueI_ = 0;
	valueD_ = 0.0;
	valueP_ = NULL;
	valueRefitem_ = NULL;
	arraySize_ = -1;
	arrayI_ = NULL;
	arrayD_ = NULL;
	arrayS_ = NULL;
	arrayV_ = NULL;
	arrayM_ = NULL;
	arrayP_ = NULL;
}
ReturnValue::ReturnValue(int i) : ListItem<ReturnValue>()
{
	type_ = VTypes::IntegerData;
	arraySize_ = -1;
	valueI_ = i;
}
ReturnValue::ReturnValue(double d) : ListItem<ReturnValue>()
{
	type_ = VTypes::DoubleData;
	arraySize_ = -1;
	valueD_ = d;
}
ReturnValue::ReturnValue(const char* s) : ListItem<ReturnValue>()
{
	type_ = VTypes::StringData;
	arraySize_ = -1;
	valueS_ = s;
}
ReturnValue::ReturnValue(QString s) : ListItem<ReturnValue>()
{
	type_ = VTypes::StringData;
	arraySize_ = -1;
	valueS_ = s;
}
ReturnValue::ReturnValue(Vec3<double> v) : ListItem<ReturnValue>()
{
	type_ = VTypes::VectorData;
	arraySize_ = -1;
	valueV_ = v;
}
ReturnValue::ReturnValue(Matrix m) : ListItem<ReturnValue>()
{
	type_ = VTypes::MatrixData;
	arraySize_ = -1;
	valueM_ = m;
}
ReturnValue::ReturnValue(VTypes::DataType type, void* ptr) : ListItem<ReturnValue>()
{
	type_ = type;
	arraySize_ = -1;
	valueP_ = ptr;
}

// Operator =
void ReturnValue::operator=(const ReturnValue& source)
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
				arrayS_ = new QString[arraySize_];
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
				valueP_ = NULL;
				arrayP_ = new void*[arraySize_];
				for (int i=0; i<arraySize_; ++i) arrayP_[i] = source.arrayP_[i];
			}
			break;
	}
}

// Operator =(double)
void ReturnValue::operator=(double d)
{
	clearArrayData();
	type_ = VTypes::DoubleData;
	arraySize_ = -1;
	valueD_ = d;
}

// Operator =(int)
void ReturnValue::operator=(int i)
{
	clearArrayData();
	type_ = VTypes::IntegerData;
	arraySize_ = -1;
	valueI_ = i;
}

// Operator =
void ReturnValue::operator=(QString s)
{
	clearArrayData();
	type_ = VTypes::StringData;
	arraySize_ = -1;
	valueS_ = s;
}

ReturnValue::~ReturnValue()
{
	clearArrayData();
}

// Clear any current array data
void ReturnValue::clearArrayData()
{
	if (arraySize_ == -1) return;
	if (type_ == VTypes::IntegerData) { delete[] arrayI_; arrayI_ = NULL; }
	else if (type_ == VTypes::DoubleData) { delete[] arrayD_; arrayD_ = NULL; }
	else if (type_ == VTypes::StringData) { delete[] arrayS_; arrayS_ = NULL; }
	else if (type_ == VTypes::VectorData) { delete[] arrayV_; arrayV_ = NULL; }
	else if (type_ == VTypes::MatrixData) { delete[] arrayM_; arrayM_ = NULL; }
	else { if (arrayP_ != NULL) delete[] arrayP_; arrayP_ = NULL; }
	arraySize_ = -1;
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
QString ReturnValue::info()
{
	QString result;
	switch (type_)
	{
		case (VTypes::NoData):
			result.sprintf("nothing (%s)", VTypes::dataType(type_));
			break;
		case (VTypes::IntegerData):
			if (arraySize_ == -1) result.sprintf("%i (%s)", valueI_, VTypes::dataType(type_));
			else result.sprintf("%i elements (array of %s)", arraySize_, VTypes::dataType(type_));
			break;
		case (VTypes::DoubleData):
			if (arraySize_ == -1) result.sprintf("%f (%s)", valueD_, VTypes::dataType(type_));
			else result.sprintf("%i elements (array of %s)", arraySize_, VTypes::dataType(type_));
			break;
		case (VTypes::StringData):
			if (arraySize_ == -1) result.sprintf("'%s' (%s)", qPrintable(valueS_), VTypes::dataType(type_));
			else result.sprintf("%i elements (array of %s)", arraySize_, VTypes::dataType(type_));
			break;
		case (VTypes::VectorData):
			if (arraySize_ == -1) result.sprintf("{%f,%f,%f} (%s)", valueV_.x, valueV_.y, valueV_.z, VTypes::dataType(type_));
			else result.sprintf("%i elements (array of %s)", arraySize_, VTypes::dataType(type_));
			break;
		case (VTypes::MatrixData):
			if (arraySize_ == -1) result.sprintf("{%f,%f,%f,%f,%f,%f,%f,%f,%f} (%s)", valueM_[0], valueM_[1], valueM_[2], valueM_[4], valueM_[5], valueM_[6], valueM_[8], valueM_[9], valueM_[10], VTypes::dataType(type_));
			else result.sprintf("%i elements (array of %s)", arraySize_, VTypes::dataType(type_));
			break;
		default:
			if (arraySize_ == -1) result.sprintf("%p (%s)", valueP_, VTypes::dataType(type_));
			else result.sprintf("%i elements (array of %s)", arraySize_, VTypes::dataType(type_));
			break;
	}
	return result;
}

// Return unique 'pair' code based on return types
int ReturnValue::dataPair(ReturnValue& source)
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
	valueRefitem_ = NULL;
}

// Set from double value
void ReturnValue::set(double d)
{
	clearArrayData();
	type_ = VTypes::DoubleData;
	valueD_ = d;
	arraySize_ = -1;
	valueRefitem_ = NULL;
}

// Set from character value
void ReturnValue::set(QString  s)
{
	clearArrayData();
	type_ = VTypes::StringData;
	valueS_ = s;
	arraySize_ = -1;
	valueRefitem_ = NULL;
}

// Set from vector value
void ReturnValue::set(Vec3<double> v)
{
	clearArrayData();
	type_ = VTypes::VectorData;
	valueV_ = v;
	arraySize_ = -1;
	valueRefitem_ = NULL;
}

// Set from individual vector data
void ReturnValue::set(double x, double y, double z)
{
	clearArrayData();
	type_ = VTypes::VectorData;
	valueV_.set(x,y,z);
	arraySize_ = -1;
	valueRefitem_ = NULL;
}

// Set from Matrix value
void ReturnValue::set(Matrix m)
{
	clearArrayData();
	type_ = VTypes::MatrixData;
	valueM_ = m;
	arraySize_ = -1;
	valueRefitem_ = NULL;
}

// Set from pointer value
void ReturnValue::set(VTypes::DataType ptrtype, void* ptr, void *refitem)
{
	clearArrayData();
	type_ = ptrtype;
	valueP_ = ptr;
	valueRefitem_ = refitem;
	arraySize_ = -1;
}

// Set from standard array
void ReturnValue::setArray(VTypes::DataType type, void *array, int arraysize)
{
	clearArrayData();
	type_ = type;
	arraySize_ = arraysize;
	valueRefitem_ = NULL;
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
		double* source = (double*) array;
		for (i = 0; i < arraySize_; ++i) arrayD_[i] = source[i];
	}
	else if (type_ == VTypes::StringData)
	{
		arrayS_ = new QString[arraySize_];
		QString* source = (QString*) array;
		for (i = 0; i < arraySize_; ++i) arrayS_[i] = source[i];
	}
	else if (type_ == VTypes::VectorData)
	{
		arrayV_ = new Vec3<double>[arraySize_];
		Vec3<double>* source = (Vec3<double>*) array;
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

// Set from Vec3<int>
void ReturnValue::setArray(Vec3<int> vec)
{
	clearArrayData();
	type_ = VTypes::IntegerData;
	arraySize_ = 3;
	valueRefitem_ = NULL;
	arrayI_ = new int[arraySize_];
	for (int i = 0; i < 3; ++i) arrayI_[i] = vec.get(i);
}

// Set array element from integer value
void ReturnValue::setElement(int id, int i)
{
	if ((type_ == VTypes::IntegerData) && (id >= 0) && (id < arraySize_)) arrayI_[id] = i;
	else Messenger::print("Error: Tried to set an integer array element when none existed/wrong type.");
}

// Set array element from real value
void ReturnValue::setElement(int id, double d)
{
	if ((type_ == VTypes::DoubleData) && (id >= 0) && (id < arraySize_)) arrayD_[id] = d;
	else Messenger::print("Error: Tried to set a double array element when none existed/wrong type.");
}

// Set array element from character value
void ReturnValue::setElement(int id, QString s)
{
	if ((type_ == VTypes::StringData) && (id >= 0) && (id < arraySize_)) arrayS_[id] = s;
	else Messenger::print("Error: Tried to set a string array element when none existed/wrong type.");
}

// Set array element from vector value
void ReturnValue::setElement(int id, Vec3<double> v)
{
	if ((type_ == VTypes::VectorData) && (id >= 0) && (id < arraySize_)) arrayV_[id] = v;
	else Messenger::print("Error: Tried to set a vector array element when none existed/wrong type.");
}

// Set array element from pointer value
void ReturnValue::setElement(int id, VTypes::DataType type, void* ptr)
{
	if ((type_ == type) && (id >= 0) && (id < arraySize_)) arrayP_[id] = ptr;
	else Messenger::print("Error: Tried to set a pointer array element when none existed/wrong type.");
}

// Return actual vector object for in-place modification
Vec3<double>& ReturnValue::vector()
{
	if (type_ != VTypes::VectorData) Messenger::print("Error: Returning reference to ReturnValue::valueV_ but type is not 'vector' (%s).", VTypes::dataType(type_));
	return valueV_;
}

// Return actual matrix object for in-place modification
Matrix& ReturnValue::matrix()
{
	if (type_ != VTypes::MatrixData) Messenger::print("Error: Returning reference to ReturnValue::valueM_ but type is not 'matrix'.",  VTypes::dataType(type_));
	return valueM_;
}

/*
// Get (with type checking)
*/

// Return as integer value
int ReturnValue::asInteger(bool& success)
{
	success = true;
	if (arraySize_ != -1) Messenger::print("Cannot return a whole array as a single integer.");
	else switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Internal error: No data in ReturnValue to return as an integer!");
			success = false;
			return 0;
			break;
		case (VTypes::IntegerData):
			return valueI_;
			break;
		case (VTypes::DoubleData):
			return (int)valueD_;
			break;
		case (VTypes::StringData):
			return valueS_.toInt();
			break;
		case (VTypes::ElementData):
			return (valueP_ == NULL ? 0 : ((Element*) valueP_)->z);
			break;
		default:
			Messenger::print("ReturnValue::asInteger() doesn't recognise this type (%s).", VTypes::dataType(type_));
			break;
	}
	success = false;
	return 0;
}

// Return as real value
double ReturnValue::asDouble(bool& success)
{
	success = true;
	if (arraySize_ != -1) Messenger::print("Cannot return a whole array as a single double.");
	else switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Internal error: No data in ReturnValue to return as a real!");
			success = false;
			return 0.0;
			break;
		case (VTypes::IntegerData):
			return (double)valueI_;
			break;
		case (VTypes::DoubleData):
			return valueD_;
			break;
		case (VTypes::StringData):
			return valueS_.toDouble();
			break;
		default:
			Messenger::print("ReturnValue::asDouble() doesn't recognise this type (%s).", VTypes::dataType(type_));
			break;
	}
	success = false;
	return 0;
}

// Return as character string
QString ReturnValue::asString(bool& success)
{
	QString tempString;
	success = true;
	if (arraySize_ != -1)
	{
		// Create a new string with all the array elements in it
		tempString += "{ ";
		for (int i=0; i<arraySize_; ++i)
		{
			if (i != 0) tempString += ", ";
			switch (type_)
			{
				case (VTypes::NoData):
					Messenger::print("Internal error: No data in ReturnValue to return as a string!");
					success = false;
					return QString("_NULL_");
					break;
				case (VTypes::IntegerData):
					tempString += QString::number(arrayI_[i]);
					break;
				case (VTypes::DoubleData):
					tempString += QString::number(arrayD_[i]);
					break;
				case (VTypes::StringData):
					tempString += '"';
					tempString += arrayS_[i];
					tempString += '"';
					break;
				default:
					break;
			}
		}
		tempString += " }";
		return tempString;
	}
	else switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Internal error: No data in ReturnValue to return as a string!");
			success = false;
			return QString("_NULL_");
			break;
		case (VTypes::IntegerData):
			return QString::number(valueI_);
			break;
		case (VTypes::DoubleData):
			return QString::number(valueD_);
			break;
		case (VTypes::StringData):
			return valueS_;
			break;
		case (VTypes::VectorData):
			tempString.sprintf("{%f,%f,%f}", valueV_.x, valueV_.y, valueV_.z);
			return tempString;
			break;
		default:
			// All pointer types
			Messenger::print("Cannot return a pointer as a string.");
			break;
	}
	success = false;
	return "";
}

// Return as vector value
Vec3<double> ReturnValue::asVector(bool& success)
{
	success = true;
	switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Internal error: No data in ReturnValue to return as a vector!");
			success = false;
			return Vec3<double>();
			break;
		case (VTypes::IntegerData):
			if (arraySize_ == -1) return Vec3<double>(valueI_, valueI_, valueI_);
			else if (arraySize_ == 3) return Vec3<double>( arrayI_[0], arrayI_[1], arrayI_[2] );
			else Messenger::print("Cannot return an array of this size (%i) as a single vector of integers.", arraySize_);
			break;
		case (VTypes::DoubleData):
			if (arraySize_ == -1) return Vec3<double>(valueD_, valueD_, valueD_);
			else if (arraySize_ == 3) return Vec3<double>( arrayD_[0], arrayD_[1], arrayD_[2] );
			else Messenger::print("Cannot return an array of this size (%i) as a single vector of doubles.", arraySize_);
			break;
		case (VTypes::VectorData):
			return valueV_;
			break;
		default:
			Messenger::print("Cannot convert return value of type '%s' into a vector.", VTypes::dataType(type_));
			break;
	}
	success = false;
	return Vec3<double>();
}

// Return as Matrix
Matrix ReturnValue::asMatrix(bool& success)
{
	success = true;
	switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Internal error: No data in ReturnValue to return as a matrix!");
			break;
		case (VTypes::IntegerData):
			if (arraySize_ == -1) Messenger::print("Cannot return a single integer as a matrix.");
			else if (arraySize_ == 9)
			{
				Matrix result;
				for (int n=0; n<9; ++n) result[(n)/3*4+(n)%3] = arrayI_[n];
				return result;
			}
			else Messenger::print("Cannot return an array of this size (%i) as a single matrix of doubles.", arraySize_);
			break;
		case (VTypes::DoubleData):
			if (arraySize_ == -1) Messenger::print("Cannot return a single double as a matrix.");
			else if (arraySize_ == 9)
			{
				Matrix result;
				for (int n=0; n<9; ++n) result[(n)/3*4+(n)%3] = arrayD_[n];
				return result;
			}
			else Messenger::print("Cannot return an array of this size (%i) as a single matrix of doubles.", arraySize_);
			break;
		case (VTypes::MatrixData):
			return valueM_;
			break;
		default:
			Messenger::print("Cannot convert return value of type '%s' into a matrix.", VTypes::dataType(type_));
			break;
	}
	success = false;
	return Matrix();
}

// Return as pointer
void *ReturnValue::asPointer(VTypes::DataType ptrtype, bool& success)
{
	success = true;
	if (arraySize_ != -1) Messenger::print("Cannot return a whole array as a single pointer.");
	else switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Error: No data in ReturnValue to return as a pointer!");
			success = false;
			return NULL;
			break;
		case (VTypes::IntegerData):
		case (VTypes::DoubleData):
		case (VTypes::StringData):
		case (VTypes::VectorData):
			if ((type_ == VTypes::IntegerData) && (ptrtype == VTypes::ElementData))
			{
				int el = asInteger();
				if ((el < 0) || (el > Elements().nElements()))
				{
					Messenger::print("Warning: Conversion of integer value %i does not correspond to a defined element. Returning '0' (undefined element 'XX').", el);
					el = 0;
				}
				return Elements().element(el);
			}
			else if (arraySize_ == -1)
			{
				Messenger::print("Error: A value of type '%s' cannot be cast into a pointer of type '%s'.", VTypes::dataType(type_), VTypes::dataType(ptrtype));
				success = false;
				return NULL;
			}
			else if (ptrtype != type_)
			{
				Messenger::print("Error: An array pointer of type '%s' cannot be cast into an array pointer of type '%s'.", VTypes::dataType(type_), VTypes::dataType(ptrtype));
				success = false;
				return NULL;
			}
			else return valueP_;
			break;
		default:
			// Check that internal pointer type matches requested pointer type
			if (ptrtype != type_)
			{
				Messenger::print("Error: A pointer of type '%s' cannot be cast into a pointer of type '%s'.", VTypes::dataType(type_), VTypes::dataType(ptrtype));
				success = false;
				return NULL;
			}
			else return valueP_;
			break;
	}
	success = false;
	return NULL;
}

// Return pointer refitem data
void *ReturnValue::refPointer()
{
	return valueRefitem_;
}

// Return integer element value
int ReturnValue::asInteger(int index, bool& success)
{
	success = true;
	if (arraySize_ == -1)
	{
		Messenger::print("Internal Error: ReturnValue doesn't contain an array of values.");
		success = false;
	}
	else if ((index < 0) || (index >= arraySize_))
	{
		Messenger::print("Internal Error: Index %i out of bounds for ReturnValue array.", index);
		success = false;
	}
	else switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Internal error: No data in ReturnValue to return as an integer!");
			success = false;
			return 0;
			break;
		case (VTypes::IntegerData):
			return arrayI_[index];
			break;
		case (VTypes::DoubleData):
			return (int) arrayD_[index];
			break;
		case (VTypes::StringData):
			return arrayS_[index].toInt();
			break;
		default:
			Messenger::print("ReturnValue::asInteger(id) doesn't recognise this type (%s).", VTypes::dataType(type_));
			break;
	}
	success = false;
	return 0;
}

// Return double element value
double ReturnValue::asDouble(int index, bool& success)
{
	success = true;
	if (arraySize_ == -1)
	{
		Messenger::print("Internal Error: ReturnValue doesn't contain an array of values.");
		success = false;
	}
	else if ((index < 0) || (index >= arraySize_))
	{
		Messenger::print("Internal Error: Index %i out of bounds for ReturnValue array.", index);
		success = false;
	}
	else switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Internal error: No data in ReturnValue to return as a double!");
			success = false;
			return 0;
			break;
		case (VTypes::IntegerData):
			return (double) arrayI_[index];
			break;
		case (VTypes::DoubleData):
			return arrayD_[index];
			break;
		case (VTypes::StringData):
			return arrayS_[index].toDouble();
			break;
		default:
			Messenger::print("ReturnValue::asDouble(id) doesn't recognise this type (%s).", VTypes::dataType(type_));
			break;
	}
	success = false;
	return 0;
}

// Return as character string
QString ReturnValue::asString(int index, bool& success)
{
	QString converted;
	success = true;
	if (arraySize_ == -1)
	{
		Messenger::print("Internal Error: ReturnValue doesn't contain an array of values.");
		success = false;
	}
	else if ((index < 0) || (index >= arraySize_))
	{
		Messenger::print("Internal Error: Index %i out of bounds for ReturnValue array.", index);
		success = false;
	}
	else switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Internal error: No data in ReturnValue to return as a character!");
			success = false;
			return "_NULL_";
			break;
		case (VTypes::IntegerData):
			return QString::number(arrayI_[index]);
			break;
		case (VTypes::DoubleData):
			return QString::number(arrayD_[index]);
			break;
		case (VTypes::StringData):
			return arrayS_[index];
			break;
		case (VTypes::VectorData):
			Messenger::print("Cannot convert return value of type 'vector' into a string.");
			break;
		default:
			// All pointer types
			converted.sprintf("%p", valueP_);
			return converted;
			break;
	}
	success = false;
	return "NULL";
}

// Return as vector value
Vec3<double> ReturnValue::asVector(int index, bool& success)
{
	success = true;
	double d;
	int i;
	if (arraySize_ == -1)
	{
		Messenger::print("Internal Error: ReturnValue doesn't contain an array of values.");
		success = false;
	}
	else if ((index < 0) || (index >= arraySize_))
	{
		Messenger::print("Internal Error: Index %i out of bounds for ReturnValue array.", index);
		success = false;
	}
	else switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Internal error: No data in ReturnValue to return as a character!");
			success = false;
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
			Messenger::print("Cannot convert return value of type '%s' into a vector.", VTypes::dataType(type_));
			break;
	}
	success = false;
	return Vec3<double>();
}

void *ReturnValue::asPointer(int index, VTypes::DataType type, bool& success)
{
	success = true;
	if (arraySize_ == -1)
	{
		Messenger::print("Internal Error: ReturnValue doesn't contain an array of values.");
		success = false;
	}
	else if ((index < 0) || (index >= arraySize_))
	{
		Messenger::print("Internal Error: Index %i out of bounds for ReturnValue array.", index);
		success =false;
	}
	else switch (type_)
	{
		case (VTypes::NoData):
			Messenger::print("Error: No data in ReturnValue to return as a pointer!");
			success = false;
			return NULL;
			break;
		case (VTypes::IntegerData):
		case (VTypes::DoubleData):
		case (VTypes::StringData):
		case (VTypes::VectorData):
			Messenger::print("Error: An array element of type '%s' cannot be cast into an array element of type '%s'.", VTypes::dataType(type_), VTypes::dataType(type));
			success = false;
			return NULL;
			break;
		default:
			// Check that internal pointer type matches requested pointer type
			if (type != type_)
			{
				Messenger::print("Error: A pointer of type '%s' cannot be cast into a pointer of type '%s'.", VTypes::dataType(type_), VTypes::dataType(type));
				success = false;
				return NULL;
			}
			else return arrayP_[index];
			break;
	}
	success = false;
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
QString ReturnValue::asString()
{
	static bool success;
	return asString(success);
}

// Return as pointer value
void* ReturnValue::asPointer(VTypes::DataType type)
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

// Return matrix data
Matrix ReturnValue::asMatrix()
{
	static bool success;
	return asMatrix(success);
}

// Return as boolean value
bool ReturnValue::asBool()
{
	QString lcase;
	if (arraySize_ > 0) return true;
	switch (type_)
	{
		case (VTypes::NoData):
			return false;
			break;
		case (VTypes::IntegerData):
			return (valueI_ > 0);
			break;
		case (VTypes::DoubleData):
			return (valueD_ > 0.0);
			break;
		case (VTypes::StringData):
			lcase = valueS_.toLower();
			if (lcase == "off") return false;
			else if (lcase == "on") return true;
			else if (lcase == "no") return false;
			else if (lcase == "yes") return true;
			else if (lcase == "false") return false;
			else if (lcase == "true") return true;
			else
			{
				Messenger::print("Character constant '%s' doesn't translate directly to a boolean value - false assumed.\n", qPrintable(lcase));
				return false;
			}
			break;
		case (VTypes::VectorData):
			Messenger::print("Can't convert an object of type 'vector' into a bool.");
			return false;
			break;
		default:
			// All pointer types here...
			return (valueP_ != NULL);
			break;
	}
	return false;
}

/*
// In-place modify
*/

// Increase the contained variable
bool ReturnValue::increase()
{
	bool result = true;
	// No arrays....
	if (arraySize_ != -1) return false;
	switch (type_)
	{
		case (VTypes::NoData):
		case (VTypes::StringData):
		case (VTypes::VectorData):
		case (VTypes::AtenData):
		case (VTypes::CellData):
		case (VTypes::ElementData):
			result = false;
			break;
		case (VTypes::IntegerData):
			++valueI_;
			break;
		case (VTypes::DoubleData):
			++valueD_;
			break;
		case (VTypes::AtomData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Atom,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Atom,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Atom*) valueP_)->next;
			break;
		case (VTypes::BondData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Bond,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Bond,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Bond*) valueP_)->next;
			break;
		case (VTypes::ForcefieldData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Forcefield,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Forcefield,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Forcefield*) valueP_)->next;
			break;
		case (VTypes::ForcefieldAtomData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<ForcefieldAtom,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<ForcefieldAtom,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((ForcefieldAtom*) valueP_)->next;
			break;
		case (VTypes::ForcefieldBoundData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<ForcefieldBound,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<ForcefieldBound,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((ForcefieldBound*) valueP_)->next;
			break;
		case (VTypes::GlyphData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Glyph,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Glyph,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Glyph*) valueP_)->next;
			break;
		case (VTypes::GridData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Grid,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Grid,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Grid*) valueP_)->next;
			break;
		case (VTypes::MeasurementData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Measurement,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Measurement,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Measurement*) valueP_)->next;
			break;
		case (VTypes::ModelData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Model,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Model,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Model*) valueP_)->next;
			break;
		case (VTypes::PatternData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Pattern,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Pattern,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Pattern*) valueP_)->next;
			break;
		case (VTypes::PatternBoundData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<PatternBound,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<PatternBound,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((PatternBound*) valueP_)->next;
			break;
		case (VTypes::ZMatrixElementData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<ZMatrixElement,int>*) valueRefitem_)->next;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<ZMatrixElement,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((ZMatrixElement*) valueP_)->next;
			break;
		default:
			Messenger::print("Internal Error: No 'increase' has been defined for %s.", VTypes::aDataType(type_));
			break;
	}
	return result;
}

// Decrease the contained variable
bool ReturnValue::decrease()
{
	bool result = true;
	if (arraySize_ != -1) return false;
	switch (type_)
	{
		case (VTypes::NoData):
		case (VTypes::StringData):
		case (VTypes::VectorData):
		case (VTypes::AtenData):
		case (VTypes::CellData):
		case (VTypes::ElementData):
			result = false;
			break;
		case (VTypes::IntegerData):
			--valueI_;
			break;
		case (VTypes::DoubleData):
			--valueD_;
			break;
		case (VTypes::AtomData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Atom,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Atom,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Atom*) valueP_)->prev;
			break;
		case (VTypes::BondData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Bond,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Bond,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Bond*) valueP_)->prev;
			break;
		case (VTypes::ForcefieldData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Forcefield,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Forcefield,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Forcefield*) valueP_)->prev;
			break;
		case (VTypes::ForcefieldAtomData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<ForcefieldAtom,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<ForcefieldAtom,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((ForcefieldAtom*) valueP_)->prev;
			break;
		case (VTypes::ForcefieldBoundData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<ForcefieldBound,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<ForcefieldBound,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((ForcefieldBound*) valueP_)->prev;
			break;
		case (VTypes::GridData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Grid,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Grid,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Grid*) valueP_)->prev;
			break;
		case (VTypes::GlyphData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Glyph,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Glyph,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Glyph*) valueP_)->prev;
			break;
		case (VTypes::MeasurementData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Measurement,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Measurement,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Measurement*) valueP_)->prev;
			break;
		case (VTypes::ModelData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Model,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Model,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Model*) valueP_)->prev;
			break;
		case (VTypes::PatternData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<Pattern,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<Pattern,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((Pattern*) valueP_)->prev;
			break;
		case (VTypes::PatternBoundData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<PatternBound,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<PatternBound,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((PatternBound*) valueP_)->prev;
			break;
		case (VTypes::ZMatrixElementData):
			if (valueP_ == NULL) result = false;
			else if (valueRefitem_ != NULL)
			{
				valueRefitem_ = ((Refitem<ZMatrixElement,int>*) valueRefitem_)->prev;
				if (valueRefitem_ == NULL) valueP_ = NULL;
				else valueP_ = ((Refitem<ZMatrixElement,int>*) valueRefitem_)->item;
			}
			else valueP_ = ((ZMatrixElement*) valueP_)->prev;
			break;
		default:
			Messenger::print("Internal Error: No 'decrease' has been defined for %s.", VTypes::aDataType(type_));
			break;
	}
	return result;
}
