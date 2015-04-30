/*
	*** Return Value
	*** src/parser/returnvalue.h
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

#ifndef ATEN_RETURNVALUE_H
#define ATEN_RETURNVALUE_H

#include "parser/vtypes.h"
#include "math/matrix.h"
#include "templates/list.h"
#include "templates/vector3.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Return Value
class ReturnValue : public ListItem<ReturnValue>
{
	public:
	// Constructors / Destructor
	ReturnValue();
	ReturnValue(int i);
	ReturnValue(double d);
	ReturnValue(QString s);
	ReturnValue(const char* s);
	ReturnValue(Vec3<double> v);
	ReturnValue(Matrix m);
	ReturnValue(VTypes::DataType type, void* ptr);
	~ReturnValue();
	// Operator=
	ReturnValue& operator=( const AtenSpace::ReturnValue& source );
	ReturnValue& operator=(double d);
	ReturnValue& operator=(int i);
	ReturnValue& operator=(QString s);


	/*
	 * Data
	 */
	private:
	// Data type contained in class
	VTypes::DataType type_;
	// Size of array (if an array is stored (-1 otherwise)
	int arraySize_;
	// Variable members for returns
	int valueI_;
	double valueD_;
	QString valueS_;
	Vec3<double> valueV_;
	Matrix valueM_;
	void* valueP_, *valueRefitem_;
	// Array members for returns
	int* arrayI_;
	double* arrayD_;
	QString* arrayS_;
	Vec3<double>* arrayV_;
	Matrix* arrayM_;
	void** arrayP_;

	private:
	// Clear any current array data
	void clearArrayData();

	public:
	// Return type of the stored data
	VTypes::DataType type();
	// Reset data
	void reset();
	// Return string of contained data
	QString info();
	// Return unique 'pair' code based on return types
	int dataPair(ReturnValue& source);


	/*
	 * Set
	 */
	public:
	// Set from integer value
	void set(int i);
	// Set from real value
	void set(double d);
	// Set from character value
	void set(QString s);
	// Set from vector value
	void set(Vec3<double> v);
	// Set from individual vector data
	void set(double x, double y, double z);
	// Set from Matrix value
	void set(Matrix m);
	// Set from pointer value
	void set(VTypes::DataType type, void* ptr, void *refitem = NULL);
	// Set from standard array
	void setArray(VTypes::DataType type, void *source, int arraysize);
	// Set from Vec3<int>
	void setArray(Vec3<int> vec);
	// Set array element from integer value
	void setElement(int id, int i);
	// Set array element from real value
	void setElement(int id, double d);
	// Set array element from character value
	void setElement(int id, QString s);
	// Set array element from vector value
	void setElement(int id, Vec3<double> v);
	// Set array element from vector value
	void setElement(int id, Matrix m);
	// Set array element from pointer value
	void setElement(int id, VTypes::DataType type, void* ptr);
	// Return actual vector object for in-place modification
	Vec3<double>& vector();
	// Return actual matrix object for in-place modification
	Matrix& matrix();


	/*
	 * Get (with type checking)
	 */
	public:
	// Return integer value
	int asInteger(bool& success);
	// Return real value
	double asDouble(bool& success);
	// Return character string
	QString asString(bool& success);
	// Return vector data
	Vec3<double> asVector(bool& success);
	// Return matrix data
	Matrix asMatrix(bool& success);
	// Return pointer data
	void *asPointer(VTypes::DataType type, bool& success);
	// Return pointer refitem data
	void *refPointer();
	// Return integer element value
	int asInteger(int index, bool& success);
	// Return real element value
	double asDouble(int index, bool& success);
	// Return character string element
	QString asString(int index, bool& success);
	// Return vector element data
	Vec3<double> asVector(int index, bool& success);
	// Return matrix element data
	Vec3<double> asMatrix(int index, bool& success);
	// Return pointer element data
	void* asPointer(int index, VTypes::DataType type, bool& success);
	// Return array size of data
	int arraySize();


	/*
	 * Get (no type checking)
	 */
	public:
	// Return integer value
	int asInteger();
	// Return real value
	double asDouble();
	// Return character string
	QString asString();
	// Return vector data
	Vec3<double> asVector();
	// Return matrix data
	Matrix asMatrix();
	// Return pointer data
	void *asPointer(VTypes::DataType type);
	// Return as boolean (guaranteed conversion)
	bool asBool();


	/*
	 * In-place modify
	 */
	public:
	// Increase the contained variable
	bool increase();
	// Decrease the contained variable
	bool decrease();
};

ATEN_END_NAMESPACE

#endif
