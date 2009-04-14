/*
	*** Return Value
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

#ifndef ATEN_RETURNVALUE_H
#define ATEN_RETURNVALUE_H

#include "parser/returnvalue.h"
#include "parser/vtypes.h"
#include "base/dnchar.h"
#include "templates/vector3.h"

// Tree Return Value
class ReturnValue
{
	public:
	// Constructor
	ReturnValue();
	ReturnValue(int i);
	ReturnValue(double d);
	ReturnValue(const char *s);
	ReturnValue(Vec3<double> v);
	ReturnValue(VTypes::DataType ptrtype, void *ptr);
	// Operator=
	void operator=(ReturnValue &rv);


	/*
	// Data
	*/
	private:
	// Temporary variable for converted string return
	Dnchar tempString_;
	// Data type contained in class
	VTypes::DataType type_;
	// Variable members for returns
	int valueI_;
	double valueD_;
	Dnchar valueS_;
	Vec3<double> valueV_;
	void *valueP_;

	public:
	// Return type of the stored data
	VTypes::DataType type();
	// Reset data
	void reset();
	// Return string of contained data
	const char *info();


	/*
	// Set
	*/
	public:
	// Set from integer value
	void set(int i);
	// Set from real value
	void set(double d);
	// Set from character value
	void set(const char *s);
	// Set from vector value
	void set(Vec3<double> v);
	// Set from individual vector data
	void set(double x, double y, double z);
	// Set from single vector data
	void set(int id, double xyz);
	// Set from pointer value
	void set(VTypes::DataType type, void *ptr);


	/*
	// Get (with type checking)
	*/
	public:
	// Return integer value
	int asInteger(bool &success);
	// Return real value
	double asDouble(bool &success);
	// Return character string
	const char *asString(bool &success);
	// Return vector data
	Vec3<double> asVector(bool &success);
	// Return pointer data
	void *asPointer(VTypes::DataType type, bool &success);


	/*
	// Get (no type checking)
	*/
	public:
	// Return integer value
	int asInteger();
	// Return real value
	double asDouble();
	// Return character string
	const char *asString();
	// Return vector data
	Vec3<double> asVector();
	// Return pointer data
	void *asPointer(VTypes::DataType type);
	// Return as boolean (guaranteed conversion)
	bool asBool();


	/*
	// In-place modify
	*/
	public:
	// Increase the contained variable
	bool increase();
	// Decrease the contained variable
	bool decrease();
};

#endif
