/*
	*** Variable Access Object
	*** src/base/vobject.h
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

#ifndef ATEN_VOBJECT_H
#define ATEN_VOBJECT_H

#include "base/dnchar.h"

// Variable Reference Object
class VObject
{
	public:
	// Constructor
	VObject();
	// List pointers
	VObject *prev, *next;
	// List types
	enum ListType { NoArray, ListArray, ReflistArray, NormalArray };
	// Data Types
	enum DataType { CharacterVariable, IntegerVariable, FloatVariable, AtomVariable, PatternVariable, ModelVariable, GridVariable, BondVariable, AngleVariable, TorsionVariable, AtomtypeVariable, ExpressionVariable, ReferenceVariable, nVariableTypes };
	static const char *variableType(VariableType);

	/*
	// Object Data
	*/
	private:
	// Name
	Dnchar name_;
	// List type of pointed data
	VObject::ListType listType_;
	// Data type pointed to
	VObject::DataType dataType_;
	// Whether variable is read/write (TRUE) or read-only (FALSE)
	bool readWrite_;
	// Pointer to data
	void *ptr_;

	/*
	// Set / Get
	*/
	public:
// 	// Set name of object
// 	void setName(const char *name);
// 	// Return name of object
// 	const char *name();
// 	// Set variable array/list type
// 	void setListType(VObject::ListType);
// 	// Return variable array/list type
// 	VObject::ListType listType();
// 	// Set datatype and varaddress
// 	void setData(int *
};

// Returned object data
class VResult
{
	public:
	// Constructor
	VResult();

	/*
	// Object Data
	*/
	private:
	// Variable type pointed to
	VObject::DataType type_;
	// Pointer to data
	void *data_;

	/*
	// Set / Get
	*/
	public:
	// Set data type 
	void setType(VObject::DataType type);
	// Return data type
	VObject::DataType type();
	// Set pointer
	void setData(void *p);
	// Return pointer value
	void *data();
};

#endif
