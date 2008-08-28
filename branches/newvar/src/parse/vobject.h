/*
	*** Variable Access Object
	*** src/parse/vobject.h
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

#include "classes/dnchar.h"

// Variable Reference Object
class VObject
{
	public:
	// Constructor
	VObject();
	// List types
	enum ListType { SimpleVariable, ListArray, ReflistArray, NormalArray };
	// Data types
	enum DataType { StringType, IntegerType, DoubleType, ModelType, PatternType, AtomType, BondType};
	// List pointers
	VObject *prev, *next;

	private:
	// Name
	Dnchar name;
	// List type
	VObject::ListType listType;
	// Variable type pointed to
	VObject::DataType dataType;
	// Whether read only
	bool readOnly;
	// Actual address of referenced data
	void *varAddress;
};

#endif
