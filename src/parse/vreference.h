/*
	*** Variable Reference
	*** src/classes/vreference.h
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

#ifndef ATEN_VREFERENCE_H
#define ATEN_VREFERENCE_H

#include "classes/dnchar.h"

// Variable Reference Object
class VReference
{
	public:
	// Constructor
	VReference();
	// List types
	enum ListType { SimpleVariable, ListArray, ReflistArray, NormalArray };
	// Data types
	enum DataType { StringType, IntegerType, DoubleType, ModelType, PatternType, AtomType, BondType};
	// Constructor
	VReference(const char *name, VReference::ListType lt, VReference::DataType datatype, void *address, bool readonly);
	// List pointers
	VReference *prev, *next;

	public:
	// Name
	Dnchar name;
	// List type
	VReference::ListType listType;
	// Variable type pointed to
	VReference::DataType dataType;
	// Whether read only
	bool readOnly;
	// Actual address of referenced data
	void *varAddress;
};

#endif
