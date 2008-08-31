/*
	*** Variable Storage Object Template
	*** src/templates/vobject.h
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

// Variable description
class VTypes
{
	public:
	// List types
	enum ListType { NoArray, ListArray, ReflistArray, NormalArray };
	// Data Types
	enum DataType { CharacterVariable, IntegerVariable, FloatVariable, AtomVariable, PatternVariable, ModelVariable, GridVariable, BondVariable, AngleVariable, TorsionVariable, AtomtypeVariable, ExpressionVariable, ReferenceVariable, nVariableTypes };
	static const char *variableType(DataType);
};

// Variable Storage Object Template
template <class T> class VObject
{
	public:
	// Object Data
	T data;
};

// Constructor
template <class T> VObject<T>::VObject()
{
	// Private variables
	listType_ = VTypes::NoArray;
	dataType_ = VTypes::NoDataSet;

	// Public variables
	prev = NULL;
	next = NULL;
}

#endif
