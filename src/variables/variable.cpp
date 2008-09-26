/*
	*** Variable
	*** src/variables/variable.cpp
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

#include "variables/variable.h"
#include "variables/expression.h"
#include "model/model.h"
#include "classes/forcefieldatom.h"
#include "classes/grid.h"
#include "base/pattern.h"
#include "base/elements.h"
#include <string.h>

// Constructor
Variable::Variable()
{
	// Private variables
	name_.set("unnamed");
	dataType_ = VTypes::NoData;
	readOnly_ = FALSE;
	arraySize_ = -1;
	listArray_ = FALSE;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor (virtual)
Variable::~Variable()
{
}

// Set name of variable
void Variable::setName(const char* s)
{
	name_.set(s);
}

// Get name of variable
const char *Variable::name()
{
	return name_.get();
}

// Sets the content type of the variable
void Variable::setType(VTypes::DataType dt)
{
	dataType_ = dt;
}

// Returns content type of the variable
VTypes::DataType Variable::type()
{
	return dataType_;
}

// Return readonly status
bool Variable::readOnly()
{
	return readOnly_;
}

// Set parent VariableList
void Variable::setParent(VariableList *vlist)
{
	parent_ = vlist;
}

// Set the readonly status of the variable to TRUE
void Variable::setReadOnly()
{
	readOnly_ = TRUE;
}

// Set listarray flag to true
void Variable::setListArray()
{
	listArray_ = TRUE;
}

// Return whether the array is a listarray
bool Variable::listArray()
{
	return listArray_;
}

// Return whether the array is a listarray
bool Variable::isArray()
{
	if (listArray_ || (arraySize_ > 0)) return TRUE;
	else return FALSE;
}

/*
// Set/get (virtuals)
*/

// Set size of array (only for non-list derivations)
bool Variable::setArraySize(int size)
{
	printf("A variable of type '%s' cannot have its array size set (%s).\n", VTypes::dataType(dataType_), name_.get());
	return FALSE;
}

// Set value of variable (char)
bool Variable::set(const char *s, Variable *index)
{
	printf("A variable of type '%s' cannot be set from a character (%s).\n", VTypes::dataType(dataType_), name_.get());
	return FALSE;
}

// Set value of variable (int)
bool Variable::set(int i, Variable *index)
{
	printf("A variable of type '%s' cannot be set from an integer (%s).\n", VTypes::dataType(dataType_), name_.get());
	return FALSE;
}

// Set value of variable (double)
bool Variable::set(double d, Variable *index)
{
	printf("A variable of type '%s' cannot be set from a double (%s).\n", VTypes::dataType(dataType_), name_.get());
	return FALSE;
}

// Set value of variable (pointer)
bool Variable::set(void *ptr, VTypes::DataType type, Variable *index)
{
	printf("A variable of type '%s' cannot be set from a pointer (%s).\n", VTypes::dataType(dataType_), name_.get());
	return FALSE;
}

// Get value of variable as character string
const char *Variable::asCharacter(Variable *index)
{
	printf("A variable of type '%s' cannot be returned as a character (%s).\n", VTypes::dataType(dataType_), name_.get());
	return "NULL";
}

// Get value of variable as integer
int Variable::asInteger(Variable *index)
{
	printf("A variable of type '%s' cannot be returned as an integer (%s).\n", VTypes::dataType(dataType_), name_.get());
	return 0;
}

// Get value of variable as double
double Variable::asDouble(Variable *index)
{
	printf("A variable of type '%s' cannot be returned as a double (%s).\n", VTypes::dataType(dataType_), name_.get());
	return 0.0;
}

// Get value of variable as float
float Variable::asFloat(Variable *index)
{
	return (float) asDouble(index);
}

// Get value of variable as bool
bool Variable::asBool(Variable *index)
{
	printf("A variable of type '%s' cannot be returned as a bool (%s).\n", VTypes::dataType(dataType_), name_.get());
	return FALSE;
}

// Get value of variable as pointer
void *Variable::asPointer(VTypes::DataType type, Variable *index)
{
	printf("A variable of type '%s' cannot be returned as a pointer (%s).\n", VTypes::dataType(dataType_), name_.get());
	return NULL;
}

// Step variable
bool Variable::step(int delta, Variable *index)
{
	printf("A variable of type '%s' cannot be stepped (%s).\n", VTypes::dataType(dataType_), name_.get());
	return FALSE;
}
