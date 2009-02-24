/*
	*** Pointer Variable List
	*** src/variables/pointerlist.h
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

#ifndef ATEN_POINTERLISTVARIABLE_H
#define ATEN_POINTERLISTVARIABLE_H

#include "variables/variable.h"
#include "model/model.h"
#include "classes/forcefieldatom.h"
#include "classes/grid.h"
#include "base/atom.h"
#include "base/pattern.h"

// Forward declarations
class Model;

// Basic Pointer Variable List
template <class T> class PointerListVariable : public Variable
{
	public:
	// Constructor
	PointerListVariable<T>(VTypes::DataType, List<T> *list);

	/*
	// Set / Get
	*/
	public:
	// Clears value of variable
	//void reset();
	// Set size of array (only for VTypes::ArrayType == NormalArray)
	bool setArraySize(int size);
	// Set value of variable (char)
	bool set(const char *s, Variable *index = NULL);
	// Set value of variable (int)
	bool set(int i, Variable *index = NULL);
	// Set value of variable (double)
	bool set(double d, Variable *index = NULL);
	// Set value of variable (pointer)
	bool set(void *ptr, VTypes::DataType type, Variable *index = NULL);
	// Get value of variable as character string
	const char *asCharacter(Variable *index = NULL);
	// Get value of variable as integer
	int asInteger(Variable *index = NULL);
	// Get value of variable as double
	double asDouble(Variable *index = NULL);
	// Get value of variable as float
	float asFloat(Variable *index = NULL);
	// Get value of variable as a boolean
	bool asBool(Variable *index = NULL);
	// Get value of variable as pointer of specified type
	void *asPointer(VTypes::DataType type, Variable *index = NULL);
	// Integer increase
	bool increase(int, Variable *index = NULL);
	// Integer decrease
	bool decrease(int, Variable *index = NULL);
	// Return size of array
	int arraySize();

	/*
	// Variable list pointer
	*/
	private:
	List<T> *listData_;
};

// Constructor
template <class T> PointerListVariable<T>::PointerListVariable(VTypes::DataType ptrtype, List<T> *list)
{
	dataType_ = ptrtype;
	listData_ = list;
	readOnly_ = TRUE;
	listArray_ = TRUE;
}

/*
// Set / Get (Basic PointerListVariable)
// All must be declared here, even those that would otherwise be taken from the base Variable class, since this is a template.
*/

// Get value of variable as character string
template <class T> const char *PointerListVariable<T>::asCharacter(Variable *index)
{
	printf("A variable of type PointerList cannot be returned as a character (%s).\n", name_.get());
	return "NULL";
}

// Get value of variable as integer
template <class T> int PointerListVariable<T>::asInteger(Variable *index)
{
	printf("A variable of type PointerList cannot be returned as an integer (%s).\n", name_.get());
	return 0;
}

// Get value of variable as double
template <class T> double PointerListVariable<T>::asDouble(Variable *index)
{
	printf("A variable of type PointerList cannot be returned as a double (%s).\n", name_.get());
	return 0.0;
}

// Get value of variable as float
template <class T> float PointerListVariable<T>::asFloat(Variable *index)
{
	printf("A variable of type PointerList cannot be returned as a float (%s).\n", name_.get());
	return 0.0f;
}

// Get value of variable as bool
template <class T> bool PointerListVariable<T>::asBool(Variable *index)
{
	printf("A variable of type PointerList cannot be returned as a bool (%s).\n", name_.get());
	return FALSE;
}

// Set size of array (only for non-list derivations)
template <class T> bool PointerListVariable<T>::setArraySize(int size)
{
	printf("Since '%s' is a PointerList variable, setting the array size is meaningless.\n", name_.get());
	return FALSE;
}

// Set value of variable (char)
template <class T> bool PointerListVariable<T>::set(const char *s, Variable *index)
{
	msg.print("A PointerList variable cannot be set from a character (and is read-only anyway).\n");
	return FALSE;
}

// Set value of variable (int)
template <class T> bool PointerListVariable<T>::set(int i, Variable *index)
{
	msg.print("A PointerList variable cannot be set from an integer (and is read-only anyway).\n");
	return FALSE;
}

// Set value of variable (double)
template <class T> bool PointerListVariable<T>::set(double d, Variable *index)
{
	msg.print("A PointerList variable cannot be set from a double (and is read-only anyway).\n");
	return FALSE;
}

// Set value of variable (double)
template <class T> bool PointerListVariable<T>::set(void *ptr, VTypes::DataType type, Variable *index)
{
	msg.print("A PointerList variable could be set from a pointer, but is read-only.\n");
	return FALSE;
}

// Get value of variable as pointer of specified type
template <class T> void *PointerListVariable<T>::asPointer(VTypes::DataType type, Variable *index)
{
	if (type != dataType_) printf("Error - a Pointer variable of type '%s' (%s) is being requested as a pointer of type '%s'\n", VTypes::dataType(dataType_), name(), VTypes::dataType(type));
	void *result = NULL;
	bool outofbounds = FALSE;
	// Check array index given
	if (index == NULL)
	{
		msg.print("No array index given to array '%s'.\n", name_.get());
		return NULL;
	}
	else
	{
		// Check array index bounds
		int n = index->asInteger();
		if ((n > listData_->nItems()) || (n < 1))
		if ((n > arraySize_) || (n < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", n, name_.get());
			return NULL;
		}
		// Get return value
		result = listData_->itemAt(n-1);
	}
	return result;
}

// Integer increase
template <class T> bool PointerListVariable<T>::increase(int i, Variable *index)
{
	printf("More work needed here...\n");
	return FALSE;
}

// Integer decrease
template <class T> bool PointerListVariable<T>::decrease(int i, Variable *index)
{
	printf("More work needed here...\n");
	return FALSE;
}

// Return size of array
template <class T> int PointerListVariable<T>::arraySize()
{
	return listData_->nItems();
}

#endif
