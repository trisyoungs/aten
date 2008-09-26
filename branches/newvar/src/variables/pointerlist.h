/*
	*** Pointer Variable List
	*** src/variables/pointerlist.h
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

#ifndef ATEN_POINTERLISTVARIABLE_H
#define ATEN_POINTERLISTVARIABLE_H

#include "variables/variable.h"
#include "model/model.h"
#include "base/atom.h"
#include "base/cell.h"

// Forward declarations
class Model;

// Basic Pointer Variable List
template <class T> class PointerListVariable : public Variable
{
	public:
	// Constructor
	PointerListVariable<T>(VTypes::DataType);

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

	/*
	// Variable list pointer
	*/
	private:
	List<T> *listData_;

	public:
	// Set list pointer variable
	void setListData(List<T> *list);
};

// Constructor
template <class T> PointerListVariable<T>::PointerListVariable(VTypes::DataType ptrtype)
{
	dataType_ = ptrtype;
	readOnly_ = TRUE;
}

/*
// Set / Get (Basic PointerListVariable)
*/

// Set value of variable (char)
template <class T> bool PointerListVariable<T>::set(const char *s, Variable *index)
{
	msg.print("A Pointer variable cannot be set from a character.\n");
	return FALSE;
}

// Set value of variable (int)
template <class T> bool PointerListVariable<T>::set(int i, Variable *index)
{
	msg.print("A Pointer variable cannot be set from an integer.\n");
	return FALSE;
}

// Set value of variable (double)
template <class T> bool PointerListVariable<T>::set(double d, Variable *index)
{
	msg.print("A Pointer variable cannot be set from a double.\n");
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

#endif
