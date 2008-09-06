/*
	*** Character Variable
	*** src/variables/character.cpp
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

#include "variables/character.h"
#include "base/constants.h"
#include "base/messenger.h"
#include "base/sysfunc.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor
CharacterVariable::CharacterVariable()
{
	// Private variables
	dataType_ = VTypes::CharacterData;
	arrayType_ = VTypes::NoArray;
	charArrayData_ = NULL;
	arraySize_ = -1;
}

/*
// Set / Get
*/

// Set value of variable (char)
bool CharacterVariable::set(const char *s, int index)
{
	// Check read/write status
	if (readOnly_)
	{
		msg.print("Variable '%s' is read-only.\n", name_.get());
		return FALSE;
	}
	bool outofbounds = FALSE;
	// Check array index given
	if (index == -1)
	{
		if (arrayType_ != VTypes::NoArray)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		charData_ = s;
	}
	else
	{
		if (arrayType_ == VTypes::NoArray)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return FALSE;
		}
		// Get array and set value...
		if (arrayType_ == VTypes::NormalArray)
		{
			if (index > arraySize_) outofbounds = TRUE;
			else arrayCharData_[index-1] = s;
		}
		else msg.print("This array element cannot be set.\n");
		if (outofbounds)
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", index, name_.get());
			return FALSE;
		}
	}
	return TRUE;
}

// Set value of variable (int)
bool CharacterVariable::set(int i, int index)
{
	return set(itoa(i), index);
}

// Set value of variable (double)
bool CharacterVariable::set(double d, int index)
{
	return set(ftoa(d), index);
}

// Set value of variable (pointer)
bool CharacterVariable::set(void *ptr, VTypes::DataType type, int index)
{
	printf("A Character variable cannot be set from a pointer.\n");
	return FALSE;
}

// Get value of variable as character string
const char *CharacterVariable::asCharacter(int index)
{
	return data.get();
}

// Get value of variable as integer
int CharacterVariable::asInteger(int index)
{
	return atoi(data.get());
}

// Get value of variable as double
double CharacterVariable::asDouble(int index)
{
	return atof(data.get());
}

// Get value of variable as float
float CharacterVariable::asFloat(int index)
{
	return (float) atof(data.get());
}

// Get value of variable as a boolean
bool CharacterVariable::asBool(int index)
{
	return data.asBool();
}

// Get value of variable as pointer of specified type
void *CharacterVariable::asPointer(VTypes::DataType type, int index)
{
	printf("A Character variable cannot be returned as a pointer.\n");
	return NULL;
}

// Character increase
bool CharacterVariable::increase(int i, int index)
{
	printf("A Character variable cannot be increased.");
	return FALSE;
}

// Character decrease
bool CharacterVariable::decrease(int i, int index)
{
	printf("A Character variable cannot be decreased.");
	return FALSE;
}

