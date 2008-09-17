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
	charArrayData_ = NULL;
	arraySize_ = -1;
}

/*
// Set / Get
*/

// Set size of array
bool CharacterVariable::setArraySize(int size)
{
	if (arraySize_ != -1)
	{
		msg.print("Character variable '%s' already has an array.\n", name_.get());
		return FALSE;
	}
	charArrayData_ = new Dnchar[size];
	arraySize_ = size;
	return TRUE;
}

// Set value of variable (char)
bool CharacterVariable::set(const char *s, int index)
{
	// Check read/write status
	if (readOnly_)
	{
		msg.print("Variable '%s' is read-only.\n", name_.get());
		return FALSE;
	}
	// Check array index given
	if (index == -1)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		charData_ = s;
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return FALSE;
		}
		if ((index > arraySize_) || (index < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", index, name_.get());
			return FALSE;
		}
		else charArrayData_[index-1] = s;
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

// Get value of variable as character string
const char *CharacterVariable::asCharacter(int index)
{
	// Check array index given
	if (index == -1)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return "NULL";
		}
		return charData_.get();
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return "NULL";
		}
		if ((index > arraySize_) || (index < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", index, name_.get());
			return "NULL";
		}
		return charArrayData_[index-1].get();
	}
}

// Get value of variable as integer
int CharacterVariable::asInteger(int index)
{
	return atoi(asCharacter(index));
}

// Get value of variable as double
double CharacterVariable::asDouble(int index)
{
	return atof(asCharacter(index));
}

// Get value of variable as a boolean
bool CharacterVariable::asBool(int index)
{
	// Check array index given
	if (index == -1)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		return charData_.asBool();
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return FALSE;
		}
		if ((index > arraySize_) || (index < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", index, name_.get());
			return FALSE;
		}
		return charArrayData_[index-1].asBool();
	}
}
