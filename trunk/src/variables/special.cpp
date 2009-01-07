/*
	*** Special Variable
	*** src/variables/special.cpp
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

#include "variables/special.h"
#include "base/sysfunc.h"
#include "base/mathfunc.h"
#include "base/constants.h"
#include "base/messenger.h"
#include "main/aten.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor / Destructor
SpecialVariable::SpecialVariable()
{
	dataType_ = VTypes::NoData;
	arraySize_ = -1;
}

// Destructor
SpecialVariable::~SpecialVariable()
{
}

/*
// Set / Get
*/

// Set type of special variable
void SpecialVariable::setSpecialData(SpecialVariable::SpecialData sd)
{
	type_ = sd;
	switch (type_)
	{
		case (SpecialVariable::SpecialNModels):
			dataType_ = VTypes::IntegerData;
			break;
		case (SpecialVariable::SpecialRandom):
			dataType_ = VTypes::RealData;
			break;
		default:
			printf("!!!ERROR!!! SpecialVariable type not recognised.\n");
			break;
	}
}

// Get value of variable as character string
const char *SpecialVariable::asCharacter(Variable *index)
{
	if (index != NULL)
	{
		msg.print("Array index given to variable '%s'.\n", name_.get());
		return "NULL";
	}
	// Lookup data type and return value explicitly
	switch (type_)
	{
		case (SpecialVariable::SpecialNModels):
			return itoa(aten.nModels());
			break;
		case (SpecialVariable::SpecialRandom):
			return ftoa(csRandom());
			break;
		default:
			printf("!!!ERROR!!! SpecialVariable type not recognised and can't be returned as a character.\n");
			break;
	}
}

// Get value of variable as integer
int SpecialVariable::asInteger(Variable *index)
{
	if (index != NULL)
	{
		msg.print("Array index given to variable '%s'.\n", name_.get());
		return 0;
	}
	// Lookup data type and return value explicitly
	switch (type_)
	{
		case (SpecialVariable::SpecialNModels):
			return aten.nModels();
			break;
		case (SpecialVariable::SpecialRandom):
			return (int) csRandom();
			break;
		default:
			printf("!!!ERROR!!! SpecialVariable type not recognised and can't be returned as an integer.\n");
			break;
	}
}

// Get value of variable as double
double SpecialVariable::asDouble(Variable *index)
{
	if (index != NULL)
	{
		msg.print("Array index given to variable '%s'.\n", name_.get());
		return 0.0;
	}
	// Lookup data type and return value explicitly
	switch (type_)
	{
		case (SpecialVariable::SpecialNModels):
			return (double) aten.nModels();
			break;
		case (SpecialVariable::SpecialRandom):
			return csRandom();
			break;
		default:
			printf("!!!ERROR!!! SpecialVariable type not recognised and can't be returned as a real.\n");
			break;
	}
}

// Get value of variable as a boolean
bool SpecialVariable::asBool(Variable *index)
{
	return (asDouble(index) <= 0 ? FALSE : TRUE);
}

// Clears value of variable (or not, in this case);
bool SpecialVariable::reset(Variable *index)
{
	return TRUE;
}
