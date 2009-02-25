/*
	*** Integer Variable
	*** src/parser/integer.cpp
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

#include "parser/integer.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include "base/messenger.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor
NuIntegerVariable::NuIntegerVariable(int i, bool constant) : integerData_(i)
{
	// Private variables
	returnType_ = VTypes::IntegerData;
	readOnly_ = constant;
}

// Destructor
NuIntegerVariable::~NuIntegerVariable()
{
}

/*
// Set / Get
*/

// Set value of variable (int)
bool NuIntegerVariable::set(int i)
{
	integerData_ = i;
	return TRUE;
}


// Get value of variable as integer
int NuIntegerVariable::asInteger()
{
	return integerData_;
}

// Step variable
bool NuIntegerVariable::step(int delta, NuVariable *index)
{
	integerData_ += delta;
	return TRUE;
}

// Clears value of variable
bool NuIntegerVariable::reset(NuVariable *index)
{
	integerData_ = 0;
	return TRUE;
}
