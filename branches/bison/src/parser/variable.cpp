/*
	*** NuVariable
	*** src/parser/variable.cpp
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

#include "parser/variable.h"
#include "parser/returnvalue.h"
#include <string.h>

// Constructor
NuVariable::NuVariable()
{
	// Private variables
	name_.set("unnamed");
}

// Destructor (virtual)
NuVariable::~NuVariable()
{
}

// Set name of variable
void NuVariable::setName(const char* s)
{
	name_.set(s);
}

// Get name of variable
const char *NuVariable::name()
{
	return name_.get();
}
