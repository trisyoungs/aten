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
#include "base/constants.h"
#include <string.h>

// Constructor
NuIntegerVariable::NuIntegerVariable(int i, bool constant) : integerData_(i)
{
	// Private variables
	returnType_ = NuVTypes::IntegerData;
	readOnly_ = constant;
}

// Destructor
NuIntegerVariable::~NuIntegerVariable()
{
}

/*
// Set / Get
*/

// Set from returnvalue node
bool NuIntegerVariable::set(NuReturnValue &rv)
{
}

// Reset variable
void NuIntegerVariable::reset()
{
	integerData_ = 0;
}


// Return value of node
bool NuIntegerVariable::execute(NuReturnValue &rv)
{
	rv.set(integerData_);
	return TRUE;
}

// Print node contents
void NuIntegerVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("%s%i (constant value)\n", tab, integerData_);
	else printf("%s%i (variable, name=%s)\n", tab, integerData_, name_.get());
	delete[] tab;
}
