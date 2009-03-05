/*
	*** Real Variable
	*** src/parser/real.cpp
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

#include "parser/real.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Constructor
NuRealVariable::NuRealVariable(double d, bool constant) : realData_(d)
{
	// Private variables
	returnType_ = NuVTypes::RealData;
	readOnly_ = constant;
}

// Destructor
NuRealVariable::~NuRealVariable()
{
}

/*
// Set / Get
*/

// Set value of variable (real)
bool NuRealVariable::set(NuReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a real) cannot be assigned to.\n");
		return FALSE;
	}
	bool success;
	realData_ = rv.asReal(success);
	return FALSE;

}

// Reset variable
void NuRealVariable::reset()
{
	realData_ = 0.0;
}

// Return value of node
bool NuRealVariable::execute(NuReturnValue &rv)
{
	rv.set(realData_);
	return TRUE;
}

// Print node contents
void NuRealVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("[C]%s%f (constant value)\n", tab, realData_);
	else printf("[V]%s%f (variable, name=%s)\n", tab, realData_, name_.get());
	delete[] tab;
}
