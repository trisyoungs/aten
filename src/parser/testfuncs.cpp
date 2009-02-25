/*
	*** Test Command Functions
	*** src/parser/testfuncs.cpp
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

#include "parser/commands.h"
#include "parser/commandnode.h"
#include <stdio.h>

// Add two quantities together
int NuCommand::function_Addition(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	printf("add.......\n");
	// The two argument (return) types determine our final return type, so get them first
	NuReturnValue v1, v2;
	c->arg(0)->execute(v1);
	c->arg(1)->execute(v2);
	// Test - calculate integer result
	rv.set( v1.asInteger() + v2.asInteger() );
	return NuCommand::Success;
}

// Subtract one quantity from another
int NuCommand::function_Subtraction(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	printf("Subtract........\n");
	return NuCommand::Success;
}

// Dummy Function
int NuCommand::function_NoFunction(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	printf("This is a dummy node.\n");
	rv.reset();
	return NuCommand::Success;
}

// Joiner
int NuCommand::function_Joiner(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	printf("Joiner.\n");
	// Execute both commands
	int result = c->arg(0)->execute(rv);
	// XXXX
	return NuCommand::Success;
}

