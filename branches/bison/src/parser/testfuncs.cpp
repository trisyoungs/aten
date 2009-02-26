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
#include "base/mathfunc.h"
#include <stdio.h>
#include <string.h>

// Operate!
bool operate(NuCommand::Function func, NuReturnValue *rv1, NuReturnValue *rv2, NuReturnValue &result)
{
	static char s[8096];
	s[0] = '\0';
	bool failed = FALSE;
	// Array or returnvalue structures
	NuReturnValue *rv[2];
	VTypes::DataType t[2];
	int hi, lo;
	rv[0] = rv1;
	rv[1] = rv2;
	// Swap values over if the type of rv2 is 'greater than' rv1, and store data type values
	// The first value rv[0] (and t[0]) will always contain the 'highest' type
	if (rv[1]->type() > rv[0]->type())
	{
		rv[1] = rv1;
		rv[0] = rv2;
	}
	t[0] = rv[0]->type();
	t[1] = rv[1]->type();
	// Check for no data type or pointer type
	if (t[0] == VTypes::NoData)
	{
		msg.print("Error: LHS of operator %s has no data type.\n", NuCommand::data[func].keyword);
		return FALSE;
	}
	if (t[1] == VTypes::NoData)
	{
		msg.print("Error: RHS of operator %s has no data type.\n", NuCommand::data[func].keyword);
		return FALSE;
	}
	if (VTypes::isPointer(t[0]))
	{
		msg.print("Error: LHS of operator %s is a pointer.\n", NuCommand::data[func].keyword);
		return FALSE;
	}
	if (VTypes::isPointer(t[1]))
	{
		msg.print("Error: RHS of operator %s is a pointer.\n", NuCommand::data[func].keyword);
		return FALSE;
	}
	// We will decide what to do based on data types of integer, real, character, or vector
	if (t[0] == t[1])
	{
		switch (func)
		{
			case (NuCommand::OperatorAdd):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger() + rv2->asInteger());
				else if (t[0] == VTypes::RealData) result.set(rv1->asReal() + rv2->asReal());
				else if (t[0] == VTypes::CharacterData)
				{
					strcpy(s, rv1->asCharacter());
					strcat(s, rv2->asCharacter());
					result.set(s);
				}
				//else if (t1 == VTypes::VectorData) result.set(v1.asVector() + v2.asVector());
				else failed = TRUE;
				break;
			case (NuCommand::OperatorSubtract):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger() - rv2->asInteger());
				else if (t[0] == VTypes::RealData) result.set(rv1->asReal() - rv2->asReal());
				//else if (t1 == VTypes::VectorData) result.set(v1.asVector() + v2.asVector());
				else failed = TRUE;
				break;
			case (NuCommand::OperatorMultiply):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger() * rv2->asInteger());
				else if (t[0] == VTypes::RealData) result.set(rv1->asReal() * rv2->asReal());
				//else if (t1 == VTypes::VectorData) result.set(v1.asVector() + v2.asVector());
				else failed = TRUE;
				break;
			case (NuCommand::OperatorDivide):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger() / rv2->asInteger());
				else if (t[0] == VTypes::RealData) result.set(rv1->asReal() / rv2->asReal());
				//else if (t1 == VTypes::VectorData) result.set(v1.asVector() + v2.asVector());
				else failed = TRUE;
				break;
			case (NuCommand::OperatorPower):
				if (t[0] == VTypes::IntegerData) result.set(power(rv1->asInteger(), rv2->asInteger()));
				else if (t[0] == VTypes::RealData) result.set(pow(rv1->asReal(), rv2->asReal()));
				//else if (t1 == VTypes::VectorData) result.set(v1.asVector() + v2.asVector());
				else failed = TRUE;
				break;
			default:
				failed = TRUE;
				break;
		}
	}
// 	else if (t[0] == VTypes::VectorData)
	else if (t[0] == VTypes::CharacterData)
	{
		// There are limited operations that we can do with character strings....
		switch (func)
		{
			// Multiply string by the (integer) number specified
			case (NuCommand::OperatorMultiply):
				if ((t[1] == VTypes::IntegerData) || (t[1] == VTypes::RealData))
				{
					s[0] = '\0';
					for (int n=0; n<rv[1]->asInteger(); n++) strcat(s, rv[0]->asCharacter());
					result.set(s);
				}
				else failed = TRUE;
			default:
				failed = TRUE;
				break;
		}
	}
	else
	{
		// Both values are numbers - one is an integer, and one is a real, so return a real result
		switch (func)
		{
			case (NuCommand::OperatorAdd):
				result.set(rv1->asReal() + rv2->asReal());
				break;
			case (NuCommand::OperatorSubtract):
				result.set(rv1->asReal() - rv2->asReal());
				break;
			case (NuCommand::OperatorMultiply):
				result.set(rv1->asReal() * rv2->asReal());
				break;
			case (NuCommand::OperatorDivide):
				result.set(rv1->asReal() / rv2->asReal());
				break;
			case (NuCommand::OperatorPower):
				result.set(pow(rv1->asReal(), rv2->asReal()));
				break;
		}
	}
	if (failed)
	{
		msg.print("Error: the expression '%s %s %s' does not return a valid result.\n", NuCommand::data[func].keyword, VTypes::dataType(rv1->type()), VTypes::dataType(rv2->type()));
		return FALSE;
	}
	return TRUE;
}

// Add two quantities together
int NuCommand::function_OperatorAdd(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	printf("add.......\n");
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	c->arg(0)->execute(v1);
	c->arg(1)->execute(v2);
	return (operate(NuCommand::OperatorAdd, &v1, &v2, rv) ? NuCommand::Success : NuCommand::Fail);
}

// Subtract one quantity from another
int NuCommand::function_OperatorSubtract(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	printf("Subtract........\n");
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	c->arg(0)->execute(v1);
	c->arg(1)->execute(v2);
	return (operate(NuCommand::OperatorSubtract, &v1, &v2, rv) ? NuCommand::Success : NuCommand::Fail);
}

// Multiply one quantity by another
int NuCommand::function_OperatorMultiply(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	printf("Multiply........\n");
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	c->arg(0)->execute(v1);
	c->arg(1)->execute(v2);
	return (operate(NuCommand::OperatorMultiply, &v1, &v2, rv) ? NuCommand::Success : NuCommand::Fail);
}

// Divide one quantity by another
int NuCommand::function_OperatorDivide(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	printf("Divide........\n");
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	c->arg(0)->execute(v1);
	c->arg(1)->execute(v2);
	return (operate(NuCommand::OperatorDivide, &v1, &v2, rv) ? NuCommand::Success : NuCommand::Fail);
}

// Raise one quantity to the power of another
int NuCommand::function_OperatorPower(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	printf("Power........\n");
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	c->arg(0)->execute(v1);
	c->arg(1)->execute(v2);
	return (operate(NuCommand::OperatorPower, &v1, &v2, rv) ? NuCommand::Success : NuCommand::Fail);
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

