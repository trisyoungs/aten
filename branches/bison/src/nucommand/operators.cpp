/*
	*** Operators
	*** src/nucommand/operators.cpp
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

#include "nucommand/commands.h"
#include "parser/commandnode.h"
#include "base/mathfunc.h"
#include <stdio.h>
#include <string.h>

// Operate!
bool operate(NuCommand::Function func, NuReturnValue *rv1, NuReturnValue *rv2, NuReturnValue &result)
{
	static char s[8096];
	s[0] = '\0';
	bool failed = FALSE, b;
	// Array or returnvalue structures
	NuReturnValue *rv[2];
	NuVTypes::DataType t[2];
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
	if (t[0] == NuVTypes::NoData)
	{
		msg.print("Error: LHS of operator %s has no data type.\n", NuCommand::data[func].keyword);
		return FALSE;
	}
	if (t[1] == NuVTypes::NoData)
	{
		msg.print("Error: RHS of operator %s has no data type.\n", NuCommand::data[func].keyword);
		return FALSE;
	}
	if (NuVTypes::isPointer(t[0]))
	{
		msg.print("Error: LHS of operator %s is a pointer.\n", NuCommand::data[func].keyword);
		return FALSE;
	}
	if (NuVTypes::isPointer(t[1]))
	{
		msg.print("Error: RHS of operator %s is a pointer.\n", NuCommand::data[func].keyword);
		return FALSE;
	}
	// We will decide what to do based on data types of integer, real, character, or vector
	rv[0]->info();
	rv[1]->info();
	if (t[0] == t[1])
	{
		switch (func)
		{
			case (NuCommand::OperatorAdd):
				if (t[0] == NuVTypes::IntegerData) result.set(rv1->asInteger(b) + rv2->asInteger(b));
				else if (t[0] == NuVTypes::RealData) result.set(rv1->asReal(b) + rv2->asReal(b));
				else if (t[0] == NuVTypes::StringData)
				{
					strcpy(s, rv1->asString(b));
					strcat(s, rv2->asString(b));
					result.set(s);
				}
				else failed = TRUE;
				break;
			case (NuCommand::OperatorSubtract):
				if (t[0] == NuVTypes::IntegerData) result.set(rv1->asInteger(b) - rv2->asInteger(b));
				else if (t[0] == NuVTypes::RealData) result.set(rv1->asReal(b) - rv2->asReal(b));
				else failed = TRUE;
				break;
			case (NuCommand::OperatorMultiply):
				if (t[0] == NuVTypes::IntegerData) result.set(rv1->asInteger(b) * rv2->asInteger(b));
				else if (t[0] == NuVTypes::RealData) result.set(rv1->asReal(b) * rv2->asReal(b));
				else failed = TRUE;
				break;
			case (NuCommand::OperatorDivide):
				if (t[0] == NuVTypes::IntegerData) result.set(rv1->asInteger(b) / rv2->asInteger(b));
				else if (t[0] == NuVTypes::RealData) result.set(rv1->asReal(b) / rv2->asReal(b));
				else failed = TRUE;
				break;
			case (NuCommand::OperatorPower):
				if (t[0] == NuVTypes::IntegerData) result.set(power(rv1->asInteger(b), rv2->asInteger(b)));
				else if (t[0] == NuVTypes::RealData) result.set(pow(rv1->asReal(b), rv2->asReal(b)));
				else failed = TRUE;
				break;
			default:
				failed = TRUE;
				break;
		}
	}
	else if (t[0] == NuVTypes::StringData)
	{
		// There are limited operations that we can do with character strings....
		switch (func)
		{
			// Multiply string by the (integer) number specified
			case (NuCommand::OperatorMultiply):
				if ((t[1] == NuVTypes::IntegerData) || (t[1] == NuVTypes::RealData))
				{
					s[0] = '\0';
					for (int n=0; n<rv[1]->asInteger(b); n++) strcat(s, rv[0]->asString(b));
					result.set(s);
				}
				else failed = TRUE;
				break;
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
				result.set(rv1->asReal(b) + rv2->asReal(b));
				break;
			case (NuCommand::OperatorSubtract):
				result.set(rv1->asReal(b) - rv2->asReal(b));
				break;
			case (NuCommand::OperatorMultiply):
				result.set(rv1->asReal(b) * rv2->asReal(b));
				break;
			case (NuCommand::OperatorDivide):
				result.set(rv1->asReal(b) / rv2->asReal(b));
				break;
			case (NuCommand::OperatorPower):
				result.set(pow(rv1->asReal(b), rv2->asReal(b)));
				break;
		}
	}
	if (failed)
	{
		msg.print("Error: the expression '%s %s %s' does not return a meaningful result.\n", NuVTypes::dataType(rv1->type()), NuCommand::data[func].keyword, NuVTypes::dataType(rv2->type()));
		return FALSE;
	}
	return TRUE;
}

// Test!
bool test(NuCommand::Function func, NuReturnValue *rv1, NuReturnValue *rv2, NuReturnValue &result)
{
	// Grab data types of operands
	NuVTypes::DataType t[2];
	t[0] = rv1->type();
	t[1] = rv2->type();
	bool failed = FALSE;
	bool b;
	if (t[0] == NuVTypes::NoData)
	{
		msg.print("Error: LHS of operator %s has no data type.\n", NuCommand::data[func].keyword);
		return FALSE;
	}
	if (t[1] == NuVTypes::NoData)
	{
		msg.print("Error: RHS of operator %s has no data type.\n", NuCommand::data[func].keyword);
		return FALSE;
	}
	// Provided the types are the same (or a mix of real/int) we're okay....
	if ((t[0] < NuVTypes::StringData) && (t[1] < NuVTypes::StringData) && (t[0] != t[1])) t[0] = t[1] = NuVTypes::RealData;
	if (t[0] == t[1])
	{
		switch (func)
		{
			case (NuCommand::OperatorEqualTo):
				if (t[0] == NuVTypes::IntegerData) result.set(rv1->asInteger(b) == rv2->asInteger(b));
				else if (t[0] == NuVTypes::RealData) result.set(rv1->asReal(b) == rv2->asReal(b));
				else if (t[0] == NuVTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) == 0);
				else if (t[0] >= NuVTypes::AtenData) result.set( rv1->asPointer(t[0],b) == rv2->asPointer(t[1],b) );
				else failed = TRUE;
				break;
			case (NuCommand::OperatorNotEqualTo):
				if (t[0] == NuVTypes::IntegerData) result.set(rv1->asInteger(b) != rv2->asInteger(b));
				else if (t[0] == NuVTypes::RealData) result.set(rv1->asReal(b) != rv2->asReal(b));
				else if (t[0] == NuVTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) != 0);
				else if (t[0] >= NuVTypes::AtenData) result.set( rv1->asPointer(t[0],b) != rv2->asPointer(t[1],b) );
				else failed = TRUE;
				break;
			case (NuCommand::OperatorGreaterThan):
				if (t[0] == NuVTypes::IntegerData) result.set(rv1->asInteger(b) > rv2->asInteger(b));
				else if (t[0] == NuVTypes::RealData) result.set(rv1->asReal(b) > rv2->asReal(b));
				else if (t[0] == NuVTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) > 0);
				else if (t[0] >= NuVTypes::AtenData) result.set( rv1->asPointer(t[0],b) > rv2->asPointer(t[1],b) );
				else failed = TRUE;
				break;
			case (NuCommand::OperatorGreaterThanEqualTo):
				if (t[0] == NuVTypes::IntegerData) result.set(rv1->asInteger(b) >= rv2->asInteger(b));
				else if (t[0] == NuVTypes::RealData) result.set(rv1->asReal(b) >= rv2->asReal(b));
				else if (t[0] == NuVTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) >= 0);
				else if (t[0] >= NuVTypes::AtenData) result.set( rv1->asPointer(t[0],b) >= rv2->asPointer(t[1],b) );
				else failed = TRUE;
				break;
			case (NuCommand::OperatorLessThan):
				if (t[0] == NuVTypes::IntegerData) result.set(rv1->asInteger(b) < rv2->asInteger(b));
				else if (t[0] == NuVTypes::RealData) result.set(rv1->asReal(b) < rv2->asReal(b));
				else if (t[0] == NuVTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) < 0);
				else if (t[0] >= NuVTypes::AtenData) result.set( rv1->asPointer(t[0],b) < rv2->asPointer(t[1],b) );
				else failed = TRUE;
				break;
			case (NuCommand::OperatorLessThanEqualTo):
				if (t[0] == NuVTypes::IntegerData) result.set(rv1->asInteger(b) <= rv2->asInteger(b));
				else if (t[0] == NuVTypes::RealData) result.set(rv1->asReal(b) <= rv2->asReal(b));
				else if (t[0] == NuVTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) <= 0);
				else if (t[0] >= NuVTypes::AtenData) result.set( rv1->asPointer(t[0],b) <= rv2->asPointer(t[1],b) );
				else failed = TRUE;
				break;
			default:
				failed = TRUE;
				break;
		}
	}
	else 
	{
		// Get 'highest' type of the two operands
		NuReturnValue *rv[2];
		// Swap values over if the type of rv2 is 'greater than' rv1, and store data type values
		// The first value rv[0] (and t[0]) will always contain the 'highest' type
		if (t[1] > t[0])
		{
			rv[0] = rv2;
			rv[1] = rv1;
			t[0] = rv[0]->type();
			t[1] = rv[1]->type();
		}
		else
		{
			rv[0] = rv1;
			rv[1] = rv2;
		}
		// There are no tests we can do between a character or a vector and another type
		if ((t[0] == NuVTypes::StringData) || (t[0] == NuVTypes::VectorData)) failed = TRUE;
		else if (t[0] >= NuVTypes::AtenData)
		{
			// We will allow (in)equality between pointer and integer but not real
			if (t[1] > NuVTypes::IntegerData) failed = TRUE;
			else switch (func)
			{
				case (NuCommand::OperatorEqualTo):
					result.set( ((long int) rv[0]->asPointer(t[0],b)) == ((long int) rv[1]->asInteger(b)) );
					break;
				case (NuCommand::OperatorNotEqualTo):
					result.set( ((long int) rv[0]->asPointer(t[0],b)) != ((long int) rv[1]->asInteger(b)) );
					break;
				default:
					failed = TRUE;
					break;
			}
		}
	}
	if (failed)
	{
		msg.print("Error: the test '%s %s %s' does not return a valid result.\n", NuVTypes::dataType(rv1->type()), NuCommand::data[func].keyword, NuVTypes::dataType(rv2->type()));
		return FALSE;
	}
	return TRUE;
}

// Add two quantities together
bool NuCommand::function_OperatorAdd(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0,v1)) return FALSE;
	if (!c->arg(1,v2)) return FALSE;
	return operate(NuCommand::OperatorAdd, &v1, &v2, rv);
}

// Subtract one quantity from another
bool NuCommand::function_OperatorSubtract(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0,v1)) return FALSE;
	if (!c->arg(1,v2)) return FALSE;
	return operate(NuCommand::OperatorSubtract, &v1, &v2, rv);
}

// Multiply one quantity by another
bool NuCommand::function_OperatorMultiply(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0,v1)) return FALSE;
	if (!c->arg(1,v2)) return FALSE;
	return operate(NuCommand::OperatorMultiply, &v1, &v2, rv);
}

// Negate value
bool NuCommand::function_OperatorNegate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (!c->arg(0, rv)) return FALSE;
	switch (c->argType(0))
	{
		case (NuVTypes::IntegerData):
			rv.set(-rv.asInteger());
			break;
		case (NuVTypes::RealData):
			rv.set(-rv.asReal());
			break;
		case (NuVTypes::VectorData):
			rv.set(-rv.asVector());
			break;
	}
	return c->setArg(0, rv);
}

// Divide one quantity by another
bool NuCommand::function_OperatorDivide(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return operate(NuCommand::OperatorDivide, &v1, &v2, rv);
}

// Raise one quantity to the power of another
bool NuCommand::function_OperatorPower(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return operate(NuCommand::OperatorPower, &v1, &v2, rv);
}

// Equal To
bool NuCommand::function_OperatorEqualTo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(NuCommand::OperatorEqualTo, &v1, &v2, rv);
}

// Not Equal To
bool NuCommand::function_OperatorNotEqualTo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(NuCommand::OperatorNotEqualTo, &v1, &v2, rv);
}

// Greater Than
bool NuCommand::function_OperatorGreaterThan(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(NuCommand::OperatorGreaterThan, &v1, &v2, rv);
}

// Greater Than Equal To
bool NuCommand::function_OperatorGreaterThanEqualTo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(NuCommand::OperatorGreaterThanEqualTo, &v1, &v2, rv);
}

// Less Than
bool NuCommand::function_OperatorLessThan(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(NuCommand::OperatorLessThan, &v1, &v2, rv);
}

// Less Than Equal To
bool NuCommand::function_OperatorLessThanEqualTo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(NuCommand::OperatorLessThanEqualTo, &v1, &v2, rv);
}

// Assignment
bool NuCommand::function_OperatorAssignment(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab the second argument result and assign it to the first
	if (!c->arg(1, rv)) return FALSE;
	if (!c->setArg(0, rv)) return FALSE;
	return (c->arg(0, rv));
}

// Assignment Divide
bool NuCommand::function_OperatorAssignmentDivide(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	if (!operate(NuCommand::OperatorDivide, &v1, &v2, rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return (c->arg(0, rv));
}

// Assignment Minus
bool NuCommand::function_OperatorAssignmentMinus(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	if (!operate(NuCommand::OperatorSubtract, &v1, &v2, rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return (c->arg(0, rv));
}

// Assignment Plus
bool NuCommand::function_OperatorAssignmentMultiply(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	if (!operate(NuCommand::OperatorMultiply, &v1, &v2, rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return (c->arg(0, rv));
}

// Assignment Plus
bool NuCommand::function_OperatorAssignmentPlus(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	NuReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	if (!operate(NuCommand::OperatorAdd, &v1, &v2, rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return (c->arg(0, rv));
}

// Postfix Increase
bool NuCommand::function_OperatorPostfixIncrease(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	NuReturnValue newvalue;
	if (c->argType(0) == NuVTypes::IntegerData) newvalue.set( rv.asInteger()+1 );
	else if (c->argType(0) == NuVTypes::RealData) newvalue.set( rv.asReal()+1 );
	return c->setArg(0, newvalue);
}

// Postfix Decrease
bool NuCommand::function_OperatorPostfixDecrease(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	NuReturnValue newvalue;
	if (c->argType(0) == NuVTypes::IntegerData) newvalue.set( rv.asInteger()-1 );
	else if (c->argType(0) == NuVTypes::RealData) newvalue.set( rv.asReal()-1 );
	return c->setArg(0, newvalue);
}

// Prefix Increase
bool NuCommand::function_OperatorPrefixIncrease(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	if (c->argType(0) == NuVTypes::IntegerData) rv.set( rv.asInteger()+1 );
	else if (c->argType(0) == NuVTypes::RealData) rv.set( rv.asReal()+1 );
	return c->setArg(0, rv);
}

// Prefix Decrease
bool NuCommand::function_OperatorPrefixDecrease(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	if (c->argType(0) == NuVTypes::IntegerData) rv.set( rv.asInteger()-1 );
	else if (c->argType(0) == NuVTypes::RealData) rv.set( rv.asReal()-1 );
	return c->setArg(0, rv);
}
