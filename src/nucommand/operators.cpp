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
bool operate(Command::Function func, ReturnValue *rv1, ReturnValue *rv2, ReturnValue &result)
{
	static char s[8096];
	s[0] = '\0';
	bool failed = FALSE, b;
	// Array or returnvalue structures
	ReturnValue *rv[2];
	VTypes::DataType t[2];
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
		msg.print("Error: LHS of operator %s has no data type.\n", Command::data[func].keyword);
		return FALSE;
	}
	if (t[1] == VTypes::NoData)
	{
		msg.print("Error: RHS of operator %s has no data type.\n", Command::data[func].keyword);
		return FALSE;
	}
	if (VTypes::isPointer(t[0]))
	{
		msg.print("Error: LHS of operator %s is a pointer.\n", Command::data[func].keyword);
		return FALSE;
	}
	if (VTypes::isPointer(t[1]))
	{
		msg.print("Error: RHS of operator %s is a pointer.\n", Command::data[func].keyword);
		return FALSE;
	}
	// We will decide what to do based on data types of integer, real, character, or vector
	rv[0]->info();
	rv[1]->info();
	if (t[0] == t[1])
	{
		switch (func)
		{
			case (Command::OperatorAdd):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) + rv2->asInteger(b));
				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) + rv2->asDouble(b));
				else if (t[0] == VTypes::StringData)
				{
					strcpy(s, rv1->asString(b));
					strcat(s, rv2->asString(b));
					result.set(s);
				}
				else failed = TRUE;
				break;
			case (Command::OperatorSubtract):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) - rv2->asInteger(b));
				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) - rv2->asDouble(b));
				else failed = TRUE;
				break;
			case (Command::OperatorMultiply):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) * rv2->asInteger(b));
				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) * rv2->asDouble(b));
				else failed = TRUE;
				break;
			case (Command::OperatorDivide):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) / rv2->asInteger(b));
				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) / rv2->asDouble(b));
				else failed = TRUE;
				break;
			case (Command::OperatorPower):
				if (t[0] == VTypes::IntegerData) result.set(power(rv1->asInteger(b), rv2->asInteger(b)));
				else if (t[0] == VTypes::DoubleData) result.set(pow(rv1->asDouble(b), rv2->asDouble(b)));
				else failed = TRUE;
				break;
			default:
				failed = TRUE;
				break;
		}
	}
	else if (t[0] == VTypes::StringData)
	{
		// There are limited operations that we can do with character strings....
		switch (func)
		{
			// Multiply string by the (integer) number specified
			case (Command::OperatorMultiply):
				if ((t[1] == VTypes::IntegerData) || (t[1] == VTypes::DoubleData))
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
			case (Command::OperatorAdd):
				result.set(rv1->asDouble(b) + rv2->asDouble(b));
				break;
			case (Command::OperatorSubtract):
				result.set(rv1->asDouble(b) - rv2->asDouble(b));
				break;
			case (Command::OperatorMultiply):
				result.set(rv1->asDouble(b) * rv2->asDouble(b));
				break;
			case (Command::OperatorDivide):
				result.set(rv1->asDouble(b) / rv2->asDouble(b));
				break;
			case (Command::OperatorPower):
				result.set(pow(rv1->asDouble(b), rv2->asDouble(b)));
				break;
		}
	}
	if (failed)
	{
		msg.print("Error: the expression '%s %s %s' does not return a meaningful result.\n", VTypes::dataType(rv1->type()), Command::data[func].keyword, VTypes::dataType(rv2->type()));
		return FALSE;
	}
	return TRUE;
}

// Test!
bool test(Command::Function func, ReturnValue *rv1, ReturnValue *rv2, ReturnValue &result)
{
	// Grab data types of operands
	VTypes::DataType t[2];
	t[0] = rv1->type();
	t[1] = rv2->type();
	bool failed = FALSE;
	bool b;
	if (t[0] == VTypes::NoData)
	{
		msg.print("Error: LHS of operator %s has no data type.\n", Command::data[func].keyword);
		return FALSE;
	}
	if (t[1] == VTypes::NoData)
	{
		msg.print("Error: RHS of operator %s has no data type.\n", Command::data[func].keyword);
		return FALSE;
	}
	// Provided the types are the same (or a mix of real/int) we're okay....
	if ((t[0] < VTypes::StringData) && (t[1] < VTypes::StringData) && (t[0] != t[1])) t[0] = t[1] = VTypes::DoubleData;
	if (t[0] == t[1])
	{
		switch (func)
		{
			case (Command::OperatorEqualTo):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) == rv2->asInteger(b));
				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) == rv2->asDouble(b));
				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) == 0);
				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) == rv2->asPointer(t[1],b) );
				else failed = TRUE;
				break;
			case (Command::OperatorNotEqualTo):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) != rv2->asInteger(b));
				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) != rv2->asDouble(b));
				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) != 0);
				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) != rv2->asPointer(t[1],b) );
				else failed = TRUE;
				break;
			case (Command::OperatorGreaterThan):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) > rv2->asInteger(b));
				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) > rv2->asDouble(b));
				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) > 0);
				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) > rv2->asPointer(t[1],b) );
				else failed = TRUE;
				break;
			case (Command::OperatorGreaterThanEqualTo):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) >= rv2->asInteger(b));
				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) >= rv2->asDouble(b));
				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) >= 0);
				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) >= rv2->asPointer(t[1],b) );
				else failed = TRUE;
				break;
			case (Command::OperatorLessThan):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) < rv2->asInteger(b));
				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) < rv2->asDouble(b));
				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) < 0);
				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) < rv2->asPointer(t[1],b) );
				else failed = TRUE;
				break;
			case (Command::OperatorLessThanEqualTo):
				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) <= rv2->asInteger(b));
				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) <= rv2->asDouble(b));
				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) <= 0);
				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) <= rv2->asPointer(t[1],b) );
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
		ReturnValue *rv[2];
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
		if ((t[0] == VTypes::StringData) || (t[0] == VTypes::VectorData)) failed = TRUE;
		else if (t[0] >= VTypes::AtenData)
		{
			// We will allow (in)equality between pointer and integer but not real
			if (t[1] > VTypes::IntegerData) failed = TRUE;
			else switch (func)
			{
				case (Command::OperatorEqualTo):
					result.set( ((long int) rv[0]->asPointer(t[0],b)) == ((long int) rv[1]->asInteger(b)) );
					break;
				case (Command::OperatorNotEqualTo):
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
		msg.print("Error: the test '%s %s %s' does not return a valid result.\n", VTypes::dataType(rv1->type()), Command::data[func].keyword, VTypes::dataType(rv2->type()));
		return FALSE;
	}
	return TRUE;
}

// Add two quantities together
bool Command::function_OperatorAdd(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0,v1)) return FALSE;
	if (!c->arg(1,v2)) return FALSE;
	return operate(Command::OperatorAdd, &v1, &v2, rv);
}

// Logical AND check on two operators
bool Command::function_OperatorAnd(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0,v1)) return FALSE;
	if (!c->arg(1,v2)) return FALSE;
	rv.set(v1.asBool() && v2.asBool());
	return TRUE;
}

// Assignment
bool Command::function_OperatorAssignment(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab the second argument result and assign it to the first
	if (!c->arg(1, rv)) return FALSE;
	if (!c->setArg(0, rv)) return FALSE;
	return TRUE;
}

// Assignment Divide
bool Command::function_OperatorAssignmentDivide(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	if (!operate(Command::OperatorDivide, &v1, &v2, rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return (c->arg(0, rv));
}

// Assignment Minus
bool Command::function_OperatorAssignmentMinus(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	if (!operate(Command::OperatorSubtract, &v1, &v2, rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return (c->arg(0, rv));
}

// Assignment Plus
bool Command::function_OperatorAssignmentMultiply(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	if (!operate(Command::OperatorMultiply, &v1, &v2, rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return (c->arg(0, rv));
}

// Assignment Plus
bool Command::function_OperatorAssignmentPlus(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	if (!operate(Command::OperatorAdd, &v1, &v2, rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return (c->arg(0, rv));
}

// Divide one quantity by another
bool Command::function_OperatorDivide(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return operate(Command::OperatorDivide, &v1, &v2, rv);
}

// Equal To
bool Command::function_OperatorEqualTo(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(Command::OperatorEqualTo, &v1, &v2, rv);
}

// Greater Than
bool Command::function_OperatorGreaterThan(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(Command::OperatorGreaterThan, &v1, &v2, rv);
}

// Greater Than Equal To
bool Command::function_OperatorGreaterThanEqualTo(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(Command::OperatorGreaterThanEqualTo, &v1, &v2, rv);
}

// Less Than
bool Command::function_OperatorLessThan(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(Command::OperatorLessThan, &v1, &v2, rv);
}

// Less Than Equal To
bool Command::function_OperatorLessThanEqualTo(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(Command::OperatorLessThanEqualTo, &v1, &v2, rv);
}

// Multiply one quantity by another
bool Command::function_OperatorMultiply(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0,v1)) return FALSE;
	if (!c->arg(1,v2)) return FALSE;
	return operate(Command::OperatorMultiply, &v1, &v2, rv);
}

// Negate value
bool Command::function_OperatorNegate(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (!c->arg(0, rv)) return FALSE;
	switch (c->argType(0))
	{
		case (VTypes::IntegerData):
			rv.set(-rv.asInteger());
			break;
		case (VTypes::DoubleData):
			rv.set(-rv.asDouble());
			break;
		case (VTypes::VectorData):
			rv.set(-rv.asVector());
			break;
		default:
			msg.print("Can't negate %s.\n", VTypes::aDataType(c->argType(0)));
			return FALSE;
	}
	return TRUE;
}

// Not (Reverse Logic)
bool Command::function_OperatorNot(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab argument and 'negate' it
	ReturnValue v1;
	if (!c->arg(0, v1)) return FALSE;
	rv.set( !v1.asBool() );
	return TRUE;
}

// Not Equal To
bool Command::function_OperatorNotEqualTo(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return test(Command::OperatorNotEqualTo, &v1, &v2, rv);
}

// Logical OR check on two operators
bool Command::function_OperatorOr(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0,v1)) return FALSE;
	if (!c->arg(1,v2)) return FALSE;
	rv.set(v1.asBool() || v2.asBool());
	return TRUE;
}

// Postfix Decrease
bool Command::function_OperatorPostfixDecrease(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	ReturnValue newvalue;
	newvalue = rv;
	newvalue.decrease();
	return c->setArg(0, newvalue);
}

// Postfix Increase
bool Command::function_OperatorPostfixIncrease(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	ReturnValue newvalue;
	newvalue = rv;
	newvalue.increase();
	return c->setArg(0, newvalue);
}

// Prefix Decrease
bool Command::function_OperatorPrefixDecrease(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	rv.decrease();
	return c->setArg(0, rv);
}

// Prefix Increase
bool Command::function_OperatorPrefixIncrease(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	rv.increase();
	return c->setArg(0, rv);
}

// Raise one quantity to the power of another
bool Command::function_OperatorPower(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0, v1)) return FALSE;
	if (!c->arg(1, v2)) return FALSE;
	return operate(Command::OperatorPower, &v1, &v2, rv);
}

// Subtract one quantity from another
bool Command::function_OperatorSubtract(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0,v1)) return FALSE;
	if (!c->arg(1,v2)) return FALSE;
	return operate(Command::OperatorSubtract, &v1, &v2, rv);
}
