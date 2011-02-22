/*
	*** Operators
	*** src/command/operators.cpp
	Copyright T. Youngs 2007-2010

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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "base/mathfunc.h"
#include <stdio.h>
#include <string.h>

Dnchar s;

// Add two quantities together
bool Command::function_OperatorAdd(CommandNode *c, ReturnValue &rv)
{
	ReturnValue lhs, rhs;
	bool b = TRUE;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntAIntA): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asInteger(i,b) + rhs.asInteger(i,b)); rv = lhs; break;
		case (VTypes::IntADblA):
		case (VTypes::DblAIntA):
		case (VTypes::DblADblA): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asDouble(i,b) + rhs.asDouble(i,b)); rv = lhs; break;
		case (VTypes::IntAInt):
		case (VTypes::IntADbl): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asInteger(i,b) + rhs.asInteger(b)); rv = lhs; break;
		case (VTypes::DblAInt):
		case (VTypes::DblADbl): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asDouble(i,b) + rhs.asDouble(b)); rv = lhs; break;
// 		case (VTypes::IntIntA):
// 		case (VTypes::IntDblA): for (int i=0; i<rhs.arraySize(); ++i) rhs.setElement(i, rhs.asInteger(i,b) + lhs.asInteger(b)); rv = rhs; break;
// 		case (VTypes::DblIntA):
// 		case (VTypes::DblDblA): for (int i=0; i<rhs.arraySize(); ++i) rhs.setElement(i, rhs.asDouble(i,b) + lhs.asDouble(b)); rv = rhs; break;
		case (VTypes::IntInt): rv.set(lhs.asInteger(b) + rhs.asInteger(b)); break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): rv.set(lhs.asDouble(b) + rhs.asDouble(b)); break;
		case (VTypes::VecInt):
		case (VTypes::VecDbl): rv.set(lhs.asVector(b) + rhs.asDouble(b)); break;
		case (VTypes::VecVec): rv.set(lhs.asVector(b) + rhs.asVector(b)); break;
		case (VTypes::IntVec):
		case (VTypes::DblVec): rv.set(rhs.asVector(b) + lhs.asDouble(b)); break;
		case (VTypes::IntAVec):
		case (VTypes::DblAVec): if (lhs.arraySize() != 3) b = FALSE;
			else { Vec3<double> v(lhs.asDouble(0,b), lhs.asDouble(1,b), lhs.asDouble(2,b)); rv.set(v + rhs.asVector()); } break;
		case (VTypes::VecIntA):
		case (VTypes::VecDblA): if (rhs.arraySize() != 3) b = FALSE;
			else { Vec3<double> v(rhs.asDouble(0,b), rhs.asDouble(1,b), rhs.asDouble(2,b)); rv.set(v + lhs.asVector()); } break;
		default:
			printf("The operator '+' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	return b;
}

// Logical AND check on two operators
bool Command::function_OperatorAnd(CommandNode *c, ReturnValue &rv)
{
	ReturnValue v1, v2;
	if (!c->arg(0,v1)) return FALSE;
	if (!c->arg(1,v2)) return FALSE;
	rv.set(v1.asBool() && v2.asBool());
	return TRUE;
}

// Assignment
bool Command::function_OperatorAssignment(CommandNode *c, ReturnValue &rv)
{
	// Grab the second argument result and assign it to the first
	if (!c->arg(1, rv)) return FALSE;
	if (!c->setArg(0, rv)) return FALSE;
	return TRUE;
}

// Assignment Divide
bool Command::function_OperatorAssignmentDivide(CommandNode *c, ReturnValue &rv)
{
	if (!function_OperatorDivide(c,rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return TRUE;
}

// Assignment Multiply
bool Command::function_OperatorAssignmentMultiply(CommandNode *c, ReturnValue &rv)
{
	if (!function_OperatorMultiply(c,rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return TRUE;
}

// Assignment Plus
bool Command::function_OperatorAssignmentPlus(CommandNode *c, ReturnValue &rv)
{
	if (!function_OperatorAdd(c,rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return TRUE;
}

// Assignment Subtract
bool Command::function_OperatorAssignmentSubtract(CommandNode *c, ReturnValue &rv)
{
	if (!function_OperatorSubtract(c,rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return TRUE;
}

// Divide one quantity by another
bool Command::function_OperatorDivide(CommandNode *c, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue lhs, rhs;
	bool b = TRUE;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntAIntA): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asInteger(i,b) / rhs.asInteger(i,b)); rv = lhs; break;
		case (VTypes::IntADblA):
		case (VTypes::DblAIntA):
		case (VTypes::DblADblA): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asDouble(i,b) / rhs.asDouble(i,b)); rv = lhs; break;
		case (VTypes::IntAInt): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asInteger(i,b) / rhs.asInteger(b)); rv = lhs; break;
		case (VTypes::IntADbl): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, (int) (lhs.asInteger(i,b) / rhs.asDouble(b))); rv = lhs; break;
		case (VTypes::DblAInt):
		case (VTypes::DblADbl): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asDouble(i,b) / rhs.asDouble(b)); rv = lhs; break;
		case (VTypes::IntInt): rv.set(lhs.asInteger(b) / rhs.asInteger(b)); break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): rv.set(lhs.asDouble(b) / rhs.asDouble(b)); break;
		case (VTypes::VecInt):
		case (VTypes::VecDbl): rv.set(lhs.asVector(b) / rhs.asDouble(b)); break;
		case (VTypes::VecVec): rv.set(lhs.asVector(b) / rhs.asVector(b)); break;
		case (VTypes::IntAVec):
		case (VTypes::DblAVec): if (lhs.arraySize() != 3) b = FALSE;
			else { Vec3<double> v(lhs.asDouble(0,b), lhs.asDouble(1,b), lhs.asDouble(2,b)); rv.set(v / rhs.asVector()); } break;
		case (VTypes::VecIntA):
		case (VTypes::VecDblA): if (rhs.arraySize() != 3) b = FALSE;
			else { Vec3<double> v(rhs.asDouble(0,b), rhs.asDouble(1,b), rhs.asDouble(2,b)); rv.set(v / lhs.asVector()); } break;
		default:
			printf("The operator '/' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	return b;
}

// Equal To
bool Command::function_OperatorEqualTo(CommandNode *c, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue lhs, rhs;
	bool b = TRUE;
	int result = 1;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntAIntA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asInteger(i,b) != rhs.asInteger(i,b)) { result = 0; break; } break;
		case (VTypes::IntADblA):
		case (VTypes::DblAIntA):
		case (VTypes::DblADblA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asDouble(i,b) != rhs.asDouble(i,b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) != rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) != rhs.asDouble(b)) result = 0; break;
		default:
			printf("The operator '==' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
}

// Greater Than
bool Command::function_OperatorGreaterThan(CommandNode *c, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue lhs, rhs;
	bool b = TRUE;
	int result = 1;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntAIntA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asInteger(i,b) <= rhs.asInteger(i,b)) { result = 0; break; } break;
		case (VTypes::IntADblA):
		case (VTypes::DblAIntA):
		case (VTypes::DblADblA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asDouble(i,b) <= rhs.asDouble(i,b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) <= rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) <= rhs.asDouble(b)) result = 0; break;
		default:
			printf("The operator '>' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
}

// Greater Than Equal To
bool Command::function_OperatorGreaterThanEqualTo(CommandNode *c, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue lhs, rhs;
	bool b = TRUE;
	int result = 1;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntAIntA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asInteger(i,b) < rhs.asInteger(i,b)) { result = 0; break; } break;
		case (VTypes::IntADblA):
		case (VTypes::DblAIntA):
		case (VTypes::DblADblA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asDouble(i,b) < rhs.asDouble(i,b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) < rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) < rhs.asDouble(b)) result = 0; break;
		default:
			printf("The operator '>=' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
}

// Less Than
bool Command::function_OperatorLessThan(CommandNode *c, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue lhs, rhs;
	bool b = TRUE;
	int result = 1;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntAIntA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asInteger(i,b) >= rhs.asInteger(i,b)) { result = 0; break; } break;
		case (VTypes::IntADblA):
		case (VTypes::DblAIntA):
		case (VTypes::DblADblA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asDouble(i,b) >= rhs.asDouble(i,b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) >= rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) >= rhs.asDouble(b)) result = 0; break;
		default:
			printf("The operator '<' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
}

// Less Than Equal To
bool Command::function_OperatorLessThanEqualTo(CommandNode *c, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue lhs, rhs;
	bool b = TRUE;
	int result = 1;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntAIntA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asInteger(i,b) > rhs.asInteger(i,b)) { result = 0; break; } break;
		case (VTypes::IntADblA):
		case (VTypes::DblAIntA):
		case (VTypes::DblADblA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asDouble(i,b) > rhs.asDouble(i,b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) > rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) > rhs.asDouble(b)) result = 0; break;
		default:
			printf("The operator '<=' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
}

// Integer remainder of A/B
bool Command::function_OperatorModulus(CommandNode *c, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue lhs, rhs;
	bool b = TRUE;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntInt): rv.set(lhs.asInteger(b) % rhs.asInteger(b)); break;
		default:
			printf("The operator '/' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	return b;
}

// Multiply one quantity by another
bool Command::function_OperatorMultiply(CommandNode *c, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue lhs, rhs;
	bool b = TRUE;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntAIntA): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asInteger(i,b) * rhs.asInteger(i,b)); rv = lhs; break;
		case (VTypes::IntADblA):
		case (VTypes::DblAIntA):
		case (VTypes::DblADblA): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asDouble(i,b) * rhs.asDouble(i,b)); rv = lhs; break;
		case (VTypes::IntAInt):
		case (VTypes::IntADbl): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asInteger(i,b) * rhs.asInteger(b)); rv = lhs; break;
		case (VTypes::DblAInt):
		case (VTypes::DblADbl): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asDouble(i,b) * rhs.asDouble(b)); rv = lhs; break;
		case (VTypes::IntIntA):
		case (VTypes::IntDblA): for (int i=0; i<rhs.arraySize(); ++i) rhs.setElement(i, rhs.asInteger(i,b) * lhs.asInteger(b)); rv = rhs; break;
		case (VTypes::DblIntA):
		case (VTypes::DblDblA): for (int i=0; i<rhs.arraySize(); ++i) rhs.setElement(i, rhs.asDouble(i,b) * lhs.asDouble(b)); rv = rhs; break;
		case (VTypes::IntInt): rv.set(lhs.asInteger(b) * rhs.asInteger(b)); break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): rv.set(lhs.asDouble(b) * rhs.asDouble(b)); break;
		case (VTypes::VecInt):
		case (VTypes::VecDbl): rv.set(lhs.asVector(b) * rhs.asDouble(b)); break;
		case (VTypes::VecVec): rv.set(lhs.asVector(b) * rhs.asVector(b)); break;
		case (VTypes::IntVec):
		case (VTypes::DblVec): rv.set(rhs.asVector(b) * lhs.asDouble(b)); break;
		case (VTypes::IntAVec):
		case (VTypes::DblAVec): if (lhs.arraySize() != 3) b = FALSE;
			else { Vec3<double> v(lhs.asDouble(0,b), lhs.asDouble(1,b), lhs.asDouble(2,b)); rv.set(v * rhs.asVector()); } break;
		case (VTypes::VecIntA):
		case (VTypes::VecDblA): if (rhs.arraySize() != 3) b = FALSE;
			else { Vec3<double> v(rhs.asDouble(0,b), rhs.asDouble(1,b), rhs.asDouble(2,b)); rv.set(v * lhs.asVector()); } break;
		default:
			printf("The operator '*' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	return b;
}

// Negate value
bool Command::function_OperatorNegate(CommandNode *c, ReturnValue &rv)
{
	if (!c->arg(0, rv)) return FALSE;
	int i;
	switch (c->argType(0))
	{
		case (VTypes::IntegerData):
			if (rv.arraySize() == -1) rv.set(-rv.asInteger());
			else
			{
				int *array = (int*) rv.asPointer(VTypes::IntegerData);
				for (i=0; i<rv.arraySize(); ++i) array[i] = -array[i];
			}
			break;
		case (VTypes::DoubleData):
			if (rv.arraySize() == -1) rv.set(-rv.asDouble());
			else
			{
				double *array = (double*) rv.asPointer(VTypes::DoubleData);
				for (i=0; i<rv.arraySize(); ++i) array[i] = -array[i];
			}
			break;
		case (VTypes::VectorData):
			rv.set(-rv.asVector());
			break;
		default:
			printf("Can't negate %s.\n", VTypes::aDataType(c->argType(0)));
			return FALSE;
	}
	return TRUE;
}

// Not (Reverse Logic)
bool Command::function_OperatorNot(CommandNode *c, ReturnValue &rv)
{
	// Grab argument and 'negate' it
	ReturnValue v1;
	if (!c->arg(0, v1)) return FALSE;
	rv.set( !v1.asBool() );
	return TRUE;
}

// Not Equal To
bool Command::function_OperatorNotEqualTo(CommandNode *c, ReturnValue &rv)
{
	ReturnValue lhs, rhs;
	bool b = TRUE;
	int result = 1;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntAIntA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asInteger(i,b) == rhs.asInteger(i,b)) { result = 0; break; } break;
		case (VTypes::IntADblA):
		case (VTypes::DblAIntA):
		case (VTypes::DblADblA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asDouble(i,b) == rhs.asDouble(i,b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) == rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) == rhs.asDouble(b)) result = 0; break;
		default:
			printf("The operator '!=' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
}

// Logical OR check on two operators
bool Command::function_OperatorOr(CommandNode *c, ReturnValue &rv)
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue v1, v2;
	if (!c->arg(0,v1)) return FALSE;
	if (!c->arg(1,v2)) return FALSE;
	rv.set(v1.asBool() || v2.asBool());
	return TRUE;
}

// Postfix Decrease
bool Command::function_OperatorPostfixDecrease(CommandNode *c, ReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	ReturnValue newvalue;
	newvalue = rv;
	newvalue.decrease();
	return c->setArg(0, newvalue);
}

// Postfix Increase
bool Command::function_OperatorPostfixIncrease(CommandNode *c, ReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	ReturnValue newvalue;
	newvalue = rv;
	newvalue.increase();
	return c->setArg(0, newvalue);
}

// Prefix Decrease
bool Command::function_OperatorPrefixDecrease(CommandNode *c, ReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	rv.decrease();
	return c->setArg(0, rv);
}

// Prefix Increase
bool Command::function_OperatorPrefixIncrease(CommandNode *c, ReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	rv.increase();
	return c->setArg(0, rv);
}

// Raise one quantity to the power of another
bool Command::function_OperatorPower(CommandNode *c, ReturnValue &rv)
{
	ReturnValue lhs, rhs;
	bool b = TRUE;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntInt): rv.set(power(lhs.asInteger(b),rhs.asInteger(b))); break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): rv.set(pow(lhs.asDouble(b),rhs.asDouble(b))); break;
		default:
			printf("The operator '^' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	return b;
}

// Subtract one quantity from another
bool Command::function_OperatorSubtract(CommandNode *c, ReturnValue &rv)
{
{
	// Grab both argument (return) values and send them to be operated on
	ReturnValue lhs, rhs;
	bool b = TRUE;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntAIntA): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asInteger(i,b) - rhs.asInteger(i,b)); rv = lhs; break;
		case (VTypes::IntADblA):
		case (VTypes::DblAIntA):
		case (VTypes::DblADblA): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asDouble(i,b) - rhs.asDouble(i,b)); rv = lhs; break;
		case (VTypes::IntAInt):
		case (VTypes::IntADbl): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asInteger(i,b) - rhs.asInteger(b)); rv = lhs; break;
		case (VTypes::DblAInt):
		case (VTypes::DblADbl): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asDouble(i,b) - rhs.asDouble(b)); rv = lhs; break;
// 		case (VTypes::IntIntA):
// 		case (VTypes::IntDblA): for (int i=0; i<rhs.arraySize(); ++i) rhs.setElement(i, rhs.asInteger(i,b) - lhs.asInteger(b)); rv = rhs; break;
// 		case (VTypes::DblIntA):
// 		case (VTypes::DblDblA): for (int i=0; i<rhs.arraySize(); ++i) rhs.setElement(i, rhs.asDouble(i,b) - lhs.asDouble(b)); rv = rhs; break;
		case (VTypes::IntInt): rv.set(lhs.asInteger(b) - rhs.asInteger(b)); break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): rv.set(lhs.asDouble(b) - rhs.asDouble(b)); break;
		case (VTypes::VecInt):
		case (VTypes::VecDbl): rv.set(lhs.asVector(b) - rhs.asDouble(b)); break;
		case (VTypes::VecVec): rv.set(lhs.asVector(b) - rhs.asVector(b)); break;
		case (VTypes::IntVec):
		case (VTypes::DblVec): rv.set(rhs.asVector(b) - lhs.asDouble(b)); break;
		case (VTypes::IntAVec):
		case (VTypes::DblAVec): if (lhs.arraySize() != 3) b = FALSE;
			else { Vec3<double> v(lhs.asDouble(0,b), lhs.asDouble(1,b), lhs.asDouble(2,b)); rv.set(v - rhs.asVector()); } break;
		case (VTypes::VecIntA):
		case (VTypes::VecDblA): if (rhs.arraySize() != 3) b = FALSE;
			else { Vec3<double> v(rhs.asDouble(0,b), rhs.asDouble(1,b), rhs.asDouble(2,b)); rv.set(v - lhs.asVector()); } break;
		default:
			printf("The operator '-' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	return b;
}
}
