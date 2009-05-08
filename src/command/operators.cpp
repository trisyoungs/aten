/*
	*** Operators
	*** src/command/operators.cpp
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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "base/mathfunc.h"
#include <stdio.h>
#include <string.h>

char s[8096];

// // Operate!
// bool operate(Command::Function func, ReturnValue *rv1, ReturnValue *rv2, ReturnValue &result)
// {
// 	static char s[8096];
// 	s[0] = '\0';
// 	bool failed = FALSE, b;
// 	// Array or returnvalue structures
// 	ReturnValue *rv[2];
// 	VTypes::DataType t[2];
// 	rv[0] = rv1;
// 	rv[1] = rv2;
// 	// Swap values over if the type of rv2 is 'greater than' rv1, and store data type values
// 	// The first value rv[0] (and t[0]) will always contain the 'highest' type
// 	if (rv[1]->type() > rv[0]->type())
// 	{
// 		rv[1] = rv1;
// 		rv[0] = rv2;
// 	}
// 	t[0] = rv[0]->type();
// 	t[1] = rv[1]->type();
// 	// Check for no data type or pointer type
// 	if (t[0] == VTypes::NoData)
// 	{
// 		msg.print("Error: LHS of operator %s has no data type.\n", Command::data[func].keyword);
// 		return FALSE;
// 	}
// 	if (t[1] == VTypes::NoData)
// 	{
// 		msg.print("Error: RHS of operator %s has no data type.\n", Command::data[func].keyword);
// 		return FALSE;
// 	}
// 	if (VTypes::isPointer(t[0]))
// 	{
// 		msg.print("Error: LHS of operator %s is a pointer.\n", Command::data[func].keyword);
// 		return FALSE;
// 	}
// 	if (VTypes::isPointer(t[1]))
// 	{
// 		msg.print("Error: RHS of operator %s is a pointer.\n", Command::data[func].keyword);
// 		return FALSE;
// 	}
// 	// We will decide what to do based on data types of integer, real, character, or vector
// 	if (rv1->type() == rv2->type())
// 	{
// 		// Array-array operators (Fortran style)
// 		if ((rv1->arraySize() == rv2->arraySize()) && (rv1->arraySize() > 0))
// 		{
// 			if (rv1->type() == VTypes::IntegerData)
// 			{
// 				int *array1 = ((int*) rv1->asPointer(VTypes::IntegerData));
// 				int *array2 = ((int*) rv2->asPointer(VTypes::IntegerData)), n;
// 				switch (func)
// 				{
// 					case (Command::OperatorAdd):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] += array2[n];
// 						break;
// 					case (Command::OperatorSubtract):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] -= array2[n];
// 						break;
// 					case (Command::OperatorMultiply):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] *= array2[n];
// 						break;
// 					case (Command::OperatorDivide):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] /= array2[n];
// 						break;
// 				}
// 				result.set(VTypes::IntegerData, array1, rv1->arraySize());
// 			}
// 			else if (rv1->type() == VTypes::DoubleData)
// 			{
// 				double *array1 = ((double*) rv1->asPointer(VTypes::DoubleData));
// 				double *array2 = ((double*) rv2->asPointer(VTypes::DoubleData));
// 				int n;
// 				switch (func)
// 				{
// 					case (Command::OperatorAdd):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] += array2[n];
// 						break;
// 					case (Command::OperatorSubtract):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] -= array2[n];
// 						break;
// 					case (Command::OperatorMultiply):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] *= array2[n];
// 						break;
// 					case (Command::OperatorDivide):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] /= array2[n];
// 						break;
// 				}
// 				result.set(VTypes::DoubleData, array1, rv1->arraySize());
// 			}
// 			else failed = TRUE;
// 		}
// 		else if ((rv1->arraySize() > 0) && (rv2->arraySize() == -1))
// 		{
// 			if (rv1->type() == VTypes::IntegerData)
// 			{
// 				int *array1 = ((int*) rv1->asPointer(VTypes::IntegerData));
// 				int n;
// 				double d = rv2->asDouble();
// 				switch (func)
// 				{
// 					case (Command::OperatorAdd):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] += d;
// 						break;
// 					case (Command::OperatorSubtract):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] -= d;
// 						break;
// 					case (Command::OperatorMultiply):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] *= d;
// 						break;
// 					case (Command::OperatorDivide):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] /= d;
// 						break;
// 				}
// 				result.set(VTypes::IntegerData, array1, rv1->arraySize());
// 			}
// 			else if (rv1->type() == VTypes::DoubleData)
// 			{
// 				double *array1 = ((double*) rv1->asPointer(VTypes::DoubleData)), d = rv2->asDouble();
// 				int n;
// 				switch (func)
// 				{
// 					case (Command::OperatorAdd):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] += d;
// 						break;
// 					case (Command::OperatorSubtract):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] -= d;
// 						break;
// 					case (Command::OperatorMultiply):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] *= d;
// 						break;
// 					case (Command::OperatorDivide):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] /= d;
// 						break;
// 				}
// 				result.set(VTypes::DoubleData, array1, rv1->arraySize());
// 			}
// 			else failed = TRUE;
// 		}
// 		else if ((rv1->arraySize() > 0) && (rv2->arraySize() > 0))
// 		{
// 			msg.print("Array sizes do not conform.\n");
// 			failed = TRUE;
// 		}
// 		else switch (func)
// 		{
// 			case (Command::OperatorAdd):
// 				if (rv1->type() == VTypes::IntegerData) result.set(rv1->asInteger(b) + rv2->asInteger(b));
// 				else if (rv1->type() == VTypes::DoubleData) result.set(rv1->asDouble(b) + rv2->asDouble(b));
// 				else if (rv1->type() == VTypes::StringData)
// 				{
// 					strcpy(s, rv1->asString(b));
// 					strcat(s, rv2->asString(b));
// 					result.set(s);
// 				}
// 				else if (rv1->type() == VTypes::VectorData) result.set(rv1->asVector(b) + rv2->asVector(b));
// 				else failed = TRUE;
// 				break;
// 			case (Command::OperatorSubtract):
// 				if (rv1->type() == VTypes::IntegerData) result.set(rv1->asInteger(b) - rv2->asInteger(b));
// 				else if (rv1->type() == VTypes::DoubleData) result.set(rv1->asDouble(b) - rv2->asDouble(b));
// 				else if (rv1->type() == VTypes::VectorData) result.set(rv1->asVector(b) - rv2->asVector(b));
// 				else failed = TRUE;
// 				break;
// 			case (Command::OperatorMultiply):
// 				if (rv1->type() == VTypes::IntegerData) result.set(rv1->asInteger(b) * rv2->asInteger(b));
// 				else if (rv1->type() == VTypes::DoubleData) result.set(rv1->asDouble(b) * rv2->asDouble(b));
// 				else if (rv1->type() == VTypes::VectorData) result.set(rv1->asVector(b) * rv2->asVector(b));
// 				else failed = TRUE;
// 				break;
// 			case (Command::OperatorDivide):
// 				if (rv1->type() == VTypes::IntegerData) result.set(rv1->asInteger(b) / rv2->asInteger(b));
// 				else if (rv1->type() == VTypes::DoubleData) result.set(rv1->asDouble(b) / rv2->asDouble(b));
// 				else if (rv1->type() == VTypes::VectorData) result.set(rv1->asVector(b) / rv2->asVector(b));
// 				else failed = TRUE;
// 				break;
// 			case (Command::OperatorPower):
// 				if (rv1->type() == VTypes::IntegerData) result.set(power(rv1->asInteger(b), rv2->asInteger(b)));
// 				else if (rv1->type() == VTypes::DoubleData) result.set(pow(rv1->asDouble(b), rv2->asDouble(b)));
// 				else failed = TRUE;
// 				break;
// 			default:
// 				failed = TRUE;
// 				break;
// 		}
// 	}
// 	else if (t[0] == VTypes::VectorData)
// 	{
// 		if (t[1] == VTypes::StringData) failed = TRUE;
// 		else if (rv[1]->arraySize() == -1) switch (func)
// 		{
// 			case (Command::OperatorAdd):
// 				result.set( rv[0]->asVector(b) + rv[1]->asDouble(b));
// 				break;
// 			case (Command::OperatorSubtract):
// 				result.set( rv[0]->asVector(b) - rv[1]->asDouble(b));
// 				break;
// 			case (Command::OperatorMultiply):
// 				result.set( rv[0]->asVector(b) * rv[1]->asDouble(b));
// 				break;
// 			case (Command::OperatorDivide):
// 				if (rv1->type() == VTypes::VectorData) result.set( rv[0]->asVector(b) / rv[1]->asDouble(b));
// 				else failed = TRUE;
// 				break;
// 			case (Command::OperatorPower):
// 				failed = TRUE;
// 				break;
// 		}
// 		else if (rv[1]->arraySize() == 3)
// 		{
// 			Vec3<double> v1 = rv1->asVector(), v2 = rv2->asVector();
// 			switch (func)
// 			{
// 				case (Command::OperatorAdd):
// 					result.set(v1 + v2);
// 					break;
// 				case (Command::OperatorSubtract):
// 					result.set(v1 - v2);
// 					break;
// 				case (Command::OperatorMultiply):
// 					result.set(v1 * v2);
// 					break;
// 				case (Command::OperatorDivide):
// 					result.set(v1 / v2);
// 					break;
// 				case (Command::OperatorPower):
// 					failed = TRUE;
// 					break;
// 			}
// 		}
// 		else
// 		{
// 			msg.print("RHS of expression '%s %s %s' must be either a single value or an array of size 3.\n", VTypes::dataType(rv1->type()), Command::data[func].keyword, VTypes::dataType(rv2->type()));
// 			failed = TRUE;
// 		}
// 	}
// 	else if (t[0] == VTypes::StringData)
// 	{
// 		// There are limited operations that we can do with character strings....
// 		switch (func)
// 		{
// 			// Multiply string by the (integer) number specified
// 			case (Command::OperatorMultiply):
// 				if ((t[1] == VTypes::IntegerData) || (t[1] == VTypes::DoubleData))
// 				{
// 					s[0] = '\0';
// 					for (int n=0; n<rv[1]->asInteger(b); n++) strcat(s, rv[0]->asString(b));
// 					result.set(s);
// 				}
// 				else failed = TRUE;
// 				break;
// 			default:
// 				failed = TRUE;
// 				break;
// 		}
// 	}
// 	else
// 	{
// 		// Both values are numbers - one is an integer, and one is a double
// 		// Array-array operators (Fortran style)
// 		if ((rv1->arraySize() == rv2->arraySize()) && (rv1->arraySize() > 0))
// 		{
// 			if (rv1->type() == VTypes::IntegerData)
// 			{
// 				int *array1 = ((int*) rv1->asPointer(VTypes::IntegerData)), n;
// 				double *array2 = ((double*) rv2->asPointer(VTypes::DoubleData));
// 				switch (func)
// 				{
// 					case (Command::OperatorAdd):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] += array2[n];
// 						break;
// 					case (Command::OperatorSubtract):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] -= array2[n];
// 						break;
// 					case (Command::OperatorMultiply):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] *= array2[n];
// 						break;
// 					case (Command::OperatorDivide):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] /= array2[n];
// 						break;
// 				}
// 				result.set(VTypes::IntegerData, array1, rv1->arraySize());
// 			}
// 			else
// 			{
// 				double *array1 = ((double*) rv1->asPointer(VTypes::DoubleData));
// 				int *array2 = ((int*) rv2->asPointer(VTypes::IntegerData));
// 				int n;
// 				switch (func)
// 				{
// 					case (Command::OperatorAdd):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] += array2[n];
// 						break;
// 					case (Command::OperatorSubtract):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] -= array2[n];
// 						break;
// 					case (Command::OperatorMultiply):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] *= array2[n];
// 						break;
// 					case (Command::OperatorDivide):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] /= array2[n];
// 						break;
// 				}
// 				result.set(VTypes::DoubleData, array1, rv1->arraySize());
// 			}
// 		}
// 		else if ((rv1->arraySize() > 0) && (rv2->arraySize() == -1))
// 		{
// 			if (rv1->type() == VTypes::IntegerData)
// 			{
// 				int *array1 = ((int*) rv1->asPointer(VTypes::IntegerData));
// 				int n;
// 				double d = rv2->asDouble();
// 				switch (func)
// 				{
// 					case (Command::OperatorAdd):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] += d;
// 						break;
// 					case (Command::OperatorSubtract):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] -= d;
// 						break;
// 					case (Command::OperatorMultiply):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] *= d;
// 						break;
// 					case (Command::OperatorDivide):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] /= d;
// 						break;
// 				}
// 				result.set(VTypes::IntegerData, array1, rv1->arraySize());
// 			}
// 			else if (rv1->type() == VTypes::DoubleData)
// 			{
// 				double *array1 = ((double*) rv1->asPointer(VTypes::DoubleData)), d = rv2->asDouble();
// 				int n;
// 				switch (func)
// 				{
// 					case (Command::OperatorAdd):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] += d;
// 						break;
// 					case (Command::OperatorSubtract):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] -= d;
// 						break;
// 					case (Command::OperatorMultiply):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] *= d;
// 						break;
// 					case (Command::OperatorDivide):
// 						for (n = 0; n<rv1->arraySize(); ++n) array1[n] /= d;
// 						break;
// 				}
// 				result.set(VTypes::DoubleData, array1, rv1->arraySize());
// 			}
// 			else failed = TRUE;
// 		}
// 		else if ((rv1->arraySize() > 0) && (rv2->arraySize() > 0))
// 		{
// 			msg.print("Array sizes do not conform.\n");
// 			failed = TRUE;
// 		}
// 		else switch (func)
// 		{
// 			case (Command::OperatorAdd):
// 				result.set(rv1->asDouble(b) + rv2->asDouble(b));
// 				break;
// 			case (Command::OperatorSubtract):
// 				result.set(rv1->asDouble(b) - rv2->asDouble(b));
// 				break;
// 			case (Command::OperatorMultiply):
// 				result.set(rv1->asDouble(b) * rv2->asDouble(b));
// 				break;
// 			case (Command::OperatorDivide):
// 				result.set(rv1->asDouble(b) / rv2->asDouble(b));
// 				break;
// 			case (Command::OperatorPower):
// 				result.set(pow(rv1->asDouble(b), rv2->asDouble(b)));
// 				break;
// 		}
// 	}
// 	if (failed)
// 	{
// 		msg.print("Error: the expression '%s %s %s' does not return a meaningful result.\n", VTypes::dataType(rv1->type()), Command::data[func].keyword, VTypes::dataType(rv2->type()));
// 		return FALSE;
// 	}
// 	return TRUE;
// }
// 
// // Test!
// bool test(Command::Function func, ReturnValue *rv1, ReturnValue *rv2, ReturnValue &result)
// {
// 	// Grab data types of operands
// 	VTypes::DataType t[2];
// 	t[0] = rv1->type();
// 	t[1] = rv2->type();
// 	bool failed = FALSE;
// 	bool b;
// 	if (t[0] == VTypes::NoData)
// 	{
// 		msg.print("Error: LHS of operator %s has no data type.\n", Command::data[func].keyword);
// 		return FALSE;
// 	}
// 	if (t[1] == VTypes::NoData)
// 	{
// 		msg.print("Error: RHS of operator %s has no data type.\n", Command::data[func].keyword);
// 		return FALSE;
// 	}
// 	// Provided the types are the same (or a mix of real/int) we're okay....
// 	if ((t[0] < VTypes::StringData) && (t[1] < VTypes::StringData) && (t[0] != t[1])) t[0] = t[1] = VTypes::DoubleData;
// 	if (t[0] == t[1])
// 	{
// 		switch (func)
// 		{
// 			case (Command::OperatorEqualTo):
// 				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) == rv2->asInteger(b));
// 				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) == rv2->asDouble(b));
// 				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) == 0);
// 				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) == rv2->asPointer(t[1],b) );
// 				else failed = TRUE;
// 				break;
// 			case (Command::OperatorNotEqualTo):
// 				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) != rv2->asInteger(b));
// 				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) != rv2->asDouble(b));
// 				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) != 0);
// 				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) != rv2->asPointer(t[1],b) );
// 				else failed = TRUE;
// 				break;
// 			case (Command::OperatorGreaterThan):
// 				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) > rv2->asInteger(b));
// 				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) > rv2->asDouble(b));
// 				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) > 0);
// 				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) > rv2->asPointer(t[1],b) );
// 				else failed = TRUE;
// 				break;
// 			case (Command::OperatorGreaterThanEqualTo):
// 				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) >= rv2->asInteger(b));
// 				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) >= rv2->asDouble(b));
// 				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) >= 0);
// 				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) >= rv2->asPointer(t[1],b) );
// 				else failed = TRUE;
// 				break;
// 			case (Command::OperatorLessThan):
// 				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) < rv2->asInteger(b));
// 				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) < rv2->asDouble(b));
// 				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) < 0);
// 				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) < rv2->asPointer(t[1],b) );
// 				else failed = TRUE;
// 				break;
// 			case (Command::OperatorLessThanEqualTo):
// 				if (t[0] == VTypes::IntegerData) result.set(rv1->asInteger(b) <= rv2->asInteger(b));
// 				else if (t[0] == VTypes::DoubleData) result.set(rv1->asDouble(b) <= rv2->asDouble(b));
// 				else if (t[0] == VTypes::StringData) result.set( strcmp(rv1->asString(b), rv2->asString(b)) <= 0);
// 				else if (t[0] >= VTypes::AtenData) result.set( rv1->asPointer(t[0],b) <= rv2->asPointer(t[1],b) );
// 				else failed = TRUE;
// 				break;
// 			default:
// 				failed = TRUE;
// 				break;
// 		}
// 	}
// 	else 
// 	{
// 		// Get 'highest' type of the two operands
// 		ReturnValue *rv[2];
// 		// Swap values over if the type of rv2 is 'greater than' rv1, and store data type values
// 		// The first value rv[0] (and t[0]) will always contain the 'highest' type
// 		if (t[1] > t[0])
// 		{
// 			rv[0] = rv2;
// 			rv[1] = rv1;
// 			t[0] = rv[0]->type();
// 			t[1] = rv[1]->type();
// 		}
// 		else
// 		{
// 			rv[0] = rv1;
// 			rv[1] = rv2;
// 		}
// 		// There are no tests we can do between a character or a vector and another type
// 		if ((t[0] == VTypes::StringData) || (t[0] == VTypes::VectorData)) failed = TRUE;
// 		else if (t[0] >= VTypes::AtenData)
// 		{
// 			// We will allow (in)equality between pointer and integer but not real
// 			if (t[1] > VTypes::IntegerData) failed = TRUE;
// 			else switch (func)
// 			{
// 				case (Command::OperatorEqualTo):
// 					result.set( ((long int) rv[0]->asPointer(t[0],b)) == ((long int) rv[1]->asInteger(b)) );
// 					break;
// 				case (Command::OperatorNotEqualTo):
// 					result.set( ((long int) rv[0]->asPointer(t[0],b)) != ((long int) rv[1]->asInteger(b)) );
// 					break;
// 				default:
// 					failed = TRUE;
// 					break;
// 			}
// 		}
// 	}
// 	if (failed)
// 	{
// 		msg.print("Error: the test '%s %s %s' does not return a valid result.\n", VTypes::dataType(rv1->type()), Command::data[func].keyword, VTypes::dataType(rv2->type()));
// 		return FALSE;
// 	}
// 	return TRUE;
// }

// Add two quantities together
bool Command::function_OperatorAdd(CommandNode *c, Bundle &obj, ReturnValue &rv)
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
		case (VTypes::StrAStrA): for (int i=0; i<lhs.arraySize(); ++i) { strcpy(s,lhs.asString(i,b)); strcat(s,rhs.asString(i,b)); lhs.setElement(i,s); } rv = lhs; break;
		case (VTypes::IntAInt):
		case (VTypes::IntADbl): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asInteger(i,b) + rhs.asInteger(b)); rv = lhs; break;
		case (VTypes::DblAInt):
		case (VTypes::DblADbl): for (int i=0; i<lhs.arraySize(); ++i) lhs.setElement(i, lhs.asDouble(i,b) + rhs.asDouble(b)); rv = lhs; break;
		case (VTypes::IntIntA):
		case (VTypes::IntDblA): for (int i=0; i<rhs.arraySize(); ++i) rhs.setElement(i, rhs.asInteger(i,b) + lhs.asInteger(b)); rv = rhs; break;
		case (VTypes::DblIntA):
		case (VTypes::DblDblA): for (int i=0; i<rhs.arraySize(); ++i) rhs.setElement(i, rhs.asDouble(i,b) + lhs.asDouble(b)); rv = rhs; break;
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
		case (VTypes::StrStr): strcpy(s,lhs.asString(b)); strcat(s,rhs.asString(b)); rv.set(s); break;
		default:
			msg.print("The operator '+' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	return b;
}

// Logical AND check on two operators
bool Command::function_OperatorAnd(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	ReturnValue v1, v2;
	if (!c->arg(0,v1)) return FALSE;
	if (!c->arg(1,v2)) return FALSE;
	rv.set(v1.asBool() && v2.asBool());
	// TGAY
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
	if (!function_OperatorDivide(c,obj,rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return TRUE;
}

// Assignment Multiply
bool Command::function_OperatorAssignmentMultiply(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (!function_OperatorMultiply(c,obj,rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return TRUE;
}

// Assignment Plus
bool Command::function_OperatorAssignmentPlus(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (!function_OperatorAdd(c,obj,rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return TRUE;
}

// Assignment Subtract
bool Command::function_OperatorAssignmentSubtract(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (!function_OperatorSubtract(c,obj,rv)) return FALSE;
	// Now, set the first argument to our return value
	if (!c->setArg(0, rv)) return FALSE;
	return TRUE;
}

// Divide one quantity by another
bool Command::function_OperatorDivide(CommandNode *c, Bundle &obj, ReturnValue &rv)
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
			msg.print("The operator '/' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	return b;
}

// Equal To
bool Command::function_OperatorEqualTo(CommandNode *c, Bundle &obj, ReturnValue &rv)
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
		case (VTypes::StrAStrA): for (int i=0; i<lhs.arraySize(); ++i) if (strcmp(lhs.asString(i,b), rhs.asString(i,b)) != 0) { result = 0; break; } break;
		case (VTypes::PtrAPtrA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asPointer(i,lhs.type(),b) != rhs.asPointer(i,rhs.type(),b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) != rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntPtr): if ( ((long int) lhs.asInteger(b)) != ((long int) rhs.asPointer(lhs.type(),b))) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) != rhs.asDouble(b)) result = 0; break;
		case (VTypes::StrStr): if (strcmp(lhs.asString(b), rhs.asString(b)) != 0) result = 0; break;
		case (VTypes::PtrPtr): if (lhs.asPointer(lhs.type(),b) != rhs.asPointer(rhs.type(),b)) result = 0; break;
		case (VTypes::PtrInt): if ( ((long int) lhs.asPointer(lhs.type(),b)) != ((long int) rhs.asInteger(b))) result = 0; break;
		default:
			msg.print("The operator '==' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
}

// Greater Than
bool Command::function_OperatorGreaterThan(CommandNode *c, Bundle &obj, ReturnValue &rv)
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
		case (VTypes::StrAStrA): for (int i=0; i<lhs.arraySize(); ++i) if (strcmp(lhs.asString(i,b), rhs.asString(i,b)) <= 0) { result = 0; break; } break;
		case (VTypes::PtrAPtrA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asPointer(i,lhs.type(),b) <= rhs.asPointer(i,rhs.type(),b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) <= rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) <= rhs.asDouble(b)) result = 0; break;
		case (VTypes::StrStr): if (strcmp(lhs.asString(b), rhs.asString(b)) <= 0) result = 0; break;
		case (VTypes::PtrPtr): if (lhs.asPointer(lhs.type(),b) <= rhs.asPointer(rhs.type(),b)) result = 0; break;
		default:
			msg.print("The operator '>' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
}

// Greater Than Equal To
bool Command::function_OperatorGreaterThanEqualTo(CommandNode *c, Bundle &obj, ReturnValue &rv)
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
		case (VTypes::StrAStrA): for (int i=0; i<lhs.arraySize(); ++i) if (strcmp(lhs.asString(i,b), rhs.asString(i,b)) < 0) { result = 0; break; } break;
		case (VTypes::PtrAPtrA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asPointer(i,lhs.type(),b) < rhs.asPointer(i,rhs.type(),b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) < rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) < rhs.asDouble(b)) result = 0; break;
		case (VTypes::StrStr): if (strcmp(lhs.asString(b), rhs.asString(b)) < 0) result = 0; break;
		case (VTypes::PtrPtr): if (lhs.asPointer(lhs.type(),b) < rhs.asPointer(rhs.type(),b)) result = 0; break;
		default:
			msg.print("The operator '>=' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
}

// Less Than
bool Command::function_OperatorLessThan(CommandNode *c, Bundle &obj, ReturnValue &rv)
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
		case (VTypes::StrAStrA): for (int i=0; i<lhs.arraySize(); ++i) if (strcmp(lhs.asString(i,b), rhs.asString(i,b)) >= 0) { result = 0; break; } break;
		case (VTypes::PtrAPtrA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asPointer(i,lhs.type(),b) >= rhs.asPointer(i,rhs.type(),b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) >= rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) >= rhs.asDouble(b)) result = 0; break;
		case (VTypes::StrStr): if (strcmp(lhs.asString(b), rhs.asString(b)) >= 0) result = 0; break;
		case (VTypes::PtrPtr): if (lhs.asPointer(lhs.type(),b) >= rhs.asPointer(rhs.type(),b)) result = 0; break;
		default:
			msg.print("The operator '<' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
}

// Less Than Equal To
bool Command::function_OperatorLessThanEqualTo(CommandNode *c, Bundle &obj, ReturnValue &rv)
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
		case (VTypes::StrAStrA): for (int i=0; i<lhs.arraySize(); ++i) if (strcmp(lhs.asString(i,b), rhs.asString(i,b)) > 0) { result = 0; break; } break;
		case (VTypes::PtrAPtrA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asPointer(i,lhs.type(),b) > rhs.asPointer(i,rhs.type(),b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) > rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) > rhs.asDouble(b)) result = 0; break;
		case (VTypes::StrStr): if (strcmp(lhs.asString(b), rhs.asString(b)) > 0) result = 0; break;
		case (VTypes::PtrPtr): if (lhs.asPointer(lhs.type(),b) > rhs.asPointer(rhs.type(),b)) result = 0; break;
		default:
			msg.print("The operator '<=' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
}

// Multiply one quantity by another
bool Command::function_OperatorMultiply(CommandNode *c, Bundle &obj, ReturnValue &rv)
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
// 		case (VTypes::StrAStrA): for (int i=0; i<lhs.arraySize(); ++i) { strcpy(s,lhs.asString(i,b)); strcat(s,rhs.asString(i,b)); lhs.setElement(i,s); } rv = lhs; breakkkkkk; TGAY
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
			msg.print("The operator '*' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	return b;
}

// Negate value
bool Command::function_OperatorNegate(CommandNode *c, Bundle &obj, ReturnValue &rv)
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
		case (VTypes::StrAStrA): for (int i=0; i<lhs.arraySize(); ++i) if (strcmp(lhs.asString(i,b), rhs.asString(i,b)) == 0) { result = 0; break; } break;
		case (VTypes::PtrAPtrA): for (int i=0; i<lhs.arraySize(); ++i) if (lhs.asPointer(i,lhs.type(),b) == rhs.asPointer(i,rhs.type(),b)) { result = 0; break; } break;
		case (VTypes::IntInt): if (lhs.asInteger(b) == rhs.asInteger(b)) result = 0; break;
		case (VTypes::IntPtr): if ( ((long int) lhs.asInteger(b)) == ((long int) rhs.asPointer(lhs.type(),b))) result = 0; break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): if (lhs.asDouble(b) == rhs.asDouble(b)) result = 0; break;
		case (VTypes::StrStr): if (strcmp(lhs.asString(b), rhs.asString(b)) == 0) result = 0; break;
		case (VTypes::PtrPtr): if (lhs.asPointer(lhs.type(),b) == rhs.asPointer(rhs.type(),b)) result = 0; break;
		case (VTypes::PtrInt): if ( ((long int) lhs.asPointer(lhs.type(),b)) == ((long int) rhs.asInteger(b))) result = 0; break;
		default:
			msg.print("The operator '!=' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	rv.set(result);
	return b;
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
	return c->setArg(0, newvalue);   // TGAY
}

// Postfix Increase
bool Command::function_OperatorPostfixIncrease(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	ReturnValue newvalue;
	newvalue = rv;
	newvalue.increase();
	return c->setArg(0, newvalue);   // TGAY
}

// Prefix Decrease
bool Command::function_OperatorPrefixDecrease(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	rv.decrease();
	return c->setArg(0, rv);   // TGAY
}

// Prefix Increase
bool Command::function_OperatorPrefixIncrease(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Get current value of argument
	if (!c->arg(0, rv)) return FALSE;
	rv.increase();
	return c->setArg(0, rv);   // TGAY
}

// Raise one quantity to the power of another
bool Command::function_OperatorPower(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	ReturnValue lhs, rhs;
	bool b = TRUE;
	if (!c->arg(0,lhs)) return FALSE;
	if (!c->arg(1,rhs)) return FALSE;
	int id = lhs.dataPair(rhs);
	if (id < 0) b = FALSE;
	else switch (id)
	{
		case (VTypes::IntInt): rv.set(lhs.asInteger(b) + rhs.asInteger(b)); break;
		case (VTypes::IntDbl):
		case (VTypes::DblInt):
		case (VTypes::DblDbl): rv.set(lhs.asDouble(b) + rhs.asDouble(b)); break;
		default:
			msg.print("The operator '^' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	return b;
}

// Subtract one quantity from another
bool Command::function_OperatorSubtract(CommandNode *c, Bundle &obj, ReturnValue &rv)
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
		case (VTypes::IntIntA):
		case (VTypes::IntDblA): for (int i=0; i<rhs.arraySize(); ++i) rhs.setElement(i, rhs.asInteger(i,b) - lhs.asInteger(b)); rv = rhs; break;
		case (VTypes::DblIntA):
		case (VTypes::DblDblA): for (int i=0; i<rhs.arraySize(); ++i) rhs.setElement(i, rhs.asDouble(i,b) - lhs.asDouble(b)); rv = rhs; break;
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
			msg.print("The operator '-' cannot act between %s and %s.\n", VTypes::aDataType(rv.type(),rv.arraySize()), VTypes::aDataType(rhs.type(),rhs.arraySize()));
	}
	return b;
}
}
