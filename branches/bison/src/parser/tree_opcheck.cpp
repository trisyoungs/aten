/*
	*** Tree (Operator Checking)
	*** src/parser/tree_opcheck.cpp
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

#include "parser/tree.h"

// Check binary operator type compatibility
VTypes::DataType Tree::checkUnaryOperatorTypes(Command::Function func, VTypes::DataType type)
{
	msg.enter("Tree::checkUnaryOperatorTypes");
	// Check for no data type
	if (type == VTypes::NoData)
	{
		printf("Internal Error: One or both operands have no defined type.\n");
		msg.exit("Tree::checkUnaryOperatorTypes");
		return VTypes::NoData;
	}
	VTypes::DataType result = VTypes::NoData;
	switch (func)
	{
		// Postfix and prefix operators (must have integer or real types)
		case (Command::OperatorPostfixIncrease):
		case (Command::OperatorPostfixDecrease):
		case (Command::OperatorPrefixIncrease):
		case (Command::OperatorPrefixDecrease):
			// Easier to list those types that we *can't* do...
			switch (type)
			{
				case (VTypes::AtenData):
				case (VTypes::ElementsData):
				case (VTypes::CellData):
				case (VTypes::ForcefieldAtomData):
				case (VTypes::ForcefieldBoundData):
					break;
				default:
					result = type;
					break;
			}
			break;
		case (Command::OperatorNegate):
			if ((type <= VTypes::VectorData) && (type != VTypes::StringData)) result = type;
			break;
		case (Command::OperatorNot):
			// Always works, and always returns an integer
			result = VTypes::IntegerData;
			break;
	}
	// Print error message if necessary
	if (result == VTypes::NoData) msg.print("Error: Unary operator %s cannot act on %s.\n", Command::data[func].keyword, VTypes::aDataType(type));
	msg.exit("Tree::checkUnaryOperatorTypes");
	return result;
}

// Check binary operator type compatibility
VTypes::DataType Tree::checkBinaryOperatorTypes(Command::Function func, VTypes::DataType type1, VTypes::DataType type2)
{
	msg.enter("Tree::checkBinaryOperatorTypes");
	// Check for no data type
	if ((type1 == VTypes::NoData) || (type2 == VTypes::NoData))
	{
		printf("Internal Error: One or both operands have no defined type (%s,%s).\n", VTypes::dataType(type1), VTypes::dataType(type2));
		msg.exit("Tree::checkBinaryOperatorTypes");
		return VTypes::NoData;
	}
	// Some operators will act on just about anything
	if ((func == Command::OperatorAnd) || (func == Command::OperatorOr))
	{
		msg.exit("Tree::checkBinaryOperatorTypes");
		return VTypes::IntegerData;
	}
	// Put types in 'precedence' order
	if (type2 > type1)
	{
		VTypes::DataType temp = type1;
		type1 = type2;
		type2 = temp;
	}
	// Like types first... (make int equivalent to real if both types are numeric)
	if ((type1 <= VTypes::DoubleData) && (type2 <= VTypes::DoubleData) && (type1 != type2)) type1 = type2 = VTypes::DoubleData;
	VTypes::DataType result = VTypes::NoData;
	if (type1 == type2)
	{
		switch (func)
		{
			// Arithmetic
			case (Command::OperatorAdd):
				// Any pair of the same type except pointers can be added together
				if (type1 < VTypes::AtenData) result = type1;
				break;
			case (Command::OperatorSubtract):
			case (Command::OperatorMultiply):
			case (Command::OperatorDivide):
				if ((type1 == VTypes::StringData) || (type1 >= VTypes::AtenData)) result = VTypes::NoData;
				else result = type1;
				break;
			case (Command::OperatorPower):
				// Only numerical types
				if (type1 > VTypes::DoubleData) result = VTypes::NoData;
				else result = type1;
				break;
			// Tests
			case (Command::OperatorEqualTo):
			case (Command::OperatorNotEqualTo):
			case (Command::OperatorGreaterThan):
			case (Command::OperatorGreaterThanEqualTo):
			case (Command::OperatorLessThan):
			case (Command::OperatorLessThanEqualTo):
				// All other test operators are fine, unless its a vector
				if (type1 != VTypes::VectorData) result = VTypes::IntegerData;
				break;
			// Assignment
			case (Command::OperatorAssignment):
				// Any value of the same type can be assigned
				result = type1;
				break;
			case (Command::OperatorAssignmentDivide):
			case (Command::OperatorAssignmentMinus):
			case (Command::OperatorAssignmentMultiply):
			case (Command::OperatorAssignmentPlus):
				// Nonsensical for character types and pointer types
				if ((type1 == VTypes::StringData) || (type1 >= VTypes::AtenData)) result = VTypes::NoData;
				else result = type1;
				break;
			default:
				printf("Operator '%s' not in table for checkOperatorTypes.\n", Command::data[func].keyword);
				result = VTypes::NoData;
				break;
		}
	}
	else
	{
		// Dissimilar types
		// First, there are no operations that we allow involving a pointer*except* for and also (in)equality with an integer
		if (type1 >= VTypes::AtenData)
		{
			if (type2 != VTypes::IntegerData) result = VTypes::NoData;
			else if ((func == Command::OperatorEqualTo) || (func == Command::OperatorNotEqualTo)) result = VTypes::IntegerData;
			else result = VTypes::NoData;
		}
		else if (type1 == VTypes::VectorData)
		{
			// We can do arithmetic and in-place assignments with simple numbers, but no test comparisons
			switch (func)
			{
				case (Command::OperatorAdd):
				case (Command::OperatorSubtract):
				case (Command::OperatorMultiply):
				case (Command::OperatorDivide):
				case (Command::OperatorAssignment):
				case (Command::OperatorAssignmentDivide):
				case (Command::OperatorAssignmentMinus):
				case (Command::OperatorAssignmentMultiply):
				case (Command::OperatorAssignmentPlus):
					if ((type2 == VTypes::DoubleData) || (type2 == VTypes::IntegerData)) result = VTypes::VectorData;
					else result = VTypes::NoData;
					break;
				default:
					result = VTypes::NoData;
					break;
			}
		}
		else if (type1 == VTypes::StringData)
		{
			// We allow multiplication of a string by a number...
			if ((type2 == VTypes::DoubleData) || (type2 == VTypes::IntegerData))
			{
				switch (func)
				{
					case (Command::OperatorMultiply):
					case (Command::OperatorAssignment):
					case (Command::OperatorAssignmentMultiply):
						result = VTypes::StringData;
						break;
					default:
						result = VTypes::NoData;
						break;
				}
			}
			else result = VTypes::NoData;
		}
	}
	// Print error message
	if (result == VTypes::NoData) msg.print("Error: Operator %s cannot act between types %s and %s.\n", Command::data[func].keyword, VTypes::dataType(type1), VTypes::dataType(type2));
	msg.exit("Tree::checkBinaryOperatorTypes");
	return result;
}
