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
NuVTypes::DataType Tree::checkUnaryOperatorTypes(NuCommand::Function func, NuVTypes::DataType type)
{
	msg.enter("Tree::checkUnaryOperatorTypes");
	// Check for no data type
	if (type == NuVTypes::NoData)
	{
		printf("Internal Error: One or both operands have no defined type.\n");
		msg.exit("Tree::checkUnaryOperatorTypes");
		return NuVTypes::NoData;
	}
	NuVTypes::DataType result = NuVTypes::NoData;
	switch (func)
	{
		// Postfix and prefix operators (must have integer or real types)
		case (NuCommand::OperatorPostfixIncrease):
		case (NuCommand::OperatorPostfixDecrease):
		case (NuCommand::OperatorPrefixIncrease):
		case (NuCommand::OperatorPrefixDecrease):
			if ((type == NuVTypes::IntegerData) || (type == NuVTypes::RealData)) result = type;
			break;
		case (NuCommand::OperatorNegate):
			if ((type <= NuVTypes::VectorData) && (type != NuVTypes::StringData)) result = type;
			break;
	}
	// Print error message if necessary
	if (result == NuVTypes::NoData) msg.print("Error: Unary operator %s cannot act on %s.\n", NuCommand::data[func].keyword, NuVTypes::aDataType(type));
	msg.exit("Tree::checkUnaryOperatorTypes");
	return result;
}

// Check binary operator type compatibility
NuVTypes::DataType Tree::checkBinaryOperatorTypes(NuCommand::Function func, NuVTypes::DataType type1, NuVTypes::DataType type2)
{
	msg.enter("Tree::checkBinaryOperatorTypes");
	// Check for no data type
	if ((type1 == NuVTypes::NoData) || (type2 == NuVTypes::NoData))
	{
		printf("Internal Error: One or both operands have no defined type.\n");
		msg.exit("Tree::checkBinaryOperatorTypes");
		return NuVTypes::NoData;
	}
	// Put types in 'precedence' order
	if (type2 > type1)
	{
		NuVTypes::DataType temp = type1;
		type1 = type2;
		type2 = temp;
	}
	// Like types first... (make int equivalent to real if both types are numeric)
	if ((type1 <= NuVTypes::RealData) && (type2 <= NuVTypes::RealData) && (type1 != type2)) type1 = type2 = NuVTypes::RealData;
	NuVTypes::DataType result = NuVTypes::NoData;
	if (type1 == type2)
	{
		switch (func)
		{
			// Arithmetic
			case (NuCommand::OperatorAdd):
				// Any pair of the same type except pointers can be added together
				if (type1 < NuVTypes::AtenData) result = type1;
				break;
			case (NuCommand::OperatorSubtract):
			case (NuCommand::OperatorMultiply):
			case (NuCommand::OperatorDivide):
				if ((type1 == NuVTypes::StringData) || (type1 >= NuVTypes::AtenData)) result = NuVTypes::NoData;
				else result = type1;
				break;
			case (NuCommand::OperatorPower):
				// Only numerical types
				if (type1 > NuVTypes::RealData) result = NuVTypes::NoData;
				else result = type1;
				break;
			// Tests
			case (NuCommand::OperatorEqualTo):
			case (NuCommand::OperatorNotEqualTo):
			case (NuCommand::OperatorGreaterThan):
			case (NuCommand::OperatorGreaterThanEqualTo):
			case (NuCommand::OperatorLessThan):
			case (NuCommand::OperatorLessThanEqualTo):
				// All other test operators are fine, unless its a vector
				if (type1 != NuVTypes::VectorData) result = NuVTypes::IntegerData;
				break;
			// Assignment
			case (NuCommand::OperatorAssignment):
				// Any value of the same type can be assigned
				result = type1;
				break;
			case (NuCommand::OperatorAssignmentDivide):
			case (NuCommand::OperatorAssignmentMinus):
			case (NuCommand::OperatorAssignmentMultiply):
			case (NuCommand::OperatorAssignmentPlus):
				// Nonsensical for character types and pointer types
				if ((type1 == NuVTypes::StringData) || (type1 >= NuVTypes::AtenData)) result = NuVTypes::NoData;
				else result = type1;
				break;
			default:
				printf("Operator '%s' not in table for checkOperatorTypes.\n", NuCommand::data[func].keyword);
				result = NuVTypes::NoData;
				break;
		}
	}
	else
	{
		// Dissimilar types
		// First, there are no operations that we allow involving a pointer*except* for and also (in)equality with an integer
		if (type1 >= NuVTypes::AtenData)
		{
			if (type2 != NuVTypes::IntegerData) result = NuVTypes::NoData;
			else if ((func == NuCommand::OperatorEqualTo) || (func == NuCommand::OperatorNotEqualTo)) result = NuVTypes::IntegerData;
			else result = NuVTypes::NoData;
		}
		else if (type1 == NuVTypes::VectorData)
		{
			// We can do arithmetic and in-place assignments with simple numbers, but no test comparisons
			switch (func)
			{
				case (NuCommand::OperatorAdd):
				case (NuCommand::OperatorSubtract):
				case (NuCommand::OperatorMultiply):
				case (NuCommand::OperatorDivide):
				case (NuCommand::OperatorAssignment):
				case (NuCommand::OperatorAssignmentDivide):
				case (NuCommand::OperatorAssignmentMinus):
				case (NuCommand::OperatorAssignmentMultiply):
				case (NuCommand::OperatorAssignmentPlus):
					if ((type2 == NuVTypes::RealData) || (type2 == NuVTypes::IntegerData)) result = NuVTypes::VectorData;
					else result = NuVTypes::NoData;
					break;
				default:
					result = NuVTypes::NoData;
					break;
			}
		}
		else if (type1 == NuVTypes::StringData)
		{
			// We allow multiplication of a string by a number...
			if ((type2 == NuVTypes::RealData) || (type2 == NuVTypes::IntegerData))
			{
				switch (func)
				{
					case (NuCommand::OperatorMultiply):
					case (NuCommand::OperatorAssignment):
					case (NuCommand::OperatorAssignmentMultiply):
						result = NuVTypes::StringData;
						break;
					default:
						result = NuVTypes::NoData;
						break;
				}
			}
			else result = NuVTypes::NoData;
		}
	}
	// Print error message
	if (result == NuVTypes::NoData) msg.print("Error: Operator %s cannot act between types %s and %s.\n", NuCommand::data[func].keyword, NuVTypes::dataType(type1), NuVTypes::dataType(type2));
	msg.exit("Tree::checkBinaryOperatorTypes");
	return result;
}
