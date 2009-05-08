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

// Check unary operator type compatibility
VTypes::DataType Tree::checkUnaryOperatorTypes(Command::Function func, VTypes::DataType type, bool array, bool &returnsarray)
{
	msg.enter("Tree::checkUnaryOperatorTypes");
	int id = VTypes::dataSinglet(type, array ? 1 : -1);
	// Check for no data type
	if (id == VTypes::UntypedData)
	{
		msg.exit("Tree::checkUnaryOperatorTypes");
		return VTypes::NoData;
	}
	VTypes::DataType result = VTypes::NoData;
	returnsarray = FALSE;
	switch (func)
	{
		// Postfix and prefix operators (must have integer or real types)
		case (Command::OperatorPostfixIncrease):
		case (Command::OperatorPostfixDecrease):
		case (Command::OperatorPrefixIncrease):
		case (Command::OperatorPrefixDecrease):
			switch (id)
			{
				case (VTypes::Int):
				case (VTypes::Dbl):
				case (VTypes::Vec):
					result = type;
					break;
				case (VTypes::Ptr):
					// Only some pointer types are supported....
					switch (type)
					{
						case (VTypes::AtenData):
						case (VTypes::ElementData):
						case (VTypes::CellData):
						case (VTypes::ForcefieldAtomData):
						case (VTypes::ForcefieldBoundData):
							break;
						default:
							result = type;
							break;
					}
					break;
			}
			break;
		case (Command::OperatorNegate):
			switch (id)
			{
				case (VTypes::Int):
				case (VTypes::Dbl):
				case (VTypes::Vec):
					result = type;
					break;
				case (VTypes::IntA):
				case (VTypes::DblA):
				case (VTypes::VecA):
					result = type;
					returnsarray = TRUE;
					break;
			}
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
VTypes::DataType Tree::checkBinaryOperatorTypes(Command::Function func, VTypes::DataType type1, bool array1, VTypes::DataType type2, bool array2, bool &returnsarray)
{
	msg.enter("Tree::checkBinaryOperatorTypes");
	int id = VTypes::dataPair(type1, array1 ? 1 : -1, type2, array2 ? 1 : -1);
	// Check for no data type
	if (id == VTypes::UntypedData)
	{
		msg.exit("Tree::checkBinaryOperatorTypes");
		return VTypes::NoData;
	}
	// Some operators will act on just about anything
	if ((func == Command::OperatorAnd) || (func == Command::OperatorOr))
	{
		msg.exit("Tree::checkBinaryOperatorTypes");
		return VTypes::IntegerData;
	}
	returnsarray = FALSE;
	switch (func)
	{
		case (Command::OperatorAdd):
			switch (id)
			{
				case (VTypes::IntInt):
				case (VTypes::VecVec):
				case (VTypes::StrStr):
					result = type1;
					break;
				case (VTypes::IntDbl):
				case (VTypes::DblInt):
				case (VTypes::DblDbl):
					result = VTypes::DoubleData;
					break;
				case (VTypes::VecInt):
				case (VTypes::VecDbl):
				case (VTypes::IntVec):
				case (VTypes::DblVec):
					result = VTypes::VectorData;
					break;
				case (VTypes::IntAIntA):
				case (VTypes::DblADblA):
				case (VTypes::StrAStrA):
					result = type1;
					returnsarray = TRUE;
					break;
				case (VTypes::IntADblA):
				case (VTypes::DblAIntA):
				case (VTypes::IntAInt):
				case (VTypes::IntADbl):
				case (VTypes::DblAInt):
				case (VTypes::DblADbl):
				case (VTypes::IntIntA):
				case (VTypes::IntDblA):
				case (VTypes::DblIntA):
				case (VTypes::DblDblA):
				case (VTypes::IntAVec):
				case (VTypes::DblAVec):
				case (VTypes::VecIntA):
				case (VTypes::VecDblA):
					result = type;
					returnsarray = TRUE;
					break;
			}
			break;
		case (Command::OperatorAnd):
			result = VTypes::IntegerData;
			returnsarray = FALSE;
			break;
		case (Command::OperatorAssignment):
		case (Command::OperatorAssignmentDivide):
		case (Command::OperatorAssignmentMultiply):
		case (Command::OperatorAssignmentPlus):
		case (Command::OperatorAssignmentSubtract):
		case (Command::OperatorDivide):
			switch (id)
			{
				case (VTypes::IntAIntA):
				case (VTypes::IntADblA):
				case (VTypes::DblAIntA):
				case (VTypes::DblADblA):
				case (VTypes::IntAInt):
				case (VTypes::IntADbl):
				case (VTypes::DblAInt):
				case (VTypes::DblADbl):
				case (VTypes::IntAVec):
				case (VTypes::DblAVec):
					result = 
				case (VTypes::IntInt):
				case (VTypes::IntDbl):
				case (VTypes::DblInt):
				case (VTypes::DblDbl):
				case (VTypes::VecInt):
				case (VTypes::VecDbl):
				case (VTypes::VecVec):
				case (VTypes::VecIntA):
				case (VTypes::VecDblA):
			}
			break;
		case (Command::OperatorEqualTo):
		case (Command::OperatorGreaterThan):
		case (Command::OperatorGreaterThanEqualTo):
		case (Command::OperatorLessThan):
		case (Command::OperatorLessThanEqualTo):
		case (Command::OperatorMultiply):
		case (Command::OperatorNotEqualTo):
		case (Command::OperatorOr):
			result = VTypes::IntegerData;
			returnsarray = FALSE;
			break;
		case (Command::OperatorPower):
		case (Command::OperatorSubtract):
	}








	// Put types in 'precedence' order
	if (type2 > type1)
	{
		VTypes::DataType temp = type1;
		type1 = type2;
		type2 = temp;
	}
	// Get array flags
	bool array1 = ((ntype1 == TreeNode::ArrayVarNode) || (ntype1 == TreeNode::ArrayConstantNode));
	bool array2 = ((ntype2 == TreeNode::ArrayVarNode) || (ntype2 == TreeNode::ArrayConstantNode));
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
				// Only numerical types, and no arrays
				if (array1 || array2) result = VTypes::NoData;
				else if (type1 > VTypes::DoubleData) result = VTypes::NoData;
				else result = type1;
				break;
			// Tests
			case (Command::OperatorGreaterThan):
			case (Command::OperatorGreaterThanEqualTo):
			case (Command::OperatorLessThan):
			case (Command::OperatorLessThanEqualTo):
				if (array1 || array2) { result = VTypes::NoData; break; }
			case (Command::OperatorEqualTo):
			case (Command::OperatorNotEqualTo):
				// All other test operators are fine, unless its a vector
				if (type1 != VTypes::VectorData) result = VTypes::IntegerData;
				break;
			// Assignment
			case (Command::OperatorAssignment):
				// Any value of the same type can be assigned
				result = type1;
				break;
			case (Command::OperatorAssignmentDivide):
			case (Command::OperatorAssignmentSubtract):
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
				case (Command::OperatorAssignmentSubtract):
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
