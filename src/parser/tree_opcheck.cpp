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
	VTypes::DataType result = VTypes::NoData;
	returnsarray = FALSE;
	switch (func)
	{
		case (Command::OperatorAdd):
		case (Command::OperatorSubtract):
		case (Command::OperatorAssignmentPlus):
		case (Command::OperatorAssignmentSubtract):
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
				case (VTypes::VecIntA):
				case (VTypes::VecDblA):
					result = VTypes::VectorData;
					break;
				case (VTypes::IntAIntA):
				case (VTypes::DblADblA):
				case (VTypes::StrAStrA):
				case (VTypes::DblAIntA):
				case (VTypes::IntADblA):
				case (VTypes::IntAInt):
				case (VTypes::DblAInt):
				case (VTypes::DblADbl):
				case (VTypes::IntAVec):
				case (VTypes::DblAVec):
				case (VTypes::IntADbl):
					result = type1;
					returnsarray = TRUE;
					break;
			}
			break;
		case (Command::OperatorAnd):
		case (Command::OperatorOr):
			result = VTypes::IntegerData;
			returnsarray = FALSE;
			break;
		case (Command::OperatorDivide):
		case (Command::OperatorAssignmentDivide):
			switch (id)
			{
				case (VTypes::IntAIntA):
				case (VTypes::IntADblA):
				case (VTypes::DblAIntA):
				case (VTypes::DblADblA):
				case (VTypes::IntAInt):
				case (VTypes::DblAInt):
				case (VTypes::DblADbl):
				case (VTypes::IntAVec):
				case (VTypes::DblAVec):
				case (VTypes::IntInt):
				case (VTypes::DblInt):
				case (VTypes::DblDbl):
				case (VTypes::VecInt):
				case (VTypes::VecDbl):
				case (VTypes::VecVec):
				case (VTypes::VecIntA):
				case (VTypes::VecDblA):
					result = type1;
					break;
				case (VTypes::IntADbl):
					returnsarray = TRUE;
				case (VTypes::IntDbl):
					result = type2;
					break;
			}
			break;
		case (Command::OperatorAssignmentMultiply):
		case (Command::OperatorMultiply):
			switch (id)
			{
				case (VTypes::IntAIntA):
				case (VTypes::IntAInt):
				case (VTypes::DblAIntA):
				case (VTypes::DblADblA):
				case (VTypes::DblAInt):
				case (VTypes::DblADbl):
				case (VTypes::DblDblA):
				case (VTypes::DblIntA):
				case (VTypes::IntAVec):
				case (VTypes::DblAVec):
					returnsarray = TRUE;
				case (VTypes::DblInt):
				case (VTypes::DblDbl):
				case (VTypes::IntInt):
				case (VTypes::VecInt):
				case (VTypes::VecDbl):
				case (VTypes::VecVec):
				case (VTypes::VecIntA):
				case (VTypes::VecDblA):
					result = type1;
					break;
				case (VTypes::IntADblA):
				case (VTypes::IntADbl):
				case (VTypes::IntIntA):
				case (VTypes::IntDblA):
					returnsarray = TRUE;
				case (VTypes::IntDbl):
				case (VTypes::IntVec):
				case (VTypes::DblVec):
					result = type2;
					break;
			}
			break;
		case (Command::OperatorEqualTo):
		case (Command::OperatorGreaterThan):
		case (Command::OperatorGreaterThanEqualTo):
		case (Command::OperatorLessThan):
		case (Command::OperatorLessThanEqualTo):
		case (Command::OperatorNotEqualTo):
			switch (id)
			{
				case (VTypes::IntAIntA):
				case (VTypes::IntADblA):
				case (VTypes::DblAIntA):
				case (VTypes::DblADblA):
				case (VTypes::StrAStrA):
				case (VTypes::PtrAPtrA):
				case (VTypes::IntInt):
				case (VTypes::IntPtr):
				case (VTypes::IntDbl):
				case (VTypes::DblInt):
				case (VTypes::DblDbl):
				case (VTypes::StrStr):
				case (VTypes::PtrPtr):
				case (VTypes::PtrInt):
					result = VTypes::IntegerData;
					break;
			}
			break;
		case (Command::OperatorPower):
			switch (id)
			{
				case (VTypes::IntInt):
				case (VTypes::DblInt):
				case (VTypes::DblDbl):
					result = type1;
					break;
				case (VTypes::IntDbl):
					result = type1;
					break;
			}
			break;
		case (Command::OperatorAssignment):
			switch (id)
			{
				case (VTypes::IntInt):
				case (VTypes::PtrPtr):
				case (VTypes::VecVec):
				case (VTypes::StrStr):
				case (VTypes::IntDbl):
				case (VTypes::DblInt):
				case (VTypes::DblDbl):
				case (VTypes::VecInt):
				case (VTypes::VecDbl):
				case (VTypes::VecIntA):
				case (VTypes::VecDblA):
				case (VTypes::PtrPtrA):
					result = type1;
					break;
				case (VTypes::IntAIntA):
				case (VTypes::DblADblA):
				case (VTypes::StrAStrA):
				case (VTypes::DblAIntA):
				case (VTypes::IntADblA):
				case (VTypes::PtrAPtrA):
				case (VTypes::IntAInt):
				case (VTypes::DblAInt):
				case (VTypes::DblADbl):
				case (VTypes::IntAVec):
				case (VTypes::DblAVec):
				case (VTypes::IntADbl):
				case (VTypes::PtrAPtr):
					result = type1;
					returnsarray = TRUE;
					break;
			}
			break;
	}
	// Print error message
	if (result == VTypes::NoData) msg.print("Error: Operator %s cannot act between types %s and %s.\n", Command::data[func].keyword, VTypes::dataType(type1), VTypes::dataType(type2));
	msg.exit("Tree::checkBinaryOperatorTypes");
	return result;
}
