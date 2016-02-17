/*
	*** Tree (Operator Checking)
	*** src/parser/tree_opcheck.cpp
	Copyright T. Youngs 2007-2016

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

ATEN_USING_NAMESPACE

// Check unary operator type compatibility
VTypes::DataType Tree::checkUnaryOperatorTypes(Commands::Function func, VTypes::DataType type, bool array, bool& returnsarray, bool quiet)
{
	Messenger::enter("Tree::checkUnaryOperatorTypes");
	int id = VTypes::dataSinglet(type, array ? 1 : -1);

	// Check for no data type
	if (id == VTypes::UntypedData)
	{
		Messenger::exit("Tree::checkUnaryOperatorTypes");
		return VTypes::NoData;
	}

	VTypes::DataType result = VTypes::NoData;
	returnsarray = false;
	switch (func)
	{
		// Postfix and prefix operators (must have integer or real types, or pointer class with list pointers)
		case (Commands::OperatorPostfixIncrease):
		case (Commands::OperatorPostfixDecrease):
		case (Commands::OperatorPrefixIncrease):
		case (Commands::OperatorPrefixDecrease):
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
						case (VTypes::CellData):
							break;
						default:
							result = type;
							break;
					}
					break;
			}
			break;
		case (Commands::OperatorNegate):
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
					returnsarray = true;
					break;
			}
			break;
		case (Commands::OperatorNot):
			// Always works, and always returns an integer
			result = VTypes::IntegerData;
			break;
		default:
			break;
	}

	// Print error message if necessary
	if ((result == VTypes::NoData) && (!quiet)) Messenger::print("Error: Unary operator %s cannot act on %s.", Commands::command(func), VTypes::aDataType(type));

	Messenger::exit("Tree::checkUnaryOperatorTypes");
	return result;
}

// Check binary operator type compatibility
VTypes::DataType Tree::checkBinaryOperatorTypes(Commands::Function func, VTypes::DataType type1, bool array1, VTypes::DataType type2, bool array2, bool& returnsarray, bool quiet)
{
	Messenger::enter("Tree::checkBinaryOperatorTypes");
	int id = VTypes::dataPair(type1, array1 ? 1 : -1, type2, array2 ? 1 : -1);

	// Check for no data type
	if (id == VTypes::UntypedData)
	{
		// Check validity of left and rhs values
		if ((type1 == VTypes::NoData) && (!quiet)) Messenger::print("Error: LHS operator has no type and can't be assigned to.");
		if ((type2 == VTypes::NoData) && (!quiet)) Messenger::print("Error: RHS operator has no type and can't be assigned to.");
		Messenger::exit("Tree::checkBinaryOperatorTypes");
		return VTypes::NoData;
	}

	VTypes::DataType result = VTypes::NoData;
	returnsarray = false;
	switch (func)
	{
		case (Commands::OperatorAdd):
		case (Commands::OperatorSubtract):
		case (Commands::OperatorAssignmentPlus):
		case (Commands::OperatorAssignmentSubtract):
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
					returnsarray = true;
					break;
			}
			break;
		case (Commands::OperatorAnd):
		case (Commands::OperatorOr):
			result = VTypes::IntegerData;
			returnsarray = false;
			break;
		case (Commands::OperatorDivide):
		case (Commands::OperatorAssignmentDivide):
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
				case (VTypes::MatIntA):
				case (VTypes::MatDblA):
					result = type1;
					break;
				case (VTypes::IntADbl):
					returnsarray = true;
				case (VTypes::IntDbl):
					result = type2;
					break;
			}
			break;
		case (Commands::OperatorAssignmentMultiply):
		case (Commands::OperatorMultiply):
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
					returnsarray = true;
				case (VTypes::DblInt):
				case (VTypes::DblDbl):
				case (VTypes::IntInt):
				case (VTypes::VecInt):
				case (VTypes::VecDbl):
				case (VTypes::VecVec):
				case (VTypes::VecMat):
				case (VTypes::MatVec):
				case (VTypes::VecIntA):
				case (VTypes::VecDblA):
				case (VTypes::MatIntA):
				case (VTypes::MatDblA):
					result = type1;
					break;
				case (VTypes::IntADblA):
				case (VTypes::IntADbl):
				case (VTypes::IntIntA):
				case (VTypes::IntDblA):
					returnsarray = true;
				case (VTypes::IntDbl):
				case (VTypes::IntVec):
				case (VTypes::DblVec):
					result = type2;
					break;
			}
			break;
		case (Commands::OperatorEqualTo):
		case (Commands::OperatorGreaterThan):
		case (Commands::OperatorGreaterThanEqualTo):
		case (Commands::OperatorLessThan):
		case (Commands::OperatorLessThanEqualTo):
		case (Commands::OperatorNotEqualTo):
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
		case (Commands::OperatorPower):
			switch (id)
			{
				case (VTypes::IntInt):
				case (VTypes::DblInt):
				case (VTypes::DblDbl):
				case (VTypes::IntDbl):
					result = VTypes::DoubleData;
					break;
			}
			break;
		case (Commands::OperatorModulus):
			switch (id)
			{
				case (VTypes::IntInt):
					result = type1;
					break;
			}
			break;
		case (Commands::OperatorAssignment):
			// First, special case between Element& and int
			if ((type1 == VTypes::ElementData) && (type2 == VTypes::IntegerData))
			{
				result = type1;
				break;
			}
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
				case (VTypes::MatIntA):
				case (VTypes::MatDblA):
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
					returnsarray = true;
					break;
			}
			break;
		default:
			break;
	}

	// Print error message
	if ((result == VTypes::NoData) && (!quiet)) Messenger::print("Error: Operator %s cannot act between types %s and %s.", Commands::command(func), VTypes::dataType(type1), VTypes::dataType(type2));

	Messenger::exit("Tree::checkBinaryOperatorTypes");
	return result;
}

// Check ternary operator type compatibility
VTypes::DataType Tree::checkTernaryOperatorTypes(Commands::Function func, VTypes::DataType type1, bool array1, VTypes::DataType type2, bool array2, VTypes::DataType type3, bool array3, bool& returnsarray, bool quiet)
{
	Messenger::enter("Tree::checkBinaryOperatorTypes");
	int id = VTypes::dataPair(type1, array1 ? 1 : -1, type2, array2 ? 1 : -1);

	// Check for no data type
	if (id == VTypes::UntypedData)
	{
		// Check validity of left and rhs values
		if ((type1 == VTypes::NoData) && (!quiet)) Messenger::print("Error: LHS operator has no type and can't be assigned to.");
		if ((type2 == VTypes::NoData) && (!quiet)) Messenger::print("Error: RHS operator has no type and can't be assigned to.");
		Messenger::exit("Tree::checkBinaryOperatorTypes");
		return VTypes::NoData;
	}

	VTypes::DataType result = VTypes::NoData;
	returnsarray = false;
	switch (func)
	{
		case (Commands::OperatorInlineIf):
			// Assume that type1 can always be resolved to a bool. Type2 and Type3 must be the same type.
			if (type2 == type3) result = type2;
			else if (!quiet) Messenger::print("Error: Type mismatch in arguments for ternary operator '?:' - return value types are '%s' and '%s'.", VTypes::dataType(type2), VTypes::dataType(type3));
			break;
		default:
			break;
	}

	Messenger::exit("Tree::checkTernaryOperatorTypes");
	return result;
}
