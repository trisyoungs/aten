/*
	*** Command Function Pointers
	*** src/command/functions.cpp
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

// Initialise Command Pointers
void Command::initPointers()
{
	/*
	// Store pointers to all command functions
	*/
	// Operators
	pointers_[OperatorAdd] = &function_OperatorAdd;
	pointers_[OperatorAnd] = &function_OperatorAnd;
	pointers_[OperatorAssignment] = &function_OperatorAssignment;
	pointers_[OperatorAssignmentDivide] = &function_OperatorAssignmentDivide;
	pointers_[OperatorAssignmentMultiply] = &function_OperatorAssignmentMultiply;
	pointers_[OperatorAssignmentPlus] = &function_OperatorAssignmentPlus;
	pointers_[OperatorAssignmentSubtract] = &function_OperatorAssignmentSubtract;
	pointers_[OperatorDivide] = &function_OperatorDivide;
	pointers_[OperatorEqualTo] = &function_OperatorEqualTo;
	pointers_[OperatorGreaterThan] = &function_OperatorGreaterThan;
	pointers_[OperatorGreaterThanEqualTo] = &function_OperatorGreaterThanEqualTo;
	pointers_[OperatorLessThan] = &function_OperatorLessThan;
	pointers_[OperatorLessThanEqualTo] = &function_OperatorLessThanEqualTo;
	pointers_[OperatorModulus] = &function_OperatorModulus;
	pointers_[OperatorMultiply] = &function_OperatorMultiply;
	pointers_[OperatorNegate] = &function_OperatorNegate;
	pointers_[OperatorNot] = &function_OperatorNot;
	pointers_[OperatorNotEqualTo] = &function_OperatorNotEqualTo;
	pointers_[OperatorOr] = &function_OperatorOr;
	pointers_[OperatorPower] = &function_OperatorPower;
	pointers_[OperatorPostfixIncrease] = &function_OperatorPostfixIncrease;
	pointers_[OperatorPostfixDecrease] = &function_OperatorPostfixDecrease;
	pointers_[OperatorPrefixIncrease] = &function_OperatorPrefixIncrease;
	pointers_[OperatorPrefixDecrease] = &function_OperatorPrefixDecrease;
	pointers_[OperatorSubtract] = &function_OperatorSubtract;

	pointers_[NoFunction] = &function_NoFunction;
	pointers_[Joiner] = &function_Joiner;
	pointers_[Declarations] = &function_Declarations;

	// Flow control
	pointers_[If] = &function_If;
	pointers_[Break] = &Command::function_Break;
	pointers_[Continue] = &Command::function_Continue;
	pointers_[DoWhile] = &Command::function_DoWhile;
	pointers_[For] = &Command::function_For;
	pointers_[If] = &Command::function_If;
	pointers_[Return] = &Command::function_Return;
	pointers_[While] = &Command::function_While;

	// Math Commands
	pointers_[Abs] = &Command::function_Abs;
	pointers_[ACos] = &Command::function_ACos;
	pointers_[ASin] = &Command::function_ASin;
	pointers_[ATan] = &Command::function_ATan;
	pointers_[Cos] = &Command::function_Cos;
	pointers_[DotProduct] = &Command::function_DotProduct;
	pointers_[Exp] = &Command::function_Exp;
	pointers_[Ln] = &Command::function_Ln;
	pointers_[Log] = &Command::function_Log;
	pointers_[Nint] = &Command::function_Nint;
	pointers_[Normalise] = &Command::function_Normalise;
	pointers_[Sin] = &Command::function_Sin;
	pointers_[Sqrt] = &Command::function_Sqrt;
	pointers_[Tan] = &Command::function_Tan;

}

