/*
	*** Command Functions
	*** src/parser/commands.h
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

#ifndef ATENCALC_COMMANDS_H
#define ATENCALC_COMMANDS_H

#include "parser/returnvalue.h"

// Forward declarations
class CommandList;
class CommandNode;
class CommandData;
class Command;
class TreeNode;

// Function pointer typedef and call #define
typedef bool (*CommandFunction)(CommandNode *c, ReturnValue &rv);
#define CALL_COMMAND(object,ptrToMember) ((object).*(ptrToMember)) 

class CommandData
{
	public:
	/*
	// Command description
	*/
	public:
	// Command keyword
	const char *keyword;
	// Command arguments
	const char *arguments;
	// Return-value datatype
	VTypes::DataType returnType;
	// Command argument names
	const char *argText;
	// Command syntax
	const char *syntax;

	// Return whether command accepts any arguments
	bool hasArguments();
};

// Command actions
class Command
{

	public:
	// Constructor / Destructor
	Command();
	~Command();

	// Command list
	enum Function
	{
		// Operators
		OperatorAdd,
		OperatorAnd,
		OperatorAssignment,
		OperatorAssignmentDivide,
		OperatorAssignmentMultiply,
		OperatorAssignmentPlus,
		OperatorAssignmentSubtract,
		OperatorDivide,
		OperatorEqualTo,
		OperatorGreaterThan,
		OperatorGreaterThanEqualTo,
		OperatorLessThan,
		OperatorLessThanEqualTo,
		OperatorModulus,
		OperatorMultiply,
		OperatorNegate,
		OperatorNot,
		OperatorNotEqualTo,
		OperatorOr,
		OperatorPostfixDecrease,
		OperatorPostfixIncrease,
		OperatorPower,
		OperatorPrefixDecrease,
		OperatorPrefixIncrease,
		OperatorSubtract,
		
		// AST-Specific nodes
		NoFunction,
		Joiner,
		Declarations,
	
		// Flow control
		Break,
		Continue,
		DoWhile,
		For,
		If,
		Return,
		While,
	
		// Math Commands
		Abs,
		ACos,
		ASin,
		ATan,
		Cos,
		DotProduct,
		Exp,
		Ln,
		Log,
		Nint,
		Normalise,
		Sin,
		Sqrt,
		Tan,

		nCommands
	};
	// Return enumerated command id from string
	Command::Function command(const char*);

	/*
	// Function declarations
	*/
	private:
	// AST-specific commands
	static bool function_NoFunction(CommandNode *c, ReturnValue &rv);
	static bool function_Joiner(CommandNode *c, ReturnValue &rv);
	static bool function_Declarations(CommandNode *c, ReturnValue &rv);
	// Flow control
	static bool function_Break(CommandNode *c, ReturnValue &rv);
	static bool function_Continue(CommandNode *c, ReturnValue &rv);
	static bool function_DoWhile(CommandNode *c, ReturnValue &rv);
	static bool function_For(CommandNode *c, ReturnValue &rv);
	static bool function_If(CommandNode *c, ReturnValue &rv);
	static bool function_Return(CommandNode *c, ReturnValue &rv);
	static bool function_While(CommandNode *c, ReturnValue &rv);
	// Math Commands
	static bool function_Abs(CommandNode *c, ReturnValue &rv);
	static bool function_ACos(CommandNode *c, ReturnValue &rv);
	static bool function_ASin(CommandNode *c, ReturnValue &rv);
	static bool function_ATan(CommandNode *c, ReturnValue &rv);
	static bool function_Cos(CommandNode *c, ReturnValue &rv);
	static bool function_DotProduct(CommandNode *c, ReturnValue &rv);
	static bool function_Exp(CommandNode *c, ReturnValue &rv);
	static bool function_Ln(CommandNode *c, ReturnValue &rv);
	static bool function_Log(CommandNode *c, ReturnValue &rv);
	static bool function_Nint(CommandNode *c, ReturnValue &rv);
	static bool function_Normalise(CommandNode *c, ReturnValue &rv);
	static bool function_Sin(CommandNode *c, ReturnValue &rv);
	static bool function_Sqrt(CommandNode *c, ReturnValue &rv);
	static bool function_Tan(CommandNode *c, ReturnValue &rv);
	// Variable Operators
	static bool function_OperatorAdd(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorAnd(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorAssignment(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorAssignmentDivide(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorAssignmentMultiply(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorAssignmentPlus(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorAssignmentSubtract(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorDivide(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorEqualTo(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorGreaterThan(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorGreaterThanEqualTo(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorLessThan(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorLessThanEqualTo(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorModulus(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorMultiply(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorNegate(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorNot(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorNotEqualTo(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorOr(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorPostfixDecrease(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorPostfixIncrease(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorPower(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorPrefixDecrease(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorPrefixIncrease(CommandNode *c, ReturnValue &rv);
	static bool function_OperatorSubtract(CommandNode *c, ReturnValue &rv);

	/*
	// Function descriptions / syntax etc.
	*/
	private:
	// Function pointers
	CommandFunction pointers_[Command::nCommands];

	public:
	// Function data
	static CommandData data[Command::nCommands];
	// Return specified command keyword
	static const char *keyword(Command::Function func);
	// Return specified command arguments
	static const char *arguments(Command::Function func);
	// Return specified return-value datatype
	static VTypes::DataType returnType(Command::Function func);
	// Return specified command argument names
	static const char *argText(Command::Function func);
	// Return specified command syntax
	static const char *syntax(Command::Function func);
	// Initialise function pointers
	void initPointers();
	// Execute specified command
	bool call(Command::Function cf, CommandNode *node, ReturnValue &rv);
};

// External declaration
extern Command commands;

#endif

