/*
	*** Command Function Definitions
	*** src/command/commands.cpp
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
#include "parser/tree.h"

// Static singleton
Command commands;

/* Argument Specification Tokens:
       Char	Meaning		Acceptable Types in VTypes
	N	Number		IntegerData, DoubleData
	I	Integer		IntegerData
	R	Real		DoubleData
	C	Character	StringData
	S	Any Simple	IntegerData, DoubleData, StringData
	U	Vector		VectorData
	B	Boolean		Any
	E	Element		StringData,IntegerData,DoubleData,AtomData
	A	Atom/Id		IntegerData, AtomData
	M	Model/ID/Name	ModelData, StringData, IntegerData
	F	Frcefld/ID/Name	ForcefieldData, IntegerData, StringData
	P	Pattern/ID/Name	PatternData, StringData, IntegerData
	O	FFAtom		ForcefieldAtomData
	G	Grid/ID		GridData, StringData, IntegerData
	X	Pointer		Any pointer object
	V	Variable	Any simple variable (not path)
	Z	Any		Any
	*	<Repeat>	Any number of the last type again
	^	<Require Var>	Next token must be a modifiable variable and not a constant
	[]	<Cluster>	Surrounds groups of optional arguments that must be specified together
	|	<Or>		Separates alternative lists of arguments for the command
	&	<Array>		Next token must be an array
	/	<type Or>	Specifies an argument may be one of two types
	2-9	<NRepeat>	Next argument should occur N times
	
	Current Usage List: ABCEFGIMNOPRSUVXZ*^[]|&/23456789
*/

// Command action
CommandData Command::data[Command::nCommands] = {

	// Operators
	{ "+",			"..",		VTypes::NoData, "",
		"Internal Operator (+)" },
	{ "&&",			"..",		VTypes::NoData, "",
		"Internal Operator (&&)" },
	{ "=",			"..",		VTypes::NoData, "",
		"Internal Operator (=)" },
	{ "/=",			"..",		VTypes::NoData, "",
		"Internal Operator (/=)" },
	{ "*=",			"..",		VTypes::NoData, "",
		"Internal Operator (*=)" },
	{ "+=",			"..",		VTypes::NoData, "",
		"Internal Operator (+=)" },
	{ "-=",			"..",		VTypes::NoData, "",
		"Internal Operator (-=)" },
	{ "/",			"..",		VTypes::NoData, "",
		"Internal Operator (/)" },
	{ "==",			"..",		VTypes::NoData, "",
		"Internal Operator (==)" },
	{ "",			"..",		VTypes::NoData, "",
		"Internal Operator (>)" },
	{ ">=",			"..",		VTypes::NoData, "",
		"Internal Operator (>=)" },
	{ "<",			"..",		VTypes::NoData, "",
		"Internal Operator (<)" },
	{ "<=",			"..",		VTypes::NoData, "",
		"Internal Operator (<=)" },
	{ "%",			"..",		VTypes::NoData, "",
		"Internal Operator (%)" },
	{ "*",			"..",		VTypes::NoData, "",
		"Internal Operator (*)" },
	{ "-NEG",		".",		VTypes::NoData, "",
		"Internal Operator (negate)" },
	{ "!",			"..",		VTypes::NoData, "",
		"Internal Operator (!)" },
	{ "!=",			"..",		VTypes::NoData, "",
		"Internal Operator (!=)" },
	{ "||",			"..",		VTypes::NoData, "",
		"Internal Operator (||)" },
	{ "X--",		"..",		VTypes::NoData, "",
		"Internal Operator (X--)" },
	{ "X++",		"..",		VTypes::NoData, "",
		"Internal Operator (X++)" },
	{ "^",			"..",		VTypes::NoData, "",
		"Internal Operator (^)" },
	{ "--X",		"..",		VTypes::NoData, "",
		"Internal Operator (--X)" },
	{ "++X",		"..",		VTypes::NoData, "",
		"Internal Operator (++X)" },
	{ "-",			"..",		VTypes::NoData, "",
		"Internal Operator (-)" },

	// AST Nodes
	{ "*nofunction*",	"",		VTypes::NoData, "",
		"" },
	{ "*joiner*",		"",		VTypes::NoData, "",
		"" },
	{ "*declaration*",	"^Z*",		VTypes::NoData, "",
		"" },

	// Flow control
	{ "break",		"",		VTypes::NoData,
		"",
		"Exit from the current for loop" },
	{ "continue",		"",		VTypes::NoData,
		"",
		"Skip to the next iteration of the current loop" },
	{ "dowhile",		"_",		VTypes::NoData,
		"",
		"Run the enclosed block or statement as many times as the supplied expression evaluates to TRUE" },
	{ "for",		"_",		VTypes::NoData,
		"initial expression; increment; termination condition",
		"" },
	{ "if",			"_",		VTypes::NoData,
		"",
		"Perform a conditional test between the supplied expressions (or variables or constants)" },
	{ "return",		"z",		VTypes::NoData,
		"value = <no value>",
		"Terminate execution of the current program/filter/function, optionally returning the value provided" },
	{ "while",		"_",		VTypes::NoData,
		"",
		"Run the enclosed block or statement as many times as <expression, evaluates to TRUE" },
	
	// Math Commands.
	{ "abs",		"N",		VTypes::DoubleData,
		"double number",
		"Return absolute (i.e. positive) of value" },
	{ "acos",		"N",		VTypes::DoubleData,
		"double cosx",
		"Return inverse cosine (in degrees) of supplied argument" },
	{ "asin",		"N",		VTypes::DoubleData,
		"double sinx",
		"Return inverse sine (in degrees) of supplied argument" },
	{ "atan",		"N",		VTypes::DoubleData,
		"double tanx",
		"Return inverse tangent (in degrees) of supplied argument" },
	{ "cos",		"N",		VTypes::DoubleData,
		"double degrees",
		"Return cosine of specified angle (supplied in degrees)" },
	{ "dotproduct",		"UU",		VTypes::DoubleData,
		"vector u, vector v",
		"Calculate dot product of the two supplied vectors" },
	{ "exp",		"N",		VTypes::DoubleData,
		"double value",
		"Return exponential of the argument" },
	{ "ln",			"N",		VTypes::DoubleData,
		"double value",
		"Return natural (base-e) logarithm" },
	{ "log",		"N",		VTypes::DoubleData,
		"double value",
		"Return base-10 logarithm" },
	{ "nint",		"N",		VTypes::IntegerData,
		"double number",
		"Return nearest integer to supplied real value" },
	{ "normalise",		"U",		VTypes::DoubleData,
		"vector v",
		"Normalise the values of the 3-vector supplied" },
	{ "sin",		"N",		VTypes::DoubleData,
		"double degrees",
		"Return sine of specified angle (supplied in degrees)" },
	{ "sqrt",		"N",		VTypes::DoubleData,
		"double number",
		"Return square root of number" },
	{ "tan",		"N",		VTypes::DoubleData,
		"double degrees",
		"Return tangent of specified angle (supplied in degrees)" }

};

// Return enumerated command from string
Command::Function Command::command(const char *s)
{
	int result;
	for (result = Command::NoFunction; result < Command::nCommands; result++) if (strcmp(data[result].keyword,s) == 0) break;
	return (Command::Function) result;
}

// Constructor
Command::Command()
{
	// Create pointer list
	initPointers();
}

// Constructor
Command::~Command()
{
}

// Return specified command keyword
const char *Command::keyword(Command::Function func)
{
	return Command::data[func].keyword;
}

// Return specified command arguments
const char *Command::arguments(Command::Function func)
{
	return Command::data[func].arguments;
}

// Return specified return-value datatype
VTypes::DataType Command::returnType(Command::Function func)
{
	return Command::data[func].returnType;
}

// Return specified command argument names
const char *Command::argText(Command::Function func)
{
	return Command::data[func].argText;
}

// Return specified command syntax
const char *Command::syntax(Command::Function func)
{
	return Command::data[func].syntax;
}

// Return whether command accepts any arguments
bool CommandData::hasArguments()
{
	return (!(arguments[0] == '\0'));
}

// Execute command
bool Command::call(Command::Function cf, CommandNode *node, ReturnValue &rv)
{
// 	printf("Calling command '%s' (node is %p)...\n", data[cf].keyword, node);
	return (this->pointers_[cf])(node, rv);
}
