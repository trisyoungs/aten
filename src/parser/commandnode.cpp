/*
	*** Command Node
	*** src/parser/commandnode.cpp
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

#include "parser/commandnode.h"
#include "parser/integer.h"
#include "parser/double.h"
#include "parser/character.h"
#include "parser/variablenode.h"
#include "main/aten.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include <string.h>

// Constructor
CommandNode::CommandNode(Command::Function func) : function_(func)
{
	// Private variables
	nodeType_ = TreeNode::CmdNode;
	format_ = NULL;
}

// Destructor
CommandNode::~CommandNode()
{
	if (format_ != NULL) delete format_;
}

// Prepare function (if possible)
bool CommandNode::prepFunction()
{
	bool result = TRUE;
	switch (function_)
	{
		// For functions that use formats, attempt to create the format *if* the format string is a character constant
		case (Command::Error):
		case (Command::Printf):
		case (Command::Verbose):
		case (Command::ReadLineFormatted):
		case (Command::WriteLineFormatted):
			if (!args_.first()->item->readOnly()) break;
			result = createFormat(0,1);
			break;
		case (Command::WriteVariable):
		case (Command::ReadVariable):
			result = createFormat(-1,1);
			break;
		case (Command::WriteVariableFormatted):
		case (Command::ReadVariableFormatted):
			if (!args_[1]->item->readOnly()) break;
			result = createFormat(1,2);
			break;
		// For the 'return' function, the return type must match the return type of the parent tree...
		case (Command::Return):
			if (parent_->returnType() == VTypes::NoData)
			{
				if (!hasArg(0)) break;
				msg.print("Error: Return value provided when none is required.\n");
				result = FALSE;
			}
			else
			{
				if ((!hasArg(0)) && (parent_->returnType() != VTypes::NoData))
				{
					msg.print("Error: No return value provided.\n");
					result = FALSE;
				}
				else if (argType(0) != parent_->returnType())
				{
					msg.print("Error: Return value of type '%s' provided for function that returns %s.\n", VTypes::dataType(argType(0)), VTypes::aDataType(parent_->returnType()));
					result = FALSE;
				}
			}
			break;
	}
	return result;
}

// Get function
Command::Function CommandNode::function()
{
	return function_;
}

// Check validity of supplied arguments
bool CommandNode::checkArguments()
{
	msg.enter("CommandNode::checkArguments");
	msg.print(Messenger::Parse, "Checking the %i argument(s) given to function '%s'...\n", args_.nItems(), Command::data[function_].keyword);
	const char *c, *altargs = Command::data[function_].arguments;
	msg.print(Messenger::Parse, "...argument list is [%s]\n", altargs);
	char upc;
	int count = 0, ngroup = -1, repeat = 0;
	bool optional, requirevar, result, cluster = FALSE, array, reset = TRUE;
	VTypes::DataType rtype;
	// If the argument list begins with '_', arguments will have already been checked and added elsewhere...
	if (*altargs == '_')
	{
		msg.exit("CommandNode::checkArguments");
		return TRUE;
	}
	// Search for an alternative set of arguments
	result = TRUE;
	do
	{
		if (reset)
		{
			c = altargs;
			if (*c == '|') ++c;
			altargs = strchr(c, '|');
			repeat = 0;
			cluster = FALSE;
			array = FALSE;
			count = 0;
			reset = FALSE;
		}
		if (*c == '\0') break;
		upc = *c;
			if (*c == '|')
			{
				// This is the start of a new set of argument specifiers - does the current set of arguments 'fit'?
				if (args_.nItems() != count)
				{
					printf("Number of arguments (%i) doesn't match number in this set (%i) - next!\n", args_.nItems(), count);
					reset = TRUE;
					continue;
				}
				msg.exit("CommandNode::checkArguments");
				return TRUE;
			}
		// Retain previous information if this is a repeat, but make it an optional argument
		if (*c == '*') optional = TRUE;
		else if (repeat == 0)
		{
			// Reset modifier values
			requirevar = FALSE;
			array = FALSE;
			repeat = 1;
			// Find next alpha character (and accompanying modifiers)
			while (!isalpha(*c) && (*c != '|') && (*c != '\0') )
			{
				switch (*c)
				{
					// Require variable
					case ('^'):	requirevar = TRUE; break;
					// Clustering
					case ('['):	cluster = TRUE; ngroup = 0; break;
					case (']'):	cluster = FALSE; ngroup = -1; break;
					// Require array
					case ('&'):	array = TRUE; break;
					case ('2'):
					case ('3'):
					case ('4'):
					case ('5'):
					case ('6'):
					case ('7'):
					case ('8'):
					case ('9'):	repeat = *c - '0'; break;
					default:
						printf("BAD CHARACTER (%c) IN COMMAND ARGUMENTS\n", *c);
						break;
				}
				c++;
			}
			if (*c == '|')
			{
				// This is the start of a new set of argument specifiers - does the current set of arguments 'fit'?
				if (args_.nItems() != count)
				{
					printf("Number of arguments (%i) doesn't match number in this set (%i) - next!\n", args_.nItems(), count);
					reset = TRUE;
					continue;
				}
				msg.exit("CommandNode::checkArguments");
				return TRUE;
			}
			// Convert character to upper case if necessary
			if ((*c > 96) && (*c < 123))
			{
				upc = *c - 32;
				optional = TRUE;
			}
			else
			{
				upc = *c;
				optional = FALSE;
			}
		}
		if (*c == '\0') break;
		msg.print(Messenger::Parse,"...next/current argument token is '%c', opt=%s, reqvar=%s, repeat=%i, ngroup=%i\n", *c, optional ? "true" : "false", requirevar ? "TRUE" : "FALSE", repeat, ngroup);
		// If we have gone over the number of arguments provided, is this an optional argument?
		if (count >= args_.nItems())
		{
			if (!optional)
			{
				// If an alternative argument list is present, check this before we fail...
				if (altargs != NULL) { reset = TRUE; continue; }
				msg.print("Error: The function '%s' requires argument %i.\n", Command::data[function_].keyword, count+1);
				msg.print("       Command syntax is '%s(%s)'.\n", Command::data[function_].keyword, Command::data[function_].argText);
				msg.exit("Tree::checkArguments");
				return FALSE;
			}
			else if (cluster && (ngroup != 0))
			{
				msg.print("Error: The optional argument %i to function '%s' is part of a group and must be specified.\n", count+1, Command::data[function_].keyword);
				msg.print("       Command syntax is '%s %s'.\n", Command::data[function_].keyword, Command::data[function_].argText);
				msg.exit("Tree::checkArguments");
				return FALSE;
			}
			else
			{
				msg.exit("Tree::checkArguments");
				return TRUE;
			}
		}
		// Check argument type
		rtype = argType(count);
		result = TRUE;
		switch (upc)
		{
			// Number		(IntegerData, DoubleData)
			case ('N'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::DoubleData))
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be a number.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Integer		(IntegerData)
			case ('I'):
				if (rtype != VTypes::IntegerData)
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be an int.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Double		(DoubleData)
			case ('D'):
				if (rtype != VTypes::DoubleData)
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be a double.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Character		(StringData)
			case ('C'):
				if (rtype != VTypes::StringData)
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be a character string.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;	
			// Vector		(VectorData)
			case ('U'):
				if (rtype != VTypes::VectorData)
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be a vector.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;	
			// Any Simple		(IntegerData, RealData, StringData)
			case ('S'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::DoubleData) && (rtype != VTypes::StringData))
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be a number or a string.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Boolean		(Any Except NoData)
			case ('B'):
				if (rtype == VTypes::NoData)
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must return something!\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Atom/Id		(IntegerData, AtomData)
			case ('A'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::AtomData))
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be an int or an atom&.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Model/ID/Name	(ModelData, StringData, IntegerData)
			case ('M'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::ModelData) && (rtype != VTypes::StringData))
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be an int, a model& or a string.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Forcefield/ID/Name	(ForcefieldData, StringData, IntegerData)
			case ('F'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::ForcefieldData) && (rtype != VTypes::StringData))
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be an int, a forcefield& or a string.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Pattern/ID/Name	(PatternData, StringData, IntegerData)
			case ('P'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::PatternData) && (rtype != VTypes::StringData))
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be an int, a pattern& or a string.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Grid/ID/Name	(GridData, StringData, IntegerData)
			case ('G'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::GridData) ) //&& (rtype != VTypes::StringData))
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be an int or a grid&.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Pointer		(Any pointer (void*) object)
			case ('X'):
				if (rtype < VTypes::AtomData)
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be a reference of some kind.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Variable of any type (but not a path)
			case ('V'):
				if (argNode(count)->nodeType() != TreeNode::VarWrapperNode)
				{
					if (altargs != NULL) { reset = TRUE; continue; }
					msg.print("Argument %i to command '%s' must be a variable of some kind.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
		}
		// Was this argument requested to be a modifiable variable value?
		if (requirevar && argNode(count)->readOnly())
		{
			msg.print("Argument %i to command '%s' must be a variable and not a constant.\n", count+1, Command::data[function_].keyword);
			result = FALSE;
		}
		// Was this argument requested to be an array (*not* an array element)?
		if (array)
		{
			if (argNode(count)->nodeType() != TreeNode::VarWrapperNode)
			{
				msg.print("Argument %i to command '%s' must be an array.\n", count+1, Command::data[function_].keyword);
				result = FALSE;
			}
			Variable *v = ((VariableNode*) argNode(count))->variable();
			if (v->nodeType() != TreeNode::ArrayVarNode)
			{
				msg.print("Argument %i to command '%s' must be an array.\n", count+1, Command::data[function_].keyword);
				result = FALSE;
			}
			else if (((VariableNode*) argNode(count))->arrayIndex() != NULL)
			{
				msg.print("Argument %i to command '%s' must be an array and not an array element.\n", count+1, Command::data[function_].keyword);
				result = FALSE;
			}
		}
		// Check for failure
		if (!result) break;
		if ((upc != '*') && (repeat == 1)) c++;
		if (cluster) ngroup++;
		repeat--;
		count++;
	} while (*c != '\0');
	// End of the argument specification - do we still have arguments left over in the command?
	if (args_.nItems() > count)
	{
		msg.print("Error: %i extra arguments given to function '%s' (syntax is '%s %s').\n", args_.nItems()-count, Command::data[function_].keyword, Command::data[function_].keyword, Command::data[function_].argText);
		msg.exit("Tree::checkArguments");
		return FALSE;
	}
	else
	{
		msg.exit("Tree::checkArguments");
		return TRUE;
	}
	msg.exit("CommandNode::checkArguments");
	return result;
}

// Create format node (if necessary) from supplied argument id
Format *CommandNode::createFormat(int fmtargid, int firstargid)
{
	msg.enter("CommandNode::createFormat");
	// fmtargid = id of argument which contains the formatting string, or -1 for no formatting string (free-form format)
	// firstargid = id of first data argument
	// If we do not currently have a format associated to the node, create it regardless
	bool result;
	Refitem<TreeNode,int> *firstarg = firstargid >= args_.nItems() ? NULL : args_[firstargid];
	if (format_ == NULL)
	{
		result = TRUE;
		format_ = fmtargid == -1 ? new Format(firstarg) : new Format(argc(fmtargid), firstarg);
		if (!format_->isValid())
		{
			result = FALSE;
			delete format_;
			format_ = NULL;
		}
	}
	else
	{
		// So a format already exists. If the source argument is a constant (or there is no source argument) don't recreate it
		if ((fmtargid == -1) || (argNode(fmtargid)->readOnly())) result = TRUE;
		else
		{
			// Delete old format
			delete format_;
			// Create new format
			format_ = fmtargid == -1 ? new Format(firstarg) : new Format(argc(fmtargid), firstarg);
			if (!format_->isValid())
			{
				result = FALSE;
				delete format_;
				format_ = NULL;
			}
		}
	}
	msg.exit("CommandNode::createFormat");
	return (result == FALSE ? NULL : format_);
}

// Execute command
bool CommandNode::execute(ReturnValue &rv)
{
	// Make sure the current rendersource is up-to-date
	aten.current.rs = (aten.current.m == NULL ? NULL : aten.current.m->renderSource());
	// Execute the command
	return aten.commands.call(function_, this, rv);
}

// Print node contents
void CommandNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	if (offset == 1) strcat(tab,"\t");
	strcat(tab,prefix);
	// Output node data
// 	printf("Function id = %li\n", function_);
	printf("[CN]%s%s (Command) (%i arguments)\n", tab, Command::data[function_].keyword, args_.nItems());
	// Output Argument data
	for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next) ri->item->nodePrint(offset+1);
	delete[] tab;
}

// Set from returnvalue node
bool CommandNode::set(ReturnValue &rv)
{
	printf("Internal Error: Trying to 'set' a CommandNode.\n");
	return FALSE;
}

// Initialise node
bool CommandNode::initialise()
{
	printf("Internal Error: A CommandNode cannot be initialised.\n");
	return FALSE;
}

// Create, run, and free a single command with simple arguments
bool CommandNode::run(Command::Function func, const char *arglist, ...)
{
	msg.enter("CommandNode::run");
	// Local tree to contain commandnode and its arguments
	Tree tree;

	// Create our temporary node
	CommandNode node(func);
	node.parent_ = &tree;

	// Set arguments from supplied list
	const char *c;
	va_list vars;
	va_start(vars, arglist);
	TreeNode *var = NULL;
	for (c = &arglist[0]; *c != '\0'; c++)
	{
		switch (*c)
		{
			case ('i'):
				var = tree.addConstant(va_arg(vars, int));
				break;
			case ('d'):
				var = tree.addConstant(va_arg(vars, double));
				break;
			case ('c'):
			case ('s'):
				var = tree.addConstant(va_arg(vars, const char *));
				break;
			default:
				printf("Invalid argument specifier '%c' in CommandNode::run.\n", *c);
				var = NULL;
				break;
		}
		node.addArgument(var);
	}
	va_end(vars);
	// Now, run the command...
	ReturnValue rv;
	bool result = node.execute(rv);
	msg.exit("CommandNode::run");
	return result;
}
