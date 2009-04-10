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
#include "parser/real.h"
#include "parser/character.h"
#include "main/aten.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include <string.h>

// Constructor
NuCommandNode::NuCommandNode(NuCommand::Function func) : function_(func)
{
	// Private variables
	nodeType_ = TreeNode::CmdNode;
	format_ = NULL;
}

// Destructor
NuCommandNode::~NuCommandNode()
{
	if (format_ != NULL) delete format_;
}

// Prepare function (if possible)
bool NuCommandNode::prepFunction()
{
	bool result = TRUE;
	switch (function_)
	{
		// For functions that use formats, attempt to create the format *if* the format string is a character constant
		case (NuCommand::Error):
		case (NuCommand::Warn):
		case (NuCommand::Printf):
		case (NuCommand::Verbose):
		case (NuCommand::ReadLineFormatted):
		case (NuCommand::WriteLineFormatted):
			if (!args_.first()->item->readOnly()) break;
			result = createFormat(0,1);
			break;
		case (NuCommand::WriteVar):
			if (!args_[1]->item->readOnly()) break;
			result = createFormat(1,2);
			break;
		// For continue and break functions, add an argument corresponding to the 
	}
	return result;
}

// Get function
NuCommand::Function NuCommandNode::function()
{
	return function_;
}

// Check validity of supplied arguments
bool NuCommandNode::checkArguments()
{
	msg.enter("NuCommandNode::checkArguments");
	msg.print(Messenger::Parse, "Checking the %i argument(s) given to function '%s'...\n", args_.nItems(), NuCommand::data[function_].keyword);
	const char *c = NuCommand::data[function_].arguments;
	msg.print(Messenger::Parse, "...argument list is [%s]\n", c);
	char upc, *altargs;
	int count = 0, ngroup = -1;
	bool optional, requirevar, result, cluster = FALSE;
	NuVTypes::DataType rtype;
	// Search for an alternative set of arguments
	altargs = strchr(c, '|');
	result = TRUE;
	do
	{
		upc = *c;
		// Retain last character if this is a repeat
		if (*c != '*')
		{
			// If the character is '^', then we get the next char and set the requirevar flag
			// If it is '[' or ']' then set the cluster flag and get the next char
			// If it is '<' or '>' then set the vector flag and get the next char
			requirevar = FALSE;
			if (*c == '^')
			{
				requirevar = TRUE;
				c++;
			}
			else if ((*c == '[') || (*c == ']'))
			{
				cluster = (*c == '[');
				ngroup = 0;
				c++;
			}
			// Get character and convert to upper case if necessary
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
		else optional = TRUE;
		// If we have reached the end of the argument specification, do we still have arguments left in the command?
		if (upc == '\0')
		{
			if (args_.nItems() > count)
			{
				msg.print("Error: %i extra arguments given to function '%s' (syntax is '%s %s').\n", args_.nItems()-count, NuCommand::data[function_].keyword, NuCommand::data[function_].keyword, NuCommand::data[function_].argText);
				msg.exit("Tree::checkArguments");
				return FALSE;
			}
			else
			{
				msg.exit("Tree::checkArguments");
				return TRUE;
			}
		}
		msg.print(Messenger::Parse,"...next argument token is '%c', opt=%s, reqvar=%s, ngroup=%i\n", *c, optional ? "true" : "false", requirevar ? "TRUE" : "FALSE", ngroup);
		// If we have gone over the number of arguments provided, is this an optional argument?
		if (count >= args_.nItems())
		{
			if (!optional)
			{
				msg.print("Error: The function '%s' requires argument %i.\n", NuCommand::data[function_].keyword, count+1);
				msg.print("       Command syntax is '%s %s'.\n", NuCommand::data[function_].keyword, NuCommand::data[function_].argText);
				msg.exit("Tree::checkArguments");
				return FALSE;
			}
			else if (cluster && (ngroup != 0))
			{
				msg.print("Error: The optional argument %i to function '%s' is part of a group and must be specified.\n", count+1, NuCommand::data[function_].keyword);
				msg.print("       Command syntax is '%s %s'.\n", NuCommand::data[function_].keyword, NuCommand::data[function_].argText);
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
			// Number		(IntegerData, RealData)
			case ('N'):
				if ((rtype != NuVTypes::IntegerData) && (rtype != NuVTypes::RealData))
				{
					msg.print("Argument %i to command '%s' must be a number.\n", count+1, NuCommand::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Character		(StringData)
			case ('C'):
				if (rtype != NuVTypes::StringData)
				{
					msg.print("Argument %i to command '%s' must be a character string.\n", count+1, NuCommand::data[function_].keyword);
					result = FALSE;
				}
				break;	
			// Vector		(VectorData)
			case ('U'):
				if (rtype != NuVTypes::VectorData)
				{
					msg.print("Argument %i to command '%s' must be a vector.\n", count+1, NuCommand::data[function_].keyword);
					result = FALSE;
				}
				break;	
			// Any Simple		(IntegerData, RealData, StringData)
			case ('S'):
				if ((rtype != NuVTypes::IntegerData) && (rtype != NuVTypes::RealData) && (rtype != NuVTypes::StringData))
				{
					msg.print("Argument %i to command '%s' must be a number or a character string.\n", count+1, NuCommand::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Boolean		(Any Except NoData)
			case ('B'):
				if (rtype == NuVTypes::NoData)
				{
					msg.print("Argument %i to command '%s' must return something!\n", count+1, NuCommand::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Atom/Id		(IntegerData, AtomData)
			case ('A'):
				if ((rtype != NuVTypes::IntegerData) && (rtype != NuVTypes::AtomData))
				{
					msg.print("Argument %i to command '%s' must be an integer or an atom&.\n", count+1, NuCommand::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Model/ID/Name	(ModelData, StringData, IntegerData)
			case ('M'):
				if ((rtype != NuVTypes::IntegerData) && (rtype != NuVTypes::ModelData) && (rtype != NuVTypes::StringData))
				{
					msg.print("Argument %i to command '%s' must be an integer, a model& or a character string.\n", count+1, NuCommand::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Pattern/ID/Name	(PatternData, StringData, IntegerData)
			case ('P'):
				if ((rtype != NuVTypes::IntegerData) && (rtype != NuVTypes::PatternData) && (rtype != NuVTypes::StringData))
				{
					msg.print("Argument %i to command '%s' must be an integer, a pattern& or a character string.\n", count+1, NuCommand::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Pointer		(Any pointer (void*) object)
			case ('X'):
				if (rtype < NuVTypes::AtomData)
				{
					msg.print("Argument %i to command '%s' must be a reference of some kind.\n", count+1, NuCommand::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Variable of any type (but not a path)
			case ('V'):
				if ((argNode(count)->nodeType() != TreeNode::VarNode) && (argNode(count)->nodeType() != TreeNode::ArrayVarNode))
				{
					msg.print("Argument %i to command '%s' must be a variable of some kind.\n", count+1, NuCommand::data[function_].keyword);
					result = FALSE;
				}
				break;
		}
		// Was this argument requested to be a modifiable variable value?
		if (requirevar && argNode(count)->readOnly())
		{
			msg.print("Argument %i to command '%s' must be a variable and not a constant.\n", count+1, NuCommand::data[function_].keyword);
			result = FALSE;
		}
		// Check for failure
		if (!result) break;
		if (upc != '*') c++;
		if (cluster) ngroup++;
		count++;
	} while (*c != '\0');
	msg.exit("NuCommandNode::checkArguments");
	return result;
}

// Create format node (if necessary) from supplied argument id
NuFormat *NuCommandNode::createFormat(int fmtargid, int firstargid)
{
	msg.enter("NuCommandNode::createFormat");
	// fmtargid = id of argument which contains the formatting string, or -1 for no formatting string (free-form format)
	// firstargid = id of first data argument
	// If we do not currently have a format associated to the node, create it regardless
	bool result;
	Refitem<TreeNode,int> *firstarg = firstargid >= args_.nItems() ? NULL : args_[firstargid];
	if (format_ == NULL)
	{
		result = TRUE;
		format_ = fmtargid == -1 ? new NuFormat(firstarg) : new NuFormat(argc(fmtargid), firstarg);
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
			format_ = fmtargid == -1 ? new NuFormat(firstarg) : new NuFormat(argc(fmtargid), firstarg);
			if (!format_->isValid())
			{
				result = FALSE;
				delete format_;
				format_ = NULL;
			}
		}
	}
	msg.exit("NuCommandNode::createFormat");
	return (result == FALSE ? NULL : format_);
}

// Execute command
bool NuCommandNode::execute(NuReturnValue &rv)
{
	// Make sure the current rendersource is up-to-date
	aten.current.rs = (aten.current.m == NULL ? NULL : aten.current.m->renderSource());
	// Execute the command
	return aten.commands.call(function_, this, rv);
}

// Print node contents
void NuCommandNode::nodePrint(int offset, const char *prefix)
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
	printf("[CN]%s%s (Command) (%i arguments)\n", tab, NuCommand::data[function_].keyword, args_.nItems());
	// Output Argument data
	for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next) ri->item->nodePrint(offset+1);
	delete[] tab;
}

// Set from returnvalue node
bool NuCommandNode::set(NuReturnValue &rv)
{
	printf("Internal Error: Trying to 'set' a CommandNode.\n");
	return FALSE;
}

// Initialise node
bool NuCommandNode::initialise()
{
	printf("Internal Error: A CommandNode cannot be initialised.\n");
	return FALSE;
}

// Create, run, and free a single command with simple arguments
bool NuCommandNode::run(NuCommand::Function func, const char *arglist, ...)
{
	msg.enter("NuCommandNode::run");
	// Local constants given as arguments
	List<TreeNode> constantArgs_;

	// Create our temporary node
	NuCommandNode node(func);
	node.parent_ = NULL;

	// Set arguments from supplied list
	const char *c;
	va_list vars;
	va_start(vars, arglist);
	NuVariable *var = NULL;
	for (c = arglist; *c != '\0'; c++)
	{
		switch (*c)
		{
			case ('i'):
				var = new NuIntegerVariable(va_arg(vars, int), TRUE);
				constantArgs_.own(var);
				break;
			case ('d'):
				var = new NuRealVariable(va_arg(vars, double), TRUE);
				constantArgs_.own(var);
				break;
			case ('c'):
			case ('s'):
				var = new StringVariable(va_arg(vars, const char*), TRUE);
				constantArgs_.own(var);
				break;
			default:
				printf("Invalid argument specifier '%c' in NuCommandNode::run.\n", *c);
				var = NULL;
				break;
		}
		node.addArgument(var);
	}
	va_end(vars);
	// Now, run the command...
	NuReturnValue rv;
	bool result = node.execute(rv);
	msg.exit("NuCommandNode::run");
	return result;
}
