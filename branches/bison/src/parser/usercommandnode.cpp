/*
	*** User Command Node
	*** src/parser/usercommandnode.cpp
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

#include "parser/usercommandnode.h"
#include "parser/tree.h"
#include "base/sysfunc.h"
#include <string.h>

// Constructor
UserCommandNode::UserCommandNode(Tree *func) : function_(func)
{
	// Private variables
	nodeType_ = TreeNode::UserCmdNode;
}

// Destructor
UserCommandNode::~UserCommandNode()
{
}

// Check validity of supplied arguments
bool UserCommandNode::checkArguments()
{
	msg.enter("UserCommandNode::checkArguments");
	msg.print(Messenger::Parse, "Checking the %i argument(s) given to user function '%s'...\n", args_.nItems(), function_->name());

/*
	char upc, *altargs;
	int count = 0, ngroup = -1;
	bool optional, requirevar, result, cluster = FALSE;
	VTypes::DataType rtype;
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
				msg.print("Error: %i extra arguments given to function '%s' (syntax is '%s %s').\n", args_.nItems()-count, Command::data[function_].keyword, Command::data[function_].keyword, Command::data[function_].argText);
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
				msg.print("Error: The function '%s' requires argument %i.\n", Command::data[function_].keyword, count+1);
				msg.print("       Command syntax is '%s %s'.\n", Command::data[function_].keyword, Command::data[function_].argText);
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
			// Number		(IntegerData, RealData)
			case ('N'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::DoubleData))
				{
					msg.print("Argument %i to command '%s' must be a number.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Character		(StringData)
			case ('C'):
				if (rtype != VTypes::StringData)
				{
					msg.print("Argument %i to command '%s' must be a character string.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;	
			// Vector		(VectorData)
			case ('U'):
				if (rtype != VTypes::VectorData)
				{
					msg.print("Argument %i to command '%s' must be a vector.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;	
			// Any Simple		(IntegerData, RealData, StringData)
			case ('S'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::DoubleData) && (rtype != VTypes::StringData))
				{
					msg.print("Argument %i to command '%s' must be a number or a character string.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Boolean		(Any Except NoData)
			case ('B'):
				if (rtype == VTypes::NoData)
				{
					msg.print("Argument %i to command '%s' must return something!\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Atom/Id		(IntegerData, AtomData)
			case ('A'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::AtomData))
				{
					msg.print("Argument %i to command '%s' must be an integer or an atom&.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Model/ID/Name	(ModelData, StringData, IntegerData)
			case ('M'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::ModelData) && (rtype != VTypes::StringData))
				{
					msg.print("Argument %i to command '%s' must be an integer, a model& or a character string.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Pattern/ID/Name	(PatternData, StringData, IntegerData)
			case ('P'):
				if ((rtype != VTypes::IntegerData) && (rtype != VTypes::PatternData) && (rtype != VTypes::StringData))
				{
					msg.print("Argument %i to command '%s' must be an integer, a pattern& or a character string.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Pointer		(Any pointer (void*) object)
			case ('X'):
				if (rtype < VTypes::AtomData)
				{
					msg.print("Argument %i to command '%s' must be a reference of some kind.\n", count+1, Command::data[function_].keyword);
					result = FALSE;
				}
				break;
			// Variable of any type (but not a path)
			case ('V'):
				if ((argNode(count)->nodeType() != TreeNode::VarNode) && (argNode(count)->nodeType() != TreeNode::ArrayVarNode))
				{
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
		// Check for failure
		if (!result) break;
		if (upc != '*') c++;
		if (cluster) ngroup++;
		count++;
	} while (*c != '\0');*/
	msg.exit("UserCommandNode::checkArguments");
	return TRUE;
}

// Execute command
bool UserCommandNode::execute(ReturnValue &rv)
{
	// Execute the tree.
	// We must pass the current input 'state' of this node's parent tree - give it the LineParser pointer...
	LineParser *parser = parent_->parser();
	bool result;
	result = parser == NULL ? function_->execute(rv) : function_->execute(parser, rv);
	return result;
}

// Print node contents
void UserCommandNode::nodePrint(int offset, const char *prefix)
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
	printf("[UC]%s%s (UserCommand) (%i arguments)\n", tab, function_->name(), args_.nItems());
	// Output Argument data
	for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next) ri->item->nodePrint(offset+1);
	delete[] tab;
}

// Set from returnvalue node
bool UserCommandNode::set(ReturnValue &rv)
{
	printf("Internal Error: Trying to 'set' a UserCommandNode.\n");
	return FALSE;
}

// Initialise node
bool UserCommandNode::initialise()
{
	printf("Internal Error: A UserCommandNode cannot be initialised.\n");
	return FALSE;
}

