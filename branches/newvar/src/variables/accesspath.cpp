/*
	*** Variable Access Path
	*** src/variables/accesspath.cpp
	Copyright T. Youngs 2007,2008

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

#include "variables/accesspath.h"
#include "variables/accessstep.h"

// Constructor
AccessPath::AccessPath()
{
	// Private variables
	returnType_ = VTypes::NoData;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set (create) access path from text path
bool AccessPath::set(const char *path, VariableList &sourcevars, Parser::ArgumentForm pathtype)
{
	msg.enter("AccessPath::set");
	static char arrayindex[256];
	// If argument form wasn't provided, attempt to work it out.
	Parser::ArgumentForm af = (pathtype == Parser::UnknownForm ? parser.argumentForm(varname) : form);
	switch (af)
	{
		case (Parser::ConstantForm):
			// Add constant value to parents variablelist
			result.item = parent_->variables.addConstant(varname);
			break;
		case (Parser::VariableForm):
			// Search for array index (left square bracket)
			int lbr = -1, rbr = -1;
			for (int n = 0; n<strlen(varname); n++) 
			{
				if (varname[n] == '[') lbr = n;
				if (varname[n] == ']') rbr = n;
			}
			// Check values of lbracket and rbracket
			if ((lbr == -1) && (rbr == -1))
			{
				// No array element, just the name. See if it has been declared
				result.item = parent_->variables.get(varname);
				if (result.item == NULL) msg.print("Error: Variable '%s' has not been declared.\n", varname);
				break;
			}
			else if ((lbr == -1) || (rbr == -1))
			{
				// One bracket given but not the other
				msg.print("Array index for variable '%s' is missing a '%c'.\n", varname, lbr == -1 ? '[' : ']');
				break;
			}
			else if (lbr > rbr)
			{
				// Brackets provided the wrong way around!
				msg.print("Brackets around array index for variable '%s' face the wrong way.\n", varname);
				break;
			}
			else
			{
				// If we get here then the array brackets are valid, and we should get the contents. But first, get the variable...
				result.item = parent_->variables.get(varname);
				if (result.item == NULL) msg.print("Error: Variable '%s' has not been declared.\n", varname);
				else
				{
					strcpy(arrayindex, afterChar(beforeChar(varname, ']'), '['));
					result.data = constructOrRetrieve(arrayindex);
					if (result.data == NULL)
					{
						msg.print("Failed to parse array index '%s' for '%s'.\n", arrayindex, varname);
						break;
					}
				}
			}
			break;
		case (Parser::ExpressionForm):
			// Add constant value to parents variablelist
			result.item = parent_->variables.addConstant(varname);
			break;
	}
	msg.exit("AccessPath::set");
}
