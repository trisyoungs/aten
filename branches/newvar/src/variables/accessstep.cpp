/*
	*** Variable Access Step
	*** src/variables/accessstep.cpp
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
#include "variables/variable.h"
#include "variables/variablelist.h"
#include "base/constants.h"
#include "base/messenger.h"
#include "base/sysfunc.h"
#include <stdlib.h>

// Constructor
AccessStep::AccessStep()
{
	// Private variables
	target_ = NULL;
	arrayIndex_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set target from variable name/array index
bool AccessStep::setTarget(const char *var, VariableList *sourcevars, Parser::ArgumentForm vartype)
{
	static char arrayindex[512];
	bool success = TRUE;
	Variable *v;
	int n, lbr = -1, rbr = -1;
	// Set this step - source 'var' must *not* be of path form
	Parser::ArgumentForm af = (vartype == Parser::UnknownForm ? parser.argumentForm(var) : vartype);
	switch (af)
	{
		case (Parser::ConstantForm):
			// Add constant value to parents variablelist
			v = sourcevars->addConstant(var);
			target_ = v;
			break;
		case (Parser::VariableForm):
			// Search for array index (left square bracket)
			for (n = 0; n<strlen(var); n++) 
			{
				if (var[n] == '[') lbr = n;
				if (var[n] == ']') rbr = n;
			}
			// Check values of lbracket and rbracket
			if ((lbr == -1) && (rbr == -1))
			{
				// No array element, just the name. See if it has been declared
				v = sourcevars->get(var);
				if (v == NULL)
				{
					msg.print("Error: Variable '%s' has not been declared.\n", var);
					success = FALSE;
				}
				break;
			}
			else if ((lbr == -1) || (rbr == -1))
			{
				// One bracket given but not the other
				msg.print("Array index for variable '%s' is missing a '%c'.\n", var, lbr == -1 ? '[' : ']');
				success = FALSE;
				break;
			}
			else if (lbr > rbr)
			{
				// Brackets provided the wrong way around!
				msg.print("Brackets around array index for variable '%s' face the wrong way.\n", var);
				success = FALSE;
				break;
			}
			else
			{
				// If we get here then the array brackets are valid, and we should get the contents. But first, get the variable...
				v = sourcevars->get(var);
				if (v == NULL)
				{
					msg.print("Error: Variable '%s' has not been declared.\n", var);
					success = FALSE;
				}
				else
				{
					target_ = v;
// 					returnType_ = v->type();
					strcpy(arrayindex, afterChar(beforeChar(var, ']'), '['));
					if (!setArrayIndex(arrayindex, sourcevars))
					{
						msg.print("Failed to parse array index '%s' for '%s'.\n", arrayindex, var);
						success = FALSE;
						break;
					}
				}
			}
			break;
		case (Parser::ExpressionForm):
			// Attempt to construct expression
			v = sourcevars->addExpression(var);
			if (v == NULL) success = FALSE;
			else
			{
				target_ = v;
// 				returnType_ = v->type();
			}
			break;
		case (Parser::VariablePathForm):
			msg.print("An AccessStep target cannot be initialised from a variable path.\n");
			success = FALSE;
			break;
	}
	msg.exit("AccessStep::setTarget");
	return success;
}

// Set target variable
void AccessStep::setTarget(Variable *v)
{
	target_ = v;
}

// Create arrayindex 'branch'
bool AccessStep::setArrayIndex(const char *path, VariableList *sourcevars)
{
	// Check existing pointer...
	if (arrayIndex_ != NULL) msg.print("AccessStep already has an array index set.\n");
	arrayIndex_ = new AccessPath;
	if (!arrayIndex_->setPath(path, sourcevars)) return FALSE;
	else return TRUE;
}

// Get return value as integer
int AccessStep::asInteger()
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as an integer.\n");
	else return target_->asInteger();
	return 0;
}

// Get return value as double
double AccessStep::asDouble()
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as a double.\n");
	else return target_->asDouble();
	return 0.0;
}

// Get return value as bool
bool AccessStep::asBool()
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as a bool.\n");
	else return target_->asBool();
	return FALSE;
}

// Get return value as pointer
void * AccessStep::asPointer(VTypes::DataType dt)
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as a pointer.\n");
	else return target_->asPointer(dt);
	return 0;
}

// Get return type of step (i.e. DataType of target variable)
VTypes::DataType AccessStep::returnType()
{
	if (target_ == NULL)
	{
		msg.print("No target variable is set in AccessStep - return type not available.\n");
		return VTypes::NoData;
	}
	return target_->type();
}
