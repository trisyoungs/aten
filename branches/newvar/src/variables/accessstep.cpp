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
#include <string.h>

// Constructor
AccessStep::AccessStep()
{
	// Private variables
	target_ = NULL;
	arrayIndex_ = NULL;
	variableId_ = -1;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
AccessStep::~AccessStep()
{
	if (arrayIndex_ != NULL) delete arrayIndex_;
}

// Set target from variable name/array index
bool AccessStep::setTarget(const char *text, VariableList *parentvars, VariableList *sourcevars)
{
	msg.enter("AccessStep::setTarget");
	Dnchar part;
	bool result = TRUE;
	Variable *v;
	int n, lbr = -1, rbr = -1;
	// Search for array index (left square bracket)
	for (n = 0; n<strlen(text); n++) 
	{
		if (text[n] == '[') lbr = n;
		if (text[n] == ']') rbr = n;
	}
	// Check values of lbracket and rbracket
	if ((lbr == -1) && (rbr == -1))
	{
		// No array element, just the name. See if it has been declared
		v = sourcevars->get( text[0] == '$' ? &text[1] : &text[0] );
		if (v == NULL)
		{
			msg.print("Error: Variable '%s' has not been declared.\n", text);
			result = FALSE;
		}
		else msg.print(Messenger::Expressions, "AccessStep variable is '%s'\n", v->name());
		target_ = v;
	}
	else if ((lbr == -1) || (rbr == -1))
	{
		// One bracket given but not the other
		msg.print("Array index for variable '%s' is missing a '%c'.\n", text, lbr == -1 ? '[' : ']');
		result = FALSE;
	}
	else if (lbr > rbr)
	{
		// Brackets provided the wrong way around!
		msg.print("Brackets around array index for variable '%s' face the wrong way.\n", text);
		result = FALSE;
	}
	else
	{
		// If we get here then the array brackets are valid, and we should get the contents. But first, get the variable...
		part = beforeChar(text[0] == '$' ? &text[1] : &text[0], '[');
		v = sourcevars->get( part.get() );
		if (v == NULL)
		{
			msg.print("Error: Variable '%s' has not been declared.\n", part.get());
			result = FALSE;
		}
		else
		{
			target_ = v;
			// At this point, we must check that the target variable actually is an array...
			if (!target_->isArray())
			{
				msg.print("Array index given to '%s', which is not an array variable.\n", target_->name());
				result = FALSE;
			}
			else
			{
				part = afterChar(beforeChar(text, ']'), '[');
				if (!setArrayIndex(part.get(), parentvars))
				{
					msg.print("Failed to parse array index '%s' for '%s'.\n", part.get(), text);
					result = FALSE;
				}
			}
		}
	}
	msg.exit("AccessStep::setTarget");
	return result;
}

// Set target as integer constant variable
void AccessStep::setConstant(int i, VariableList *sourcevars)
{
	target_ = sourcevars->addConstant(i);
}

// Set target as expression
bool AccessStep::setExpression(const char *s, VariableList *sourcevars)
{
	target_ = sourcevars->addExpression(s);
	return (target_ != NULL);
}

// Create arrayindex 'branch'
bool AccessStep::setArrayIndex(const char *path, VariableList *parentvars)
{
	msg.enter("AccessStep::setArrayIndex");
	// Check existing pointer...
	if (arrayIndex_ != NULL) msg.print("AccessStep already has an array index set.\n");
	arrayIndex_ = new AccessPath;
	arrayIndex_->setParent(parentvars);
	bool result = arrayIndex_->setPath(path, TRUE);
	msg.exit("AccessStep::setArrayIndex");
	return result;
}

// Return target variable's name
const char *AccessStep::targetName()
{
	if (target_ == NULL) msg.print("No target variable set in AccessStep - no name to return.");
	else return target_->name();
	return "NONAME";
}

// Return target variable pointer
Variable *AccessStep::target()
{
	return target_;
}

// Return whether the step has an array index path set
bool AccessStep::hasArrayIndex()
{
	return (arrayIndex_ != NULL);
}

// Return array index as integer value
int AccessStep::arrayIndex()
{
	return arrayIndex_->asInteger();
}

// Set enumerated target variable ID
void AccessStep::setVariableId(int id)
{
	variableId_ = id;
}

// Return variable ID
int AccessStep::variableId()
{
	return variableId_;
}

// Get return value as integer
int AccessStep::asInteger()
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as an integer.\n");
	else return target_->asInteger( arrayIndex_ );
	return 0;
}

// Get return value as double
double AccessStep::asDouble()
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as a double.\n");
	else return target_->asDouble( arrayIndex_ );
	return 0.0;
}

// Get return value as character
const char *AccessStep::asCharacter()
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as a character.\n");
	else return target_->asCharacter( arrayIndex_ );
	return "NULL";
}

// Get return value as bool
bool AccessStep::asBool()
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as a bool.\n");
	else return target_->asBool( arrayIndex_ );
	return FALSE;
}

// Get return value as pointer
void *AccessStep::asPointer(VTypes::DataType dt)
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as a pointer.\n");
	else return target_->asPointer(dt,  arrayIndex_ );
	return 0;
}

// Get return type of step (i.e. DataType of target variable)
VTypes::DataType AccessStep::type()
{
	if (target_ == NULL)
	{
		msg.print("No target variable is set in AccessStep - return type not available.\n");
		return VTypes::NoData;
	}
	return target_->type();
}

// Set value of target variable from source variable
void AccessStep::setTargetVariable(Variable *srcvar)
{
	// Check type of target variable and set accordingly
	VTypes::DataType dt = target_ == NULL ? VTypes::NoData : target_->type();
	switch (dt)
	{
		case (VTypes::NoData):
			printf("Error setting target variable in AccessStep - no data type set.\n");
			break;
		case (VTypes::IntegerData):
			target_->set(srcvar->asInteger(), arrayIndex_ );
			break;
		case (VTypes::CharacterData):
			target_->set(srcvar->asCharacter(), arrayIndex_ );
			break;
		case (VTypes::RealData):
			target_->set(srcvar->asDouble(), arrayIndex_ );
			break;
		default:
			target_->set(srcvar->asPointer(dt), dt, arrayIndex_ );
			break;
	}
}

// Step value of target variable from source variable
bool AccessStep::stepTargetVariable(int delta)
{
	return target_->step(delta, arrayIndex_ );
}
