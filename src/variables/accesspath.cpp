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
#include "variables/variablelist.h"
#include "base/messenger.h"
#include "base/sysfunc.h"

// Constructor
AccessPath::AccessPath()
{
	// Private variables
	returnType_ = VTypes::NoData;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Walk path to retrieve end variable
Variable *AccessPath::walk()
{
	msg.enter("AccessPath::walk");
	AccessStep *step = NULL;
	// DataType of the most recently stored pointer, and the pointer itself
	VTypes::DataType ptrType = VTypes::NoData;
	void *ptr = NULL;
	// Get first node in path, its type, and its value
	step = path_.first();
	// Go through nodes in the list one by one, calling the relevant static member functions in access-enabled objects
	for (step = path_.first(); step != NULL; step = step->next)
	{
		// If t
	}
	msg.exit("AccessPath::walk");
}

// Set (create) access path from text path
bool AccessPath::setPath(const char *path, VariableList *sourcevars, Parser::ArgumentForm pathtype)
{
	msg.enter("AccessPath::set");
	static char arrayindex[256];
	AccessStep *step;
	Variable *v;
	int n, lbr = -1, rbr = -1;
	bool success = TRUE;
	// Store original path
	originalPath_ = path;
	// If argument form wasn't provided, attempt to work it out.
	Parser::ArgumentForm af = (pathtype == Parser::UnknownForm ? parser.argumentForm(path) : pathtype);
	switch (af)
	{
		case (Parser::ConstantForm):
			// Add constant value to parents variablelist
			step = path_.add();
			v = sourcevars->addConstant(path);
			step->setTarget(v);
			returnType_ = v->type();
			break;
		case (Parser::VariableForm):
			// Search for array index (left square bracket)
			for (n = 0; n<strlen(path); n++) 
			{
				if (path[n] == '[') lbr = n;
				if (path[n] == ']') rbr = n;
			}
			// Check values of lbracket and rbracket
			if ((lbr == -1) && (rbr == -1))
			{
				// No array element, just the name. See if it has been declared
				v = sourcevars->get(path);
				if (v == NULL)
				{
					msg.print("Error: Variable '%s' has not been declared.\n", path);
					success = FALSE;
				}
				else returnType_ = v->type();
				break;
			}
			else if ((lbr == -1) || (rbr == -1))
			{
				// One bracket given but not the other
				msg.print("Array index for variable '%s' is missing a '%c'.\n", path, lbr == -1 ? '[' : ']');
				success = FALSE;
				break;
			}
			else if (lbr > rbr)
			{
				// Brackets provided the wrong way around!
				msg.print("Brackets around array index for variable '%s' face the wrong way.\n", path);
				success = FALSE;
				break;
			}
			else
			{
				// If we get here then the array brackets are valid, and we should get the contents. But first, get the variable...
				v = sourcevars->get(path);
				if (v == NULL)
				{
					msg.print("Error: Variable '%s' has not been 	declared.\n", path);
					success = FALSE;
				}
				else
				{
					step = path_.add();
					step->setTarget(v);
					returnType_ = v->type();
					strcpy(arrayindex, afterChar(beforeChar(path, ']'), '['));
					if (!step->setArrayIndex(arrayindex, sourcevars))
					{
						msg.print("Failed to parse array index '%s' for '%s'.\n", arrayindex, path);
						success = FALSE;
						break;
					}
				}
			}
			break;
		case (Parser::ExpressionForm):
			// Attempt to construct expression
			v = sourcevars->addExpression(path);
			if (v == NULL) success = FALSE;
			else
			{
				step = path_.add();
				step->setTarget(v);
				returnType_ = v->type();
			}
			break;
		case (Parser::VariablePathForm):
			break;
	}
	msg.exit("AccessPath::set");
}

// Set single-node path from target variable
void AccessPath::setPath(Variable *v)
{
	AccessStep *step = path_.add();
	step->setTarget(v);
}

// Get return type of path
VTypes::DataType AccessPath::returnType()
{
	return returnType_;
}

// Return original path as text
const char *AccessPath::originalPath()
{
	return originalPath_.get();
}

// Get return value as integer
int AccessPath::asInteger()
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return 0;
	return v->asInteger();
}

// Get return value as double
double AccessPath::asDouble()
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return 0.0;
	return v->asDouble();
}

// Get return value as float
float AccessPath::asFloat()
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return 0.0f;
	return v->asFloat();
}

// Get return value as character
const char *AccessPath::asCharacter()
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return "NULL";
	return v->asCharacter();
}

// Get return value as bool
bool AccessPath::asBool()
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->asBool();
}

// Get return value as pointer
void *AccessPath::asPointer(VTypes::DataType dt)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return NULL;
	return v->asPointer(dt);
}

// Increase variable by integer amount
bool AccessPath::step(int delta)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->step(delta);
}

// Set variable target from integer
bool AccessPath::set(int i)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->set(i);
}

// Set variable target from double
bool AccessPath::set(double d)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->set(d);
}

// Set variable target from character
bool AccessPath::set(const char *s)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->set(s);
}

// Set variable target from pointer
bool AccessPath::set(void *ptr, VTypes::DataType dt)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->set(ptr, dt);
}
