/*
	*** Variable list
	*** src/variables/variablelist.cpp
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

#include "variables/variablelist.h"
#include "variables/expression.h"
#include "variables/integer.h"
#include "variables/character.h"
#include "variables/double.h"
// #include "base/vaccess.h"
#include "main/aten.h"
#include <string.h>
#include <stdarg.h>

// Constructor
VariableList::VariableList()
{
	// Added reference to 'models' accessor in Aten
	//addReference("models", atenAccessors.findAccessor("models"));
}

/*
// Variable Creation
*/

// Create variable of specified type (private function)
Variable *VariableList::createVariable(VTypes::DataType dt)
{
	Variable *result = NULL;
	switch (dt)
	{
		case (VTypes::CharacterData):
			result = new CharacterVariable;
			break;
		case (VTypes::IntegerData):
			result = new IntegerVariable;
			break;
		case (VTypes::RealData):
			result = new RealVariable;
			break;
		case (VTypes::ExpressionData):
			result = new ExpressionVariable;
			break;
		default:
			printf("Don't yet know how to create a variable of type %i\n", dt);
			break;
	}
	if (result != NULL) result->setParent(this);
	return result;
}

// Add named variable
Variable *VariableList::addVariable(const char *name, VTypes::DataType dt)
{
	return addVariable(name,"",dt);
}
Variable *VariableList::addVariable(const char *prefix, const char *suffix, VTypes::DataType dt)
{
	static char name[128];
	strcpy(name,prefix);
	if (suffix[0] != '\0')
	{
		if (prefix[0] != '\0') strcat(name,".");
		strcat(name,suffix);
	}
	Variable *newvar = createVariable(dt);
	vars_.own(newvar);
	newvar->setName(name);
	return newvar;
}

// Add constant
Variable *VariableList::addConstant(const char *s, bool forcecharacter)
{
	static char newname[24];
	VTypes::DataType dt = forcecharacter ? VTypes::CharacterData : VTypes::determineType(s);
	Variable *newvar = createVariable(dt);
	constants_.own(newvar);
	sprintf(newname,"constant%i", constants_.nItems());
	newvar->setName(newname);
	newvar->set(s);
	return newvar;
}

// Add constant
Variable *VariableList::addConstant(int i)
{
	static char newname[24];
	Variable *newvar = new IntegerVariable;
	constants_.own(newvar);
	sprintf(newname,"constant%i", constants_.nItems());
	newvar->setName(newname);
	newvar->set(i);
	return newvar;
}

// Add expression
Variable *VariableList::addExpression(const char *s)
{
	// Create structure and try to cache the expression text within it
	Expression *ex = new Expression();
	if (!ex->set(s, this))
	{
		msg.print( "Failed to cache expression.\n");
		delete ex;
		return NULL;
	}
	// Now create variable overcoat to put it in
	static char newname[24];
	Variable *result = expressions_.add();
	sprintf(newname,"expression%i",expressions_.nItems());
	result->setName(newname);
	result->set(ex);
	return result;
}

/*
// Set Methods
*/

// Set existing variable (or add new and set) (Variable::CharacterVariable)
void VariableList::set(const char *name, const char *value)
{
	set(name,"",value);
}
void VariableList::set(const char *prefix, const char *suffix, const char *value)
{
	static char newname[128];
	strcpy(newname,prefix);
	if (suffix[0] != '\0')
	{
		if (prefix[0] != '\0') strcat(newname,".");
		strcat(newname,suffix);
	}
	Variable *v = get(newname);
	if (v == NULL) v = addVariable(newname, Variable::CharacterVariable);
	v->set(value);
}

// Set existing variable (or add new and set) (Variable::IntegerVariable)
void VariableList::set(const char *name, int value)
{
	set(name,"",value);
}

void VariableList::set(const char *prefix, const char *suffix, int value)
{
	static char newname[128];
	strcpy(newname,prefix);
	if (suffix[0] != '\0')
	{
		if (prefix[0] != '\0') strcat(newname,".");
		strcat(newname,suffix);
	}
	Variable *v = get(newname);
	if (v == NULL) v = addVariable(newname, Variable::IntegerVariable);
	v->set(value);
}

// Set existing variable (or add new and set) (Variable::FloatVariable)
void VariableList::set(const char *name, double value)
{
	set(name,"",value);
}
void VariableList::set(const char *prefix, const char *suffix, double value)
{
	static char newname[128];
	strcpy(newname,prefix);
	if (suffix[0] != '\0')
	{
		if (prefix[0] != '\0') strcat(newname,".");
		strcat(newname,suffix);
	}
	Variable *v = get(newname);
	if (v == NULL) v = addVariable(newname, Variable::FloatVariable);
	v->set(value);
}

// Set existing variable (or add new and set) (Variable::AtomVariable)
void VariableList::set(const char *name, Atom *i)
{
	set(name,"",i);
}
void VariableList::set(const char *prefix, const char *suffix, Atom *i)
{
	static char newname[128];
	strcpy(newname,prefix);
	if (suffix[0] != '\0')
	{
		if (prefix[0] != '\0') strcat(newname,".");
		strcat(newname,suffix);
	}
	Variable *v = get(newname);
	if (v == NULL) v = addVariable(newname, Variable::AtomVariable);
	v->set(i);
}

/*
// Variable Retrieval
*/

// Return dummy variable
Variable *VariableList::dummy()
{
	return &dummy_;
}

// Retrieve named variable
Variable *VariableList::get(const char *name)
{
	return get(name,"");
}

// Retrieve named variable (prefix.suffix)
Variable *VariableList::get(const char *prefix, const char *suffix)
{
	static char name[128];
	strcpy(name,prefix);
	if (suffix[0] != '\0')
	{
		strcat(name,".");
		strcat(name,suffix);
	}
	for (Variable *v = vars_.first(); v != NULL; v = v->next)
		if (strcmp(name,v->name()) == 0) return v;
	return NULL;
}

/*
// Print / Reset
*/

// Print list of variables in list
void VariableList::print()
{
	for (Variable *v = vars_.first(); v != NULL; v = v->next)
		printf("VAR=[%s] (%li) VALUE=[%s]\n",v->name(),v,v->asCharacter());
}

// Clear all variable values
void VariableList::resetAll()
{
	msg.enter("VariableList::resetAll");
	for (Variable *v = vars_.first(); v != NULL; v = v->next) v->reset();
	msg.exit("VariableList::resetAll");
}

// Clear list of variables
void VariableList::reset(const char *s, ...)
{
	msg.enter("VariableList::reset");
	// List of variables must be ended by "".
	static char name[64];
	va_list namelist;
	va_start(namelist,s);
	Variable *v;
	// Reset 's' first
	get(s)->reset();
	do
	{
		strcpy(name,va_arg(namelist,char*));
		if (name[0] != '\0')
		{
			v = get(name);
			if (v == NULL) printf("VariableList::reset <<<< '%s' not in list >>>>\n",name);
			else v->reset();
		}
	} while (name[0] != '\0');
	msg.exit("VariableList::reset");
}
