/*
	*** Variable list
	*** src/classes/variablelist.cpp
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

#include "classes/variablelist.h"
#include "classes/atom.h"
#include "classes/pattern.h"
#include "classes/forcefield.h"
#include "classes/expression.h"
#include "model/model.h"
#include "base/elements.h"
#include <string.h>
#include <stdarg.h>

// Constructor
VariableList::VariableList()
{
}

// Destructor
VariableList::~VariableList()
{
	// Delete expression variables manually.
}

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

// Add named variable
Variable *VariableList::addVariable(const char *name, Variable::VariableType vt)
{
	return addVariable(name,"",vt);
}
Variable *VariableList::addVariable(const char *prefix, const char *suffix, Variable::VariableType vt)
{
	static char name[128];
	strcpy(name,prefix);
	if (suffix[0] != '\0')
	{
		if (prefix[0] != '\0') strcat(name,".");
		strcat(name,suffix);
	}
	Variable *result = vars_.add();
	result->setName(name);
	result->setType(vt);
	return result;
}

// Create, don't set, named variable
Variable *VariableList::createVariable(const char *prefix, const char *suffix, Variable::VariableType vt)
{
	// First, see if this variable already exists
	Variable *result = get(prefix, suffix);
	if (result == NULL) result = addVariable(prefix, suffix, vt);
	else
	{
		// Check type_ of existing variable
		static char name[128];
		strcpy(name,prefix);
		if (suffix[0] != '\0')
		{
			if (prefix[0] != '\0') strcat(name,".");
			strcat(name,suffix);
		}
		if (result->type() != vt)
		{
			printf("Variable '%s' already exists and is of type_ '%s'.\n", name, Variable::variableType(vt));
			result = NULL;
		}
	}
	return result;
}

// Add constant
Variable *VariableList::addConstant(const char *s)
{
	static char newname[24];
	Variable *result = constants_.add();
	sprintf(newname,"constant%i",constants_.nItems());
	result->setName(newname);
	result->setType(Variable::determineType(s));
	result->set(s);
	return result;
}

// Add expression
Variable *VariableList::addExpression(const char *s)
{
	// Create structure and try to cache the expression text within it
	Expression *ex = new Expression();
	if (!ex->set(s, this))
	{
		msg(Debug::None, "Failed to cache expression.\n");
		delete ex;
		return NULL;
	}
	// Now create variable overcoat to put it in
	static char newname[24];
	Variable *result = expressions_.add();
	sprintf(newname,"expression%i",expressions_.nItems());
	result->setName(newname);
	result->setType(Variable::ExpressionVariable);
	result->set(ex);
	return result;
}

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

// Print list of variables in list
void VariableList::print()
{
	for (Variable *v = vars_.first(); v != NULL; v = v->next)
		printf("VAR=[%s] (%li) VALUE=[%s]\n",v->name(),v,v->asCharacter());
}

// Clear all variable values
void VariableList::resetAll()
{
	dbgBegin(Debug::Calls,"VariableList::resetAll");
	for (Variable *v = vars_.first(); v != NULL; v = v->next) v->reset();
	dbgEnd(Debug::Calls,"VariableList::resetAll");
}

// Clear list of variables
void VariableList::reset(const char *s, ...)
{
	dbgBegin(Debug::Calls,"VariableList::reset");
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
	dbgEnd(Debug::Calls,"VariableList::reset");
}
