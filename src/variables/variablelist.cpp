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
#include "variables/accesspath.h"
#include "variables/expression.h"
#include "variables/integer.h"
#include "variables/character.h"
#include "variables/real.h"
#include "variables/pointer.h"
#include "variables/bundle.h"
#include "main/aten.h"
#include <string.h>
#include <stdarg.h>

// Constructor
VariableList::VariableList()
{
	// Add accessors to model list and current model/frame in Aten
	Variable *v;
// 	v = addBundlePointer("frame", VTypes::ModelData);
// 	v->set(&aten.current, VTypes::ModelData);
	v = addBundlePointer("model", VTypes::ModelData);
	v->set(&aten.current, VTypes::ModelData);
	//v = addVariable("models", VTypes::ModelData, VTypes::ListArray);
	//v->set(aten.modelList(), VTypes::ModelData);
}

/*
// Variable Creation
*/

// Create variable of specified type (private function)
Variable *VariableList::createVariable(VTypes::DataType dt, int arraysize)
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
		case (VTypes::AtomData):
		case (VTypes::PatternData):
		case (VTypes::ModelData):
		case (VTypes::GridData):
		case (VTypes::BondData):
		case (VTypes::AngleData):
		case (VTypes::TorsionData):
		case (VTypes::AtomtypeData):
			result = new PointerVariable(dt);
			break;
		default:
			printf("Don't yet know how to create a variable of type %i\n", dt);
			break;
	}
	if (result != NULL)
	{
		result->setParent(this);
		// Create array if a valid size was given
		if (arraysize != -1)
		{
			if (arraysize > 0) result->setArraySize(arraysize);
			msg.print("Invalid array size (%i) given for variable.\n", arraysize);
			return NULL;
		}
	}
	return result;
}

// Add named variable
Variable *VariableList::addVariable(const char *name, VTypes::DataType dt, int arraysize)
{
	return addVariable(name,"",dt,arraysize);
}
Variable *VariableList::addVariable(const char *prefix, const char *suffix, VTypes::DataType dt, int arraysize)
{
	static char name[128];
	strcpy(name,prefix);
	if (suffix[0] != '\0')
	{
		if (prefix[0] != '\0') strcat(name,".");
		strcat(name,suffix);
	}
	Variable *newvar = createVariable(dt, arraysize);
	vars_.own(newvar);
	newvar->setName(name);
	return newvar;
}

// Add pointer to pointer reference variable
Variable *VariableList::addBundlePointer(const char *name, VTypes::DataType dt)
{
	Variable *newvar = new BundleVariable(dt);
	newvar->setParent(this);
	newvar->setName(name);
	vars_.own(newvar);
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
	newvar->setReadOnly();
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
	// Create new variable in which to store expression
	Variable *newvar = createVariable(VTypes::ExpressionData);
	static char newname[24];
	expressions_.own(newvar);
	sprintf(newname,"expression%i", expressions_.nItems());
	newvar->setName(newname);
	// Cast to ExpressionVariable and initialise it
	ExpressionVariable *result = (ExpressionVariable*) newvar;
	if (!result->initialise(s))
	{
		msg.print( "Failed to cache expression.\n");
		return NULL;
	}
	return result;
}

// Add variable acces path
Variable *VariableList::addPath(const char *s)
{
	// Create new variable in which to store the path
	AccessPath *newvar = new AccessPath;
	newvar->setParent(this);
	paths_.own(newvar);
	if (!newvar->setPath(s))
	{
		msg.print( "Failed to cache variable/path.\n");
		return NULL;
	}
	return newvar;
}

/*
// Set Methods
*/

// Set existing variable (character)
void VariableList::set(const char *name, const char *value)
{
	set(name,"",value);
}
void VariableList::set(const char *prefix, const char *suffix, const char *value)
{
	Variable *v = get(prefix, suffix);
	if (v == NULL) printf("CRITICAL - Variable %s[.%s] does not exist in the variable list.\n", prefix, suffix);
	v->set(value);
}

// Set existing variable (integer)
void VariableList::set(const char *name, int value)
{
	set(name,"",value);
}

void VariableList::set(const char *prefix, const char *suffix, int value)
{
	Variable *v = get(prefix, suffix);
	if (v == NULL) printf("CRITICAL - Variable %s[.%s] does not exist in the variable list.\n", prefix, suffix);
	v->set(value);
}

// Set existing variable (real)
void VariableList::set(const char *name, double value)
{
	set(name,"",value);
}
void VariableList::set(const char *prefix, const char *suffix, double value)
{
	Variable *v = get(prefix, suffix);
	if (v == NULL) printf("CRITICAL - Variable %s[.%s] does not exist in the variable list.\n", prefix, suffix);
	v->set(value);
}

// Set existing variable (pointer data)
void VariableList::set(const char *name, void *ptr, VTypes::DataType dt)
{
	set(name,"",ptr, dt);
}
void VariableList::set(const char *prefix, const char *suffix, void *ptr, VTypes::DataType dt)
{
	Variable *v = get(prefix, suffix);
	if (v == NULL) printf("CRITICAL - Variable %s[.%s] does not exist in the variable list.\n", prefix, suffix);
	v->set(ptr, dt);
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
	static char name[256];
	strcpy(name,prefix);
	if (suffix[0] != '\0')
	{
		if (prefix[0] != '\0') strcat(name,".");
		strcat(name, suffix);
	}
	for (Variable *v = vars_.first(); v != NULL; v = v->next) if (strcmp(name,v->name()) == 0) return v;
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
	//for (Variable *v = vars_.first(); v != NULL; v = v->next) v->reset();
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
// 	get(s)->reset();
	do
	{
		strcpy(name,va_arg(namelist,char*));
		if (name[0] != '\0')
		{
			v = get(name);
// 			if (v == NULL) printf("VariableList::reset <<<< '%s' not in list >>>>\n",name);
// 			else v->reset();
		}
	} while (name[0] != '\0');
	msg.exit("VariableList::reset");
}
