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
#include "variables/pointerlist.h"
#include "variables/bundle.h"
#include "classes/forcefieldbound.h"
#include "classes/forcefieldatom.h"
#include "main/aten.h"
#include <string.h>
#include <stdarg.h>

// Constructor
VariableList::VariableList()
{
	// Add accessors to model list and current model/frame in Aten
	Variable *v;
	v = addVariable("header", VTypes::CharacterData);
	v->set("false");
	v = addBundlePointer("model", VTypes::ModelData);
	v->set(&aten.current, VTypes::ModelData);
	v = addListVariable("models", VTypes::ModelData, aten.modelList());
	v = addVariable("prefs", VTypes::PrefsData);
}

// Return list position (id) of Variable in list
int VariableList::variableId(Variable *v)
{
	return variables_.indexOf(v);
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
		case (VTypes::AtomData):
		case (VTypes::BondData):
		case (VTypes::PatternData):
		case (VTypes::ModelData):
		case (VTypes::GridData):
		case (VTypes::PatternBoundData):
		case (VTypes::ForcefieldBoundData):
		case (VTypes::ForcefieldAtomData):
		case (VTypes::ForcefieldData):
		case (VTypes::CellData):
		case (VTypes::PrefsData):
			result = new PointerVariable(dt);
			break;
		default:
			printf("Don't yet know how to create a variable of type '%s'\n", VTypes::dataType(dt));
			break;
	}
	if (result != NULL)
	{
		result->setParent(this);
		// Create array if a valid size was given
		if (arraysize != -1)
		{
			if (arraysize > 0) result->setArraySize(arraysize);
			else
			{
				msg.print("Invalid array size (%i) given for variable.\n", arraysize);
				return NULL;
			}
		}
	}
	return result;
}

// Add named variable
Variable *VariableList::addVariable(const char *name, VTypes::DataType dt, int arraysize)
{
	Variable *newvar = createVariable(dt, arraysize);
	variables_.own(newvar);
	newvar->setName(name);
	return newvar;
}

// Add pointer to pointer reference variable
Variable *VariableList::addBundlePointer(const char *name, VTypes::DataType dt)
{
	Variable *newvar = new BundleVariable(dt);
	newvar->setParent(this);
	newvar->setName(name);
	variables_.own(newvar);
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
	ExpressionVariable *newvar = new ExpressionVariable;
	newvar->setParent(this);
	static char newname[24];
	expressions_.own(newvar);
	sprintf(newname,"expression%i", expressions_.nItems());
	newvar->setName(newname);
	// Cast to ExpressionVariable and initialise it
	if (!newvar->initialise(s))
	{
		msg.print( "Failed to cache expression.\n");
		return NULL;
	}
	return newvar;
}

// Add variable acces path
Variable *VariableList::addPath(const char *s)
{
	// Create new variable in which to store the path
	AccessPath *newvar = new AccessPath;
	newvar->setParent(this);
	paths_.own(newvar);
	if (!newvar->setPath(s)) return NULL;
	return newvar;
}

// Add List<> type variable
Variable *VariableList::addListVariable(const char *name, VTypes::DataType vt, void *ptr)
{
	Variable *newvar;
	switch (vt)
	{
		case (VTypes::CharacterData):
		case (VTypes::IntegerData):
		case (VTypes::RealData):
		case (VTypes::CellData):
			printf("Cannot create a list variable in this VariableList since '%s' is a non-pointer/unsuitable type.\n", VTypes::dataType(vt));
			newvar = NULL;
			break;
		case (VTypes::AtomData):
			newvar = new PointerListVariable<Atom>(vt, (List<Atom>*) ptr);
			break;
		case (VTypes::BondData):
			newvar = new PointerListVariable<Bond>(vt, (List<Bond>*) ptr);
			break;
		case (VTypes::PatternData):
			newvar = new PointerListVariable<Pattern>(vt, (List<Pattern>*) ptr);
			break;
		case (VTypes::ModelData):
			newvar = new PointerListVariable<Model>(vt, (List<Model>*) ptr);
			break;
		case (VTypes::GridData):
			newvar = new PointerListVariable<Grid>(vt, (List<Grid>*) ptr);
			break;
		case (VTypes::ForcefieldBoundData):
			newvar = new PointerListVariable<ForcefieldBound>(vt, (List<ForcefieldBound>*) ptr);
			break;
		case (VTypes::ForcefieldAtomData):
			newvar = new PointerListVariable<ForcefieldAtom>(vt, (List<ForcefieldAtom>*) ptr);
			break;
		default:
			printf("Don't yet know how to create a list variable of type '%s'\n", VTypes::dataType(vt));
			newvar = NULL;
			break;
	}
	if (newvar != NULL)
	{	
		variables_.own(newvar);
		newvar->setName(name);
		newvar->setListArray();
	}
	return newvar;
}

/*
// Set Methods
*/

// Set existing variable (character)
void VariableList::set(const char *name, const char *value)
{
	Variable *v = get(name);
	if (v == NULL) printf("CRITICAL - Variable %s does not exist in the variable list.\n", name);
	else v->set(value);
}

// Set existing variable (integer)
void VariableList::set(const char *name, int value)
{
	Variable *v = get(name);
	if (v == NULL) printf("CRITICAL - Variable %s does not exist in the variable list.\n", name);
	else v->set(value);
}

// Set existing variable (real)
void VariableList::set(const char *name, double value)
{
	Variable *v = get(name);
	if (v == NULL) printf("CRITICAL - Variable %s does not exist in the variable list.\n", name);
	v->set(value);
}

// Set existing variable (pointer data)
void VariableList::set(const char *name, void *ptr, VTypes::DataType dt)
{
	Variable *v = get(name);
	if (v == NULL) printf("CRITICAL - Variable %s does not exist in the variable list.\n", name);
	else v->set(ptr, dt);
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
	for (Variable *v = variables_.first(); v != NULL; v = v->next) if (strcmp(name,v->name()) == 0) return v;
	return NULL;
}

/*
// Misc
*/

// Print list of variables in list
void VariableList::print()
{
	for (Variable *v = variables_.first(); v != NULL; v = v->next)
		printf("VAR=[%s] (%li) VALUE=[%s]\n",v->name(),v,v->asCharacter());
}

// Reset all variable values
void VariableList::resetVariables()
{
	for (Variable *v = variables_.first(); v != NULL; v = v->next) v->reset();
}

// Clear all variables, expressions etc. stored in class
void VariableList::clear()
{
	variables_.clear();
	expressions_.clear();
	constants_.clear();
	paths_.clear();
}
