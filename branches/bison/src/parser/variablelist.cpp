/*
	*** Variable List
	*** src/parser/variablelist.cpp
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

#include "parser/variablelist.h"
#include "parser/array.h"
#include "parser/aten.h"
#include "parser/atom.h"
#include "parser/bond.h"
#include "parser/cell.h"
#include "parser/character.h"
#include "parser/elements.h"
#include "parser/integer.h"
#include "parser/forcefield.h"
#include "parser/forcefieldatom.h"
#include "parser/forcefieldbound.h"
#include "parser/model.h"
#include "parser/real.h"
#include "parser/vector.h"
#include <string.h>
// #include <stdarg.h>

// Constructor
NuVariableList::NuVariableList()
{
	// Add accessors to model list and current model/frame in Aten
//	Variable *v;
//	v = addVariable("header", VTypes::CharacterData);
//	v->set("false");
//	v = addVariable("infile", VTypes::CharacterData);
//	v->set("none");
//	v = addVariable("outfile", VTypes::CharacterData);
//	v->set("none");
//	v = addVariable("prefs", VTypes::PrefsData);
}

// Pass a newly-created variable / constant to the list for it to take ownership of
void NuVariableList::take(NuVariable *v)
{
	// Check the readonly status to determine where we put it
	if (v->readOnly()) constants_.own(v);
	else variables_.own(v);
}

// Retrieve a named variable from the list
NuVariable *NuVariableList::find(const char *name)
{
	NuVariable *result = NULL;
	for (result = variables_.first(); result != NULL; result = result->next) if (strcmp(name,result->name()) == 0) break;
	return result;
}

// Create a new variable in the list
NuVariable *NuVariableList::createVariable(NuVTypes::DataType type, const char *name, TreeNode *initialValue)
{
	NuVariable *v = NULL;
	switch (type)
	{
		case (NuVTypes::NoData):
			printf("No data type passed to VariableList::create().\n");
			break;
		case (NuVTypes::IntegerData):
			v = (NuVariable*) new NuIntegerVariable(0, FALSE);
			break;
		case (NuVTypes::RealData):
			v = (NuVariable*) new NuRealVariable(0.0, FALSE);
			break;
		case (NuVTypes::CharacterData):
			v = (NuVariable*) new NuCharacterVariable("", FALSE);
			break;
		case (NuVTypes::VectorData):
			v = (NuVariable*) new NuVectorVariable(FALSE);
			break;
		case (NuVTypes::AtenData):
			v = (NuVariable*) new AtenVariable();
			break;
		case (NuVTypes::AtomData):
			v = (NuVariable*) new AtomVariable(NULL, FALSE);
			break;
		case (NuVTypes::BondData):
			v = (NuVariable*) new BondVariable(NULL, FALSE);
			break;
		case (NuVTypes::CellData):
			v = (NuVariable*) new CellVariable(NULL, FALSE);
			break;
		case (NuVTypes::ElementsData):
			v = (NuVariable*) new ElementsVariable();
			break;
		case (NuVTypes::ForcefieldData):
			v = (NuVariable*) new ForcefieldVariable(NULL, FALSE);
			break;
		case (NuVTypes::ForcefieldAtomData):
			v = (NuVariable*) new ForcefieldAtomVariable(NULL, FALSE);
			break;
		case (NuVTypes::ForcefieldBoundData):
			v = (NuVariable*) new ForcefieldBoundVariable(NULL, FALSE);
			break;
		case (NuVTypes::ModelData):
			v = (NuVariable*) new ModelVariable(NULL, FALSE);
			break;
		default:
			printf("Don't know how to VariableList::create() of type %s.\n", NuVTypes::dataType(type));
			break;
	} 
	if (v != NULL)
	{
		v->setName(name);
		v->setInitialValue(initialValue);
	}
	return v;
}

// Create variable
NuVariable *NuVariableList::create(NuVTypes::DataType type, const char *name, TreeNode *initialValue)
{
	NuVariable *v = createVariable(type, name, initialValue);
	if (v != NULL) variables_.own(v);
	return v;
}

// Create variable without owning it
NuVariable *NuVariableList::createFree(NuVTypes::DataType type, const char *name, TreeNode *initialValue)
{
	return createVariable(type, name, initialValue);
}

// Create a new array variable in the list
NuVariable *NuVariableList::createArray(NuVTypes::DataType type, const char *name, TreeNode *sizeexpr, TreeNode *initialValue)
{
	ArrayVariable *array = new ArrayVariable(type, sizeexpr);
	variables_.own(array);
	array->setName(name);
	array->setInitialValue(initialValue);
	return array;
}

// Create a constant from supplied integer data
NuVariable *NuVariableList::createConstant(int i)
{
	NuIntegerVariable *var = new NuIntegerVariable(i, TRUE);
	constants_.own(var);
	return var;
}

// Create a constant from supplied real data
NuVariable *NuVariableList::createConstant(double d)
{
	NuRealVariable *var = new NuRealVariable(d, TRUE);
	constants_.own(var);
	return var;
}

// Create a constant from supplied character data
NuVariable *NuVariableList::createConstant(const char *s)
{
	NuCharacterVariable *var = new NuCharacterVariable(s, TRUE);
	constants_.own(var);
	return var;
}

// Return the number of variables (not constants) contained in the list
int NuVariableList::nVariables()
{
	return variables_.nItems();
}

// Return first variable in the list
NuVariable *NuVariableList::first()
{
	return variables_.first();
}

// Initialise/reset all variables
void NuVariableList::initialise()
{
	for (NuVariable *v = variables_.first(); v != NULL; v = v->next) v->initialise();
}
