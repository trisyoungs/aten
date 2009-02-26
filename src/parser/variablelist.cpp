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
#include "parser/integer.h"
#include "parser/character.h"
#include "parser/real.h"
// #include "parser/vector.h"
#include <string.h>
#include <stdarg.h>

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
//	v = addBundlePointer("model", VTypes::ModelData);
//	v->set(&aten.current, VTypes::ModelData);
//	v = addListVariable("models", VTypes::ModelData, aten.modelList());
//	v = addVariable("prefs", VTypes::PrefsData);
//	v = addVariable("elements", VTypes::ElementsData);
//	v = addSpecialVariable("nmodels", SpecialVariable::SpecialNModels);
//	v = addSpecialVariable("random", SpecialVariable::SpecialRandom);
}

// Pass a newly-created variable / constant to the list for it to take ownership of
void NuVariableList::take(NuVariable *v)
{
	// Check the readonly status to determine where we put it
	if (v->readOnly()) constants_.own(v);
	else variables_.own(v);
}

// Retrieve a named variable from the list
NuVariable *NuVariableList::get(const char *name)
{
}
