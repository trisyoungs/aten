/*
	*** Character Variable
	*** src/parser/character.cpp
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

#include "parser/character.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor
NuCharacterVariable::NuCharacterVariable(const char *s, bool constant) : characterData_(s)
{
	// Private variables
	returnType_ = VTypes::CharacterData;
	readOnly_ = constant;
}

// Destructor
NuCharacterVariable::~NuCharacterVariable()
{
}

/*
// Set / Get
*/

// Set value of variable (character)
bool NuCharacterVariable::set(const char *s)
{
	characterData_ = s;
	return TRUE;
}

// Clears value of variable
bool NuCharacterVariable::reset(NuVariable *index)
{
	characterData_.clear();
	return TRUE;
}

// Return value of node
int NuCharacterVariable::execute(NuReturnValue &rv)
{
	rv.set(characterData_.get());
	return TRUE;
}
