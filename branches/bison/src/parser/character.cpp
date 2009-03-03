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
#include <string.h>

// Constructor
NuCharacterVariable::NuCharacterVariable(const char *s, bool constant) : characterData_(s)
{
	// Private variables
	returnType_ = NuVTypes::CharacterData;
	readOnly_ = constant;
}

// Destructor
NuCharacterVariable::~NuCharacterVariable()
{
}

/*
// Set / Get
*/

// Set value of node from returnvalue
bool NuCharacterVariable::set(NuReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a character) cannot be assigned to.\n");
		return FALSE;
	}
	characterData_ = rv.asCharacter();
	return TRUE;
}

// Reset node
void NuCharacterVariable::reset()
{
	characterData_.clear();
}

// Return value of node
bool NuCharacterVariable::execute(NuReturnValue &rv)
{
	rv.set(characterData_.get());
	return TRUE;
}

// Print node contents
void NuCharacterVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("%s\"%s\" (constant value)\n", tab, characterData_.get());
	else printf("%s\"%s\" (variable, name=%s)\n", tab, characterData_.get(), name_.get());
	delete[] tab;
}

/*
// Accessors
*/

// Search variable access list for provided accessor
TreeNode *NuCharacterVariable::findAccessor(const char *s)
{
	msg.enter("NuCharacterVariable::searchAccessors");
	TreeNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.exit("NuCharacterVariable::searchAccessors");
		return NULL;
	}
	// Create a suitable variable to return...
	
	msg.exit("NuCharacterVariable::searchAccessors");
	return result;
}
