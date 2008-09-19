/*
	*** Model Access
	*** src/variables/modelaccess.cpp
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

#include "variables/modelaccess.h"
#include "variables/vaccess.h"

ModelAccessors modelAccessors;

// Constructor
ModelAccessors::ModelAccessors()
{
// 	addAccessor("atoms",		VObject::ListArray,	VObject::AtomData,	FALSE);
 	accessorPointers[ModelAccessors::Name] = addAccessor("name",		VTypes::CharacterData,	TRUE);
// 	addAccessor("natoms",		VObject::NoArray,	VObject::IntegerData,	FALSE);
};

// Retrieve specified data
bool ModelAccessors::findAccessor(void *classptr, Variable *accessor, ReturnValue &rv)
{
	msg.enter("ModelAccessors::findAccessor");
	bool result = TRUE;
	// Search through list of accessors to get enumerated value
// 	for (Variable *v = 
	msg.exit("ModelAccessors::findAccessor");
	return result;
}
