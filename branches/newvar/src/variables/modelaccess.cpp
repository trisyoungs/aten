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
#include "variables/accessstep.h"
#include "variables/vaccess.h"
#include "model/model.h"
#include "base/messenger.h"

ModelAccessors modelAccessors;

// Constructor
ModelAccessors::ModelAccessors()
{
 	accessorPointers[ModelAccessors::Atoms] = addListAccessor("atoms",		VTypes::AtomData);
 	accessorPointers[ModelAccessors::Cell] = addAccessor("cell",		VTypes::CellData, TRUE);
 	accessorPointers[ModelAccessors::Name] = addAccessor("name",		VTypes::CharacterData,	FALSE);
 	accessorPointers[ModelAccessors::NAtoms] = addAccessor("natoms",		VTypes::IntegerData,	TRUE);
};

// Retrieve specified data
bool ModelAccessors::retrieve(void *classptr, AccessStep *step, ReturnValue &rv)
{
	msg.enter("ModelAccessors::retrieve");
	bool result = TRUE;
	// Cast pointer into Model*
	Model *m = (Model*) classptr;
	if (m == NULL) printf("Warning - NULL Model pointer passed to ModelAccessors::retrieve.\n");
// 	printf("Enumerated ID supplied to ModelAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > ModelAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to ModelAccessors::retrieve.\n", vid);
		msg.exit("ModelAccessors::retrieve");
		return FALSE;
	} 
	// Retrieve value based on enumerated id
	switch (vid)
	{
		case (ModelAccessors::Atoms):
			rv.set(m->atoms(), VTypes::AtomData);
			break;
		case (ModelAccessors::Cell):
			rv.set(m->cell(), VTypes::CellData);
			break;
		case (ModelAccessors::Name):
			rv.set(m->name());
			break;
		case (ModelAccessors::NAtoms):
			rv.set(m->nAtoms());
			break;
		default:
			printf("ModelAccessors::retrieve doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("ModelAccessors::retrieve");
	return result;
}

// Set specified data
bool ModelAccessors::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("ModelAccessors::set");
	bool result = TRUE;
	// Cast pointer into Model*
	Model *m = (Model*) classptr;
	if (m == NULL) printf("Warning - NULL Model pointer passed to ModelAccessors::set.\n");
// 	printf("Enumerated ID supplied to ModelAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > ModelAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to ModelAccessors::set.\n", vid);
		msg.exit("ModelAccessors::set");
		return FALSE;
	} 
	// Set value based on enumerated id
	switch (vid)
	{
		case (ModelAccessors::Name):
			m->setName(srcvar->asCharacter());
			break;
		case (ModelAccessors::Atoms):
		case (ModelAccessors::Cell):
		case (ModelAccessors::NAtoms):
			msg.print("Member '%s' in Model is read-only.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
		default:
			printf("ModelAccessors::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("ModelAccessors::set");
	return result;
}
