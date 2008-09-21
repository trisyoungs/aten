/*
	*** Cell Access
	*** src/variables/cellaccess.cpp
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

#include "variables/cellaccess.h"
#include "variables/vaccess.h"
#include "base/cell.h"
#include "base/messenger.h"

CellAccessors cellAccessors;

// Constructor
CellAccessors::CellAccessors()
{
 	accessorPointers[CellAccessors::A] = addAccessor("a",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::B] = addAccessor("b",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::C] = addAccessor("c",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::Alpha] = addAccessor("alpha",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::Beta] = addAccessor("beta",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::Gamma] = addAccessor("gamma",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::AX] = addAccessor("ax",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::AY] = addAccessor("ay",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::AZ] = addAccessor("az",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::BX] = addAccessor("bx",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::BY] = addAccessor("by",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::BZ] = addAccessor("bz",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::CX] = addAccessor("cx",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::CY] = addAccessor("cy",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::CZ] = addAccessor("cz",		VTypes::RealData, FALSE);
 	accessorPointers[CellAccessors::Type] = addAccessor("type",		VTypes::CharacterData, TRUE);
};

// Retrieve specified data
bool CellAccessors::retrieve(void *classptr, int vid, ReturnValue &rv)
{
	msg.enter("CellAccessors::retrieve");
	bool result = TRUE;
	// Cast pointer into Model*
	Cell *c = (Cell*) classptr;
	if (c == NULL) printf("Warning - NULL Cell pointer passed to CellAccessors::retrieve.\n");
	// Search through list of accessors to get enumerated value
// 	printf("Enumerated ID supplied to CellAccessors is %i.\n", vid);
	switch (vid)
	{
		case (CellAccessors::A):
		case (CellAccessors::B):
		case (CellAccessors::C):
			rv.set(c->lengths().get(vid - CellAccessors::A));
			break;
		case (CellAccessors::Alpha):
		case (CellAccessors::Beta):
		case (CellAccessors::Gamma):
			rv.set(c->angles().get(vid - CellAccessors::Alpha));
			break;
		case (CellAccessors::AX):
		case (CellAccessors::AY):
		case (CellAccessors::AZ):
		case (CellAccessors::BX):
		case (CellAccessors::BY):
		case (CellAccessors::BZ):
		case (CellAccessors::CX):
		case (CellAccessors::CY):
		case (CellAccessors::CZ):
			rv.set(c->axes().getElement(vid - CellAccessors::AX));
			break;
		case (CellAccessors::Type):
			rv.set(Cell::cellType(c->type()));
			break;
		default:
			printf("Unknown enumeration %i given to CellAccessors::retrieve.\n", vid);
			result = FALSE;
			break;
	}
	msg.exit("CellAccessors::retrieve");
	return result;
}

// Set specified data
bool CellAccessors::set(void *classptr, int vid, Variable *srcvar)
{
	msg.enter("CellAccessors::set");
	bool result = TRUE;
	// Cast pointer into Model*
	Cell *c = (Cell*) classptr;
	if (c == NULL) printf("Warning - NULL Cell pointer passed to CellAccessors::set.\n");
	// Search through list of accessors to get enumerated value
// 	printf("Enumerated ID supplied to CellAccessors is %i.\n", vid);
	// Check range of supplied vid
	if ((vid < 0) || (vid > CellAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to CellAccessors::set.\n", vid);
		msg.exit("CellAccessors::set");
		return FALSE;
	} 
	// For all values other than Type (which is read-only) cast vid into a CellParameter
	if (vid == CellAccessors::Type)
	{
		msg.print("Member '%s' in Cell is read-only.\n", accessorPointers[vid]->name());
		result = FALSE;
	}
	else
	{
		Cell::CellParameter cp = (Cell::CellParameter) vid;
		c->setParameter(cp, srcvar->asDouble());
	}
	msg.exit("CellAccessors::set");
	return result;
}
