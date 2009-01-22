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
#include "variables/accessstep.h"
#include "variables/vaccess.h"
#include "model/model.h"
#include "base/cell.h"
#include "base/spacegroup.h"
#include "base/messenger.h"
#include "base/sysfunc.h"

CellAccessors cellAccessors;

// Constructor
CellAccessors::CellAccessors()
{
	accessorPointers[CellAccessors::A] = addAccessor("a",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::B] = addAccessor("b",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::C] = addAccessor("c",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::Alpha] = addAccessor("alpha",	VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::Beta] = addAccessor("beta",	VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::Gamma] = addAccessor("gamma",	VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::AX] = addAccessor("ax",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::AY] = addAccessor("ay",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::AZ] = addAccessor("az",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::BX] = addAccessor("bx",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::BY] = addAccessor("by",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::BZ] = addAccessor("bz",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::CX] = addAccessor("cx",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::CY] = addAccessor("cy",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::CZ] = addAccessor("cz",		VTypes::RealData, FALSE);
	accessorPointers[CellAccessors::CentreX] = addAccessor("centrex",	VTypes::RealData, TRUE);
	accessorPointers[CellAccessors::CentreY] = addAccessor("centrey",	VTypes::RealData, TRUE);
	accessorPointers[CellAccessors::CentreZ] = addAccessor("centrez",	VTypes::RealData, TRUE);
	accessorPointers[CellAccessors::Density] = addAccessor("density", VTypes::RealData, TRUE);
	accessorPointers[CellAccessors::Matrix] = addAccessor("matrix", VTypes::RealData, FALSE, 9);
	accessorPointers[CellAccessors::SpacegroupId] = addAccessor("sgid",	VTypes::IntegerData, FALSE);
	accessorPointers[CellAccessors::SpacegroupName] = addAccessor("sgname",	VTypes::CharacterData, FALSE);
	accessorPointers[CellAccessors::SpacegroupSetting] = addAccessor("sgsetting",	VTypes::IntegerData, FALSE);
	accessorPointers[CellAccessors::Type] = addAccessor("type",	VTypes::CharacterData, TRUE);
	accessorPointers[CellAccessors::Volume] = addAccessor("volume", VTypes::RealData, TRUE);
};

// Retrieve specified data
bool CellAccessors::retrieve(void *classptr, AccessStep *step, ReturnValue &rv)
{
	msg.enter("CellAccessors::retrieve");
	bool result = TRUE;
	// Cast pointer into Cell*
	Cell *c = (Cell*) classptr;
	if (c == NULL) printf("Warning - NULL Cell pointer passed to CellAccessors::retrieve.\n");
// 	printf("Enumerated ID supplied to CellAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > CellAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to CellAccessors::retrieve.\n", vid);
		msg.exit("CellAccessors::retrieve");
		return FALSE;
	} 
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("CellAccessors::retrieve");
		return FALSE;
	}
	// Retrieve value based on enumerated id
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
			rv.set(c->axes().element(vid - CellAccessors::AX));
			break;
		case (CellAccessors::CentreX):
		case (CellAccessors::CentreY):
		case (CellAccessors::CentreZ):
			rv.set(c->centre().get(vid - CellAccessors::CentreX));
			break;
		case (CellAccessors::Density):
			rv.set(c->density());
			break;
		case (CellAccessors::Matrix):
			rv.set(c->axes().element(index-1));
			break;
		case (CellAccessors::SpacegroupId):
			rv.set(c->spacegroup());
			break;
		case (CellAccessors::SpacegroupName):
			rv.set(spacegroups.name(c->spacegroup()));
			break;
		case (CellAccessors::SpacegroupSetting):
			rv.set(c->spacegroupSetting());
			break;
		case (CellAccessors::Type):
			rv.set(lowerCase(Cell::cellType(c->type())));
			break;
		case (CellAccessors::Volume):
			rv.set(c->volume());
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
bool CellAccessors::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("CellAccessors::set");
	bool result = TRUE;
	// Cast pointer into Cell*
	Cell *c = (Cell*) classptr;
	if (c == NULL) printf("Warning - NULL Cell pointer passed to CellAccessors::set.\n");
// 	printf("Enumerated ID supplied to CellAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > CellAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to CellAccessors::set.\n", vid);
		msg.exit("CellAccessors::set");
		return FALSE;
	}
	// Check read-only status
	if (accessorPointers[vid]->readOnly())
	{
		msg.print("Member '%s' of 'cell' type is read-only.\n", accessorPointers[vid]->name());
		msg.exit("CellAccessors::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("CellAccessors::set");
		return FALSE;
	}
	Mat3<double> m;
	Vec3<double> v;
	// Set value based on enumerated id
	switch (vid)
	{
		case (CellAccessors::A):
		case (CellAccessors::B):
		case (CellAccessors::C):
		case (CellAccessors::Alpha):
		case (CellAccessors::Beta):
		case (CellAccessors::Gamma):
		case (CellAccessors::AX):
		case (CellAccessors::AY):
		case (CellAccessors::AZ):
		case (CellAccessors::BX):
		case (CellAccessors::BY):
		case (CellAccessors::BZ):
		case (CellAccessors::CX):
		case (CellAccessors::CY):
		case (CellAccessors::CZ):
			// Cast vid into a CellParameter
			c->parent()->setCell( (Cell::CellParameter) vid, srcvar->asDouble());
			break;
		case (CellAccessors::Matrix):
			// Cast vid into a CellParameter
			c->parent()->setCell( (Cell::CellParameter) ((index-1) + Cell::CellAX), srcvar->asDouble());
			break;
		default:
			printf("CellAccessors::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("CellAccessors::set");
	return result;
}
