/*
	*** Vector Access
	*** src/variables/vectoraccess.cpp
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

#include "variables/vectoraccess.h"
#include "variables/accessstep.h"
#include "variables/vaccess.h"
#include "base/messenger.h"

VectorAccessors vectorAccessors;

// Constructor
VectorAccessors::VectorAccessors()
{
	accessorPointers[VectorAccessors::Magnitude] = addAccessor("mag",VTypes::RealData, TRUE);
	accessorPointers[VectorAccessors::X] = addAccessor("x",	VTypes::RealData, FALSE);
	accessorPointers[VectorAccessors::Y] = addAccessor("y",	VTypes::RealData, FALSE);
	accessorPointers[VectorAccessors::Z] = addAccessor("z",	VTypes::RealData, FALSE);
};

// Retrieve specified data
bool VectorAccessors::retrieve(void *classptr, AccessStep *step, ReturnValue &rv)
{
	msg.enter("VectorAccessors::retrieve");
	bool result = TRUE;
	// Cast pointer into VectorVariable
	Vec3<double> *v = (Vec3<double>*) classptr;
	if (v == NULL) printf("Warning - NULL Vector pointer passed to VectorAccessors::retrieve.\n");
// 	printf("Enumerated ID supplied to VectorAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > VectorAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to VectorAccessors::set.\n", vid);
		msg.exit("VectorAccessors::retrieve");
		return FALSE;
	} 
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("VectorAccessors::retrieve");
		return FALSE;
	}
	// Retrieve value based on enumerated id
	switch (vid)
	{
		case (VectorAccessors::Magnitude):
			rv.set(v->magnitude());
			break;
		case (VectorAccessors::X):
			rv.set(v->x);
			break;
		case (VectorAccessors::Y):
			rv.set(v->y);
			break;
		case (VectorAccessors::Z):
			rv.set(v->z);
			break;
		default:
			printf("VectorAccessors::retrieve doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("VectorAccessors::retrieve");
	return result;
}

// Set specified data
bool VectorAccessors::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("VectorAccessors::set");
	bool result = TRUE;
	// Cast pointer into Vec3<double>*
	Vec3<double> *v = (Vec3<double>*) classptr;
	if (v == NULL) printf("Warning - NULL Vector pointer passed to VectorAccessors::set.\n");
// 	printf("Enumerated ID supplied to VectorAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > VectorAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to VectorAccessors::set.\n", vid);
		msg.exit("VectorAccessors::set");
		return FALSE;
	} 
	// Check read-only status
	if (accessorPointers[vid]->readOnly())
	{
		msg.print("Member '%s' of 'vector' type is read-only.\n", accessorPointers[vid]->name());
		msg.exit("VectorAccessors::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("VectorAccessors::set");
		return FALSE;
	}
	// Set value based on enumerated id
	switch (vid)
	{
		case (VectorAccessors::X):
			v->x = srcvar->asDouble();
			break;
		case (VectorAccessors::Y):
			v->y = srcvar->asDouble();
			break;
		case (VectorAccessors::Z):
			v->z = srcvar->asDouble();
			break;
		default:
			printf("VectorAccessors::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("VectorAccessors::set");
	return result;
}
