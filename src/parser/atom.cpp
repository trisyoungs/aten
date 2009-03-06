/*
	*** Atom Variable
	*** src/parser/atom.cpp
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

#include "parser/atom.h"
#include "parser/stepnode.h"
#include "base/atom.h"
#include "base/elements.h"
#include <string.h>

// Constructor
AtomVariable::AtomVariable(Atom *ptr, bool constant) : atomData_(ptr)
{
	// Private variables
	returnType_ = NuVTypes::AtomData;
	readOnly_ = constant;
}

// Destructor
AtomVariable::~AtomVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool AtomVariable::set(NuReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a atom&) cannot be assigned to.\n");
		return FALSE;
	}
	bool success;
	atomData_ = rv.asPointer(NuVTypes::AtomData, success);
	return success;
}

// Reset variable
void AtomVariable::reset()
{
	atomData_ = NULL;
}

// Return value of node
bool AtomVariable::execute(NuReturnValue &rv)
{
	// If this vector is a constant, read the three stored expressions to recreate it
	rv.set(NuVTypes::AtomData, atomData_);
	return TRUE;
}

// Print node contents
void AtomVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("%s%li (atom) (constant value)\n", tab, atomData_);
	else printf("%s%li (atom) (variable, name=%s)\n", tab, atomData_, name_.get());
	delete[] tab;
}

/*
// Accessors
*/

// Accessor data
Accessor AtomVariable::accessorData[AtomVariable::nAccessors] = {
	{ "fixed", 	NuVTypes::IntegerData,		FALSE, FALSE },
	{ "f",		NuVTypes::VectorData,		FALSE, FALSE },
	{ "fx",		NuVTypes::RealData,		FALSE, FALSE },
	{ "fy",		NuVTypes::RealData,		FALSE, FALSE },
	{ "fz",		NuVTypes::RealData,		FALSE, FALSE },
	{ "hidden",	NuVTypes::IntegerData,		FALSE, FALSE },
	{ "id",		NuVTypes::IntegerData,		FALSE, TRUE },
	{ "mass",	NuVTypes::RealData,		FALSE, TRUE },
	{ "name",	NuVTypes::CharacterData,	FALSE, TRUE },
	{ "q",		NuVTypes::RealData,		FALSE, FALSE },
	{ "r",		NuVTypes::VectorData,		FALSE, FALSE },
	{ "rx",		NuVTypes::RealData,		FALSE, FALSE },
	{ "ry",		NuVTypes::RealData,		FALSE, FALSE },
	{ "rz",		NuVTypes::RealData,		FALSE, FALSE },
	{ "selected",	NuVTypes::IntegerData,		FALSE, FALSE },
	{ "symbol",	NuVTypes::CharacterData,	FALSE, TRUE },
	{ "type",	NuVTypes::ForcefieldAtomData,	FALSE, FALSE },
	{ "v",		NuVTypes::VectorData,		FALSE, FALSE },
	{ "vx",		NuVTypes::RealData,		FALSE, FALSE },
	{ "vy",		NuVTypes::RealData,		FALSE, FALSE },
	{ "vz",		NuVTypes::RealData,		FALSE, FALSE },
	{ "z",		NuVTypes::IntegerData, 		FALSE, TRUE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *AtomVariable::findAccessor(const char *s)
{
	return AtomVariable::accessorSearch(s);
}

// Private static function to search accessors
StepNode *AtomVariable::accessorSearch(const char *s)
{
	msg.enter("AtomVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'atom&' has no member named '%s'.\n", s);
		msg.exit("AtomVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	printf("Accessor match = %i\n", i);
	result = new StepNode(i, NuVTypes::AtomData, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("AtomVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool AtomVariable::retrieveAccessor(int i, NuReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("AtomVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Atom type.\n");
		msg.exit("AtomVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if (!accessorData[i].isArray)
	{
		if (hasArrayIndex) msg.print("Warning: Irrelevent array index provided for member '%s'.\n", accessorData[i].name);
	}
	else if (!hasArrayIndex)
	{
		msg.print("Error: No array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("AtomVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Atom *ptr= (Atom*) rv.asPointer(NuVTypes::AtomData, result);
	if (result) switch (acc)
	{
		case (AtomVariable::Fixed):
			rv.set(ptr->isPositionFixed());
			break;
		case (AtomVariable::F):
			rv.set(ptr->f());
			break;
		case (AtomVariable::FX):
		case (AtomVariable::FY):
		case (AtomVariable::FZ):
			rv.set(ptr->f().get(acc - AtomVariable::FX));
			break;
		case (AtomVariable::Hidden):
			rv.set(ptr->isHidden());
			break;
		case (AtomVariable::Id):
			rv.set(ptr->id()+1);
			break;
		case (AtomVariable::Mass):
			rv.set(elements().atomicMass(i));
			break;
		case (AtomVariable::Name):
			rv.set(elements().name(i));
			break;
		case (AtomVariable::Q):
			rv.set(ptr->charge());
			break;
		case (AtomVariable::R):
			rv.set(ptr->r());
			break;
		case (AtomVariable::RX):
		case (AtomVariable::RY):
		case (AtomVariable::RZ):
			rv.set(ptr->r().get(acc - AtomVariable::RX));
			break;
		case (AtomVariable::Selected):
			rv.set(ptr->isSelected());
			break;
		case (AtomVariable::Symbol):
			rv.set(elements().symbol(i));
			break;
		case (AtomVariable::Type):
			rv.set(NuVTypes::ForcefieldAtomData, ptr->type());
			break;
		case (AtomVariable::V):
			rv.set(ptr->v());
			break;
		case (AtomVariable::VX):
		case (AtomVariable::VY):
		case (AtomVariable::VZ):
			rv.set(ptr->v().get(acc - AtomVariable::VX));
			break;
		case (AtomVariable::Z):
			rv.set(ptr->element());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in AtomVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("AtomVariable::retrieveAccessor");
	return result;
}

/*
// Set specified data
bool AtomVariable::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("AtomVariable::set");
	bool result = TRUE;
	// Cast pointer into Atom*
	Atom *i = (Atom*) classptr;
	if (i == NULL) printf("Warning - NULL Atom pointer passed to AtomVariable::set.\n");
// 	printf("Enumerated ID supplied to AtomAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > AtomVariable::nAccessors))
	{
		printf("Unknown enumeration %i given to AtomVariable::set.\n", vid);
		msg.exit("AtomVariable::set");
		return FALSE;
	}
	// Check read-only status
	if (accessorPointers[vid]->readOnly())
	{
		msg.print("Member '%s' of 'atom' type is read-only.\n", accessorPointers[vid]->name());
		msg.exit("AtomVariable::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("AtomVariable::set");
		return FALSE;
	}
	Vec3<double> v;
	// Set value based on enumerated id
	switch (vid)
	{
		case (AtomVariable::Fixed):
			ptr->setPositionFixed(srcvar->asBool());
			break;
		case (AtomVariable::F):
			ptr->f() = srcvar->asVector();
			break;
		case (AtomVariable::FX):
		case (AtomVariable::FY):
		case (AtomVariable::FZ):
			ptr->f().set(vid - AtomVariable::FX, srcvar->asDouble());
			break;
		case (AtomVariable::Hidden):
			ptr->parent()->setHidden(i, srcvar->asBool());
			break;
		case (AtomVariable::Q):
			ptr->parent()->chargeAtom(i, srcvar->asDouble());
			break;
		case (AtomVariable::R):
			ptr->parent()->positionAtom(i, srcvar->asVector());
			break;
		case (AtomVariable::RX):
		case (AtomVariable::RY):
		case (AtomVariable::RZ):
			v = ptr->r();
			v.set(vid - AtomVariable::RX, srcvar->asDouble());
			ptr->parent()->positionAtom(i, v);
			break;
		case (AtomVariable::Selected):
			srcvar->asBool() ? ptr->parent()->deselectAtom(i) : ptr->parent()->selectAtom(i);
			break;
		case (AtomVariable::Type):
			ptr->setType( (ForcefieldAtom*) srcvar->asPointer(VTypes::ForcefieldAtomData));
			break;
		case (AtomVariable::V):
			ptr->v() = srcvar->asVector();
			break;
		case (AtomVariable::VX):
		case (AtomVariable::VY):
		case (AtomVariable::VZ):
			ptr->v().set(vid - AtomVariable::VX, srcvar->asDouble());
			break;
		case (AtomVariable::Z):
			ptr->parent()->transmuteAtom(i, srcvar->asInteger());
			break;
		default:
			printf("AtomVariable::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("AtomVariable::set");
	return result;
}*/
