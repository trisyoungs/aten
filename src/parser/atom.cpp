/*
	*** Atom Variable and Array
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
#include "model/model.h"
#include <string.h>

/*
// Variable
*/

// Constructor
AtomVariable::AtomVariable(Atom *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::AtomData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
AtomVariable::~AtomVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor AtomVariable::accessorData[AtomVariable::nAccessors] = {
	{ "fixed", 	VTypes::IntegerData,		FALSE, FALSE },
	{ "f",		VTypes::VectorData,		FALSE, FALSE },
	{ "fx",		VTypes::DoubleData,		FALSE, FALSE },
	{ "fy",		VTypes::DoubleData,		FALSE, FALSE },
	{ "fz",		VTypes::DoubleData,		FALSE, FALSE },
	{ "hidden",	VTypes::IntegerData,		FALSE, FALSE },
	{ "id",		VTypes::IntegerData,		FALSE, TRUE },
	{ "mass",	VTypes::DoubleData,		FALSE, TRUE },
	{ "name",	VTypes::StringData,		FALSE, TRUE },
	{ "q",		VTypes::DoubleData,		FALSE, FALSE },
	{ "r",		VTypes::VectorData,		FALSE, FALSE },
	{ "rx",		VTypes::DoubleData,		FALSE, FALSE },
	{ "ry",		VTypes::DoubleData,		FALSE, FALSE },
	{ "rz",		VTypes::DoubleData,		FALSE, FALSE },
	{ "selected",	VTypes::IntegerData,		FALSE, FALSE },
	{ "symbol",	VTypes::StringData,		FALSE, TRUE },
	{ "type",	VTypes::ForcefieldAtomData,	FALSE, FALSE },
	{ "v",		VTypes::VectorData,		FALSE, FALSE },
	{ "vx",		VTypes::DoubleData,		FALSE, FALSE },
	{ "vy",		VTypes::DoubleData,		FALSE, FALSE },
	{ "vz",		VTypes::DoubleData,		FALSE, FALSE },
	{ "z",		VTypes::IntegerData, 		FALSE, TRUE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *AtomVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return AtomVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *AtomVariable::accessorSearch(const char *s, TreeNode *arrayindex)
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
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, VTypes::AtomData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("AtomVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool AtomVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
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
	if ((!accessorData[i].isArray) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("AtomVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Atom *ptr= (Atom*) rv.asPointer(VTypes::AtomData, result);
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
			rv.set(elements().atomicMass(ptr));
			break;
		case (AtomVariable::Name):
			rv.set(elements().name(ptr));
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
			rv.set(elements().symbol(ptr));
			break;
		case (AtomVariable::Type):
			rv.set(VTypes::ForcefieldAtomData, ptr->type());
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

// Set specified data
bool AtomVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("AtomVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Atom type.\n");
		msg.exit("AtomVariable::setAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if (!accessorData[i].isArray)
	{
		if (hasArrayIndex) msg.print("Warning: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
	}
	else if (!hasArrayIndex)
	{
		msg.print("Error: No array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("AtomVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Vec3<double> v;
	Atom *ptr= (Atom*) sourcerv.asPointer(VTypes::AtomData, result);
	// Set value based on enumerated id
	if (result) switch (acc)
	{
		case (AtomVariable::Fixed):
			ptr->setPositionFixed(newvalue.asBool());
			break;
		case (AtomVariable::F):
			ptr->f() = newvalue.asVector();
			break;
		case (AtomVariable::FX):
		case (AtomVariable::FY):
		case (AtomVariable::FZ):
			ptr->f().set(acc - AtomVariable::FX, newvalue.asDouble());
			break;
		case (AtomVariable::Hidden):
			ptr->parent()->setHidden(ptr, newvalue.asBool());
			break;
		case (AtomVariable::Q):
			ptr->parent()->chargeAtom(ptr, newvalue.asDouble());
			break;
		case (AtomVariable::R):
			ptr->parent()->positionAtom(ptr, newvalue.asVector());
			break;
		case (AtomVariable::RX):
		case (AtomVariable::RY):
		case (AtomVariable::RZ):
			v = ptr->r();
			v.set(acc - AtomVariable::RX, newvalue.asDouble());
			ptr->parent()->positionAtom(ptr, v);
			break;
		case (AtomVariable::Selected):
			newvalue.asBool() ? ptr->parent()->deselectAtom(i) : ptr->parent()->selectAtom(ptr);
			break;
		case (AtomVariable::Type):
			ptr->setType( (ForcefieldAtom*) newvalue.asPointer(VTypes::ForcefieldAtomData));
			break;
		case (AtomVariable::V):
			ptr->v() = newvalue.asVector();
			break;
		case (AtomVariable::VX):
		case (AtomVariable::VY):
		case (AtomVariable::VZ):
			ptr->v().set(acc - AtomVariable::VX, newvalue.asDouble());
			break;
		case (AtomVariable::Z):
			ptr->parent()->transmuteAtom(ptr, newvalue.asInteger());
			break;
		default:
			printf("AtomVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("AtomVariable::setAccessor");
	return result;
}

/*
// Variable Array
*/

// Constructor
AtomArrayVariable::AtomArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::AtomData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *AtomArrayVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return AtomVariable::accessorSearch(s, arrayindex);
}
