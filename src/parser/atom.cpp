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
	{ "fixed", 	VTypes::IntegerData,		0, FALSE },
	{ "f",		VTypes::VectorData,		0, FALSE },
	{ "fx",		VTypes::DoubleData,		0, FALSE },
	{ "fy",		VTypes::DoubleData,		0, FALSE },
	{ "fz",		VTypes::DoubleData,		0, FALSE },
	{ "hidden",	VTypes::IntegerData,		0, FALSE },
	{ "id",		VTypes::IntegerData,		0, TRUE },
	{ "mass",	VTypes::DoubleData,		0, TRUE },
	{ "name",	VTypes::StringData,		0, TRUE },
	{ "q",		VTypes::DoubleData,		0, FALSE },
	{ "r",		VTypes::VectorData,		0, FALSE },
	{ "rx",		VTypes::DoubleData,		0, FALSE },
	{ "ry",		VTypes::DoubleData,		0, FALSE },
	{ "rz",		VTypes::DoubleData,		0, FALSE },
	{ "selected",	VTypes::IntegerData,		0, FALSE },
	{ "symbol",	VTypes::StringData,		0, TRUE },
	{ "type",	VTypes::ForcefieldAtomData,	0, FALSE },
	{ "v",		VTypes::VectorData,		0, FALSE },
	{ "vx",		VTypes::DoubleData,		0, FALSE },
	{ "vy",		VTypes::DoubleData,		0, FALSE },
	{ "vz",		VTypes::DoubleData,		0, FALSE },
	{ "z",		VTypes::IntegerData, 		0, FALSE }
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
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	// Were we given an array index when we didn't want one?
	if ((accessorData[i].arraySize == 0) && (arrayindex != NULL))
	{
		msg.print("Error: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
		result = NULL;
	}
	else result = new StepNode(i, VTypes::AtomData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
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
		printf("Internal Error: Accessor id %i is out of range for Atom type.\n", i);
		msg.exit("AtomVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("AtomVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("ElementVariable::retrieveAccessor");
			return FALSE;
		}
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
		printf("Internal Error: Accessor id %i is out of range for Atom type.\n", i);
		msg.exit("AtomVariable::setAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = TRUE;
	if (accessorData[i].arraySize != 0)
	{
		if (hasArrayIndex)
		{
			if ((accessorData[i].arraySize > 0) && ( (arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize) ))
			{
				msg.print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = FALSE;
			}
			if (newvalue.arraySize() > 0)
			{
				msg.print("Error: An array can't be assigned to the single valued member '%s'.\n", accessorData[i].name);
				result = FALSE;
			}
		}
		else
		{
			if ((newvalue.arraySize() > 0) && (newvalue.arraySize() != accessorData[i].arraySize))
			{
				msg.print("Error: The array being assigned to member '%s' is not of the same size (%i cf. %i).\n", accessorData[i].name, newvalue.arraySize(), accessorData[i].arraySize);
				result = FALSE;
			}
		}
	}
	else
	{
		// This is not an array member, so cannot be assigned an array unless its a Vector
		if (newvalue.arraySize() != -1)
		{
			if (accessorData[i].returnType != VTypes::VectorData)
			{
				msg.print("Error: An array can't be assigned to the single valued member '%s'.\n", accessorData[i].name);
				result = FALSE;
			}
			else if ((newvalue.type() != VTypes::VectorData) && (newvalue.arraySize() != 3))
			{
				msg.print("Error: Only an array of size 3 can be assigned to a vector (member '%s').\n", accessorData[i].name);
				result = FALSE;
			}
		}
	}
	if (!result)
	{
		msg.exit("ElementVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
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
			ptr->parent()->beginUndoState("Charge atom");
			ptr->parent()->chargeAtom(ptr, newvalue.asDouble());
			ptr->parent()->endUndoState();
			break;
		case (AtomVariable::R):
			ptr->parent()->beginUndoState("Position atom");
			ptr->parent()->positionAtom(ptr, newvalue.asVector());
			ptr->parent()->endUndoState();
			break;
		case (AtomVariable::RX):
		case (AtomVariable::RY):
		case (AtomVariable::RZ):
			v = ptr->r();
			v.set(acc - AtomVariable::RX, newvalue.asDouble());
			ptr->parent()->beginUndoState("Position atom");
			ptr->parent()->positionAtom(ptr, v);
			ptr->parent()->endUndoState();
			break;
		case (AtomVariable::Selected):
			ptr->parent()->beginUndoState("(De)select atom");
			newvalue.asBool() ? ptr->parent()->deselectAtom(i) : ptr->parent()->selectAtom(ptr);
			ptr->parent()->endUndoState();
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
			ptr->parent()->beginUndoState("Transmute atom");
			ptr->parent()->transmuteAtom(ptr, newvalue.asInteger());
			ptr->parent()->endUndoState();
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
