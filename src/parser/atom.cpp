/*
	*** Atom Variable and Array
	*** src/parser/atom.cpp
	Copyright T. Youngs 2007-2015

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
	{ "bit",	VTypes::IntegerData,		0, FALSE },
	{ "bonds", 	VTypes::BondData,		-1, TRUE },
	{ "colour",	VTypes::DoubleData,		4, FALSE },
	{ "data",	VTypes::StringData,		0, FALSE },
	{ "element",	VTypes::ElementData,		0, FALSE },
	{ "f",		VTypes::VectorData,		0, FALSE },
	{ "fixed", 	VTypes::IntegerData,		0, FALSE },
	{ "fracX",	VTypes::DoubleData,		0, FALSE },
	{ "fracY",	VTypes::DoubleData,		0, FALSE },
	{ "fracZ",	VTypes::DoubleData,		0, FALSE },
	{ "fx",		VTypes::DoubleData,		0, FALSE },
	{ "fy",		VTypes::DoubleData,		0, FALSE },
	{ "fz",		VTypes::DoubleData,		0, FALSE },
	{ "hidden",	VTypes::IntegerData,		0, FALSE },
	{ "id",		VTypes::IntegerData,		0, TRUE },
	{ "mass",	VTypes::DoubleData,		0, TRUE },
	{ "name",	VTypes::StringData,		0, TRUE },
	{ "nBonds",	VTypes::IntegerData,		0, TRUE },
	{ "q",		VTypes::DoubleData,		0, FALSE },
	{ "r",		VTypes::VectorData,		0, FALSE },
	{ "rx",		VTypes::DoubleData,		0, FALSE },
	{ "ry",		VTypes::DoubleData,		0, FALSE },
	{ "rz",		VTypes::DoubleData,		0, FALSE },
	{ "selected",	VTypes::IntegerData,		0, FALSE },
	{ "style",	VTypes::StringData,		0, FALSE },
	{ "symbol",	VTypes::StringData,		0, TRUE },
	{ "type",	VTypes::ForcefieldAtomData,	0, FALSE },
	{ "v",		VTypes::VectorData,		0, FALSE },
	{ "vx",		VTypes::DoubleData,		0, FALSE },
	{ "vy",		VTypes::DoubleData,		0, FALSE },
	{ "vz",		VTypes::DoubleData,		0, FALSE },
	{ "z",		VTypes::IntegerData, 		0, FALSE }
};

// Function data
FunctionAccessor AtomVariable::functionData[AtomVariable::nFunctions] = {
	{ "addBit",	VTypes::NoData,		"I",	"int bit" },
	{ "copy",	VTypes::NoData,		"A",	"Atom j" },
	{ "findBond",	VTypes::BondData,	"A",	"Atom j" },
	{ "hasBit",	VTypes::IntegerData,	"I",	"int bit" },
	{ "removeBit",	VTypes::NoData,		"I",	"int bit" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *AtomVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return AtomVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *AtomVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("AtomVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	i = Variable::searchAccessor(s, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(s, nFunctions, functionData);
		if (i == -1)
		{
			msg.print("Error: Type 'Atom&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("AtomVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'Atom&' function '%s'.\n", s);
			msg.exit("AtomVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::AtomData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'Atom&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
			delete result;
			result = NULL;
		}
	}
	else
	{
		msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayindex != NULL))
		{
			msg.print("Error: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
			result = NULL;
		}
		// Were we given an argument list when we didn't want one?
		if (arglist != NULL)
		{
			msg.print("Error: Argument list given to 'Atom&' array member '%s'.\n", s);
			msg.exit("AtomVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::AtomData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
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
			msg.exit("AtomVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Atom *ptr = (Atom*) rv.asPointer(VTypes::AtomData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::AtomData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (AtomVariable::Bit):
			rv.set( ptr->bit() );
			break;
		case (AtomVariable::Bonds):
			if (!hasArrayIndex) rv.set( VTypes::BondData, ptr->bonds() == NULL ? NULL : ptr->bonds()->item, ptr->bonds());
			else if (arrayIndex > ptr->nBonds())
			{
				msg.print("Bond array index (%i) is out of bounds for atom '%i'\n", arrayIndex, ptr->id()+1);
				result = FALSE;
			}
			else rv.set( VTypes::BondData, ptr->bond(arrayIndex-1) == NULL ? NULL : ptr->bond(arrayIndex-1)->item, ptr->bond(arrayIndex-1));
			break;
		case (AtomVariable::Colour):
			if (hasArrayIndex) rv.set( ptr->colour()[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->colour(), 4);
			break;
		case (AtomVariable::Data):
			rv.set(ptr->data());
			break;
		case (AtomVariable::ElementInfo):
			rv.set(VTypes::ElementData, &Elements().el[ptr->element()]);
			break;
		case (AtomVariable::F):
			rv.set(ptr->f());
			break;
		case (AtomVariable::Fixed):
			rv.set(ptr->isPositionFixed());
			break;
		case (AtomVariable::FracX):
		case (AtomVariable::FracY):
		case (AtomVariable::FracZ):
			if (ptr->parent()) rv.set((ptr->parent()->cell()->inverse() * ptr->r()).get(acc - AtomVariable::FracX));
			else
			{
				msg.print("Can't retrieve the fractional coordinate of an unparented Atom (since it has no associated UnitCell).\n");
				result = FALSE;
			}
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
			rv.set(Elements().atomicMass(ptr));
			break;
		case (AtomVariable::Name):
			rv.set(Elements().name(ptr));
			break;
		case (AtomVariable::NBonds):
			rv.set(ptr->nBonds());
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
		case (AtomVariable::Style):
			rv.set(Atom::drawStyle(ptr->style()));
			break;
		case (AtomVariable::Symbol):
			rv.set(Elements().symbol(ptr));
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
			if (newvalue.arraySize() > accessorData[i].arraySize)
			{
				msg.print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).\n", accessorData[i].name, newvalue.arraySize(), accessorData[i].arraySize);
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
		msg.exit("AtomVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Vec3<double> v;
	int n;
	Atom::DrawStyle ds;
	Element *el;
	Atom *ptr = (Atom*) sourcerv.asPointer(VTypes::AtomData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::AtomData));
		result = FALSE;
	}
	Model *ptrParent = ptr->parent();
	
	// Set value based on enumerated id
	if (result) switch (acc)
	{
		case (AtomVariable::Bit):
			ptr->setBit( newvalue.asInteger() );
			break;
		case (AtomVariable::Colour):
			if (newvalue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->setColour(n, newvalue.asVector(result)[n]);
			else if (newvalue.arraySize() != -1) for (n=0; n<newvalue.arraySize(); ++n) ptr->setColour(n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setColour(arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setColour(n, newvalue.asDouble(result));
			break;
		case (AtomVariable::Data):
			ptr->setData(newvalue.asString());
			break;
		case (AtomVariable::ElementInfo):
			el = (Element*) newvalue.asPointer(VTypes::ElementData);
			if (el == NULL)
			{
				msg.print("Invalid (NULL) element reference encountered while setting atom's element.\n");
				result = FALSE;
			}
			else if (&Elements().el[ptr->element()] != el)
			{
				if (ptrParent)
				{
					ptrParent->beginUndoState("Transmute atom");
					ptrParent->transmuteAtom(ptr, el->z);
					ptrParent->endUndoState();
				}
				else ptr->setElement(el->z);
			}
			break;
		case (AtomVariable::F):
			ptr->f() = newvalue.asVector();
			break;
		case (AtomVariable::Fixed):
			if (ptrParent) ptrParent->atomSetFixed(ptr, newvalue.asBool());
			else ptr->setPositionFixed(newvalue.asBool());
			break;
		case (AtomVariable::FracX):
		case (AtomVariable::FracY):
		case (AtomVariable::FracZ):
			if (ptrParent)
			{
				v = ptrParent->cell()->inverse() * ptr->r();
				v.set(acc - AtomVariable::FracX, newvalue.asDouble());
				v = ptrParent->cell()->fracToReal(v);
				ptrParent->beginUndoState("Position atom (fractional coordinates)");
				ptrParent->positionAtom(ptr, v);
				ptrParent->endUndoState();
			}
			else
			{
				msg.print("Can't set the fractional coordinate of an unparented Atom (since it has no associated UnitCell).\n");
				result = FALSE;
			}
			break;
		case (AtomVariable::FX):
		case (AtomVariable::FY):
		case (AtomVariable::FZ):
			ptr->f().set(acc - AtomVariable::FX, newvalue.asDouble());
			break;
		case (AtomVariable::Hidden):
			if (ptrParent) ptrParent->atomSetHidden(ptr, newvalue.asBool());
			else ptr->setHidden( newvalue.asBool() );
			break;
		case (AtomVariable::Q):
			if (ptrParent)
			{
				ptrParent->beginUndoState("Charge atom");
				ptrParent->atomSetCharge(ptr, newvalue.asDouble());
				ptrParent->endUndoState();
			}
			else ptr->setCharge(newvalue.asDouble());
			break;
		case (AtomVariable::R):
			if (ptrParent)
			{
				ptrParent->beginUndoState("Position atom");
				ptrParent->positionAtom(ptr, newvalue.asVector());
				ptrParent->endUndoState();
			}
			else ptr->r() = newvalue.asVector();
			break;
		case (AtomVariable::RX):
		case (AtomVariable::RY):
		case (AtomVariable::RZ):
			v = ptr->r();
			v.set(acc - AtomVariable::RX, newvalue.asDouble());
			if (ptrParent)
			{
				ptrParent->beginUndoState("Position atom");
				ptrParent->positionAtom(ptr, v);
				ptrParent->endUndoState();
			}
			else ptr->r() = v;
			break;
		case (AtomVariable::Selected):
			if (ptrParent)
			{
				ptrParent->beginUndoState("(De)select atom");
				newvalue.asBool() ? ptrParent->selectAtom(ptr) : ptrParent->deselectAtom(ptr);
				ptrParent->endUndoState();
			}
			else ptr->setSelected(newvalue.asBool());
			break;
		case (AtomVariable::Style):
			ds = Atom::drawStyle( newvalue.asString() );
			if (ds != Atom::nDrawStyles) ptr->setStyle(ds);
			else result = FALSE;
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
			if (ptrParent)
			{
				ptrParent->beginUndoState("Transmute atom");
				ptrParent->transmuteAtom(ptr, newvalue.asInteger());
				ptrParent->endUndoState();
			}
			else ptr->setElement( newvalue.asInteger() );
			break;
		default:
			printf("AtomVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("AtomVariable::setAccessor");
	return result;
}

// Perform desired function
bool AtomVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("AtomVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Atom type.\n", i);
		msg.exit("AtomVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Atom *ptr = (Atom*) rv.asPointer(VTypes::AtomData, result);
	Model *ptrParent = ptr->parent();
	if (result) switch (i)
	{
		case (AtomVariable::AddBit):
			ptr->addBit( node->argi(0) );
			break;
		case (AtomVariable::Copy):
			if (!((Atom*) node->argp(0, VTypes::AtomData)))
			{
				msg.print("Error: NULL pointer given to Atom's 'copy' function.\n");
				result = FALSE;
			}
			else if (ptrParent)
			{
				Atom *i = (Atom*) node->argp(0, VTypes::AtomData);
				ptrParent->beginUndoState("Copy atom data");
				i->isSelected() ? ptrParent->selectAtom(ptr) : ptrParent->deselectAtom(ptr);
				ptrParent->positionAtom(ptr, i->r());
				if (ptr->element() != i->element()) ptrParent->transmuteAtom(ptr, i->element());
				ptrParent->endUndoState();
			}
			else result = ptr->copy( (Atom*) node->argp(0, VTypes::AtomData) );
			rv.reset();
			break;
		case (AtomVariable::FindBond):
			rv.set(VTypes::BondData, ptr->findBond( (Atom*) node->argp(0, VTypes::AtomData) ) );
			break;
		case (AtomVariable::HasBit):
			rv.set( ptr->hasBit( node->argi(0) ) );
			break;
		case (AtomVariable::RemoveBit):
			ptr->removeBit( node->argi(0) );
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in AtomVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("AtomVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void AtomVariable::printAccessors()
{
	if (AtomVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<AtomVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((AtomVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<AtomVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
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
StepNode *AtomArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return AtomVariable::accessorSearch(s, arrayindex, arglist);
}
