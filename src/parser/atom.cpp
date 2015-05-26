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
#include "base/elementmap.h"
#include "model/model.h"
#include <string.h>

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
AtomVariable::AtomVariable(Atom* ptr, bool constant)
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
 * Accessors
 */

// Accessor data
Accessor AtomVariable::accessorData[AtomVariable::nAccessors] = {
	{ "bit",	VTypes::IntegerData,		0, false },
	{ "bonds", 	VTypes::BondData,		-1, true },
	{ "colour",	VTypes::DoubleData,		4, false },
	{ "data",	VTypes::StringData,		0, false },
	{ "element",	VTypes::ElementData,		0, false },
	{ "f",		VTypes::VectorData,		0, false },
	{ "fixed", 	VTypes::IntegerData,		0, false },
	{ "fracX",	VTypes::DoubleData,		0, false },
	{ "fracY",	VTypes::DoubleData,		0, false },
	{ "fracZ",	VTypes::DoubleData,		0, false },
	{ "fx",		VTypes::DoubleData,		0, false },
	{ "fy",		VTypes::DoubleData,		0, false },
	{ "fz",		VTypes::DoubleData,		0, false },
	{ "hidden",	VTypes::IntegerData,		0, false },
	{ "id",		VTypes::IntegerData,		0, true },
	{ "mass",	VTypes::DoubleData,		0, true },
	{ "name",	VTypes::StringData,		0, true },
	{ "nBonds",	VTypes::IntegerData,		0, true },
	{ "q",		VTypes::DoubleData,		0, false },
	{ "r",		VTypes::VectorData,		0, false },
	{ "rx",		VTypes::DoubleData,		0, false },
	{ "ry",		VTypes::DoubleData,		0, false },
	{ "rz",		VTypes::DoubleData,		0, false },
	{ "selected",	VTypes::IntegerData,		0, false },
	{ "style",	VTypes::StringData,		0, false },
	{ "symbol",	VTypes::StringData,		0, true },
	{ "type",	VTypes::ForcefieldAtomData,	0, false },
	{ "v",		VTypes::VectorData,		0, false },
	{ "vx",		VTypes::DoubleData,		0, false },
	{ "vy",		VTypes::DoubleData,		0, false },
	{ "vz",		VTypes::DoubleData,		0, false },
	{ "z",		VTypes::IntegerData, 		0, false }
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
StepNode* AtomVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return AtomVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* AtomVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("AtomVariable::accessorSearch");
	StepNode* result = NULL;
	int i = 0;
	i = Variable::searchAccessor(name, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(name, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'Atom&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("AtomVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Atom&' function '%s'.", qPrintable(name));
			Messenger::exit("AtomVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::AtomData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'Atom&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
			delete result;
			result = NULL;
		}
	}
	else
	{
		Messenger::print(Messenger::Parse, "Accessor match = %i (%s)", i, accessorData[i].name);
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayIndex != NULL))
		{
			Messenger::print("Error: Irrelevant array index provided for member '%s'.", accessorData[i].name);
			result = NULL;
		}
		// Were we given an argument list when we didn't want one?
		if (argList != NULL)
		{
			Messenger::print("Error: Argument list given to 'Atom&' array member '%s'.", qPrintable(name));
			Messenger::exit("AtomVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::AtomData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("AtomVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool AtomVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("AtomVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Atom type.\n", i);
		Messenger::exit("AtomVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("AtomVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("AtomVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	Atom* ptr = (Atom*) rv.asPointer(VTypes::AtomData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::AtomData));
		result = false;
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
				Messenger::print("Bond array index (%i) is out of bounds for atom '%i'", arrayIndex, ptr->id()+1);
				result = false;
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
			rv.set(VTypes::ElementData, Elements().element(ptr->element()));
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
			if (ptr->parent()) rv.set((ptr->parent()->cell().inverse() * ptr->r()).get(acc - AtomVariable::FracX));
			else
			{
				Messenger::print("Can't retrieve the fractional coordinate of an unparented Atom (since it has no associated UnitCell).");
				result = false;
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
			rv.set(Prefs::drawStyle(ptr->style()));
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
			result = false;
			break;
	}
	Messenger::exit("AtomVariable::retrieveAccessor");
	return result;
}

// Set specified data
bool AtomVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("AtomVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Atom type.\n", i);
		Messenger::exit("AtomVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = true;
	if (accessorData[i].arraySize != 0)
	{
		if (hasArrayIndex)
		{
			if ((accessorData[i].arraySize > 0) && ( (arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize) ))
			{
				Messenger::print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = false;
			}
			if (newValue.arraySize() > 0)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = false;
			}
		}
		else
		{
			if (newValue.arraySize() > accessorData[i].arraySize)
			{
				Messenger::print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).", accessorData[i].name, newValue.arraySize(), accessorData[i].arraySize);
				result = false;
			}
		}
	}
	else
	{
		// This is not an array member, so cannot be assigned an array unless its a Vector
		if (newValue.arraySize() != -1)
		{
			if (accessorData[i].returnType != VTypes::VectorData)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = false;
			}
			else if ((newValue.type() != VTypes::VectorData) && (newValue.arraySize() != 3))
			{
				Messenger::print("Error: Only an array of size 3 can be assigned to a vector (member '%s').", accessorData[i].name);
				result = false;
			}
		}
	}
	if (!result)
	{
		Messenger::exit("AtomVariable::setAccessor");
		return false;
	}
	// Get current data from ReturnValue
	Vec3<double> v;
	int n;
	Prefs::DrawStyle ds;
	Element* el;
	Atom* ptr = (Atom*) sourcerv.asPointer(VTypes::AtomData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::AtomData));
		result = false;
	}
	Model* ptrParent = ptr->parent();
	
	// Set value based on enumerated id
	if (result) switch (acc)
	{
		case (AtomVariable::Bit):
			ptr->setBit( newValue.asInteger() );
			break;
		case (AtomVariable::Colour):
			if (newValue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->setColour(n, newValue.asVector(result)[n]);
			else if (newValue.arraySize() != -1) for (n=0; n<newValue.arraySize(); ++n) ptr->setColour(n, newValue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setColour(arrayIndex-1, newValue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setColour(n, newValue.asDouble(result));
			break;
		case (AtomVariable::Data):
			ptr->setData(qPrintable(newValue.asString()));
			break;
		case (AtomVariable::ElementInfo):
			el = (Element*) newValue.asPointer(VTypes::ElementData);
			if (el == NULL)
			{
				Messenger::print("Invalid (NULL) element reference encountered while setting atom's element.");
				result = false;
			}
			else if (Elements().element(ptr->element()) != el)
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
			ptr->f() = newValue.asVector();
			break;
		case (AtomVariable::Fixed):
			if (ptrParent) ptrParent->atomSetFixed(ptr, newValue.asBool());
			else ptr->setPositionFixed(newValue.asBool());
			break;
		case (AtomVariable::FracX):
		case (AtomVariable::FracY):
		case (AtomVariable::FracZ):
			if (ptrParent)
			{
				v = ptrParent->cell().inverse() * ptr->r();
				v.set(acc - AtomVariable::FracX, newValue.asDouble());
				v = ptrParent->cell().fracToReal(v);
				ptrParent->beginUndoState("Position atom (fractional coordinates)");
				ptrParent->positionAtom(ptr, v);
				ptrParent->endUndoState();
			}
			else
			{
				Messenger::print("Can't set the fractional coordinate of an unparented Atom (since it has no associated UnitCell).");
				result = false;
			}
			break;
		case (AtomVariable::FX):
		case (AtomVariable::FY):
		case (AtomVariable::FZ):
			ptr->f().set(acc - AtomVariable::FX, newValue.asDouble());
			break;
		case (AtomVariable::Hidden):
			if (ptrParent) ptrParent->atomSetHidden(ptr, newValue.asBool());
			else ptr->setHidden( newValue.asBool() );
			break;
		case (AtomVariable::Q):
			if (ptrParent)
			{
				ptrParent->beginUndoState("Charge atom");
				ptrParent->atomSetCharge(ptr, newValue.asDouble());
				ptrParent->endUndoState();
			}
			else ptr->setCharge(newValue.asDouble());
			break;
		case (AtomVariable::R):
			if (ptrParent)
			{
				ptrParent->beginUndoState("Position atom");
				ptrParent->positionAtom(ptr, newValue.asVector());
				ptrParent->endUndoState();
			}
			else ptr->r() = newValue.asVector();
			break;
		case (AtomVariable::RX):
		case (AtomVariable::RY):
		case (AtomVariable::RZ):
			v = ptr->r();
			v.set(acc - AtomVariable::RX, newValue.asDouble());
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
				newValue.asBool() ? ptrParent->selectAtom(ptr) : ptrParent->deselectAtom(ptr);
				ptrParent->endUndoState();
			}
			else ptr->setSelected(newValue.asBool());
			break;
		case (AtomVariable::Style):
			ds = Prefs::drawStyle( newValue.asString() );
			if (ds != Prefs::nDrawStyles) ptr->setStyle(ds);
			else result = false;
			break;
		case (AtomVariable::Type):
			ptr->setType( (ForcefieldAtom*) newValue.asPointer(VTypes::ForcefieldAtomData));
			break;
		case (AtomVariable::V):
			ptr->v() = newValue.asVector();
			break;
		case (AtomVariable::VX):
		case (AtomVariable::VY):
		case (AtomVariable::VZ):
			ptr->v().set(acc - AtomVariable::VX, newValue.asDouble());
			break;
		case (AtomVariable::Z):
			if (ptrParent)
			{
				ptrParent->beginUndoState("Transmute atom");
				ptrParent->transmuteAtom(ptr, newValue.asInteger());
				ptrParent->endUndoState();
			}
			else ptr->setElement( newValue.asInteger() );
			break;
		default:
			printf("AtomVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = false;
			break;
	}
	Messenger::exit("AtomVariable::setAccessor");
	return result;
}

// Perform desired function
bool AtomVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("AtomVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Atom type.\n", i);
		Messenger::exit("AtomVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	Atom* ptr = (Atom*) rv.asPointer(VTypes::AtomData, result);
	Model* ptrParent = ptr->parent();
	if (result) switch (i)
	{
		case (AtomVariable::AddBit):
			ptr->addBit( node->argi(0) );
			break;
		case (AtomVariable::Copy):
			if (!((Atom*) node->argp(0, VTypes::AtomData)))
			{
				Messenger::print("Error: NULL pointer given to Atom's 'copy' function.");
				result = false;
			}
			else if (ptrParent)
			{
				Atom* i = (Atom*) node->argp(0, VTypes::AtomData);
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
			result = false;
			break;
	}
	Messenger::exit("AtomVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void AtomVariable::printAccessors()
{
	if (AtomVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<AtomVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((AtomVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<AtomVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}

/*
 * Variable Array
 */

// Constructor
AtomArrayVariable::AtomArrayVariable(TreeNode* sizeexpr, bool constant)
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
StepNode* AtomArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return AtomVariable::accessorSearch(name, arrayIndex, argList);
}
