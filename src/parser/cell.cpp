/*
	*** Cell Variable and Array
	*** src/parser/cell.cpp
	Copyright T. Youngs 2007-2016

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

#include "parser/cell.h"
#include "parser/stepnode.h"
#include "base/cell.h"
#include "base/sysfunc.h"
#include "sg/spacegroup.h"
#include "base/prefs.h"
#include "model/model.h"
#include <string.h>

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
CellVariable::CellVariable(UnitCell* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::CellData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
CellVariable::~CellVariable()
{
}

/*
 * Accessors
 */

// Accessor data
Accessor CellVariable::accessorData[CellVariable::nAccessors] = {
	{ "a",		VTypes::DoubleData,	0, false },
	{ "b",		VTypes::DoubleData,	0, false },
	{ "c",		VTypes::DoubleData,	0, false },
	{ "alpha",	VTypes::DoubleData,	0, false },
	{ "beta",	VTypes::DoubleData,	0, false },
	{ "gamma",	VTypes::DoubleData,	0, false },
	{ "ax",		VTypes::DoubleData,	0, false },
	{ "ay",		VTypes::DoubleData,	0, false },
	{ "az",		VTypes::DoubleData,	0, false },
	{ "bx",		VTypes::DoubleData,	0, false },
	{ "by",		VTypes::DoubleData,	0, false },
	{ "bz",		VTypes::DoubleData,	0, false },
	{ "cx",		VTypes::DoubleData,	0, false },
	{ "cy",		VTypes::DoubleData,	0, false },
	{ "cz",		VTypes::DoubleData,	0, false },
	{ "centre",	VTypes::VectorData,	0, true },
	{ "centreX",	VTypes::DoubleData,	0, true },
	{ "centreY",	VTypes::DoubleData,	0, true },
	{ "centreZ",	VTypes::DoubleData,	0, true },
	{ "density",	VTypes::DoubleData,	0, true },
	{ "matrix", 	VTypes::DoubleData,	9, false },
	{ "sgId",	VTypes::IntegerData,	0, false },
	{ "sgName",	VTypes::StringData,	0, true },
	{ "type",	VTypes::StringData,	0, true },
	{ "volume",	VTypes::DoubleData,	0, true },
};

// Function data
FunctionAccessor CellVariable::functionData[CellVariable::nFunctions] = {
	{ "copy",		VTypes::NoData,		"K",		"UnitCell cell" },
	{ "fracToReal",		VTypes::VectorData,	"NNN",		"double fracx, double fracy, double fracz" },
	{ "mim",		VTypes::VectorData,	"WW",		"Atom i | vector u, Atom j | vector v" },
	{ "mimVector",		VTypes::VectorData,	"WW",		"Atom i | vector u, Atom j | vector v" },
	{ "realToFrac",		VTypes::VectorData,	"NNN",		"double x, double y, double z" },
	{ "translateAtom",	VTypes::VectorData,	"JNNN",		"Atom i, double fracx, double fracy, double fracz" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* CellVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return CellVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* CellVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("CellVariable::accessorSearch");
	StepNode* result = NULL;
	int i = 0;
	i = Variable::searchAccessor(name, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(qPrintable(functionData[i].name),s) == 0) break;
		i = Variable::searchAccessor(name, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'Cell&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("CellVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Cell&' function named '%s'.", qPrintable(name));
			Messenger::exit("CellVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::CellData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'Cell&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
			delete result;
			result = NULL;
		}
	}
	else
	{
		Messenger::print(Messenger::Parse, "Accessor match = %i (%s)", i, qPrintable(accessorData[i].name));
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayIndex != NULL))
		{
			Messenger::print("Error: Irrelevant array index provided for member '%s'.", qPrintable(accessorData[i].name));
			result = NULL;
		}
		// Were we given an argument list when we didn't want one?
		if (argList != NULL)
		{
			Messenger::print("Error: Argument list given to 'Cell&' array member '%s'.", qPrintable(name));
			Messenger::exit("CellVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::CellData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("CellVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool CellVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("CellVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Cell type.\n", i);
		Messenger::exit("CellVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("CellVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("CellVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	UnitCell* ptr = (UnitCell*) rv.asPointer(VTypes::CellData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::CellData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (CellVariable::A):
		case (CellVariable::B):
		case (CellVariable::C):
			rv.set(ptr->lengths().get(acc - CellVariable::A));
			break;
		case (CellVariable::Alpha):
		case (CellVariable::Beta):
		case (CellVariable::Gamma):
			rv.set(ptr->angles().get(acc - CellVariable::Alpha));
			break;
		case (CellVariable::AX):
		case (CellVariable::AY):
		case (CellVariable::AZ):
		case (CellVariable::BX):
		case (CellVariable::BY):
		case (CellVariable::BZ):
		case (CellVariable::CX):
		case (CellVariable::CY):
		case (CellVariable::CZ):
			rv.set(ptr->axes()[((acc - CellVariable::AX)/3)*4+(acc - CellVariable::AX)%3]);
			break;
		case (CellVariable::Centre):
			rv.set(ptr->centre());
			break;
		case (CellVariable::CentreX):
		case (CellVariable::CentreY):
		case (CellVariable::CentreZ):
			rv.set(ptr->centre().get(acc - CellVariable::CentreX));
			break;
		case (CellVariable::Density):
			rv.set(ptr->density());
			break;
		case (CellVariable::Matrix):
			if ((arrayIndex < 1) || (arrayIndex > 9))
			{
				Messenger::print("Array index [%i] is out of range for 'matrix' member.", arrayIndex);
				result = false;
			}
			else rv.set(ptr->axes()[((arrayIndex-1)/3)*4+(arrayIndex-1)%3]);
			break;
		case (CellVariable::SpacegroupId):
			rv.set(ptr->spacegroupId());
			break;
		case (CellVariable::SpacegroupName):
			rv.set(ptr->spacegroupName());
			break;
		case (CellVariable::Type):
			rv.set(UnitCell::cellType(ptr->type()));
			break;
		case (CellVariable::Volume):
			rv.set(ptr->volume());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in CellVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("CellVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool CellVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("CellVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Cell type.\n", i);
		Messenger::exit("CellVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("CellVariable::setAccessor");
		return false;
	}
	
	// Get current data from ReturnValue
	UnitCell* ptr = (UnitCell*) sourcerv.asPointer(VTypes::CellData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::CellData));
		result = false;
	}
	
	Model* ptrParent = ptr->parent();
	if (result) switch (acc)
	{
		case (CellVariable::A):
		case (CellVariable::B):
		case (CellVariable::C):
		case (CellVariable::Alpha):
		case (CellVariable::Beta):
		case (CellVariable::Gamma):
		case (CellVariable::AX):
		case (CellVariable::AY):
		case (CellVariable::AZ):
		case (CellVariable::BX):
		case (CellVariable::BY):
		case (CellVariable::BZ):
		case (CellVariable::CX):
		case (CellVariable::CY):
		case (CellVariable::CZ):
			// Cast accessor into a CellParameter
			if (ptrParent) ptrParent->setCell( (UnitCell::CellParameter) acc, newValue.asDouble());
			else ptr->setParameter( (UnitCell::CellParameter) acc, newValue.asDouble());
			break;
		case (CellVariable::Matrix):
			// Cast accessor into a CellParameter
			if (ptrParent) ptrParent->setCell( (UnitCell::CellParameter) ((arrayIndex-1) + UnitCell::CellAX), newValue.asDouble());
			else ptr->setParameter( (UnitCell::CellParameter) ((arrayIndex-1) + UnitCell::CellAX), newValue.asDouble());
			break;
		case (CellVariable::SpacegroupId):
			ptr->setSpacegroup( qPrintable(newValue.asString()), false );
			break;
		default:
			printf("CellVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("CellVariable::setAccessor");
	return result;
}

// Perform desired function
bool CellVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("CellVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Cell type.\n", i);
		Messenger::exit("CellVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	Atom* ii, *jj;
	Vec3<double> v1, v2;
	UnitCell* ptr = (UnitCell*) rv.asPointer(VTypes::CellData, result);
	if (result) switch (i)
	{
		case (CellVariable::Copy):
			if (ptr->parent()) result = ptr->parent()->setCell( (UnitCell*) node->argp(0, VTypes::CellData) );
			else result = ptr->copy( (UnitCell*) node->argp(0, VTypes::CellData) );
			rv.reset();
			break;
		case (CellVariable::FracToReal):
			v1 = node->arg3d(0);
			rv.set(ptr->fracToReal(v1));
			break;
		case (CellVariable::MinimumImage):
			if (node->argType(0) == VTypes::VectorData) v1 = node->argv(0);
			else
			{
				ii = (Atom*) node->argp(0, VTypes::AtomData);
				if (ii == NULL)
				{
					Messenger::print("Error: Source atom given to cell 'mim' function is NULL.");
					result = false;
					break;
				}
				v1 = ii->r();
			}
			if (node->argType(1) == VTypes::VectorData) v2 = node->argv(1);
			else
			{
				jj = (Atom*) node->argp(1, VTypes::AtomData);
				if (jj == NULL)
				{
					Messenger::print("Error: Reference atom given to cell 'mim' function is NULL.");
					result = false;
					break;
				}
				v2 = jj->r();
			}
			rv.set(ptr->mim(v1,v2));
			break;
		case (CellVariable::MinimumImageVector):
			if (node->argType(0) == VTypes::VectorData) v1 = node->argv(0);
			else
			{
				ii = (Atom*) node->argp(0, VTypes::AtomData);
				if (ii == NULL)
				{
					Messenger::print("Error: Source atom given to cell 'mimVector' function is NULL.");
					result = false;
					break;
				}
				v1 = ii->r();
			}
			if (node->argType(1) == VTypes::VectorData) v2 = node->argv(1);
			else
			{
				jj = (Atom*) node->argp(1, VTypes::AtomData);
				if (jj == NULL)
				{
					Messenger::print("Error: Reference atom given to cell 'mimVector' function is NULL.");
					result = false;
					break;
				}
				v2 = jj->r();
			}
			rv.set(ptr->mimVector(v1,v2));
			break;
		case (CellVariable::RealToFrac):
			v1 = node->arg3d(0);
			v1.print();
			rv.set(ptr->realToFrac(v1));
			break;
		case (CellVariable::TranslateAtom):
			ii = (Atom*) node->argp(0, VTypes::AtomData);
			if (ii == NULL)
			{
				Messenger::print("Error: Target atom given to cell 'translateAtom' function is NULL.");
				result = false;
				break;
			}
			rv.set(ii->r() + ptr->fracToReal( node->arg3d(1) ));
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in CellVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("CellVariable::performFunction");
	return result;
}

/*
 * Variable Array
 */

// Constructor
CellArrayVariable::CellArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::CellData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* CellArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return CellVariable::accessorSearch(name, arrayIndex, argList);
}
