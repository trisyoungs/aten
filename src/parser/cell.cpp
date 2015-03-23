/*
	*** Cell Variable and Array
	*** src/parser/cell.cpp
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

#include "parser/cell.h"
#include "parser/stepnode.h"
#include "base/cell.h"
#include "base/sysfunc.h"
#include "base/spacegroup.h"
#include "base/prefs.h"
#include "model/model.h"
#include <string.h>

ATEN_USING_NAMESPACE

/*
// Variable
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
// Accessors
*/

// Accessor data
Accessor CellVariable::accessorData[CellVariable::nAccessors] = {
	{ "a",		VTypes::DoubleData,	0, FALSE },
	{ "b",		VTypes::DoubleData,	0, FALSE },
	{ "c",		VTypes::DoubleData,	0, FALSE },
	{ "alpha",	VTypes::DoubleData,	0, FALSE },
	{ "beta",	VTypes::DoubleData,	0, FALSE },
	{ "gamma",	VTypes::DoubleData,	0, FALSE },
	{ "ax",		VTypes::DoubleData,	0, FALSE },
	{ "ay",		VTypes::DoubleData,	0, FALSE },
	{ "az",		VTypes::DoubleData,	0, FALSE },
	{ "bx",		VTypes::DoubleData,	0, FALSE },
	{ "by",		VTypes::DoubleData,	0, FALSE },
	{ "bz",		VTypes::DoubleData,	0, FALSE },
	{ "cx",		VTypes::DoubleData,	0, FALSE },
	{ "cy",		VTypes::DoubleData,	0, FALSE },
	{ "cz",		VTypes::DoubleData,	0, FALSE },
	{ "centre",	VTypes::VectorData,	0, TRUE },
	{ "centreX",	VTypes::DoubleData,	0, TRUE },
	{ "centreY",	VTypes::DoubleData,	0, TRUE },
	{ "centreZ",	VTypes::DoubleData,	0, TRUE },
	{ "density",	VTypes::DoubleData,	0, TRUE },
	{ "matrix", 	VTypes::DoubleData,	9, FALSE },
	{ "sgId",	VTypes::IntegerData,	0, FALSE },
	{ "sgName",	VTypes::StringData,	0, TRUE },
	{ "type",	VTypes::StringData,	0, TRUE },
	{ "volume",	VTypes::DoubleData,	0, TRUE },
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
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(name, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'Cell&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("CellVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Cell&' function named '%s'.", qPrintable(name));
			Messenger::exit("CellVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::CellData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'Cell&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("CellVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("CellVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	UnitCell* ptr = (UnitCell*) rv.asPointer(VTypes::CellData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::CellData));
		result = FALSE;
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
				result = FALSE;
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
			rv.set(lowerCase(UnitCell::cellType(ptr->type())));
			break;
		case (CellVariable::Volume):
			rv.set(ptr->volume());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in CellVariable.\n", accessorData[i].name);
			result = FALSE;
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
				Messenger::print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = FALSE;
			}
			if (newValue.arraySize() > 0)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = FALSE;
			}
		}
		else
		{
			if (newValue.arraySize() > accessorData[i].arraySize)
			{
				Messenger::print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).", accessorData[i].name, newValue.arraySize(), accessorData[i].arraySize);
				result = FALSE;
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
				result = FALSE;
			}
			else if ((newValue.type() != VTypes::VectorData) && (newValue.arraySize() != 3))
			{
				Messenger::print("Error: Only an array of size 3 can be assigned to a vector (member '%s').", accessorData[i].name);
				result = FALSE;
			}
		}
	}
	if (!result)
	{
		Messenger::exit("CellVariable::setAccessor");
		return FALSE;
	}
	
	// Get current data from ReturnValue
	UnitCell* ptr = (UnitCell*) sourcerv.asPointer(VTypes::CellData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::CellData));
		result = FALSE;
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
			ptr->setSpacegroup( qPrintable(newValue.asString()), prefs.forceRhombohedral() );
			break;
		default:
			printf("CellVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
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
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
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
					result = FALSE;
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
					result = FALSE;
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
					result = FALSE;
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
					result = FALSE;
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
				result = FALSE;
				break;
			}
			rv.set(ii->r() + ptr->fracToReal( node->arg3d(1) ));
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in CellVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("CellVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void CellVariable::printAccessors()
{
	if (CellVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		for (int n=0; n<CellVariable::nAccessors; ++n) Messenger::print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print("");
	}
	if ((CellVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		for (int n=0; n<CellVariable::nFunctions; ++n) Messenger::print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print("");
	}
}

/*
// Variable Array
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
