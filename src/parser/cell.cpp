/*
	*** Cell Variable and Array
	*** src/parser/cell.cpp
	Copyright T. Youngs 2007-2011

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
#include "model/model.h"
#include <string.h>

/*
// Variable
*/

// Constructor
CellVariable::CellVariable(UnitCell *ptr, bool constant)
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
	{ "mim",		VTypes::VectorData,	"WW",		"Atom i | vector u, Atom j | vector v" },
	{ "mimd",		VTypes::VectorData,	"WW",		"Atom i | vector u, Atom j | vector v" },
	{ "translateAtom",	VTypes::VectorData,	"JNNN",		"Atom i, double dx, double dy, double dz" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *CellVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return CellVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *CellVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("CellVariable::accessorSearch");
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
			msg.print("Error: Type 'cell&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("CellVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'cell&' function '%s'.\n", s);
			msg.exit("CellVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::CellData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'cell&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::CellData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("CellVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool CellVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("CellVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Cell type.\n", i);
		msg.exit("CellVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("CellVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("CellVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	UnitCell *ptr = (UnitCell*) rv.asPointer(VTypes::CellData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::CellData));
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
				msg.print("Array index [%i] is out of range for 'matrix' member.\n", arrayIndex);
				result = FALSE;
			}
			else rv.set(ptr->axes()[((arrayIndex-1)/3)*4+(arrayIndex-1)%3]);
			break;
		case (CellVariable::SpacegroupId):
			rv.set(ptr->spacegroupId());
			break;
		case (CellVariable::SpacegroupName):
			rv.set(ptr->spacegroup());
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
	msg.exit("CellVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool CellVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("CellVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Cell type.\n", i);
		msg.exit("CellVariable::setAccessor");
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
		msg.exit("CellVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	UnitCell *ptr = (UnitCell*) sourcerv.asPointer(VTypes::CellData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::CellData));
		result = FALSE;
	}
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
			ptr->parent()->setCell( (UnitCell::CellParameter) acc, newvalue.asDouble());
			break;
		case (CellVariable::Matrix):
			// Cast accessor into a CellParameter
			ptr->parent()->setCell( (UnitCell::CellParameter) ((arrayIndex-1) + UnitCell::CellAX), newvalue.asDouble());
			break;
		case (CellVariable::SpacegroupId):
			ptr->parent()->setSpacegroup( newvalue.asString() );
			break;
		default:
			printf("CellVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("CellVariable::setAccessor");
	return result;
}

// Perform desired function
bool CellVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("CellVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Cell type.\n", i);
		msg.exit("CellVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Atom *ii, *jj;
	Vec3<double> v1, v2;
	UnitCell *ptr = (UnitCell*) rv.asPointer(VTypes::CellData, result);
	if (result) switch (i)
	{
		case (CellVariable::MinimumImage):
			if (node->argType(0) == VTypes::VectorData) v1 = node->argv(0);
			else
			{
				ii = (Atom*) node->argp(0, VTypes::AtomData);
				if (ii == NULL)
				{
					msg.print("Error: Source atom given to cell 'mim' function is NULL.\n");
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
					msg.print("Error: Reference atom given to cell 'mim' function is NULL.\n");
					result = FALSE;
					break;
				}
				v2 = jj->r();
			}
			rv.set(ptr->mim(v1,v2));
			break;
		case (CellVariable::MinimumImageDistance):
			if (node->argType(0) == VTypes::VectorData) v1 = node->argv(0);
			else
			{
				ii = (Atom*) node->argp(0, VTypes::AtomData);
				if (ii == NULL)
				{
					msg.print("Error: Source atom given to cell 'mimd' function is NULL.\n");
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
					msg.print("Error: Reference atom given to cell 'mimd' function is NULL.\n");
					result = FALSE;
					break;
				}
				v2 = jj->r();
			}
			rv.set(ptr->mimd(v1,v2));
			break;
		case (CellVariable::TranslateAtom):
			ii = (Atom*) node->argp(0, VTypes::AtomData);
			if (ii == NULL)
			{
				msg.print("Error: Target atom given to cell 'translateatom' function is NULL.\n");
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
	msg.exit("CellVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void CellVariable::printAccessors()
{
	if (CellVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<CellVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((CellVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<CellVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
CellArrayVariable::CellArrayVariable(TreeNode *sizeexpr, bool constant)
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
StepNode *CellArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return CellVariable::accessorSearch(s, arrayindex, arglist);
}
