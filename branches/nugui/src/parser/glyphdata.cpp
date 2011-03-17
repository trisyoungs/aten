/*
	*** GlyphData Variable and Array
	*** src/parser/glyphdata.cpp
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

#include "parser/glyphdata.h"
#include "parser/stepnode.h"
#include "base/glyph.h"
#include "base/constants.h"
#include "base/elements.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
GlyphDataVariable::GlyphDataVariable(GlyphData *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::GlyphDataData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
GlyphDataVariable::~GlyphDataVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor GlyphDataVariable::accessorData[GlyphDataVariable::nAccessors] = {
	{ "atom",	VTypes::AtomData,	0, FALSE },
	{ "atomdata",	VTypes::IntegerData,	0, FALSE },
	{ "colour",	VTypes::DoubleData,	4, FALSE },
	{ "vector",	VTypes::VectorData,	0, FALSE }
};

// Function data
FunctionAccessor GlyphDataVariable::functionData[GlyphDataVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *GlyphDataVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return GlyphDataVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *GlyphDataVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("GlyphDataVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'glyphdata&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("GlyphDataVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'glyphdata&' function '%s'.\n", s);
			msg.exit("GlyphDataVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::GlyphDataData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'glyphdata&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::GlyphDataData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("GlyphDataVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool GlyphDataVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("GlyphDataVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Glyph type.\n", i);
		msg.exit("GlyphDataVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("GlyphDataVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("GlyphDataVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	GlyphData *ptr = (GlyphData*) rv.asPointer(VTypes::GlyphDataData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::GlyphDataData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (GlyphDataVariable::Atom_Ptr):
			rv.set( VTypes::AtomData, ptr->atom() );
			break;
		case (GlyphDataVariable::AtomData):
			rv.set( ptr->atomData() );
			break;
		case (GlyphDataVariable::Colour):
			if (hasArrayIndex) rv.set( ptr->colour()[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->colour(), 4);
			break;
		case (GlyphDataVariable::Vector):
			rv.set( ptr->vector() );
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in GlyphDataVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("GlyphDataVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool GlyphDataVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("GlyphDataVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Glyph type.\n", i);
		msg.exit("GlyphDataVariable::setAccessor");
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
			if ((newvalue.arraySize() > 0) && (accessorData[i].returnType != VTypes::VectorData))
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
		msg.exit("GlyphDataVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	GlyphData *ptr = (GlyphData*) sourcerv.asPointer(VTypes::GlyphDataData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::GlyphDataData));
		result = FALSE;
	}
	int n;
	if (result) switch (acc)
	{
		case (GlyphDataVariable::Atom_Ptr):
			ptr->setAtom( (Atom*) newvalue.asPointer(VTypes::AtomData, result) );
			break;
		case (GlyphDataVariable::AtomData):
			ptr->setAtomData( (GlyphData::GlyphDataType) newvalue.asInteger() );
			break;
		case (GlyphDataVariable::Colour):
			if (newvalue.arraySize() != -1) for (n=0; n<newvalue.arraySize(); ++n) ptr->setColour(n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setColour(arrayIndex-1, newvalue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setColour(n, newvalue.asDouble(result));
			break;
		case (GlyphDataVariable::Vector):
			ptr->setVector(newvalue.asVector());
			break;
		default:
			printf("GlyphDataVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("GlyphDataVariable::setAccessor");
	return result;
}

// Perform desired function
bool GlyphDataVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("GlyphDataVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Glyph type.\n", i);
		msg.exit("GlyphDataVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	GlyphData *ptr = (GlyphData*) rv.asPointer(VTypes::GlyphDataData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in GlyphDataVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("GlyphDataVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void GlyphDataVariable::printAccessors()
{
	if (GlyphDataVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<GlyphDataVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((GlyphDataVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<GlyphDataVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
GlyphDataArrayVariable::GlyphDataArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::GlyphDataData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *GlyphDataArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return GlyphDataVariable::accessorSearch(s, arrayindex, arglist);
}

