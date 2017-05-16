/*
	*** GlyphData Variable and Array
	*** src/parser/glyphdata.cpp
	Copyright T. Youngs 2007-2017

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

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
GlyphDataVariable::GlyphDataVariable(GlyphData* ptr, bool constant)
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
 * Accessors
 */

// Accessor data
Accessor GlyphDataVariable::accessorData[GlyphDataVariable::nAccessors] = {
	{ "atom",	VTypes::AtomData,	0, false },
	{ "atomdata",	VTypes::IntegerData,	0, false },
	{ "colour",	VTypes::DoubleData,	4, false },
	{ "vector",	VTypes::VectorData,	0, false }
};

// Function data
FunctionAccessor GlyphDataVariable::functionData[GlyphDataVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* GlyphDataVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return GlyphDataVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* GlyphDataVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("GlyphDataVariable::accessorSearch");
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
			Messenger::print("Error: Type 'GlyphData&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("GlyphDataVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'GlyphData&' function named '%s'.", qPrintable(name));
			Messenger::exit("GlyphDataVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::GlyphDataData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'GlyphData&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
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
			Messenger::print("Error: Argument list given to 'GlyphData&' array member '%s'.", qPrintable(name));
			Messenger::exit("GlyphDataVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::GlyphDataData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("GlyphDataVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool GlyphDataVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("GlyphDataVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Glyph type.\n", i);
		Messenger::exit("GlyphDataVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("GlyphDataVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("GlyphDataVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	GlyphData* ptr = (GlyphData*) rv.asPointer(VTypes::GlyphDataData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::GlyphDataData));
		result = false;
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
			printf("Internal Error: Access to member '%s' has not been defined in GlyphDataVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("GlyphDataVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool GlyphDataVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("GlyphDataVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Glyph type.\n", i);
		Messenger::exit("GlyphDataVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("GlyphDataVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	GlyphData* ptr = (GlyphData*) sourcerv.asPointer(VTypes::GlyphDataData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::GlyphDataData));
		result = false;
	}
	int n;
	if (result) switch (acc)
	{
		case (GlyphDataVariable::Atom_Ptr):
			ptr->setAtom( (Atom*) newValue.asPointer(VTypes::AtomData, result) );
			break;
		case (GlyphDataVariable::AtomData):
			ptr->setAtomData( (GlyphData::GlyphDataType) newValue.asInteger() );
			break;
		case (GlyphDataVariable::Colour):
			if (newValue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->setColour(n, newValue.asVector(result)[n]);
			else if (newValue.arraySize() != -1) for (n=0; n<newValue.arraySize(); ++n) ptr->setColour(n, newValue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setColour(arrayIndex-1, newValue.asDouble(result));
			else for (n=0; n<4; ++n) ptr->setColour(n, newValue.asDouble(result));
			break;
		case (GlyphDataVariable::Vector):
			ptr->setVector(newValue.asVector());
			break;
		default:
			printf("GlyphDataVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("GlyphDataVariable::setAccessor");
	return result;
}

// Perform desired function
bool GlyphDataVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("GlyphDataVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Glyph type.\n", i);
		Messenger::exit("GlyphDataVariable::performFunction");
		return false;
	}

	// Get current data from ReturnValue
	bool result = true;
	GlyphData* ptr = (GlyphData*) rv.asPointer(VTypes::GlyphDataData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in GlyphDataVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("GlyphDataVariable::performFunction");
	return result;
}

/*
 * Variable Array
 */

// Constructor
GlyphDataArrayVariable::GlyphDataArrayVariable(TreeNode* sizeexpr, bool constant)
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
StepNode* GlyphDataArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return GlyphDataVariable::accessorSearch(name, arrayIndex, argList);
}

