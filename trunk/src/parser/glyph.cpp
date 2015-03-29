/*
	*** Glyph Variable and Array
	*** src/parser/glyph.cpp
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

#include "parser/glyph.h"
#include "parser/stepnode.h"
#include "base/glyph.h"

ATEN_USING_NAMESPACE

/*
// Variable
*/

// Constructor
GlyphVariable::GlyphVariable(Glyph* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::GlyphData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
GlyphVariable::~GlyphVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor GlyphVariable::accessorData[GlyphVariable::nAccessors] = {
	{ "data",	VTypes::GlyphDataData,	4, true },
	{ "ndata",	VTypes::IntegerData,	0, true },
	{ "rotated",	VTypes::IntegerData,	0, false },
	{ "rotation",	VTypes::DoubleData,	9, false },
	{ "selected",	VTypes::IntegerData,	0, false },
	{ "solid",	VTypes::IntegerData,	0, false },
	{ "text",	VTypes::StringData,	0, false },
	{ "type",	VTypes::StringData,	0, false },
	{ "visible",	VTypes::IntegerData,	0, false }
};

// Function data
FunctionAccessor GlyphVariable::functionData[GlyphVariable::nFunctions] = {
	{ "recolour",		VTypes::NoData,		"NNNn",	"double r, double g, double b, double a = 1.0" },
	{ "resetRotation",	VTypes::NoData,		"",	"" },
	{ "rotate",		VTypes::NoData,		"NNNN",	"double x, double y, double z, double angle" },
	{ "rotateX",		VTypes::NoData,		"N",	"double angle" },
	{ "rotateY",		VTypes::NoData,		"N",	"double angle" },
	{ "rotateZ",		VTypes::NoData,		"N",	"double angle" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* GlyphVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return GlyphVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* GlyphVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("GlyphVariable::accessorSearch");
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
			Messenger::print("Error: Type 'Glyph&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("GlyphVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Glyph&' function named '%s'.", qPrintable(name));
			Messenger::exit("GlyphVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::GlyphData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'Glyph&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			Messenger::print("Error: Argument list given to 'Glyph&' array member '%s'.", qPrintable(name));
			Messenger::exit("GlyphVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::GlyphData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("GlyphVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool GlyphVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("GlyphVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Glyph type.\n", i);
		Messenger::exit("GlyphVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("GlyphVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("GlyphVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	Glyph* ptr = (Glyph*) rv.asPointer(VTypes::GlyphData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::GlyphData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (GlyphVariable::Data):
			if (hasArrayIndex) rv.set( VTypes::GlyphDataData, ptr->data(arrayIndex-1) );
			else rv.set( VTypes::GlyphDataData, ptr->data(0) );
			break;
		case (GlyphVariable::NData):
			rv.set( Glyph::nGlyphData(ptr->type()) );
			break;
		case (GlyphVariable::Rotated):
			rv.set( ptr->rotated() );
			break;
		case (GlyphVariable::Rotation):
			if ((arrayIndex < 1) || (arrayIndex > 9))
			{
				Messenger::print("Array index [%i] is out of range for 'rotation' member.", arrayIndex);
				result = false;
			}
			else rv.set(ptr->getRotationElement(((arrayIndex-1)/3)*4 + (arrayIndex-1)%3));
			break;
		case (GlyphVariable::Selected):
			rv.set( ptr->isSelected() );
			break;
		case (GlyphVariable::Solid):
			rv.set( ptr->isSolid() );
			break;
		case (GlyphVariable::Text):
			rv.set( qPrintable(ptr->text()) );
			break;
		case (GlyphVariable::Type):
			rv.set( Glyph::glyphType(ptr->type()) );
			break;
		case (GlyphVariable::Visible):
			rv.set( ptr->isVisible() );
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in GlyphVariable.\n", accessorData[i].name);
			result = false;
			break;
	}
	Messenger::exit("GlyphVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool GlyphVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("GlyphVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Glyph type.\n", i);
		Messenger::exit("GlyphVariable::setAccessor");
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
			if ((newValue.arraySize() > 0) && (accessorData[i].returnType != VTypes::VectorData))
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
		Messenger::exit("GlyphVariable::setAccessor");
		return false;
	}
	// Get current data from ReturnValue
	Glyph* ptr = (Glyph*) sourcerv.asPointer(VTypes::GlyphData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::GlyphData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (GlyphVariable::Rotated):
			if (newValue.asInteger() == 0) ptr->resetRotation();
			break;
		case (GlyphVariable::Rotation):
			ptr->setRotationElement( ((arrayIndex-1)/3)*4 + (arrayIndex-1)%3, newValue.asDouble());
			break;
		case (GlyphVariable::Selected):
			ptr->setSelected(newValue.asBool());
			break;
		case (GlyphVariable::Solid):
			ptr->setSolid(newValue.asBool());
			break;
		case (GlyphVariable::Text):
			ptr->setText(newValue.asString());
			break;
		case (GlyphVariable::Visible):
			ptr->setVisible(newValue.asBool());
			break;
		default:
			printf("GlyphVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = false;
			break;
	}
	Messenger::exit("GlyphVariable::setAccessor");
	return result;
}

// Perform desired function
bool GlyphVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("GlyphVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Glyph type.\n", i);
		Messenger::exit("GlyphVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	Glyph* ptr = (Glyph*) rv.asPointer(VTypes::GlyphData, result);
	if (result) switch (i)
	{
		case (GlyphVariable::Recolour):
			ptr->setColour(node->argd(0), node->argd(1), node->argd(2), node->hasArg(3) ? node->argd(3) : 1.0);
			break;
		case (GlyphVariable::ResetRotation):
			ptr->resetRotation();
			rv.reset();
			break;
		case (GlyphVariable::Rotate):
			ptr->rotate(node->argd(0), node->argd(1), node->argd(2), node->argd(3));
			rv.reset();
			break;
		case (GlyphVariable::RotateX):
			ptr->rotateX(node->argd(0));
			rv.reset();
			break;
		case (GlyphVariable::RotateY):
			ptr->rotateY(node->argd(0));
			rv.reset();
			break;
		case (GlyphVariable::RotateZ):
			ptr->rotateZ(node->argd(0));
			rv.reset();
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in GlyphVariable.\n", functionData[i].name);
			result = false;
			break;
	}
	Messenger::exit("GlyphVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void GlyphVariable::printAccessors()
{
	if (GlyphVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<GlyphVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((GlyphVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<GlyphVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}

/*
// Variable Array
*/

// Constructor
GlyphArrayVariable::GlyphArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::GlyphData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* GlyphArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return GlyphVariable::accessorSearch(name, arrayIndex, argList);
}

