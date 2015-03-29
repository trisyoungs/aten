/*
	*** PatternBound Variable
	*** src/parser/patternbound.cpp
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

#include "parser/patternbound.h"
#include "parser/stepnode.h"
#include "base/pattern.h"
#include "base/forcefieldbound.h"

ATEN_USING_NAMESPACE

// Constructor
PatternBoundVariable::PatternBoundVariable(PatternBound* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::PatternBoundData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
PatternBoundVariable::~PatternBoundVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor PatternBoundVariable::accessorData[PatternBoundVariable::nAccessors] = {
	{ "data", 	VTypes::DoubleData,		MAXFFPARAMDATA, true },
	{ "eScale", 	VTypes::DoubleData,		0, true },
	{ "form", 	VTypes::StringData,		0, true },
	{ "id", 	VTypes::IntegerData,		MAXFFBOUNDTYPES, true },
	{ "termId",	VTypes::IntegerData,		0, true },
	{ "type", 	VTypes::StringData,		0, true },
	{ "typeNames", 	VTypes::StringData,		MAXFFBOUNDTYPES, true },
	{ "vScale",	VTypes::DoubleData,		0, true }
};

// Function data
FunctionAccessor PatternBoundVariable::functionData[PatternBoundVariable::nFunctions] = {
	{ "parameter",	VTypes::DoubleData,	"C",	"string name" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* PatternBoundVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return PatternBoundVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* PatternBoundVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("PatternBoundVariable::accessorSearch");
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
			Messenger::print("Error: Type 'Bound&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("PatternBoundVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Bound&' function named '%s'.", qPrintable(name));
			Messenger::exit("PatternBoundVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::PatternBoundData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'Bound&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			Messenger::print("Error: Argument list given to 'Bound&' array member '%s'.", qPrintable(name));
			Messenger::exit("PatternBoundVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::PatternBoundData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("PatternBoundVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool PatternBoundVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("PatternBoundVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for PatternBound type.\n", i);
		Messenger::exit("PatternBoundVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("PatternBoundVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("PatternBoundVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	PatternBound* ptr = (PatternBound*) rv.asPointer(VTypes::PatternBoundData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::PatternBoundData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (PatternBoundVariable::Data):
			if (ptr->data() == NULL)
			{
				Messenger::print("NULL ForcefieldBound pointer found in PatternBound class.");
				result = false;
			}
			else if (hasArrayIndex) rv.set(ptr->data()->parameter(arrayIndex-1));
			else rv.setArray(VTypes::DoubleData, ptr->data()->parameters(), MAXFFPARAMDATA);
			break;
		case (PatternBoundVariable::EScale):
			if (ptr->data() == NULL)
			{
				Messenger::print("NULL ForcefieldBound pointer found in PatternBound class.");
				result = false;
			}
			else
			{
				if (ptr->data()->type() != ForcefieldBound::TorsionInteraction)
				{
					Messenger::print("Tried to retrieve the 1-4 coulombic scale factor for a non-torsion bound interaction.");
					result = false;
				}
				else rv.set(ptr->data()->elecScale());
			}
			break;
		case (PatternBoundVariable::Form):
			if (ptr->data() == NULL)
			{
				Messenger::print("NULL ForcefieldBound pointer found in PatternBound class.");
				result = false;
			}
			else rv.set(ptr->data()->formText());
			break;
		case (PatternBoundVariable::Id):
			if (ptr->data() == NULL)
			{
				Messenger::print("NULL ForcefieldBound pointer found in PatternBound class.");
				result = false;
			}
			else if (hasArrayIndex) rv.set(ptr->atomId(arrayIndex-1)+1);
			else
			{
				// Need to adjust atom ids to go from 1-N....
				int ids[MAXFFBOUNDTYPES];
				for (int n=0; n<MAXFFBOUNDTYPES; ++n) ids[n] = ptr->atomIds_[n]+1;
				rv.setArray(VTypes::IntegerData, &ids, MAXFFBOUNDTYPES);
			}
			break;
		case (PatternBoundVariable::TermId):
			rv.set(ptr->forcefieldDataId()+1);
			break;
		case (PatternBoundVariable::TypeNames):
			if (ptr->data() == NULL)
			{
				Messenger::print("NULL ForcefieldBound pointer found in PatternBound class.");
				result = false;
			}
			else if (hasArrayIndex) rv.set(ptr->data()->typeName(arrayIndex-1));
			else rv.setArray(VTypes::StringData, ptr->data()->typeNames(), MAXFFPARAMDATA);
			break;
		case (PatternBoundVariable::Type):
			if (ptr->data() == NULL)
			{
				Messenger::print("NULL ForcefieldBound pointer found in PatternBound class.");
				result = false;
			}
			else rv.set(ForcefieldBound::boundType(ptr->data()->type()));
			break;
		case (PatternBoundVariable::VScale):
			if (ptr->data() == NULL)
			{
				Messenger::print("NULL ForcefieldBound pointer found in PatternBound class.");
				result = false;
			}
			else
			{
				if (ptr->data()->type() != ForcefieldBound::TorsionInteraction)
				{
					Messenger::print("Tried to retrieve the 1-4 VDW scale factor for a non-torsion bound interaction.");
					result = false;
				}
				else rv.set(ptr->data()->vdwScale());
			}
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in PatternBoundVariable.\n", accessorData[i].name);
			result = false;
			break;
	}
	Messenger::exit("PatternBoundVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool PatternBoundVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("PatternBoundVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Pattern type.\n", i);
		Messenger::exit("PatternBoundVariable::setAccessor");
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
		Messenger::exit("PatternBoundVariable::setAccessor");
		return false;
	}
	// Get current data from ReturnValue
	PatternBound* ptr = (PatternBound*) sourcerv.asPointer(VTypes::PatternBoundData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::PatternBoundData));
		result = false;
	}
	// Set value based on enumerated id
	if (result) switch (acc)
	{
		default:
			printf("PatternBoundVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = false;
			break;
	}
	Messenger::exit("PatternBoundVariable::setAccessor");
	return result;
}

// Perform desired function
bool PatternBoundVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("PatternBoundVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for PatternBound type.\n", i);
		Messenger::exit("PatternBoundVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	PatternBound* ptr = (PatternBound*) rv.asPointer(VTypes::PatternBoundData, result);
	int id;
	if (result) switch (i)
	{
		case (PatternBoundVariable::Parameter):
			switch (ptr->data()->type())
			{
				case (ForcefieldBound::BondInteraction):
				case (ForcefieldBound::UreyBradleyInteraction):
					id = BondFunctions::bondParameter(ptr->data()->bondForm(), node->argc(0), true);
					if (id == BondFunctions::BondFunctions[ptr->data()->bondForm()].nParameters) result = false;
					else rv.set(ptr->data()->parameter(id));
					break;
				case (ForcefieldBound::AngleInteraction):
					id = AngleFunctions::angleParameter(ptr->data()->angleForm(), node->argc(0), true);
					if (id == AngleFunctions::AngleFunctions[ptr->data()->angleForm()].nParameters) result = false;
					else rv.set(ptr->data()->parameter(id));
					break;
				case (ForcefieldBound::TorsionInteraction):
				case (ForcefieldBound::ImproperInteraction):
					id = TorsionFunctions::torsionParameter(ptr->data()->torsionForm(), node->argc(0), true);
					if (id == TorsionFunctions::TorsionFunctions[ptr->data()->torsionForm()].nParameters) result = false;
					else rv.set(ptr->data()->parameter(id));
					break;
			}
			break;

		default:
			printf("Internal Error: Access to function '%s' has not been defined in PatternBoundVariable.\n", functionData[i].name);
			result = false;
			break;
	}
	Messenger::exit("PatternBoundVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void PatternBoundVariable::printAccessors()
{
	if (PatternBoundVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<PatternBoundVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((PatternBoundVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<PatternBoundVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}

/*
// Variable Array
*/

// Constructor
PatternBoundArrayVariable::PatternBoundArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::PatternBoundData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* PatternBoundArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return PatternBoundVariable::accessorSearch(name, arrayIndex, argList);
}
