/*
	*** Aten Variable
	*** src/parser/aten.cpp
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

#include "parser/aten.h"
#include "parser/stepnode.h"
#include "math/constants.h"
#include "base/prefs.h"
#include "model/model.h"
#include "main/aten.h"
#include "methods/mc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ATEN_USING_NAMESPACE

// Constructors
AtenVariable::AtenVariable()
{
	// Private variables
	returnType_ = VTypes::AtenData;
	readOnly_ = true;
}

// Destructor
AtenVariable::~AtenVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool AtenVariable::set(ReturnValue& rv)
{
	Messenger::print("A constant value (in this case Aten itself) cannot be assigned to.");
	return false;
}

// Reset variable
void AtenVariable::reset()
{
	// No action
}

// Return value of node
bool AtenVariable::execute(ReturnValue& rv)
{
	rv.set(VTypes::AtenData, &aten_);
	return true;
}

/*
 * Variable Data
 */

// Print node contents
void AtenVariable::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	tab += prefix;

	// Output node data
	printf("[V]%s&%p (Aten) (constant value)\n", qPrintable(tab), &aten_);
}

/*
// Accessors
*/

// Accessor data
Accessor AtenVariable::accessorData[AtenVariable::nAccessors] = {
	{ "elements",	VTypes::ElementData,		Elements().nElements(), true },
	{ "frame",	VTypes::ModelData,		0, true },
	{ "mc",		VTypes::MonteCarloData,		0, true },
	{ "model",	VTypes::ModelData,		0, true },
	{ "models",	VTypes::ModelData,		-1, true },
	{ "nElements",	VTypes::IntegerData,		0, true },
	{ "nModels",	VTypes::IntegerData,		0, true },
	{ "prefs",	VTypes::PreferencesData,	0, true }
};

// Function data
FunctionAccessor AtenVariable::functionData[AtenVariable::nFunctions] = {
	{ "convertEnergy",	VTypes::DoubleData,	"NS",	"double value, string oldunits" },
	{ "findElement",	VTypes::ElementData,	"S",	"string name" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* AtenVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return AtenVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* AtenVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("AtenVariable::accessorSearch");
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
			Messenger::print("Error: Type 'Aten&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("AtenVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Aten&' function '%s'.", qPrintable(name));
			Messenger::exit("AtenVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::AtenData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'Aten&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			Messenger::print("Error: Argument list given to 'Aten&' array member '%s'.", qPrintable(name));
			Messenger::exit("AtenVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::AtenData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("AtenVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool AtenVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("AtenVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		Messenger::exit("AtenVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("AtenVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < (acc == AtenVariable::ElementsMap ? 0 : 1)) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("AtenVariable::retrieveAccessor");
			return false;
		}
	}
	// Variables used in retrieval
	Model* m = NULL;
	bool result = true;
	if (result) switch (acc)
	{
		case (AtenVariable::ElementsMap):
			if (hasArrayIndex)
			{
				if ((arrayIndex < 0) || (arrayIndex > Elements().nElements()))
				{
					Messenger::print("Array index [%i] is out of range for 'elements' member.", arrayIndex);
					result = false;
				}
				else rv.set(VTypes::ElementData, &Elements().el[arrayIndex]);
				// Note: array index is not decreased by 1, since element 0 is 'XX'
			}
			else rv.set(VTypes::ElementData, &Elements().el[0]);
			break;
		case (AtenVariable::Frame):
			if (aten_->currentModel() == NULL) rv.set(VTypes::ModelData, NULL);
			else rv.set(VTypes::ModelData, aten_->currentModel()->renderSourceModel());
			break;
		case (AtenVariable::MC):
			rv.set(VTypes::MonteCarloData, &mc);
			break;
		case (AtenVariable::Modeldata):
			rv.set(VTypes::ModelData, aten_->currentModel());
			break;
		case (AtenVariable::Models):
			if (hasArrayIndex)
			{
				if ((arrayIndex < 1) || (arrayIndex > aten_->nModels()))
				{
					Messenger::print("Array index [%i] is out of range for 'model' member.", arrayIndex);
					result = false;
				}
				else m = aten_->model(arrayIndex-1);
			}
			else m = aten_->model(0);
			if (m == NULL) result = false;
			rv.set(VTypes::ModelData, m);
			break;
		case (AtenVariable::NElements):
			rv.set(Elements().nElements());
			break;
		case (AtenVariable::NModels):
			rv.set(aten_->nModels());
			break;
		case (AtenVariable::Preferences):
			rv.set(VTypes::PreferencesData, &prefs);
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in AtenVariable.\n", accessorData[i].name);
			result = false;
			break;
	}
	Messenger::exit("AtenVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool AtenVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("AtenVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		Messenger::exit("AtenVariable::setAccessor");
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
		Messenger::exit("AtenVariable::setAccessor");
		return false;
	}
	// Get current data from ReturnValue
	Aten* ptr = (Aten*) sourcerv.asPointer(VTypes::AtenData, result);
	switch (acc)
	{
		default:
			printf("AtenVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = false;
			break;
	}
	Messenger::exit("AtenVariable::setAccessor");
	return result;
}

// Perform desired function
bool AtenVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("AtenVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Aten type.\n", i);
		Messenger::exit("AtenVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	int el;
	Prefs::EnergyUnit eu;
	Aten* ptr = (Aten*) rv.asPointer(VTypes::AtenData, result);
	if (result) switch (i)
	{
		case (AtenVariable::ConvertEnergy):
			eu = Prefs::energyUnit(node->argc(1), true);
			if (eu == Prefs::nEnergyUnits) result = false;
			else rv.set( prefs.convertEnergy(node->argd(0), eu) );
			break;
		case (AtenVariable::FindElement):
			el = Elements().find(node->argc(0));
			if (el != 0) rv.set(VTypes::ElementData, &Elements().el[el]);
			else rv.set(VTypes::ElementData, NULL);
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in AtenVariable.\n", functionData[i].name);
			result = false;
			break;
	}
	Messenger::exit("AtenVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void AtenVariable::printAccessors()
{
	if (AtenVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<AtenVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((AtenVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<AtenVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}
