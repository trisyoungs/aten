/*
	*** Aten Variable
	*** src/parser/aten.cpp
	Copyright T. Youngs 2007-2010

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
#include "base/constants.h"
#include "classes/prefs.h"
#include "model/model.h"
#include "main/aten.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Constructors
AtenVariable::AtenVariable()
{
	// Private variables
	returnType_ = VTypes::AtenData;
	readOnly_ = TRUE;
}

// Destructor
AtenVariable::~AtenVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool AtenVariable::set(ReturnValue &rv)
{
	msg.print("A constant value (in this case Aten itself) cannot be assigned to.\n");
	return FALSE;
}

// Reset variable
void AtenVariable::reset()
{
	// No action
}

// Return value of node
bool AtenVariable::execute(ReturnValue &rv)
{
	rv.set(VTypes::AtenData, &aten);
	return TRUE;
}

// Print node contents
void AtenVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	Dnchar tab(offset+32);
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab.strcat("   |--> ");
	tab.strcat(prefix);

	// Output node data
	printf("[V]%s&%p (Aten) (constant value)\n", tab.get(), &aten);
}

/*
// Accessors
*/

// Accessor data
Accessor AtenVariable::accessorData[AtenVariable::nAccessors] = {
	{ "elements",	VTypes::ElementData,		elements().nElements(), TRUE },
	{ "frame",	VTypes::ModelData,		0, TRUE },
	{ "model",	VTypes::ModelData,		0, TRUE },
	{ "models",	VTypes::ModelData,		-1, TRUE },
	{ "nelements",	VTypes::IntegerData,		0, TRUE },
	{ "nmodels",	VTypes::IntegerData,		0, TRUE },
	{ "prefs",	VTypes::PreferencesData,	0, TRUE }
};

// Function data
FunctionAccessor AtenVariable::functionData[AtenVariable::nFunctions] = {
	{ "convertenergy",	VTypes::DoubleData,	"NS",	"double value, string oldunits" },
	{ "findelement",	VTypes::ElementData,	"S",	"string name" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *AtenVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return AtenVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *AtenVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("AtenVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'aten&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("AtenVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'aten&' function '%s'.\n", s);
			msg.exit("AtenVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::AtenData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'aten&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::AtenData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("AtenVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool AtenVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("AtenVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		msg.exit("AtenVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("AtenVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < (acc == AtenVariable::Elements ? 0 : 1)) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("AtenVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Variables used in retrieval
	Model *m = NULL;
	bool result = TRUE;
	if (result) switch (acc)
	{
		case (AtenVariable::Elements):
			if (hasArrayIndex)
			{
				if ((arrayIndex < 0) || (arrayIndex > elements().nElements()))
				{
					msg.print("Array index [%i] is out of range for 'elements' member.\n", arrayIndex);
					result = FALSE;
				}
				else rv.set(VTypes::ElementData, &elements().el[arrayIndex]);
				// Note: array index is not decreased by 1, since element 0 is 'XXX'
			}
			else rv.set(VTypes::ElementData, &elements().el[0]);
			break;
		case (AtenVariable::Frame):
			rv.set(VTypes::ModelData, aten.currentModel()->renderSourceModel());
			break;
		case (AtenVariable::Modeldata):
			rv.set(VTypes::ModelData, aten.currentModel());
			break;
		case (AtenVariable::Models):
			if (hasArrayIndex)
			{
				if ((arrayIndex < 1) || (arrayIndex > aten.nModels()))
				{
					msg.print("Array index [%i] is out of range for 'model' member.\n", arrayIndex);
					result = FALSE;
				}
				else m = aten.model(arrayIndex-1);
			}
			else m = aten.model(0);
			if (m == NULL) result = FALSE;
			rv.set(VTypes::ModelData, m);
			break;
		case (AtenVariable::NElements):
			rv.set(elements().nElements());
			break;
		case (AtenVariable::NModels):
			rv.set(aten.nModels());
			break;
		case (AtenVariable::Preferences):
			rv.set(VTypes::PreferencesData, &prefs);
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in AtenVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("AtenVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool AtenVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("AtenVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		msg.exit("AtenVariable::setAccessor");
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
		msg.exit("AtenVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Aten *ptr= (Aten*) sourcerv.asPointer(VTypes::AtenData, result);
	switch (acc)
	{
		default:
			printf("AtenVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("AtenVariable::setAccessor");
	return result;
}

// Perform desired function
bool AtenVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("AtenVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Aten type.\n", i);
		msg.exit("AtenVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	int el;
	Prefs::EnergyUnit eu;
	Aten *ptr= (Aten*) rv.asPointer(VTypes::AtenData, result);
	if (result) switch (i)
	{
		case (AtenVariable::ConvertEnergy):
			eu = Prefs::energyUnit(node->argc(1), TRUE);
			if (eu == Prefs::nEnergyUnits) result = FALSE;
			else rv.set( prefs.convertEnergy(node->argd(0), eu) );
			break;
		case (AtenVariable::FindElement):
			el = elements().find(node->argc(0));
			if (el != 0) rv.set(VTypes::ElementData, &elements().el[el]);
			else rv.set(VTypes::ElementData, NULL);
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in AtenVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("AtenVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void AtenVariable::printAccessors()
{
	if (AtenVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<AtenVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((AtenVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<AtenVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}
