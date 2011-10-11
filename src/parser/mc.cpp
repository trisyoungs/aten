/*
	*** Monte Carlo Variable
	*** src/parser/mc.cpp
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

#include "parser/mc.h"
#include "parser/stepnode.h"
#include "methods/mc.h"
#include "parser/commandnode.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Constructors
MonteCarloVariable::MonteCarloVariable()
{
	// Private variables
	returnType_ = VTypes::MonteCarloData;
	readOnly_ = TRUE;
}

// Destructor
MonteCarloVariable::~MonteCarloVariable()
{
}

/*
// Accessors
*/

// Accessor data - name, type, arraysize, ro?
Accessor MonteCarloVariable::accessorData[MonteCarloVariable::nAccessors] = {
	{ "disorderAccuracy",		VTypes::DoubleData,	0, FALSE },
	{ "disorderDeltaAngle",		VTypes::DoubleData,	0, FALSE },
	{ "disorderDeltaDistance",	VTypes::DoubleData,	0, FALSE },
	{ "disorderMaxCycles",		VTypes::IntegerData,	0, FALSE },
	{ "disorderMaxFailures",	VTypes::IntegerData,	0, FALSE },
	{ "disorderMaximumScaleFactor",	VTypes::DoubleData,	0, FALSE },
	{ "disorderMinimumScaleFactor",	VTypes::DoubleData,	0, FALSE },
	{ "disorderNTweaks",		VTypes::IntegerData,	0, FALSE },
	{ "disorderRecoveryMaxCycles",	VTypes::IntegerData,	0, FALSE },
	{ "disorderRecoveryMaxTweaks",	VTypes::IntegerData,	0, FALSE },
	{ "disorderRecoveryThreshold",	VTypes::DoubleData,	0, FALSE },
	{ "disorderReductionFactor",	VTypes::DoubleData,	0, FALSE },
	{ "nCycles",			VTypes::IntegerData,	0, FALSE },
	{ "temperature",		VTypes::DoubleData,	0, FALSE }
};

// Function data
FunctionAccessor MonteCarloVariable::functionData[MonteCarloVariable::nFunctions] = {
	{ "eAccept",		VTypes::DoubleData,	"Cn", "string movetype, double newvalue = <not set>" },
	{ "maxStep",		VTypes::DoubleData,	"Cn", "string movetype, double newvalue = <not set>" },
	{ "moveAllowed",	VTypes::IntegerData,	"Cn", "string movetype, int allowed = <not set>" },
	{ "nTrials",		VTypes::IntegerData,	"Cn", "string movetype, int newvalue = <not set>" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *MonteCarloVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return MonteCarloVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *MonteCarloVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("MonteCarloVariable::accessorSearch");
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
			msg.print("Error: Type 'mc&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("MonteCarloVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'mc&' function '%s'.\n", s);
			msg.exit("MonteCarloVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::MonteCarloData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'mc&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::MonteCarloData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("MonteCarloVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool MonteCarloVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("MonteCarloVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for MonteCarlo type.\n", i);
		msg.exit("MonteCarloVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("MonteCarloVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("MonteCarloVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Variables used in retrieval
	bool result;
	MonteCarlo *ptr = (MonteCarlo*) rv.asPointer(VTypes::MonteCarloData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::MonteCarloData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (MonteCarloVariable::DisorderAccuracy):
			rv.set( ptr->disorderAccuracy() );
			break;
		case (MonteCarloVariable::DisorderDeltaAngle):
			rv.set( ptr->disorderDeltaAngle() );
			break;
		case (MonteCarloVariable::DisorderDeltaDistance):
			rv.set( ptr->disorderDeltaDistance() );
			break;
		case (MonteCarloVariable::DisorderMaxCycles):
			rv.set( ptr->disorderMaxCycles() );
			break;
		case (MonteCarloVariable::DisorderMaxFailures):
			rv.set( ptr->disorderMaxFailures() );
			break;
		case (MonteCarloVariable::DisorderMaximumScaleFactor):
			rv.set( ptr->disorderMaximumScaleFactor() );
			break;
		case (MonteCarloVariable::DisorderMinimumScaleFactor):
			rv.set( ptr->disorderMinimumScaleFactor() );
			break;
		case (MonteCarloVariable::DisorderNTweaks):
			rv.set( ptr->disorderNTweaks() );
			break;
		case (MonteCarloVariable::DisorderRecoveryMaxCycles):
			rv.set( ptr->disorderRecoveryMaxCycles() );
			break;
		case (MonteCarloVariable::DisorderRecoveryMaxTweaks):
			rv.set( ptr->disorderRecoveryMaxTweaks() );
			break;
		case (MonteCarloVariable::DisorderRecoveryThreshold):
			rv.set( ptr->disorderRecoveryThreshold() );
			break;
		case (MonteCarloVariable::DisorderReductionFactor):
			rv.set( ptr->disorderReductionFactor() );
			break;
		case (MonteCarloVariable::NCycles):
			rv.set( ptr->nCycles() );
			break;
		case (MonteCarloVariable::Temperature):
			rv.set( ptr->temperature() );
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in MonteCarloVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("MonteCarloVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool MonteCarloVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("MonteCarloVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for MonteCarlo type.\n", i);
		msg.exit("MonteCarloVariable::setAccessor");
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
		msg.exit("MonteCarloVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	MonteCarlo *ptr = (MonteCarlo*) sourcerv.asPointer(VTypes::MonteCarloData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::MonteCarloData));
		result = FALSE;
	}
	int n;
	if (result) switch (acc)
	{
		case (MonteCarloVariable::DisorderAccuracy):
			ptr->setDisorderAccuracy( newvalue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderDeltaAngle):
			ptr->setDisorderDeltaAngle( newvalue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderDeltaDistance):
			ptr->setDisorderDeltaDistance( newvalue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderMaxCycles):
			ptr->setDisorderMaxCycles( newvalue.asInteger(result) );
			break;
		case (MonteCarloVariable::DisorderMaxFailures):
			ptr->setDisorderMaxFailures( newvalue.asInteger(result) );
			break;
		case (MonteCarloVariable::DisorderMaximumScaleFactor):
			ptr->setDisorderMinimumScaleFactor( newvalue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderMinimumScaleFactor):
			ptr->setDisorderMinimumScaleFactor( newvalue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderNTweaks):
			ptr->setDisorderNTweaks( newvalue.asInteger(result) );
			break;
		case (MonteCarloVariable::DisorderRecoveryMaxCycles):
			ptr->setDisorderMaxCycles( newvalue.asInteger(result) );
			break;
		case (MonteCarloVariable::DisorderRecoveryMaxTweaks):
			ptr->setDisorderRecoveryMaxTweaks( newvalue.asInteger(result) );
			break;
		case (MonteCarloVariable::DisorderRecoveryThreshold):
			ptr->setDisorderRecoveryThreshold( newvalue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderReductionFactor):
			ptr->setDisorderReductionFactor( newvalue.asDouble(result) );
			break;
		case (MonteCarloVariable::NCycles):
			ptr->setNCycles( newvalue.asInteger(result) );
			break;
		case (MonteCarloVariable::Temperature):
			ptr->setTemperature( newvalue.asDouble(result) );
			break;
		default:
			printf("MonteCarloVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("MonteCarloVariable::setAccessor");
	return result;
}

// Perform desired function
bool MonteCarloVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("MonteCarloVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for MonteCarlo type.\n", i);
		msg.exit("MonteCarloVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	MonteCarlo::MoveType mt;
	MonteCarlo *ptr = (MonteCarlo*) rv.asPointer(VTypes::MonteCarloData, result);
	if (result) switch (i)
	{
		case (MonteCarloVariable::AcceptanceEnergy):
			mt = MonteCarlo::moveType(node->argc(0), TRUE);
			if (mt == MonteCarlo::nMoveTypes) return FALSE;
			if (node->hasArg(1)) ptr->setAcceptanceEnergy(mt, node->argd(1));
			rv.set( ptr->acceptanceEnergy(mt) );
			break;
		case (MonteCarloVariable::MaxStep):
			mt = MonteCarlo::moveType(node->argc(0), TRUE);
			if (mt == MonteCarlo::nMoveTypes) return FALSE;
			if (node->hasArg(1)) ptr->setMaxStep(mt, node->argd(1));
			rv.set( ptr->maxStep(mt) );
			break;
		case (MonteCarloVariable::MoveAllowed):
			mt = MonteCarlo::moveType(node->argc(0), TRUE);
			if (mt == MonteCarlo::nMoveTypes) return FALSE;
			if (node->hasArg(1)) ptr->setMoveAllowed(mt, node->argb(1));
			rv.set( ptr->isMoveAllowed(mt) );
			break;
		case (MonteCarloVariable::NTrials):
			mt = MonteCarlo::moveType(node->argc(0), TRUE);
			if (mt == MonteCarlo::nMoveTypes) return FALSE;
			if (node->hasArg(1)) ptr->setNTrials(mt, node->argi(1));
			rv.set( ptr->nTrials(mt) );
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in MonteCarloVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("MonteCarloVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void MonteCarloVariable::printAccessors()
{
	if (MonteCarloVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<MonteCarloVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((MonteCarloVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<MonteCarloVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}
