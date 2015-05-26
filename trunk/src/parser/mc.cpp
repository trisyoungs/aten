/*
	*** Monte Carlo Variable
	*** src/parser/mc.cpp
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

#include "parser/mc.h"
#include "parser/stepnode.h"
#include "methods/mc.h"

ATEN_USING_NAMESPACE

// Constructors
MonteCarloVariable::MonteCarloVariable()
{
	// Private variables
	returnType_ = VTypes::MonteCarloData;
	readOnly_ = true;
}

// Destructor
MonteCarloVariable::~MonteCarloVariable()
{
}

/*
 * Accessors
 */

// Accessor data - name, type, arraysize, ro?
Accessor MonteCarloVariable::accessorData[MonteCarloVariable::nAccessors] = {
	{ "disorderAccuracy",		VTypes::DoubleData,	0, false },
	{ "disorderDeltaAngle",		VTypes::DoubleData,	0, false },
	{ "disorderDeltaDistance",	VTypes::DoubleData,	0, false },
	{ "disorderMaxCycles",		VTypes::IntegerData,	0, false },
	{ "disorderMaxFailures",	VTypes::IntegerData,	0, false },
	{ "disorderMaximumScaleFactor",	VTypes::DoubleData,	0, false },
	{ "disorderMinimumScaleFactor",	VTypes::DoubleData,	0, false },
	{ "disorderNTweaks",		VTypes::IntegerData,	0, false },
	{ "disorderRecoveryMaxCycles",	VTypes::IntegerData,	0, false },
	{ "disorderRecoveryMaxTweaks",	VTypes::IntegerData,	0, false },
	{ "disorderRecoveryThreshold",	VTypes::DoubleData,	0, false },
	{ "disorderReductionFactor",	VTypes::DoubleData,	0, false },
	{ "nCycles",			VTypes::IntegerData,	0, false },
	{ "temperature",		VTypes::DoubleData,	0, false }
};

// Function data
FunctionAccessor MonteCarloVariable::functionData[MonteCarloVariable::nFunctions] = {
	{ "eAccept",		VTypes::DoubleData,	"Cn", "string movetype, double newValue = <not set>" },
	{ "maxStep",		VTypes::DoubleData,	"Cn", "string movetype, double newValue = <not set>" },
	{ "moveAllowed",	VTypes::IntegerData,	"Cn", "string movetype, int allowed = <not set>" },
	{ "nTrials",		VTypes::IntegerData,	"Cn", "string movetype, int newValue = <not set>" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* MonteCarloVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return MonteCarloVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* MonteCarloVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("MonteCarloVariable::accessorSearch");
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
			Messenger::print("Error: Type 'MC&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("MonteCarloVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'MC&' function named '%s'.", qPrintable(name));
			Messenger::exit("MonteCarloVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::MonteCarloData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'MC&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			Messenger::print("Error: Argument list given to 'MC&' array member '%s'.", qPrintable(name));
			Messenger::exit("MonteCarloVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::MonteCarloData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("MonteCarloVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool MonteCarloVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("MonteCarloVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for MonteCarlo type.\n", i);
		Messenger::exit("MonteCarloVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("MonteCarloVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("MonteCarloVariable::retrieveAccessor");
			return false;
		}
	}
	// Variables used in retrieval
	bool result;
	MonteCarlo* ptr = (MonteCarlo*) rv.asPointer(VTypes::MonteCarloData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::MonteCarloData));
		result = false;
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
			result = false;
			break;
	}
	Messenger::exit("MonteCarloVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool MonteCarloVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("MonteCarloVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for MonteCarlo type.\n", i);
		Messenger::exit("MonteCarloVariable::setAccessor");
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
		Messenger::exit("MonteCarloVariable::setAccessor");
		return false;
	}
	// Get current data from ReturnValue
	MonteCarlo* ptr = (MonteCarlo*) sourcerv.asPointer(VTypes::MonteCarloData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::MonteCarloData));
		result = false;
	}
	int n;
	if (result) switch (acc)
	{
		case (MonteCarloVariable::DisorderAccuracy):
			ptr->setDisorderAccuracy( newValue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderDeltaAngle):
			ptr->setDisorderDeltaAngle( newValue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderDeltaDistance):
			ptr->setDisorderDeltaDistance( newValue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderMaxCycles):
			ptr->setDisorderMaxCycles( newValue.asInteger(result) );
			break;
		case (MonteCarloVariable::DisorderMaxFailures):
			ptr->setDisorderMaxFailures( newValue.asInteger(result) );
			break;
		case (MonteCarloVariable::DisorderMaximumScaleFactor):
			ptr->setDisorderMinimumScaleFactor( newValue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderMinimumScaleFactor):
			ptr->setDisorderMinimumScaleFactor( newValue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderNTweaks):
			ptr->setDisorderNTweaks( newValue.asInteger(result) );
			break;
		case (MonteCarloVariable::DisorderRecoveryMaxCycles):
			ptr->setDisorderMaxCycles( newValue.asInteger(result) );
			break;
		case (MonteCarloVariable::DisorderRecoveryMaxTweaks):
			ptr->setDisorderRecoveryMaxTweaks( newValue.asInteger(result) );
			break;
		case (MonteCarloVariable::DisorderRecoveryThreshold):
			ptr->setDisorderRecoveryThreshold( newValue.asDouble(result) );
			break;
		case (MonteCarloVariable::DisorderReductionFactor):
			ptr->setDisorderReductionFactor( newValue.asDouble(result) );
			break;
		case (MonteCarloVariable::NCycles):
			ptr->setNCycles( newValue.asInteger(result) );
			break;
		case (MonteCarloVariable::Temperature):
			ptr->setTemperature( newValue.asDouble(result) );
			break;
		default:
			printf("MonteCarloVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = false;
			break;
	}
	Messenger::exit("MonteCarloVariable::setAccessor");
	return result;
}

// Perform desired function
bool MonteCarloVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("MonteCarloVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for MonteCarlo type.\n", i);
		Messenger::exit("MonteCarloVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	MonteCarlo::MoveType mt;
	MonteCarlo* ptr = (MonteCarlo*) rv.asPointer(VTypes::MonteCarloData, result);
	if (result) switch (i)
	{
		case (MonteCarloVariable::AcceptanceEnergy):
			mt = MonteCarlo::moveType(node->argc(0), true);
			if (mt == MonteCarlo::nMoveTypes) return false;
			if (node->hasArg(1)) ptr->setAcceptanceEnergy(mt, node->argd(1));
			rv.set( ptr->acceptanceEnergy(mt) );
			break;
		case (MonteCarloVariable::MaxStep):
			mt = MonteCarlo::moveType(node->argc(0), true);
			if (mt == MonteCarlo::nMoveTypes) return false;
			if (node->hasArg(1)) ptr->setMaxStep(mt, node->argd(1));
			rv.set( ptr->maxStep(mt) );
			break;
		case (MonteCarloVariable::MoveAllowed):
			mt = MonteCarlo::moveType(node->argc(0), true);
			if (mt == MonteCarlo::nMoveTypes) return false;
			if (node->hasArg(1)) ptr->setMoveAllowed(mt, node->argb(1));
			rv.set( ptr->isMoveAllowed(mt) );
			break;
		case (MonteCarloVariable::NTrials):
			mt = MonteCarlo::moveType(node->argc(0), true);
			if (mt == MonteCarlo::nMoveTypes) return false;
			if (node->hasArg(1)) ptr->setNTrials(mt, node->argi(1));
			rv.set( ptr->nTrials(mt) );
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in MonteCarloVariable.\n", functionData[i].name);
			result = false;
			break;
	}
	Messenger::exit("MonteCarloVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void MonteCarloVariable::printAccessors()
{
	if (MonteCarloVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<MonteCarloVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((MonteCarloVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<MonteCarloVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}
