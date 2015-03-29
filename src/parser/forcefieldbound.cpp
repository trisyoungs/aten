/*
	*** ForcefieldBound Variable and Array
	*** src/parser/forcefieldbound.cpp
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

#include "parser/forcefieldbound.h"
#include "parser/stepnode.h"
#include "base/forcefieldbound.h"
#include "base/prefs.h"

ATEN_USING_NAMESPACE

/*
// Variable
*/

// Constructor
ForcefieldBoundVariable::ForcefieldBoundVariable(ForcefieldBound* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldBoundData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ForcefieldBoundVariable::~ForcefieldBoundVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor ForcefieldBoundVariable::accessorData[ForcefieldBoundVariable::nAccessors] = {
	{ "data",		VTypes::DoubleData,	MAXFFPARAMDATA, FALSE },
	{ "dataKeyword",	VTypes::StringData,	MAXFFPARAMDATA, TRUE },
	{ "dataName",		VTypes::StringData,	MAXFFPARAMDATA, TRUE },
	{ "eScale",		VTypes::DoubleData,	0, FALSE },
	{ "form",		VTypes::StringData,	0, FALSE },
	{ "nAtoms",		VTypes::IntegerData,	0, TRUE },
	{ "nParams",		VTypes::IntegerData,	0, TRUE },
	{ "type",		VTypes::StringData,	0, TRUE },
	{ "typeNames",		VTypes::StringData,	MAXFFPARAMDATA, FALSE },
	{ "vScale",		VTypes::DoubleData,	0, FALSE }
};

// Function data
FunctionAccessor ForcefieldBoundVariable::functionData[ForcefieldBoundVariable::nFunctions] = {
	{ "parameter",		VTypes::DoubleData,	"C",	"string name" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* ForcefieldBoundVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ForcefieldBoundVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* ForcefieldBoundVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("ForcefieldBoundVariable::accessorSearch");
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
			Messenger::print("Error: Type 'FFBound&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("ForcefieldBoundVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'FFBound&' function named '%s'.", qPrintable(name));
			Messenger::exit("ForcefieldBoundVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ForcefieldBoundData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'FFBound&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			Messenger::print("Error: Argument list given to 'FFBound&' array member '%s'.", qPrintable(name));
			Messenger::exit("ForcefieldBoundVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::ForcefieldBoundData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("ForcefieldBoundVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ForcefieldBoundVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ForcefieldBoundVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ForcefieldBound type.\n", i);
		Messenger::exit("ForcefieldBoundVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("ForcefieldBoundVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ForcefieldBoundVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	int n;
	ForcefieldBound* ptr = (ForcefieldBound*) rv.asPointer(VTypes::ForcefieldBoundData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ForcefieldBoundData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ForcefieldBoundVariable::Data):
			if (hasArrayIndex)
			{
				rv.set(ptr->parameter(arrayIndex-1));
				// Autoconversion of energy parameters?
				if ((prefs.autoConversionUnit() != Prefs::nEnergyUnits) && BondFunctions::BondFunctions[ptr->bondForm()].isEnergyParameter[arrayIndex-1]) rv.set( prefs.convertEnergy(rv.asDouble(), prefs.energyUnit(), prefs.autoConversionUnit()) );
			}
			else
			{
				rv.setArray(VTypes::DoubleData, ptr->parameters(), MAXFFPARAMDATA);
				// Autoconversion of energy parameters?
				if (prefs.autoConversionUnit() != Prefs::nEnergyUnits)
				{
					for (n = 0; n<MAXFFPARAMDATA; ++n) if (BondFunctions::BondFunctions[ptr->bondForm()].isEnergyParameter[n])
						rv.setElement(n, prefs.convertEnergy(ptr->parameter(n), prefs.energyUnit(), prefs.autoConversionUnit()) );
				}
			}
			break;
		case (ForcefieldBoundVariable::DataKeyword):
			// Must have an array index here...
			if (!hasArrayIndex)
			{
				Messenger::print("Accessor 'datakeyword' must have an array index.");
				result = FALSE;
			}
			else switch (ptr->type())
			{
				case (ForcefieldBound::BondInteraction):
				case (ForcefieldBound::UreyBradleyInteraction):
					rv.set(BondFunctions::BondFunctions[ptr->bondForm()].parameterKeywords[arrayIndex-1]);
					break;
				case (ForcefieldBound::AngleInteraction):
					rv.set(AngleFunctions::AngleFunctions[ptr->angleForm()].parameterKeywords[arrayIndex-1]);
					break;
				case (ForcefieldBound::TorsionInteraction):
				case (ForcefieldBound::ImproperInteraction):
					rv.set(TorsionFunctions::TorsionFunctions[ptr->torsionForm()].parameterKeywords[arrayIndex-1]);
					break;
				default:
					break;
			}
			break;
		case (ForcefieldBoundVariable::DataName):
			// Must have an array index here...
			if (!hasArrayIndex)
			{
				Messenger::print("Accessor 'dataname' must have an array index.");
				result = FALSE;
			}
			else switch (ptr->type())
			{
				case (ForcefieldBound::BondInteraction):
				case (ForcefieldBound::UreyBradleyInteraction):
					rv.set(BondFunctions::BondFunctions[ptr->bondForm()].parameters[arrayIndex-1]);
					break;
				case (ForcefieldBound::AngleInteraction):
					rv.set(AngleFunctions::AngleFunctions[ptr->angleForm()].parameters[arrayIndex-1]);
					break;
				case (ForcefieldBound::TorsionInteraction):
				case (ForcefieldBound::ImproperInteraction):
					rv.set(TorsionFunctions::TorsionFunctions[ptr->torsionForm()].parameters[arrayIndex-1]);
					break;
				default:
					break;
			}
			break;
		case (ForcefieldBoundVariable::EScale):
			if (ptr->type() != ForcefieldBound::TorsionInteraction)
			{
				Messenger::print("Tried to retrieve the 1-4 coulombic scale factor for a non-torsion bound interaction.");
				result = FALSE;
			}
			else rv.set(ptr->elecScale());
			break;
		case (ForcefieldBoundVariable::Form):
			rv.set(ptr->formText());
			break;
		case (ForcefieldBoundVariable::NAtoms):
			rv.set(ForcefieldBound::boundTypeNAtoms(ptr->type()));
			break;
		case (ForcefieldBoundVariable::NParams):
			switch (ptr->type())
			{
				case (ForcefieldBound::BondInteraction):
				case (ForcefieldBound::UreyBradleyInteraction):
					rv.set(BondFunctions::BondFunctions[ptr->bondForm()].nParameters);
					break;
				case (ForcefieldBound::AngleInteraction):
					rv.set(AngleFunctions::AngleFunctions[ptr->angleForm()].nParameters);
					break;
				case (ForcefieldBound::TorsionInteraction):
				case (ForcefieldBound::ImproperInteraction):
					rv.set(TorsionFunctions::TorsionFunctions[ptr->torsionForm()].nParameters);
					break;
			}
			break;
		case (ForcefieldBoundVariable::Type):
			rv.set(ForcefieldBound::boundType(ptr->type()));
			break;
		case (ForcefieldBoundVariable::TypeNames):
			if (hasArrayIndex) rv.set(ptr->typeName(arrayIndex-1));
			else rv.setArray(VTypes::StringData, ptr->typeNames(), MAXFFPARAMDATA);
			break;
		case (ForcefieldBoundVariable::VScale):
			if (ptr->type() != ForcefieldBound::TorsionInteraction)
			{
				Messenger::print("Tried to retrieve the 1-4 VDW scale factor for a non-torsion bound interaction.");
				result = FALSE;
			}
			else rv.set(ptr->vdwScale());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ForcefieldBoundVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ForcefieldBoundVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ForcefieldBoundVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ForcefieldBoundVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ForcefieldBound type.\n", i);
		Messenger::exit("ForcefieldBoundVariable::setAccessor");
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
		Messenger::exit("ForcefieldBoundVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	ForcefieldBound* ptr = (ForcefieldBound*) sourcerv.asPointer(VTypes::ForcefieldBoundData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ForcefieldBoundData));
		result = FALSE;
	}
	int n;
	if (result) switch (acc)
	{
		case (ForcefieldBoundVariable::Data):
			if ((newValue.arraySize() != -1) && (newValue.arraySize() <= MAXFFPARAMDATA)) for (n=0; n<newValue.arraySize(); ++n) ptr->setParameter(n, newValue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setParameter(arrayIndex-1, newValue.asDouble());
			else for (n=0; n<MAXFFPARAMDATA; ++n) ptr->setParameter(n, newValue.asDouble());
			break;
		case (ForcefieldBoundVariable::EScale):
			if (ptr->type() != ForcefieldBound::TorsionInteraction)
			{
				Messenger::print("Tried to set the 1-4 coulombic scale factor for a non-torsion bound interaction.");
				result = FALSE;
			}
			else ptr->setElecScale( newValue.asDouble() );
			break;
		case (ForcefieldBoundVariable::Form):
			result = ptr->setForm(newValue.asString());
			break;
		case (ForcefieldBoundVariable::TypeNames):
			if ((newValue.arraySize() != -1) && (newValue.arraySize() <= MAXFFBOUNDTYPES)) for (n=0; n<newValue.arraySize(); ++n) ptr->setTypeName(n, newValue.asString(n, result));
			else if (hasArrayIndex) ptr->setTypeName(arrayIndex-1, newValue.asString());
			else for (n=0; n<MAXFFBOUNDTYPES; ++n) ptr->setTypeName(n, newValue.asString());
			break;
		case (ForcefieldBoundVariable::VScale):
			if (ptr->type() != ForcefieldBound::TorsionInteraction)
			{
				Messenger::print("Tried to set the 1-4 coulombic scale factor for a non-torsion bound interaction.");
				result = FALSE;
			}
			else ptr->setVdwScale( newValue.asDouble() );
			break;
		default:
			printf("ForcefieldBoundVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ForcefieldBoundVariable::setAccessor");
	return result;
}

// Perform desired function
bool ForcefieldBoundVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("ForcefieldBoundVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for ForcefieldBound type.\n", i);
		Messenger::exit("ForcefieldBoundVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ForcefieldBound* ptr = (ForcefieldBound*) rv.asPointer(VTypes::ForcefieldBoundData, result);
	int id;
	if (result) switch (i)
	{
		case (ForcefieldBoundVariable::Parameter):
			switch (ptr->type())
			{
				case (ForcefieldBound::BondInteraction):
				case (ForcefieldBound::UreyBradleyInteraction):
					id = BondFunctions::bondParameter(ptr->bondForm(), node->argc(0), TRUE);
					if (id == BondFunctions::BondFunctions[ptr->bondForm()].nParameters) result = FALSE;
					else
					{
						// Autoconversion of energy parameters?
						if ((prefs.autoConversionUnit() != Prefs::nEnergyUnits) && BondFunctions::BondFunctions[ptr->bondForm()].isEnergyParameter[id]) rv.set( prefs.convertEnergy(ptr->parameter(id), prefs.energyUnit(), prefs.autoConversionUnit()) );
						else rv.set(ptr->parameter(id));
					}
					break;
				case (ForcefieldBound::AngleInteraction):
					id = AngleFunctions::angleParameter(ptr->angleForm(), node->argc(0), TRUE);
					if (id == AngleFunctions::AngleFunctions[ptr->angleForm()].nParameters) result = FALSE;
					else
					{
						// Autoconversion of energy parameters?
						if ((prefs.autoConversionUnit() != Prefs::nEnergyUnits) && AngleFunctions::AngleFunctions[ptr->angleForm()].isEnergyParameter[id]) rv.set( prefs.convertEnergy(ptr->parameter(id), prefs.energyUnit(), prefs.autoConversionUnit()) );
						else rv.set(ptr->parameter(id));
					}
					break;
				case (ForcefieldBound::TorsionInteraction):
				case (ForcefieldBound::ImproperInteraction):
					id = TorsionFunctions::torsionParameter(ptr->torsionForm(), node->argc(0), TRUE);
					if (id == TorsionFunctions::TorsionFunctions[ptr->torsionForm()].nParameters) result = FALSE;
					else
					{
						// Autoconversion of energy parameters?
						if ((prefs.autoConversionUnit() != Prefs::nEnergyUnits) && TorsionFunctions::TorsionFunctions[ptr->torsionForm()].isEnergyParameter[id]) rv.set( prefs.convertEnergy(ptr->parameter(id), prefs.energyUnit(), prefs.autoConversionUnit()) );
						else rv.set(ptr->parameter(id));
					}
					break;
			}
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ForcefieldBoundVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ForcefieldBoundVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void ForcefieldBoundVariable::printAccessors()
{
	if (ForcefieldBoundVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<ForcefieldBoundVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((ForcefieldBoundVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<ForcefieldBoundVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}

/*
// Variable Array
*/

// Constructor
ForcefieldBoundArrayVariable::ForcefieldBoundArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldBoundData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* ForcefieldBoundArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ForcefieldBoundVariable::accessorSearch(name, arrayIndex, argList);
}
