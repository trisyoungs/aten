/*
	*** ForcefieldAtom Variable and Array
	*** src/parser/forcefieldatom.cpp
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

#include "parser/forcefieldatom.h"
#include "parser/stepnode.h"
#include "base/forcefieldatom.h"
#include "base/prefs.h"
#include "math/constants.h"
#include "main/aten.h"

ATEN_USING_NAMESPACE

/*
// Variable
*/

// Constructor
ForcefieldAtomVariable::ForcefieldAtomVariable(ForcefieldAtom* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldAtomData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ForcefieldAtomVariable::~ForcefieldAtomVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor ForcefieldAtomVariable::accessorData[ForcefieldAtomVariable::nAccessors] = {
	{ "charge",		VTypes::DoubleData,		0, FALSE },
	{ "data",		VTypes::DoubleData,		MAXFFPARAMDATA, FALSE },
	{ "dataKeyword",	VTypes::StringData,		MAXFFPARAMDATA, TRUE },
	{ "dataName",		VTypes::StringData,		MAXFFPARAMDATA, TRUE },
	{ "description",	VTypes::StringData,		0, FALSE },
	{ "equivalent",		VTypes::StringData,		0, FALSE },
	{ "ff",			VTypes::ForcefieldData,		0, TRUE },
	{ "form",		VTypes::StringData,		0, FALSE },
	{ "id",			VTypes::IntegerData,		0, TRUE },
	{ "mass",		VTypes::DoubleData,		0, TRUE },
	{ "name",		VTypes::StringData,		0, FALSE },
	{ "neta",		VTypes::StringData,		0, FALSE },
	{ "nParams",		VTypes::IntegerData,		0, TRUE },
	{ "z",			VTypes::IntegerData,		0, FALSE }
};

// Function data
FunctionAccessor ForcefieldAtomVariable::functionData[ForcefieldAtomVariable::nFunctions] = {
	{ "combine",		VTypes::DoubleData,	"ON",	"FFAtom j, int param" },
	{ "dataD",		VTypes::DoubleData,	"C",	"string name" },
	{ "dataI",		VTypes::IntegerData,	"C",	"string name" },
	{ "dataS",		VTypes::StringData,	"C",	"string name" },
	{ "parameter",		VTypes::DoubleData,	"C",	"string name" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* ForcefieldAtomVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ForcefieldAtomVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* ForcefieldAtomVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("ForcefieldAtomVariable::accessorSearch");
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
			Messenger::print("Error: Type 'FFAtom&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("ForcefieldAtomVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'FFAtom&' function named '%s'.", qPrintable(name));
			Messenger::exit("ForcefieldAtomVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ForcefieldAtomData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'FFAtom&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			Messenger::print("Error: Argument list given to 'FFAtom&' array member '%s'.", qPrintable(name));
			Messenger::exit("ForcefieldAtomVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::ForcefieldAtomData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("ForcefieldAtomVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ForcefieldAtomVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ForcefieldAtomVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ForcefieldAtom type.\n", i);
		Messenger::exit("ForcefieldAtomVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("ForcefieldAtomVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ForcefieldAtomVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	int n;
	ForcefieldAtom* ptr = (ForcefieldAtom*) rv.asPointer(VTypes::ForcefieldAtomData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ForcefieldAtomData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ForcefieldAtomVariable::Charge):
			rv.set(ptr->charge());
			break;
		case (ForcefieldAtomVariable::Data):
			if (hasArrayIndex)
			{
				rv.set(ptr->parameter(arrayIndex-1));
				// Autoconversion of energy parameters?
				if ((prefs.autoConversionUnit() != Prefs::nEnergyUnits) && VdwFunctions::VdwFunctions[ptr->vdwForm()].isEnergyParameter[arrayIndex-1]) rv.set( prefs.convertEnergy(rv.asDouble(), prefs.energyUnit(), prefs.autoConversionUnit()) );
			}
			else
			{
				rv.setArray(VTypes::DoubleData, ptr->parameters(), MAXFFPARAMDATA);
				// Autoconversion of energy parameters?
				if (prefs.autoConversionUnit() != Prefs::nEnergyUnits)
				{
					for (n = 0; n<MAXFFPARAMDATA; ++n) if (VdwFunctions::VdwFunctions[ptr->vdwForm()].isEnergyParameter[n])
						rv.setElement(n, prefs.convertEnergy(ptr->parameter(n), prefs.energyUnit(), prefs.autoConversionUnit()) );
				}
			}
			break;
		case (ForcefieldAtomVariable::Description):
			rv.set(ptr->description());
			break;
		case (ForcefieldAtomVariable::DataKeyword):
			// Must have an array index here...
			if (!hasArrayIndex)
			{
				Messenger::print("Accessor 'datakeyword' must have an array index.");
				result = FALSE;
			}
			else rv.set(VdwFunctions::VdwFunctions[ptr->vdwForm()].parameterKeywords[arrayIndex-1]);
			break;
		case (ForcefieldAtomVariable::DataName):
			// Must have an array index here...
			if (!hasArrayIndex)
			{
				Messenger::print("Accessor 'dataname' must have an array index.");
				result = FALSE;
			}
			else rv.set(VdwFunctions::VdwFunctions[ptr->vdwForm()].parameters[arrayIndex-1]);
			break;
		case (ForcefieldAtomVariable::Equivalent):
			if (aten_->typeExportMapping()) rv.set(aten_->typeExportConvert(ptr->equivalent()));
			else rv.set(ptr->equivalent());
			break;
		case (ForcefieldAtomVariable::FField):
			rv.set(VTypes::ForcefieldData, ptr->parent());
			break;
		case (ForcefieldAtomVariable::Form):
			rv.set(VdwFunctions::VdwFunctions[ptr->vdwForm()].keyword);
			break;
		case (ForcefieldAtomVariable::Id):
			rv.set(ptr->typeId());
			break;
		case (ForcefieldAtomVariable::Mass):
			rv.set(ptr->elementMass());
			break;
		case (ForcefieldAtomVariable::Name):
			if (aten_->typeExportMapping()) rv.set(aten_->typeExportConvert(ptr->name()));
			else rv.set(ptr->name());
			break;
		case (ForcefieldAtomVariable::Neta):
			rv.set(ptr->netaString());
			break;
		case (ForcefieldAtomVariable::NParams):
			rv.set(VdwFunctions::VdwFunctions[ptr->vdwForm()].nParameters);
			break;
		case (ForcefieldAtomVariable::Z):
			rv.set(ptr->neta()->characterElement());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ForcefieldAtomVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ForcefieldAtomVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ForcefieldAtomVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ForcefieldAtomVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ForcefieldAtom type.\n", i);
		Messenger::exit("ForcefieldAtomVariable::retrieveAccessor");
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
		Messenger::exit("ForcefieldAtomVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	VdwFunctions::VdwFunction vf;
	int n;
	ForcefieldAtom* ptr = (ForcefieldAtom*) sourcerv.asPointer(VTypes::ForcefieldAtomData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ForcefieldAtomData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ForcefieldAtomVariable::Charge):
			ptr->setCharge(newValue.asDouble());
			break;
		case (ForcefieldAtomVariable::Data):
			if ((newValue.arraySize() != -1) && (newValue.arraySize() <= MAXFFPARAMDATA)) for (n=0; n<newValue.arraySize(); ++n) ptr->setParameter(n, newValue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setParameter(arrayIndex-1, newValue.asDouble());
			else for (n=0; n<MAXFFPARAMDATA; ++n) ptr->setParameter(n, newValue.asDouble());
			break;
		case (ForcefieldAtomVariable::Description):
			ptr->setDescription(newValue.asString());
			break;
		case (ForcefieldAtomVariable::Equivalent):
			ptr->setEquivalent(newValue.asString());
			break;
		case (ForcefieldAtomVariable::Form):
			vf = VdwFunctions::vdwFunction(newValue.asString());
			if (vf == VdwFunctions::None) result = FALSE;
			else ptr->setVdwForm(vf);
			break;
		case (ForcefieldAtomVariable::Name):
			ptr->setName(newValue.asString());
			break;
		case (ForcefieldAtomVariable::Neta):
			ptr->setNeta(newValue.asString(), NULL);
			break;
		case (ForcefieldAtomVariable::Z):
			ptr->neta()->setCharacterElement(newValue.asInteger());
			break;
		default:
			printf("ForcefieldAtomVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ForcefieldAtomVariable::setAccessor");
	return result;
}

// Perform desired function
bool ForcefieldAtomVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("ForcefieldAtomVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for ForcefieldAtom type.\n", i);
		Messenger::exit("ForcefieldAtomVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	int param;
	ForcefieldAtom* at2;
	ForcefieldAtom* ptr = (ForcefieldAtom*) rv.asPointer(VTypes::ForcefieldAtomData, result);
	Variable* v;
	int id;
	ReturnValue resultrv;
	if (result) switch (i)
	{
		case (ForcefieldAtomVariable::Combine):
			// Grab the two FFAtom variables
			at2 = (ForcefieldAtom*) node->argp(0, VTypes::ForcefieldAtomData);
			// Check for NULL pointers
			if ((ptr == NULL) || (at2 == NULL))
			{
				result = FALSE;
				Messenger::print("Error: NULL ForcefieldAtom (or target) passed to 'combine' function.");
				break;
			}
			// Check functional forms
			if (ptr->vdwForm() != at2->vdwForm())
			{
				result = FALSE;
				Messenger::print("Error: ForcefieldAtom passed to 'combine' function has differing functional form ('%s' cf. '%s').", VdwFunctions::VdwFunctions[ptr->vdwForm()].name, VdwFunctions::VdwFunctions[at2->vdwForm()].name);
				break;
			}
			// Check parameter ID
			param = node->argi(1)-1;
			if ((param < 0) || (param >= VdwFunctions::VdwFunctions[ptr->vdwForm()].nParameters))
			{
				result = FALSE;
				Messenger::print("Error: Parameter ID is out of range for VDW functional form - asked for %i, valid parameter range is 1 - %i.", param, VdwFunctions::VdwFunctions[ptr->vdwForm()].nParameters);
				break;
			}
			// Combine parameters
			rv.set(aten_->combinationRules().combine(VdwFunctions::VdwFunctions[ptr->vdwForm()].combinationRules[param], ptr->parameter(param), at2->parameter(param)));
			break;
		case (ForcefieldAtomVariable::DataD):
		case (ForcefieldAtomVariable::DataI):
		case (ForcefieldAtomVariable::DataS):
			// Find data item in local variable list
			v = ptr->data(node->argc(0));
			if (v == NULL)
			{
				result = FALSE;
				Messenger::print("Error: Data '%s' has not been defined in this ForcefieldAtom.", qPrintable(node->argc(0)));
				break;
			}
			v->execute(resultrv);
			if (i == ForcefieldAtomVariable::DataD) rv.set(resultrv.asDouble());
			else if (i == ForcefieldAtomVariable::DataI) rv.set(resultrv.asInteger());
			else if (i == ForcefieldAtomVariable::DataS) rv.set(resultrv.asString());
			else result = FALSE;
			break;
		case (ForcefieldAtomVariable::Parameter):
			id = VdwFunctions::vdwParameter(ptr->vdwForm(), node->argc(0), TRUE);
			if (id == VdwFunctions::VdwFunctions[ptr->vdwForm()].nParameters) result = FALSE;
			else
			{
				// Autoconversion of energy parameters?
				if ((prefs.autoConversionUnit() != Prefs::nEnergyUnits) && VdwFunctions::VdwFunctions[ptr->vdwForm()].isEnergyParameter[id]) rv.set( prefs.convertEnergy(ptr->parameter(id), prefs.energyUnit(), prefs.autoConversionUnit()) );
				else rv.set(ptr->parameter(id));
			}
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ForcefieldAtomVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ForcefieldAtomVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void ForcefieldAtomVariable::printAccessors()
{
	if (ForcefieldAtomVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<ForcefieldAtomVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((ForcefieldAtomVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<ForcefieldAtomVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}

/*
// Variable Array
*/

// Constructor
ForcefieldAtomArrayVariable::ForcefieldAtomArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldAtomData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* ForcefieldAtomArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ForcefieldAtomVariable::accessorSearch(name, arrayIndex, argList);
}
