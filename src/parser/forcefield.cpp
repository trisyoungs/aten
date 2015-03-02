/*
	*** Forcefield Variable and Array
	*** src/parser/Forcefield.cpp
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

#include "parser/forcefield.h"
#include "parser/stepnode.h"
#include "ff/forcefield.h"
#include "main/aten.h"

ATEN_USING_NAMESPACE

/*
// Variable
*/

// Constructor
ForcefieldVariable::ForcefieldVariable(Forcefield* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ForcefieldVariable::~ForcefieldVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor ForcefieldVariable::accessorData[ForcefieldVariable::nAccessors] = {
	{ "atomTypes",		VTypes::ForcefieldAtomData,	-1, TRUE },
	{ "filename",		VTypes::StringData,		0, TRUE },
	{ "name",		VTypes::StringData,		0, FALSE },
	{ "nAngles",		VTypes::IntegerData,		0, TRUE },
	{ "nAtomTypes",		VTypes::IntegerData,		0, TRUE },
	{ "nBonds",		VTypes::IntegerData,		0, TRUE },
	{ "nImpropers",		VTypes::IntegerData,		0, TRUE },
	{ "nTorsions",		VTypes::IntegerData,		0, TRUE },
	{ "units",		VTypes::StringData,		0, FALSE }
};

// Function data
FunctionAccessor ForcefieldVariable::functionData[ForcefieldVariable::nFunctions] = {
	{ "addAngle",		VTypes::ForcefieldBoundData,	aten_.commandArguments(Commands::AngleDef),	aten_.commandArgText(Commands::AngleDef) },
	{ "addBond",		VTypes::ForcefieldBoundData,	aten_.commandArguments(Commands::BondDef),	aten_.commandArgText(Commands::BondDef) },
	{ "addInter",		VTypes::NoData,			aten_.commandArguments(Commands::InterDef),	aten_.commandArgText(Commands::InterDef) },
	{ "addTorsion",		VTypes::ForcefieldBoundData,	aten_.commandArguments(Commands::TorsionDef),	aten_.commandArgText(Commands::TorsionDef) },
	{ "addType",		VTypes::ForcefieldAtomData,	aten_.commandArguments(Commands::TypeDef),	aten_.commandArgText(Commands::TypeDef) },
	{ "finalise",		VTypes::NoData, 		aten_.commandArguments(Commands::Finalise),	aten_.commandArgText(Commands::Finalise) },
	{ "findAngle",		VTypes::ForcefieldBoundData, 	"CCC",					"string typei, string typej, string typek" },
	{ "findBond",		VTypes::ForcefieldBoundData, 	"CC",					"string typei, string typej" },
	{ "findImproper",	VTypes::ForcefieldBoundData, 	"CCCC",					"string typei, string typej, string typek, string typel" },
	{ "findTorsion",	VTypes::ForcefieldBoundData, 	"CCCC",					"string typei, string typej, string typek, string typel" },
	{ "findType",		VTypes::ForcefieldAtomData, 	"C",					"string type" },
	{ "findUreyBradley",	VTypes::ForcefieldBoundData, 	"CCC",					"string typei, string typej, string typek" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* ForcefieldVariable::findAccessor(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	return ForcefieldVariable::accessorSearch(s, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* ForcefieldVariable::accessorSearch(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("ForcefieldVariable::accessorSearch");
	StepNode* result = NULL;
	int i = 0;
	i = Variable::searchAccessor(s, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(s, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'Forcefield&' has no member or function named '%s'.\n", s);
			printAccessors();
			Messenger::exit("ForcefieldVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Forcefield&' function '%s'.\n", s);
			Messenger::exit("ForcefieldVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ForcefieldData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'Forcefield&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
			delete result;
			result = NULL;
		}
	}
	else
	{
		Messenger::print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayIndex != NULL))
		{
			Messenger::print("Error: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
			result = NULL;
		}
		// Were we given an argument list when we didn't want one?
		if (argList != NULL)
		{
			Messenger::print("Error: Argument list given to 'Forcefield&' array member '%s'.\n", s);
			Messenger::exit("ForcefieldVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::ForcefieldData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("ForcefieldVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ForcefieldVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ForcefieldVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Forcefield type.\n", i);
		Messenger::exit("ForcefieldVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		Messenger::exit("ForcefieldVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ForcefieldVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Forcefield* ptr = (Forcefield*) rv.asPointer(VTypes::ForcefieldData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ForcefieldData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ForcefieldVariable::AtomTypes):
			if (hasArrayIndex)
			{
				if (arrayIndex > ptr->nTypes())
				{
					Messenger::print("Error: Array index is out of bounds for 'atomTypes' accessor (n = %i, nTypes = %i)\n", arrayIndex, ptr->nTypes());
					return FALSE;
				}
				else rv.set(VTypes::ForcefieldAtomData, ptr->type(arrayIndex-1));
			}
			else rv.set(VTypes::ForcefieldAtomData, ptr->types());
			break;
		case (FileName):
			rv.set( ptr->filename() );
			break;
		case (Name):
			rv.set( ptr->name() );
			break;
		case (NAngles):
			rv.set( ptr->nAngles() );
			break;
		case (NAtomTypes):
			rv.set( ptr->nTypes() );
			break;
		case (NBonds):
			rv.set( ptr->nBonds() );
			break;
		case (NImpropers):
			rv.set( ptr->nImpropers() );
			break;
		case (NTorsions):
			rv.set( ptr->nTorsions() );
			break;
		case (Units):
			rv.set ( Prefs::energyUnit(ptr->energyUnit()) );
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ForcefieldVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ForcefieldVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ForcefieldVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ForcefieldVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Forcefield type.\n", i);
		Messenger::exit("ForcefieldVariable::setAccessor");
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
				Messenger::print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = FALSE;
			}
			if (newValue.arraySize() > 0)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.\n", accessorData[i].name);
				result = FALSE;
			}
		}
		else
		{
			if (newValue.arraySize() > accessorData[i].arraySize)
			{
				Messenger::print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).\n", accessorData[i].name, newValue.arraySize(), accessorData[i].arraySize);
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
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.\n", accessorData[i].name);
				result = FALSE;
			}
			else if ((newValue.type() != VTypes::VectorData) && (newValue.arraySize() != 3))
			{
				Messenger::print("Error: Only an array of size 3 can be assigned to a vector (member '%s').\n", accessorData[i].name);
				result = FALSE;
			}
		}
	}
	if (!result)
	{
		Messenger::exit("ForcefieldVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Forcefield* ptr = (Forcefield*) sourcerv.asPointer(VTypes::ForcefieldData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ForcefieldData));
		result = FALSE;
	}
	Prefs::EnergyUnit eu;
	if (result) switch (acc)
	{
		case (Name):
			ptr->setName( newValue.asString() );
			break;
		case (Units):
			eu = Prefs::energyUnit(newValue.asString(), TRUE);
			if (eu == Prefs::nEnergyUnits) result = FALSE;
			else ptr->setEnergyUnit(eu);
			break;
		default:
			printf("ForcefieldVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ForcefieldVariable::setAccessor");
	return result;
}

// Perform desired function
bool ForcefieldVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("ForcefieldVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Forcefield type.\n", i);
		Messenger::exit("ForcefieldVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Forcefield* ptr = (Forcefield*) rv.asPointer(VTypes::ForcefieldData, result);
	// Construct temporary bundle object containing our Forcefield pointer
	Bundle bundle(ptr);
	if (result) switch (i)
	{
		case (ForcefieldVariable::AddAngle):
			result = aten_.callCommand(Commands::AngleDef, node, rv, bundle);
			break;
		case (ForcefieldVariable::AddBond):
			result = aten_.callCommand(Commands::BondDef, node, rv, bundle);
			break;
		case (ForcefieldVariable::AddInter):
			result = aten_.callCommand(Commands::InterDef, node, rv, bundle);
			break;
		case (ForcefieldVariable::AddTorsion):
			result = aten_.callCommand(Commands::TorsionDef, node, rv, bundle);
			break;
		case (ForcefieldVariable::AddType):
			result = aten_.callCommand(Commands::TypeDef, node, rv, bundle);
			break;
		case (ForcefieldVariable::Finalise):
			result = aten_.callCommand(Commands::FinaliseFF, node, rv, bundle);
			break;
		case (ForcefieldVariable::FindAngle):
			rv.set(VTypes::ForcefieldBoundData, ptr->findAngle(node->argc(0), node->argc(1), node->argc(2)));
			break;
		case (ForcefieldVariable::FindBond):
			rv.set(VTypes::ForcefieldBoundData, ptr->findBond(node->argc(0), node->argc(1)));
			break;
		case (ForcefieldVariable::FindImproper):
			rv.set(VTypes::ForcefieldBoundData, ptr->findImproper(node->argc(0), node->argc(1), node->argc(2), node->argc(3)));
			break;
		case (ForcefieldVariable::FindTorsion):
			rv.set(VTypes::ForcefieldBoundData, ptr->findTorsion(node->argc(0), node->argc(1), node->argc(2), node->argc(3)));
			break;
		case (ForcefieldVariable::FindType):
			rv.set(VTypes::ForcefieldAtomData, ptr->findType(node->argc(0)));
			break;
		case (ForcefieldVariable::FindUreyBradley):
			rv.set(VTypes::ForcefieldBoundData, ptr->findUreyBradley(node->argc(0), node->argc(1), node->argc(2)));
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ForcefieldVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ForcefieldVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void ForcefieldVariable::printAccessors()
{
	if (ForcefieldVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:\n");
		for (int n=0; n<ForcefieldVariable::nAccessors; ++n) Messenger::print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print("\n");
	}
	if ((ForcefieldVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:\n");
		for (int n=0; n<ForcefieldVariable::nFunctions; ++n) Messenger::print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
ForcefieldArrayVariable::ForcefieldArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* ForcefieldArrayVariable::findAccessor(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	return ForcefieldVariable::accessorSearch(s, arrayIndex, argList);
}
