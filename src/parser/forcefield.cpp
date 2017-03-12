/*
	*** Forcefield Variable and Array
	*** src/parser/Forcefield.cpp
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

#include "parser/forcefield.h"
#include "parser/stepnode.h"
#include "ff/forcefield.h"
#include "base/forcefieldatom.h"
#include "main/aten.h"

ATEN_USING_NAMESPACE

/*
 * Variable
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
 * Accessors
 */

// Accessor data
Accessor ForcefieldVariable::accessorData[ForcefieldVariable::nAccessors] = {
	{ "atomTypes",		VTypes::ForcefieldAtomData,	-1, true },
	{ "filename",		VTypes::StringData,		0, true },
	{ "name",		VTypes::StringData,		0, false },
	{ "nAngles",		VTypes::IntegerData,		0, true },
	{ "nAtomTypes",		VTypes::IntegerData,		0, true },
	{ "nBonds",		VTypes::IntegerData,		0, true },
	{ "nImpropers",		VTypes::IntegerData,		0, true },
	{ "nTorsions",		VTypes::IntegerData,		0, true },
	{ "units",		VTypes::StringData,		0, false }
};

// Function data
FunctionAccessor ForcefieldVariable::functionData[ForcefieldVariable::nFunctions] = {
	{ "addAngle",		VTypes::ForcefieldBoundData,	aten_->commandArguments(Commands::AngleDef),	aten_->commandArgText(Commands::AngleDef) },
	{ "addBond",		VTypes::ForcefieldBoundData,	aten_->commandArguments(Commands::BondDef),	aten_->commandArgText(Commands::BondDef) },
	{ "addInter",		VTypes::NoData,			aten_->commandArguments(Commands::InterDef),	aten_->commandArgText(Commands::InterDef) },
	{ "addTorsion",		VTypes::ForcefieldBoundData,	aten_->commandArguments(Commands::TorsionDef),	aten_->commandArgText(Commands::TorsionDef) },
	{ "addType",		VTypes::ForcefieldAtomData,	aten_->commandArguments(Commands::TypeDef),	aten_->commandArgText(Commands::TypeDef) },
	{ "finalise",		VTypes::NoData, 		aten_->commandArguments(Commands::Finalise),	aten_->commandArgText(Commands::Finalise) },
	{ "findAngle",		VTypes::ForcefieldBoundData, 	"CCC",					"string typei, string typej, string typek" },
	{ "findBond",		VTypes::ForcefieldBoundData, 	"CC",					"string typei, string typej" },
	{ "findImproper",	VTypes::ForcefieldBoundData, 	"CCCC",					"string typei, string typej, string typek, string typel" },
	{ "findTorsion",	VTypes::ForcefieldBoundData, 	"CCCC",					"string typei, string typej, string typek, string typel" },
	{ "findType",		VTypes::ForcefieldAtomData, 	"C",					"string type" },
	{ "findUreyBradley",	VTypes::ForcefieldBoundData, 	"CCC",					"string typei, string typej, string typek" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* ForcefieldVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ForcefieldVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* ForcefieldVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("ForcefieldVariable::accessorSearch");
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
			Messenger::print("Error: Type 'Forcefield&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("ForcefieldVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Forcefield&' function named '%s'.", qPrintable(name));
			Messenger::exit("ForcefieldVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ForcefieldData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'Forcefield&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
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
			Messenger::print("Error: Argument list given to 'Forcefield&' array member '%s'.", qPrintable(name));
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
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("ForcefieldVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ForcefieldVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	Forcefield* ptr = (Forcefield*) rv.asPointer(VTypes::ForcefieldData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ForcefieldData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (ForcefieldVariable::AtomTypes):
			if (hasArrayIndex)
			{
				if (arrayIndex > ptr->nTypes())
				{
					Messenger::print("Error: Array index is out of bounds for 'atomTypes' accessor (n = %i, nTypes = %i)", arrayIndex, ptr->nTypes());
					return false;
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
			printf("Internal Error: Access to member '%s' has not been defined in ForcefieldVariable.\n", qPrintable(accessorData[i].name));
			result = false;
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
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("ForcefieldVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	Forcefield* ptr = (Forcefield*) sourcerv.asPointer(VTypes::ForcefieldData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ForcefieldData));
		result = false;
	}
	Prefs::EnergyUnit eu;
	if (result) switch (acc)
	{
		case (Name):
			ptr->setName( newValue.asString() );
			break;
		case (Units):
			eu = Prefs::energyUnit(newValue.asString(), true);
			if (eu == Prefs::nEnergyUnits) result = false;
			else ptr->setEnergyUnit(eu);
			break;
		default:
			printf("ForcefieldVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
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
		return false;
	}

	// Get current data from ReturnValue
	bool result = true;
	Forcefield* ptr = (Forcefield*) rv.asPointer(VTypes::ForcefieldData, result);
	// Construct temporary bundle object containing our Forcefield pointer
	Bundle bundle(ptr);
	if (result) switch (i)
	{
		case (ForcefieldVariable::AddAngle):
			result = aten_->callCommand(Commands::AngleDef, node, rv, bundle);
			break;
		case (ForcefieldVariable::AddBond):
			result = aten_->callCommand(Commands::BondDef, node, rv, bundle);
			break;
		case (ForcefieldVariable::AddInter):
			result = aten_->callCommand(Commands::InterDef, node, rv, bundle);
			break;
		case (ForcefieldVariable::AddTorsion):
			result = aten_->callCommand(Commands::TorsionDef, node, rv, bundle);
			break;
		case (ForcefieldVariable::AddType):
			result = aten_->callCommand(Commands::TypeDef, node, rv, bundle);
			break;
		case (ForcefieldVariable::Finalise):
			// Print some information about the terms read in from the forcefield
			Messenger::print("Forcefield contains:");
			Messenger::print("\t%i type descriptions", ptr->nTypes() - 1);
			Messenger::print("\t%i bond definitions", ptr->nBonds());
			Messenger::print("\t%i angle definitions", ptr->nAngles());
			Messenger::print("\t%i torsion definitions", ptr->nTorsions());

			// Check that some forcefield types were defined...
			if (ptr->nTypes() <= 1) Messenger::print("Warning - no types are defined in this forcefield.");

			// Link forcefield type references (&N) to their actual forcefield types
			for (ForcefieldAtom* ffa = ptr->types(); ffa != NULL; ffa = ffa->next) ffa->neta()->linkReferenceTypes();

			// Convert energetic units in the forcefield to the internal units of the program
			ptr->convertParameters();
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
			printf("Internal Error: Access to function '%s' has not been defined in ForcefieldVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("ForcefieldVariable::performFunction");
	return result;
}

/*
 * Variable Array
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
StepNode* ForcefieldArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ForcefieldVariable::accessorSearch(name, arrayIndex, argList);
}
