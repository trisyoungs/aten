/*
	*** Pattern Variable
	*** src/parser/pattern.cpp
	Copyright T. Youngs 2007-2018

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

#include "parser/pattern.h"
#include "parser/stepnode.h"
#include "base/pattern.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
PatternVariable::PatternVariable(Pattern* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::PatternData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
PatternVariable::~PatternVariable()
{
}

/*
 * Accessors
 */

// Accessor data
Accessor PatternVariable::accessorData[PatternVariable::nAccessors] = {
	{ "angles", 	VTypes::PatternBoundData,	-1, true },
	{ "atoms", 	VTypes::AtomData,		-1, true },
	{ "bonds", 	VTypes::PatternBoundData,	-1, true },
	{ "ffAngles",	VTypes::ForcefieldBoundData,	0, true },
	{ "ffBonds",	VTypes::ForcefieldBoundData,	0, true },
	{ "ffTorsions",	VTypes::ForcefieldBoundData,	0, true },
	{ "ffTypes",	VTypes::ForcefieldAtomData,	0, true },
	{ "ff",		VTypes::ForcefieldData,		0, false },
	{ "firstAtom",	VTypes::AtomData,		0, true },
	{ "firstAtomId",VTypes::IntegerData,		0, true },
	{ "fixed",	VTypes::IntegerData,		0, false },
	{ "lastAtom",	VTypes::AtomData,		0, true },
	{ "lastAtomId",	VTypes::IntegerData,		0, true },
	{ "name",	VTypes::StringData,		0, false },
	{ "nAngles",	VTypes::IntegerData,		0, true },
	{ "nAtoms",	VTypes::IntegerData,		0, true },
	{ "nBonds",	VTypes::IntegerData,		0, true },
	{ "nFFAngles",	VTypes::IntegerData,		0, true },
	{ "nFFBonds",	VTypes::IntegerData,		0, true },
	{ "nFFTorsions",VTypes::IntegerData,		0, true },
	{ "nFFTypes",	VTypes::IntegerData,		0, true },
	{ "nMolAtoms",	VTypes::IntegerData,		0, true },
	{ "nMols",	VTypes::IntegerData,		0, true },
	{ "nTorsions",	VTypes::IntegerData,		0, true },
	{ "torsions",	VTypes::PatternBoundData,	-1, true }
};

// Function data
FunctionAccessor PatternVariable::functionData[PatternVariable::nFunctions] = {
	{ "atomsInRing",	VTypes::IntegerData,	"Ii",	"int i, int j = -1" },
	{ "cog", 		VTypes::VectorData,	"I",	"int id" },
	{ "com", 		VTypes::VectorData,	"I",	"int id" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* PatternVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return PatternVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* PatternVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("PatternVariable::accessorSearch");
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
			Messenger::print("Error: Type 'Pattern&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("PatternVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Pattern&' function named '%s'.", qPrintable(name));
			Messenger::exit("PatternVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::PatternData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'Pattern&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
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
			Messenger::print("Error: Argument list given to 'Pattern&' array member '%s'.", qPrintable(name));
			Messenger::exit("PatternVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::PatternData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("PatternVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool PatternVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("PatternVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Pattern type.\n", i);
		Messenger::exit("PatternVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("PatternVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("PatternVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	Pattern* ptr = (Pattern*) rv.asPointer(VTypes::PatternData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::PatternData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (PatternVariable::Angles):
			if (!hasArrayIndex) rv.set(VTypes::PatternBoundData, ptr->angles());
			else if (arrayIndex > ptr->nAngles())
			{
				Messenger::print("Angle array index (%i) is out of bounds for pattern '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::PatternBoundData, ptr->angle(arrayIndex-1));
			break;
		case (PatternVariable::Atoms):
			if (!hasArrayIndex) rv.set(VTypes::AtomData, ptr->parent()->atom(ptr->startAtom()));
			else if (arrayIndex > ptr->totalAtoms())
			{
				Messenger::print("Atom array index (%i) is out of bounds for pattern '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::AtomData, ptr->parent()->atom(arrayIndex-1+ptr->startAtom()));
			break;
		case (PatternVariable::Bonds):
			if (!hasArrayIndex) rv.set(VTypes::PatternBoundData, ptr->bonds());
			else if (arrayIndex > ptr->nBonds())
			{
				Messenger::print("Bond array index (%i) is out of bounds for pattern '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::PatternBoundData, ptr->bond(arrayIndex-1));
			break;
		case (PatternVariable::FFAngles):
			if (!hasArrayIndex)
			{
				if (ptr->forcefieldAngles() != NULL) rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldAngles()->item, ptr->forcefieldAngles());
				else rv.set(VTypes::ForcefieldBoundData, NULL);
			}
			else if (arrayIndex > ptr->nForcefieldAngles())
			{
				Messenger::print("Forcefield angle array index (%i) is out of bounds for pattern '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldAngle(arrayIndex-1)->item, ptr->forcefieldAngle(arrayIndex-1));
			break;
		case (PatternVariable::FFBonds):
			if (!hasArrayIndex)
			{
				if (ptr->forcefieldBonds() != NULL) rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldBonds()->item, ptr->forcefieldBonds());
				else rv.set(VTypes::ForcefieldBoundData, NULL);
			}
			else if (arrayIndex > ptr->nForcefieldBonds())
			{
				Messenger::print("Forcefield bond array index (%i) is out of bounds for pattern '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldBond(arrayIndex-1)->item, ptr->forcefieldBond(arrayIndex-1));
			break;
		case (PatternVariable::FFTorsions):
			if (!hasArrayIndex)
			{
				if (ptr->forcefieldTorsions() != NULL) rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldTorsions()->item, ptr->forcefieldTorsions());
				else rv.set(VTypes::ForcefieldBoundData, NULL);
			}
			else if (arrayIndex > ptr->nForcefieldTorsions())
			{
				Messenger::print("Forcefield torsion array index (%i) is out of bounds for pattern '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldTorsion(arrayIndex-1)->item, ptr->forcefieldTorsion(arrayIndex-1));
			break;
		case (PatternVariable::FFTypes):
			if (!hasArrayIndex)
			{
				if (ptr->uniqueForcefieldTypes() != NULL) rv.set(VTypes::ForcefieldAtomData, ptr->uniqueForcefieldTypes()->item, ptr->uniqueForcefieldTypes());
				else rv.set(VTypes::ForcefieldAtomData, NULL);
			}
			else if (arrayIndex > ptr->nUniqueForcefieldTypes())
			{
				Messenger::print("Forcefield types array index (%i) is out of bounds for pattern '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::ForcefieldAtomData, ptr->uniqueForcefieldType(arrayIndex-1)->item, ptr->uniqueForcefieldType(arrayIndex-1));
			break;
		case (PatternVariable::FField):
			rv.set(VTypes::ForcefieldData, ptr->forcefield());
			break;
		case (PatternVariable::FirstAtom):
			rv.set(VTypes::AtomData, ptr->firstAtom());
			break;
		case (PatternVariable::Fixed):
			rv.set(ptr->areAtomsFixed());
			break;
		case (PatternVariable::FirstAtomId):
			rv.set(ptr->startAtom() + 1);
			break;
		case (PatternVariable::LastAtom):
			rv.set(VTypes::AtomData, ptr->lastAtom());
			break;
		case (PatternVariable::LastAtomId):
			rv.set(ptr->endAtom() + 1);
			break;
		case (PatternVariable::Name):
			rv.set(qPrintable(ptr->name()));
			break;
		case (PatternVariable::NAngles):
			rv.set(ptr->nAngles());
			break;
		case (PatternVariable::NAtoms):
			rv.set(ptr->totalAtoms());
			break;
		case (PatternVariable::NBonds):
			rv.set(ptr->nBonds());
			break;
		case (PatternVariable::NFFAngles):
			rv.set(ptr->nForcefieldAngles());
			break;
		case (PatternVariable::NFFBonds):
			rv.set(ptr->nForcefieldAngles());
			break;
		case (PatternVariable::NFFTorsions):
			rv.set(ptr->nForcefieldTorsions());
			break;
		case (PatternVariable::NFFTypes):
			rv.set(ptr->nUniqueForcefieldTypes());
			break;
		case (PatternVariable::NMolAtoms):
			rv.set(ptr->nAtoms());
			break;
		case (PatternVariable::NMols):
			rv.set(ptr->nMolecules());
			break;
		case (PatternVariable::NTorsions):
			rv.set(ptr->nTorsions());
			break;
		case (PatternVariable::Torsions):
			if (!hasArrayIndex) rv.set(VTypes::PatternBoundData, ptr->torsions());
			else if (arrayIndex > ptr->nTorsions())
			{
				Messenger::print("Torsion array index (%i) is out of bounds for pattern '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::PatternBoundData, ptr->torsion(arrayIndex-1));
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in PatternVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("PatternVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool PatternVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("PatternVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Pattern type.\n", i);
		Messenger::exit("PatternVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("PatternVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	Pattern* ptr = (Pattern*) sourcerv.asPointer(VTypes::PatternData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::PatternData));
		result = false;
	}
	// Set value based on enumerated id
	if (result) switch (acc)
	{
		case (PatternVariable::Name):
			ptr->setName(newValue.asString());
			break;
		case (PatternVariable::Fixed):
			ptr->setAtomsFixed( newValue.asBool() );
			break;
		case (PatternVariable::FField):
 			ptr->setForcefield( (Forcefield*) newValue.asPointer(VTypes::ForcefieldData));
			break;
		default:
			printf("PatternVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("PatternVariable::setAccessor");
	return result;
}

// Perform desired function
bool PatternVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("PatternVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Pattern type.\n", i);
		Messenger::exit("PatternVariable::performFunction");
		return false;
	}

	// Get current data from ReturnValue
	bool result = true;
	Pattern* ptr = (Pattern*) rv.asPointer(VTypes::PatternData, result);
	int index, id_i, id_j;
	if (result) switch (i)
	{
		case (PatternVariable::AtomsInRing):
			id_i = node->argi(0) - 1;
			if ((id_i < 0) || (id_i >= ptr->nAtoms()))
			{
				Messenger::print("First atom id %i is out of range for 'atomsinring' function in pattern '%s'.", id_i, qPrintable(ptr->name()));
				result = false;
			}
			id_j = node->hasArg(1) ? node->argi(1)-1 : -1;
			if ((id_j != -1) && ((id_j < 0) || (id_j >= ptr->nAtoms())))
			{
				Messenger::print("Second atom id %i is out of range for 'atomsinring' function in pattern '%s'.", id_j, qPrintable(ptr->name()));
				result = false;
			}
			if (result) rv.set(ptr->atomsInRing(id_i, id_j));
			break;
		case (PatternVariable::Cog):
			index = node->argi(0);
			if ((index < 1) || (index > ptr->nMolecules()))
			{
				Messenger::print("Molecule id %i is out of range for 'cog' function in pattern '%s'.", index, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(ptr->calculateCog(index-1));
			break;
		case (PatternVariable::Com):
			index = node->argi(0);
			if ((index < 1) || (index > ptr->nMolecules()))
			{
				Messenger::print("Molecule id %i is out of range for 'com' function in pattern '%s'.", index, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(ptr->calculateCom(index-1));
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in PatternVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("PatternVariable::performFunction");
	return result;
}

/*
 * Variable Array
 */

// Constructor
PatternArrayVariable::PatternArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::PatternData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* PatternArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return PatternVariable::accessorSearch(name, arrayIndex, argList);
}
