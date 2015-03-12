/*
	*** Pattern Variable
	*** src/parser/pattern.cpp
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
// Accessors
*/

// Accessor data
Accessor PatternVariable::accessorData[PatternVariable::nAccessors] = {
	{ "angles", 	VTypes::PatternBoundData,	-1, TRUE },
	{ "atoms", 	VTypes::AtomData,		-1, TRUE },
	{ "bonds", 	VTypes::PatternBoundData,	-1, TRUE },
	{ "ffAngles",	VTypes::ForcefieldBoundData,	0, TRUE },
	{ "ffBonds",	VTypes::ForcefieldBoundData,	0, TRUE },
	{ "ffTorsions",	VTypes::ForcefieldBoundData,	0, TRUE },
	{ "ffTypes",	VTypes::ForcefieldAtomData,	0, TRUE },
	{ "ff",		VTypes::ForcefieldData,		0, FALSE },
	{ "firstAtom",	VTypes::AtomData,		0, TRUE },
	{ "firstAtomId",VTypes::IntegerData,		0, TRUE },
	{ "fixed",	VTypes::IntegerData,		0, FALSE },
	{ "lastAtom",	VTypes::AtomData,		0, TRUE },
	{ "lastAtomId",	VTypes::IntegerData,		0, TRUE },
	{ "name",	VTypes::StringData,		0, FALSE },
	{ "nAngles",	VTypes::IntegerData,		0, TRUE },
	{ "nAtoms",	VTypes::IntegerData,		0, TRUE },
	{ "nBonds",	VTypes::IntegerData,		0, TRUE },
	{ "nFFAngles",	VTypes::IntegerData,		0, TRUE },
	{ "nFFBonds",	VTypes::IntegerData,		0, TRUE },
	{ "nFFTorsions",VTypes::IntegerData,		0, TRUE },
	{ "nFFTypes",	VTypes::IntegerData,		0, TRUE },
	{ "nMolAtoms",	VTypes::IntegerData,		0, TRUE },
	{ "nMols",	VTypes::IntegerData,		0, TRUE },
	{ "nTorsions",	VTypes::IntegerData,		0, TRUE },
	{ "torsions",	VTypes::PatternBoundData,	-1, TRUE }
};

// Function data
FunctionAccessor PatternVariable::functionData[PatternVariable::nFunctions] = {
	{ "atomsInRing",	VTypes::IntegerData,	"Ii",	"int i, int j = -1" },
	{ "cog", 		VTypes::VectorData,	"I",	"int id" },
	{ "com", 		VTypes::VectorData,	"I",	"int id" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* PatternVariable::findAccessor(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	return PatternVariable::accessorSearch(s, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* PatternVariable::accessorSearch(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("PatternVariable::accessorSearch");
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
			Messenger::print("Error: Type 'Pattern&' has no member or function named '%s'.", s);
			printAccessors();
			Messenger::exit("PatternVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Pattern&' function '%s'.", s);
			Messenger::exit("PatternVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::PatternData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'Pattern&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			Messenger::print("Error: Argument list given to 'Pattern&' array member '%s'.", s);
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
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("PatternVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("PatternVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Pattern* ptr = (Pattern*) rv.asPointer(VTypes::PatternData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::PatternData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (PatternVariable::Angles):
			if (!hasArrayIndex) rv.set(VTypes::PatternBoundData, ptr->angles());
			else if (arrayIndex > ptr->nAngles())
			{
				Messenger::print("Angle array index (%i) is out of bounds for pattern '%s'", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::PatternBoundData, ptr->angle(arrayIndex-1));
			break;
		case (PatternVariable::Atoms):
			if (!hasArrayIndex) rv.set(VTypes::AtomData, ptr->parent()->atom(ptr->startAtom()));
			else if (arrayIndex > ptr->totalAtoms())
			{
				Messenger::print("Atom array index (%i) is out of bounds for pattern '%s'", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::AtomData, ptr->parent()->atom(arrayIndex-1+ptr->startAtom()));
			break;
		case (PatternVariable::Bonds):
			if (!hasArrayIndex) rv.set(VTypes::PatternBoundData, ptr->bonds());
			else if (arrayIndex > ptr->nBonds())
			{
				Messenger::print("Bond array index (%i) is out of bounds for pattern '%s'", arrayIndex, ptr->name());
				result = FALSE;
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
				Messenger::print("Forcefield angle array index (%i) is out of bounds for pattern '%s'", arrayIndex, ptr->name());
				result = FALSE;
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
				Messenger::print("Forcefield bond array index (%i) is out of bounds for pattern '%s'", arrayIndex, ptr->name());
				result = FALSE;
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
				Messenger::print("Forcefield torsion array index (%i) is out of bounds for pattern '%s'", arrayIndex, ptr->name());
				result = FALSE;
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
				Messenger::print("Forcefield types array index (%i) is out of bounds for pattern '%s'", arrayIndex, ptr->name());
				result = FALSE;
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
			rv.set(ptr->name());
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
				Messenger::print("Torsion array index (%i) is out of bounds for pattern '%s'", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::PatternBoundData, ptr->torsion(arrayIndex-1));
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in PatternVariable.\n", accessorData[i].name);
			result = FALSE;
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
		Messenger::exit("PatternVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Pattern* ptr = (Pattern*) sourcerv.asPointer(VTypes::PatternData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::PatternData));
		result = FALSE;
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
			printf("PatternVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
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
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Pattern* ptr = (Pattern*) rv.asPointer(VTypes::PatternData, result);
	int index, id_i, id_j;
	if (result) switch (i)
	{
		case (PatternVariable::AtomsInRing):
			id_i = node->argi(0) - 1;
			if ((id_i < 0) || (id_i >= ptr->nAtoms()))
			{
				Messenger::print("First atom id %i is out of range for 'atomsinring' function in pattern '%s'.", id_i, ptr->name());
				result = FALSE;
			}
			id_j = node->hasArg(1) ? node->argi(1)-1 : -1;
			if ((id_j != -1) && ((id_j < 0) || (id_j >= ptr->nAtoms())))
			{
				Messenger::print("Second atom id %i is out of range for 'atomsinring' function in pattern '%s'.", id_j, ptr->name());
				result = FALSE;
			}
			if (result) rv.set(ptr->atomsInRing(id_i, id_j));
			break;
		case (PatternVariable::Cog):
			index = node->argi(0);
			if ((index < 1) || (index > ptr->nMolecules()))
			{
				Messenger::print("Molecule id %i is out of range for 'cog' function in pattern '%s'.", index, ptr->name());
				result = FALSE;
			}
			else rv.set(ptr->calculateCog(index-1));
			break;
		case (PatternVariable::Com):
			index = node->argi(0);
			if ((index < 1) || (index > ptr->nMolecules()))
			{
				Messenger::print("Molecule id %i is out of range for 'com' function in pattern '%s'.", index, ptr->name());
				result = FALSE;
			}
			else rv.set(ptr->calculateCom(index-1));
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in PatternVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("PatternVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void PatternVariable::printAccessors()
{
	if (PatternVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		for (int n=0; n<PatternVariable::nAccessors; ++n) Messenger::print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print("");
	}
	if ((PatternVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		for (int n=0; n<PatternVariable::nFunctions; ++n) Messenger::print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print("");
	}
}

/*
// Variable Array
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
StepNode* PatternArrayVariable::findAccessor(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	return PatternVariable::accessorSearch(s, arrayIndex, argList);
}
