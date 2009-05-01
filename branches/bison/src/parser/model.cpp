/*
	*** Model Variable and Array
	*** src/parser/model.cpp
	Copyright T. Youngs 2007-2009

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

#include "parser/model.h"
#include "parser/stepnode.h"
#include "model/model.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
ModelVariable::ModelVariable(Model *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ModelData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ModelVariable::~ModelVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor ModelVariable::accessorData[ModelVariable::nAccessors] = {
 	{ "atoms",		VTypes::AtomData,		-1, TRUE },
 	{ "atomtypes",		VTypes::ForcefieldAtomData,	-1, TRUE },
 	{ "bonds",		VTypes::BondData,		-1, TRUE },
 	{ "cell",		VTypes::CellData,		0, TRUE },
 	{ "frame",		VTypes::ModelData,		0, TRUE },
//  	{ "frames",		VTypes::ModelData };
 	{ "name",		VTypes::StringData,		0, FALSE },
 	{ "nangleterms",	VTypes::IntegerData,		0, TRUE },
 	{ "natoms",		VTypes::IntegerData,		0, TRUE },
 	{ "natomtypes",		VTypes::IntegerData,		0, TRUE },
 	{ "nbonds",		VTypes::IntegerData,		0, TRUE },
 	{ "nbondterms",		VTypes::IntegerData,		0, TRUE },
 	{ "npatterns",		VTypes::IntegerData,		0, TRUE },
 	{ "nselected",		VTypes::IntegerData,		0, TRUE },
 	{ "ntorsionterms",	VTypes::IntegerData,		0, TRUE },
 	{ "patterns",		VTypes::PatternData,		-1, TRUE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *ModelVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return ModelVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *ModelVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("ModelVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'model&' has no member named '%s'.\n", s);
		msg.exit("ModelVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, VTypes::ModelData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("ModelVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ModelVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ModelVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Model type.\n");
		msg.exit("ModelVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ModelVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Model *ptr= (Model*) rv.asPointer(VTypes::ModelData, result);
	if (result) switch (acc)
	{
		case (ModelVariable::Atoms):
			if (!hasArrayIndex) rv.set(VTypes::AtomData, ptr->atoms());
			else if (arrayIndex > ptr->nAtoms())
			{
				msg.print("Atom array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::AtomData, ptr->atom(arrayIndex-1));
			break;
		case (ModelVariable::Atomtypes):
			if (!hasArrayIndex) rv.set(VTypes::ForcefieldAtomData, ptr->uniqueTypes());
			else if (arrayIndex > ptr->nUniqueTypes())
			{
				msg.print("Unique types array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::ForcefieldAtomData, ptr->uniqueType(arrayIndex-1));
			break;
		case (ModelVariable::Bonds):
			if (!hasArrayIndex) rv.set(VTypes::BondData, ptr->bonds());
			else if (arrayIndex > ptr->nBonds())
			{
				msg.print("Bond array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::BondData, ptr->bond(arrayIndex-1));
			break;
		case (ModelVariable::Cell):
			rv.set(VTypes::CellData, ptr->cell());
			break;
		case (ModelVariable::Frame):
			rv.set(VTypes::ModelData, ptr->currentFrame());
			break;
// 		case (ModelVariable::Frames):
// 			if (index > ModelnTrajectoryFrames())
// 			{
// 				msg.print("Frame array index is out of bounds for model '%s'\n", Modelname());
// 				result = FALSE;
// 			}
// 			else rv.set(ptr->atom(index-1), VTypes::AtomData);
		case (ModelVariable::Name):
			rv.set(ptr->name());
			break;
 		case (ModelVariable::NAngleTerms):
			rv.set(ptr->nUniqueAngleTerms());
			break;
		case (ModelVariable::NAtoms):
			rv.set(ptr->nAtoms());
			break;
		case (ModelVariable::NAtomtypes):
			rv.set(ptr->nUniqueTypes());
			break;
		case (ModelVariable::NBonds):
			rv.set(ptr->nBonds());
			break;
		case (ModelVariable::NBondTerms):
			rv.set(ptr->nUniqueBondTerms());
			break;
		case (ModelVariable::NPatterns):
			rv.set(ptr->nPatterns());
			break;
		case (ModelVariable::NSelected):
			rv.set(ptr->nSelected());
			break;
		case (ModelVariable::NTorsionTerms):
			rv.set(ptr->nUniqueTorsionTerms());
			break;
		case (ModelVariable::Patterns):
			if (!hasArrayIndex) rv.set(VTypes::PatternData, ptr->patterns());
			else if (arrayIndex > ptr->nPatterns())
			{
				msg.print("Pattern array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::PatternData, ptr->pattern(arrayIndex-1));
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ModelVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ModelVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ModelVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ModelVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Model type.\n");
		msg.exit("ModelVariable::setAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if (accessorData[i].arraySize == 0)
	{
		if (hasArrayIndex) msg.print("Warning: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
	}
	else if (!hasArrayIndex)
	{
		msg.print("Error: No array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ModelVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Model *ptr= (Model*) sourcerv.asPointer(VTypes::ModelData, result);
	if (result) switch (acc)
	{
		case (ModelVariable::Name):
			ptr->setName(newvalue.asString());
			break;
		default:
			printf("ModelVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("ModelVariable::setAccessor");
	return result;
}

/*
// Variable Array
*/

// Constructor
ModelArrayVariable::ModelArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ModelData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *ModelArrayVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return ModelVariable::accessorSearch(s, arrayindex);
}

