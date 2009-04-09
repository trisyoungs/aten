/*
	*** Model Variable
	*** src/parser/atom.cpp
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

// Constructor
ModelVariable::ModelVariable(Model *ptr, bool constant) : modelData_(ptr)
{
	// Private variables
	returnType_ = NuVTypes::ModelData;
	readOnly_ = constant;
}

// Destructor
ModelVariable::~ModelVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool ModelVariable::set(NuReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a vector) cannot be assigned to.\n");
		return FALSE;
	}
	bool success;
	modelData_ = rv.asPointer(NuVTypes::ModelData, success);
	return success;
}

// Reset variable
void ModelVariable::reset()
{
	modelData_ = NULL;
}

// Return value of node
bool ModelVariable::execute(NuReturnValue &rv)
{
	// If this vector is a constant, read the three stored expressions to recreate it
	rv.set(NuVTypes::ModelData, modelData_);
	return TRUE;
}

// Print node contents
void ModelVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("%s%li (model) (constant value)\n", tab, modelData_);
	else printf("%s%li (model) (variable, name=%s)\n", tab, modelData_, name_.get());
	delete[] tab;
}

/*
// Accessors
*/

// Accessor data
Accessor ModelVariable::accessorData[ModelVariable::nAccessors] = {
 	{ "atoms",		NuVTypes::AtomData,		TRUE, TRUE },
 	{ "atomtypes",		NuVTypes::ForcefieldAtomData,	TRUE, TRUE },
 	{ "bonds",		NuVTypes::BondData,		TRUE, TRUE },
 	{ "cell",		NuVTypes::CellData,		FALSE, TRUE },
 	{ "frame",		NuVTypes::ModelData,		FALSE, TRUE },
//  	{ "frames",		NuVTypes::ModelData };
 	{ "name",		NuVTypes::StringData,		FALSE, FALSE },
 	{ "nangleterms",	NuVTypes::IntegerData,		FALSE, TRUE },
 	{ "natoms",		NuVTypes::IntegerData,		FALSE, TRUE },
 	{ "natomtypes",		NuVTypes::IntegerData,		FALSE, TRUE },
 	{ "nbonds",		NuVTypes::IntegerData,		FALSE, TRUE },
 	{ "nbondterms",		NuVTypes::IntegerData,		FALSE, TRUE },
 	{ "npatterns",		NuVTypes::IntegerData,		FALSE, TRUE },
 	{ "nselected",		NuVTypes::IntegerData,		FALSE, TRUE },
 	{ "ntorsionterms",	NuVTypes::IntegerData,		FALSE, TRUE },
 	{ "patterns",		NuVTypes::PatternData,		TRUE, TRUE }
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
	result = new StepNode(i, NuVTypes::ModelData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("ModelVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ModelVariable::retrieveAccessor(int i, NuReturnValue &rv, bool hasArrayIndex, int arrayIndex)
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
	if ((!accessorData[i].isArray) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ModelVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Model *ptr= (Model*) rv.asPointer(NuVTypes::ModelData, result);
	if (result) switch (acc)
	{
		case (ModelVariable::Atoms):
			if (!hasArrayIndex) rv.set(NuVTypes::AtomData, ptr->atoms());
			else if (arrayIndex > ptr->nAtoms())
			{
				msg.print("Atom array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(NuVTypes::AtomData, ptr->atom(arrayIndex-1));
			break;
		case (ModelVariable::Atomtypes):
			if (!hasArrayIndex) rv.set(NuVTypes::ForcefieldAtomData, ptr->uniqueTypes());
			else if (arrayIndex > ptr->nUniqueTypes())
			{
				msg.print("Unique types array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(NuVTypes::ForcefieldAtomData, ptr->uniqueType(arrayIndex-1));
			break;
		case (ModelVariable::Bonds):
			if (!hasArrayIndex) rv.set(NuVTypes::BondData, ptr->bonds());
			else if (arrayIndex > ptr->nBonds())
			{
				msg.print("Bond array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(NuVTypes::BondData, ptr->bond(arrayIndex-1));
			break;
		case (ModelVariable::Cell):
			rv.set(NuVTypes::CellData, ptr->cell());
			break;
		case (ModelVariable::Frame):
			rv.set(NuVTypes::ModelData, ptr->currentFrame());
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
			if (!hasArrayIndex) rv.set(NuVTypes::PatternData, ptr->patterns());
			else if (arrayIndex > ptr->nPatterns())
			{
				msg.print("Pattern array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(NuVTypes::PatternData, ptr->pattern(arrayIndex-1));
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ModelVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ModelVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ModelVariable::setAccessor(int i, NuReturnValue &sourcerv, NuReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
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
	if (!accessorData[i].isArray)
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
	Model *ptr= (Model*) sourcerv.asPointer(NuVTypes::ModelData, result);
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
