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
#include "parser/accessnode.h"
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
 	{ "cell",		NuVTypes::CellData,		TRUE, FALSE },
 	{ "frame",		NuVTypes::ModelData,		TRUE, FALSE },
//  	{ "frames",		NuVTypes::ModelData };
 	{ "name",		NuVTypes::CharacterData,	FALSE, FALSE },
 	{ "nangleterms",	NuVTypes::IntegerData,		TRUE, FALSE },
 	{ "natoms",		NuVTypes::IntegerData,		TRUE, FALSE },
 	{ "natomtypes",		NuVTypes::IntegerData,		TRUE, FALSE },
 	{ "nbonds",		NuVTypes::IntegerData,		TRUE, FALSE },
 	{ "nbondterms",		NuVTypes::IntegerData,		TRUE, FALSE },
 	{ "npatterns",		NuVTypes::IntegerData,		TRUE, FALSE },
 	{ "nselected",		NuVTypes::IntegerData,		TRUE, FALSE },
 	{ "ntorsionterms",	NuVTypes::IntegerData,		TRUE, FALSE },
 	{ "patterns",		NuVTypes::PatternData,		TRUE, TRUE }
};

// Search variable access list for provided accessor (call private static function)
AccessNode *ModelVariable::findAccessor(const char *s)
{
	return ModelVariable::accessorSearch(s);
}

// Private static function to search accessors
AccessNode *ModelVariable::accessorSearch(const char *s)
{
	msg.enter("ModelVariable::accessorSearch");
	AccessNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.exit("ModelVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	printf("Accessor match = %i\n", i);
	result = new AccessNode(i, NuVTypes::VectorData, accessorData[i].returnType);
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
		printf("Internal Error: Accessor id %i is out of range for Vector type.\n");
		msg.exit("ModelVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if (!accessorData[i].isArray)
	{
		if (hasArrayIndex) msg.print("Warning: Irrelevent array index provided for member '%s'.\n", accessorData[i].name);
	}
	else if (!hasArrayIndex)
	{
		msg.print("Error: No array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ModelVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result;
	Model *ptr= (Model*) rv.asPointer(NuVTypes::ModelData, result);
	if (result) switch (acc)
	{
		case (ModelVariable::Atoms):
			if (arrayIndex > ptr->nAtoms())
			{
				msg.print("Atom array index is out of bounds for model '%s'\n", ptr->name());
				result = FALSE;
			}
			else rv.set(NuVTypes::AtomData, ptr->atom(arrayIndex-1));
			break;
		case (ModelVariable::Atomtypes):
			rv.set(NuVTypes::ForcefieldAtomData, ptr->uniqueType(arrayIndex-1));
			break;
		case (ModelVariable::Bonds):
			if (arrayIndex > ptr->nBonds())
			{
				msg.print("Bond array index is out of bounds for model '%s'\n", ptr->name());
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
			if (arrayIndex > ptr->nPatterns())
			{
				msg.print("Pattern array index is out of bounds for model '%s'\n", ptr->name());
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

/*
bool ModelVariable::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("ModelVariable::set");
	bool result = TRUE;
	// Cast pointer into Model*
	Model *m = (Model*) classptr;
	if (m == NULL) printf("Warning - NULL Model pointer passed to ModelVariable::set.\n");
// 	printf("Enumerated ID supplied to ModelAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > ModelVariable::nAccessors))
	{
		printf("Unknown enumeration %i given to ModelVariable::set.\n", vid);
		msg.exit("ModelVariable::set");
		return FALSE;
	} 
	// Check read-only status
	if (accessorPointers[vid]->readOnly())
	{
		msg.print("Member '%s' of 'model' type is read-only.\n", accessorPointers[vid]->name());
		msg.exit("ModelVariable::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("ModelVariable::set");
		return FALSE;
	}
	// Set value based on enumerated id
	switch (vid)
	{
		case (ModelVariable::Name):
			m->setName(srcvar->asCharacter());
			break;
		default:
			printf("ModelVariable::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("ModelVariable::set");
	return result;
}
*/
