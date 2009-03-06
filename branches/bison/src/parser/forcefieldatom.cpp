/*
	*** ForcefieldAtom Variable
	*** src/parser/forcefieldatom.cpp
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

#include "parser/forcefieldatom.h"
#include "parser/stepnode.h"
#include "classes/forcefieldatom.h"
#include "base/constants.h"
#include <string.h>

// Constructor
ForcefieldAtomVariable::ForcefieldAtomVariable(ForcefieldAtom *ptr, bool constant) : ffatomData_(ptr)
{
	// Private variables
	returnType_ = NuVTypes::ForcefieldAtomData;
	readOnly_ = constant;
}

// Destructor
ForcefieldAtomVariable::~ForcefieldAtomVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool ForcefieldAtomVariable::set(NuReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a ffatom&) cannot be assigned to.\n");
		return FALSE;
	}
	bool success;
	ffatomData_ = rv.asPointer(NuVTypes::ForcefieldAtomData, success);
	return success;
}

// Reset variable
void ForcefieldAtomVariable::reset()
{
	ffatomData_ = NULL;
}

// Return value of node
bool ForcefieldAtomVariable::execute(NuReturnValue &rv)
{
	// If this vector is a constant, read the three stored expressions to recreate it
	rv.set(NuVTypes::ForcefieldAtomData, ffatomData_);
	return TRUE;
}

// Print node contents
void ForcefieldAtomVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("[C]%s%li (ffatom) (constant value)\n", tab, ffatomData_);
	else printf("[V]%s%li (ffatom) (variable, name=%s)\n", tab, ffatomData_, name_.get());
	delete[] tab;
}

/*
// Accessors
*/

// Accessor data
Accessor ForcefieldAtomVariable::accessorData[ForcefieldAtomVariable::nAccessors] = {
	{ "atomtype",		NuVTypes::CharacterData,	FALSE, TRUE },
	{ "charge",		NuVTypes::RealData,		FALSE, FALSE },
	{ "data",		NuVTypes::RealData,		TRUE, FALSE },
	{ "description",	NuVTypes::CharacterData,	FALSE, FALSE },
	{ "equivalent",		NuVTypes::CharacterData,	FALSE, FALSE },
	{ "form",		NuVTypes::CharacterData,	FALSE, FALSE },
	{ "id",			NuVTypes::IntegerData,		FALSE, TRUE },
	{ "name",		NuVTypes::CharacterData,	FALSE, FALSE },
	{ "ff",			NuVTypes::ForcefieldData,	FALSE, TRUE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *ForcefieldAtomVariable::findAccessor(const char *s)
{
	return ForcefieldAtomVariable::accessorSearch(s);
}

// Private static function to search accessors
StepNode *ForcefieldAtomVariable::accessorSearch(const char *s)
{
	msg.enter("ForcefieldAtomVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'bond&' has no member named '%s'.\n", s);
		msg.exit("ForcefieldAtomVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	printf("Accessor match = %i\n", i);
	result = new StepNode(i, NuVTypes::ForcefieldAtomData, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("ForcefieldAtomVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ForcefieldAtomVariable::retrieveAccessor(int i, NuReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ForcefieldAtomVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ForcefieldAtom type.\n");
		msg.exit("ForcefieldAtomVariable::retrieveAccessor");
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
		msg.exit("ForcefieldAtomVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ForcefieldAtom *ptr= (ForcefieldAtom*) rv.asPointer(NuVTypes::ForcefieldAtomData, result);
	if (result) switch (acc)
	{
		case (ForcefieldAtomVariable::Atomtype):
			rv.set(ptr->atomtypeString());
			break;
		case (ForcefieldAtomVariable::Charge):
			rv.set(ptr->charge());
			break;
		case (ForcefieldAtomVariable::Data):
			if ((arrayIndex < 1) || (arrayIndex > MAXFFPARAMDATA))
			{
				msg.print("Array index [%i] is out of range for 'data' member.\n", arrayIndex);
				result = FALSE;
			}
			else rv.set(ptr->parameter(arrayIndex-1));
			break;
		case (ForcefieldAtomVariable::Description):
			rv.set(ptr->description());
			break;
		case (ForcefieldAtomVariable::Equivalent):
			rv.set(ptr->equivalent());
			break;
		case (ForcefieldAtomVariable::Form):
			rv.set(VdwFunctions::VdwFunctions[ptr->vdwForm()].keyword);
			break;
		case (ForcefieldAtomVariable::Id):
			rv.set(ptr->typeId());
			break;
		case (ForcefieldAtomVariable::Name):
			rv.set(ptr->name());
			break;
		case (ForcefieldAtomVariable::ParentFF):
			rv.set(NuVTypes::ForcefieldData, ptr->parent());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ForcefieldAtomVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldAtomVariable::retrieveAccessor");
	return result;
}

/*
// Set specified data
bool ForcefieldAtomVariable::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("ForcefieldAtomVariable::set");
	bool result = TRUE;
	VdwFunctions::VdwFunction vf;
	// Cast pointer into ForcefieldAtom*
	ForcefieldAtom *ffa = (ForcefieldAtom*) classptr;
	if (ffa == NULL) printf("Warning - NULL ForcefieldAtom pointer passed to ForcefieldAtomVariable::set.\n");
// 	printf("Enumerated ID supplied to FFAtomAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > ForcefieldAtomVariable::nAccessors))
	{
		printf("Unknown enumeration %i given to ForcefieldAtomVariable::set.\n", vid);
		msg.exit("ForcefieldAtomVariable::set");
		return FALSE;
	}
	// Check read-only status
	if (accessorPointers[vid]->readOnly())
	{
		msg.print("Member '%s' of 'prefs' type is read-only.\n", accessorPointers[vid]->name());
		msg.exit("PrefsAccessors::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("ForcefieldAtomVariable::set");
		return FALSE;
	}
	// Set value based on enumerated id
	switch (vid)
	{
		case (ForcefieldAtomVariable::Charge):
			ptr->setCharge(srcvar->asDouble());
			break;
		case (ForcefieldAtomVariable::Data):
			ptr->setParameter(index-1, srcvar->asDouble());
			break;
		case (ForcefieldAtomVariable::Description):
			ptr->setDescription(srcvar->asCharacter());
			break;
		case (ForcefieldAtomVariable::Equivalent):
			ptr->setEquivalent(srcvar->asCharacter());
			break;
		case (ForcefieldAtomVariable::Form):
			vf = VdwFunctions::vdwFunction(srcvar->asCharacter());
			if (vf == VdwFunctions::None) result = FALSE;
			else ptr->setVdwForm(vf);
			break;
		case (ForcefieldAtomVariable::Name):
			ptr->setName(srcvar->asCharacter());
			break;
		default:
			printf("ForcefieldAtomVariable::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldAtomVariable::set");
	return result;
}
*/
