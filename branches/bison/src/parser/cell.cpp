/*
	*** Cell Variable
	*** src/parser/cell.cpp
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

#include "parser/cell.h"
#include "parser/stepnode.h"
#include "base/cell.h"
#include "base/sysfunc.h"
#include "base/spacegroup.h"
#include <string.h>

// Constructor
CellVariable::CellVariable(Cell *ptr, bool constant) : cellData_(ptr)
{
	// Private variables
	returnType_ = NuVTypes::CellData;
	readOnly_ = constant;
}

// Destructor
CellVariable::~CellVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool CellVariable::set(NuReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a cell&) cannot be assigned to.\n");
		return FALSE;
	}
	bool success;
	cellData_ = rv.asPointer(NuVTypes::CellData, success);
	return success;
}

// Reset variable
void CellVariable::reset()
{
	cellData_ = NULL;
}

// Return value of node
bool CellVariable::execute(NuReturnValue &rv)
{
	// If this vector is a constant, read the three stored expressions to recreate it
	rv.set(NuVTypes::CellData, cellData_);
	return TRUE;
}

// Print node contents
void CellVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("%s%li (cell) (constant value)\n", tab, cellData_);
	else printf("%s%li (cell) (variable, name=%s)\n", tab, cellData_, name_.get());
	delete[] tab;
}

/*
// Accessors
*/

// Accessor data
Accessor CellVariable::accessorData[CellVariable::nAccessors] = {
	{ "a",		NuVTypes::RealData,	FALSE, FALSE },
	{ "b",		NuVTypes::RealData,	FALSE, FALSE },
	{ "c",		NuVTypes::RealData,	FALSE, FALSE },
	{ "alpha",	NuVTypes::RealData,	FALSE, FALSE },
	{ "beta",	NuVTypes::RealData,	FALSE, FALSE },
	{ "gamma",	NuVTypes::RealData,	FALSE, FALSE },
	{ "ax",		NuVTypes::RealData,	FALSE, FALSE },
	{ "ay",		NuVTypes::RealData,	FALSE, FALSE },
	{ "az",		NuVTypes::RealData,	FALSE, FALSE },
	{ "bx",		NuVTypes::RealData,	FALSE, FALSE },
	{ "by",		NuVTypes::RealData,	FALSE, FALSE },
	{ "bz",		NuVTypes::RealData,	FALSE, FALSE },
	{ "cx",		NuVTypes::RealData,	FALSE, FALSE },
	{ "cy",		NuVTypes::RealData,	FALSE, FALSE },
	{ "cz",		NuVTypes::RealData,	FALSE, FALSE },
	{ "centrex",	NuVTypes::RealData,	FALSE, TRUE },
	{ "centrey",	NuVTypes::RealData,	FALSE, TRUE },
	{ "centrez",	NuVTypes::RealData,	FALSE, TRUE },
	{ "density",	 NuVTypes::RealData,	FALSE, TRUE },
	{ "matrix", 	NuVTypes::RealData,	TRUE, FALSE },
	{ "sgid",	NuVTypes::IntegerData,	FALSE, FALSE },
	{ "sgname",	NuVTypes::StringData,	FALSE, FALSE },
	{ "sgsetting",	NuVTypes::IntegerData,	FALSE, FALSE },
	{ "type",	NuVTypes::StringData,	FALSE, TRUE },
	{ "volume",	 NuVTypes::RealData,	FALSE, TRUE },
};

// Search variable access list for provided accessor (call private static function)
StepNode *CellVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return CellVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *CellVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("CellVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'cell&' has no member named '%s'.\n", s);
		msg.exit("CellVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, NuVTypes::CellData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("CellVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool CellVariable::retrieveAccessor(int i, NuReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("CellVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Cell type.\n");
		msg.exit("CellVariable::retrieveAccessor");
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
		msg.exit("CellVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Cell *ptr= (Cell*) rv.asPointer(NuVTypes::CellData, result);
	if (result) switch (acc)
	{
		case (CellVariable::A):
		case (CellVariable::B):
		case (CellVariable::C):
			rv.set(ptr->lengths().get(acc - CellVariable::A));
			break;
		case (CellVariable::Alpha):
		case (CellVariable::Beta):
		case (CellVariable::Gamma):
			rv.set(ptr->angles().get(acc - CellVariable::Alpha));
			break;
		case (CellVariable::AX):
		case (CellVariable::AY):
		case (CellVariable::AZ):
		case (CellVariable::BX):
		case (CellVariable::BY):
		case (CellVariable::BZ):
		case (CellVariable::CX):
		case (CellVariable::CY):
		case (CellVariable::CZ):
			rv.set(ptr->axes().element(acc - CellVariable::AX));
			break;
		case (CellVariable::CentreX):
		case (CellVariable::CentreY):
		case (CellVariable::CentreZ):
			rv.set(ptr->centre().get(acc - CellVariable::CentreX));
			break;
		case (CellVariable::Density):
			rv.set(ptr->density());
			break;
		case (CellVariable::Matrix):
			if ((arrayIndex < 1) || (arrayIndex > 9))
			{
				msg.print("Array index [%i] is out of range for 'matrix' member.\n", arrayIndex);
				result = FALSE;
			}
			else rv.set(ptr->axes().element(arrayIndex-1));
			break;
		case (CellVariable::SpacegroupId):
			rv.set(ptr->spacegroup());
			break;
		case (CellVariable::SpacegroupName):
			rv.set(spacegroups.name(ptr->spacegroup()));
			break;
		case (CellVariable::SpacegroupSetting):
			rv.set(ptr->spacegroupSetting());
			break;
		case (CellVariable::Type):
			rv.set(lowerCase(Cell::cellType(ptr->type())));
			break;
		case (CellVariable::Volume):
			rv.set(ptr->volume());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in CellVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("CellVariable::retrieveAccessor");
	return result;
}

/*
// Set specified data
bool CellVariable::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("CellVariable::set");
	bool result = TRUE;
	// Cast pointer into Cell*
	Cell *c = (Cell*) classptr;
	if (c == NULL) printf("Warning - NULL Cell pointer passed to CellVariable::set.\n");
// 	printf("Enumerated ID supplied to CellAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > CellVariable::nAccessors))
	{
		printf("Unknown enumeration %i given to CellVariable::set.\n", vid);
		msg.exit("CellVariable::set");
		return FALSE;
	}
	// Check read-only status
	if (accessorPointers[vid]->readOnly())
	{
		msg.print("Member '%s' of 'cell' type is read-only.\n", accessorPointers[vid]->name());
		msg.exit("CellVariable::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("CellVariable::set");
		return FALSE;
	}
	Mat3<double> m;
	Vec3<double> v;
	// Set value based on enumerated id
	switch (vid)
	{
		case (CellVariable::A):
		case (CellVariable::B):
		case (CellVariable::C):
		case (CellVariable::Alpha):
		case (CellVariable::Beta):
		case (CellVariable::Gamma):
		case (CellVariable::AX):
		case (CellVariable::AY):
		case (CellVariable::AZ):
		case (CellVariable::BX):
		case (CellVariable::BY):
		case (CellVariable::BZ):
		case (CellVariable::CX):
		case (CellVariable::CY):
		case (CellVariable::CZ):
			// Cast vid into a CellParameter
			c->parent()->setCell( (Cell::CellParameter) vid, srcvar->asDouble());
			break;
		case (CellVariable::Matrix):
			// Cast vid into a CellParameter
			c->parent()->setCell( (Cell::CellParameter) ((index-1) + Cell::CellAX), srcvar->asDouble());
			break;
		default:
			printf("CellVariable::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("CellVariable::set");
	return result;
}
*/
