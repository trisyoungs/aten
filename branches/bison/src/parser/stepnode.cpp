/*
	*** Step Node
	*** src/parser/stepnode.cpp
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

#include "parser/stepnode.h"
#include "parser/aten.h"
#include "parser/atom.h"
#include "parser/bond.h"
#include "parser/cell.h"
#include "parser/elements.h"
#include "parser/forcefield.h"
#include "parser/forcefieldatom.h"
#include "parser/forcefieldbound.h"
#include "parser/model.h"
#include "parser/vector.h"
#include <string.h>

// Constructor
StepNode::StepNode(int id, VTypes::DataType prevtype, TreeNode *arrayindex, VTypes::DataType rtntype, bool readonly) : arrayIndex_(arrayindex), accessor_(id), previousType_(prevtype)
{
	// Private variables
	readOnly_ = readonly;
	returnType_ = rtntype;
	nodeType_ = TreeNode::SteppedNode;
// 	printf("Return type of StepNode is %s\n", VTypes::dataType(returnType_));
}

// Destructor
StepNode::~StepNode()
{
}

// Return accessor id
int StepNode::accessor()
{
	return accessor_;
}

// Execute command
bool StepNode::execute(ReturnValue &rv)
{
	msg.enter("StepNode::execute");
	// Check that the ReturnValue contains the type that we are expecting
	if (rv.type() != previousType_)
	{
		printf("Internal Error: StepNode was expecting a type of '%s' but was given type '%s'\n", VTypes::dataType(previousType_), VTypes::dataType(rv.type()));
		msg.exit("StepNode::execute");
		return FALSE;
	}
	// Retrieve a value from the relevant class
	bool result = FALSE;
	// Get array index if present
	int i = -1;
	if (arrayIndex_ != NULL)
	{
		ReturnValue arrayrv;
		if (!arrayIndex_->execute(arrayrv))
		{
			printf("Failed to retrieve array index.\n");
			return FALSE;
		}
		if ((arrayrv.type() != VTypes::IntegerData) && (arrayrv.type() != VTypes::DoubleData))
		{
			printf("Invalid datatype used as an array index (%s).\n", arrayrv.info());
			return FALSE;
		}
		i = arrayrv.asInteger();
	}
	switch (previousType_)
	{
		case (VTypes::NoData):
			printf("Internal Error: StepNode was expecting NoData (execute).\n");
			break;
		case (VTypes::AtenData):
			result = AtenVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::AtomData):
			result = AtomVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::BondData):
			result = BondVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::CellData):
			result = CellVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ElementsData):
			result = ElementsVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ForcefieldData):
			result = ForcefieldVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ForcefieldAtomData):
			result = ForcefieldAtomVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ForcefieldBoundData):
			result = ForcefieldBoundVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::ModelData):
			result = ModelVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		case (VTypes::VectorData):
			result = VectorVariable::retrieveAccessor(accessor_, rv, arrayIndex_ != NULL, i);
			break;
		default:
			printf("Internal Error: StepNode doesn't recognise this type (%s)\n", VTypes::dataType(previousType_));
			break;
	}
	msg.exit("StepNode::execute");
	return result;
}

// Print node contents
void StepNode::nodePrint(int offset, const char *prefix)
{
	// Stepnodes print in a slightly different way, with no newlines...
	switch (previousType_)
	{
		case (VTypes::NoData):
			printf("Internal Error: StepNode was expecting NoData (print).\n");
			break;
		case (VTypes::AtenData):
			printf("%s", AtenVariable::accessorData[accessor_].name);
			break;
		case (VTypes::AtomData):
			printf("%s", AtomVariable::accessorData[accessor_].name);
			break;
		case (VTypes::ModelData):
			printf("%s", ModelVariable::accessorData[accessor_].name);
			break;
		case (VTypes::VectorData):
			printf("%s", VectorVariable::accessorData[accessor_].name);
			break;
		default:
			printf("Internal Error: StepNode doesn't know how to print a member from type (%s)\n", VTypes::dataType(previousType_));
			break;
	}
}

// Set from returnvalue nodes
bool StepNode::set(ReturnValue &executerv, ReturnValue &setrv)
{
	msg.enter("StepNode::set");
	// Check that the ReturnValue contains the type that we are expecting
	if (executerv.type() != previousType_)
	{
		printf("Internal Error: StepNode was expecting a type of '%s' but was given type '%s' (in set)\n", VTypes::dataType(previousType_), VTypes::dataType(executerv.type()));
		msg.exit("StepNode::set");
		return FALSE;
	}
	// Retrieve a value from the relevant class
	bool result = FALSE;
	switch (previousType_)
	{
		case (VTypes::NoData):
			printf("Internal Error: StepNode was expecting NoData (set).\n");
			break;
		case (VTypes::AtenData):
			result = AtenVariable::setAccessor(accessor_, executerv, setrv, FALSE);
			break;
		case (VTypes::AtomData):
			result = AtomVariable::setAccessor(accessor_, executerv, setrv, FALSE);
			break;
		case (VTypes::BondData):
			result = BondVariable::setAccessor(accessor_, executerv, setrv, FALSE);
			break;
		case (VTypes::CellData):
			result = CellVariable::setAccessor(accessor_, executerv, setrv, FALSE);
			break;
		case (VTypes::ElementsData):
			result = ElementsVariable::setAccessor(accessor_, executerv, setrv, FALSE);
			break;
		case (VTypes::ForcefieldData):
			result = ForcefieldVariable::setAccessor(accessor_, executerv, setrv, FALSE);
			break;
		case (VTypes::ForcefieldAtomData):
			result = ForcefieldAtomVariable::setAccessor(accessor_, executerv, setrv, FALSE);
			break;
		case (VTypes::ForcefieldBoundData):
			result = ForcefieldBoundVariable::setAccessor(accessor_, executerv, setrv, FALSE);
			break;
		case (VTypes::ModelData):
			result = ModelVariable::setAccessor(accessor_, executerv, setrv, FALSE);
			break;
		case (VTypes::VectorData):
			result = VectorVariable::setAccessor(accessor_, executerv, setrv, FALSE);
			break;
		default:
			printf("Internal Error: StepNode doesn't recognise this type (%s) (set)\n", VTypes::dataType(previousType_));
			break;
	}
	msg.exit("StepNode::set");
	return result;
}

// Set from returnvalue node
bool StepNode::set(ReturnValue &rv)
{
	printf("Internal Error: Use StepNode::set(NUreturnValue,ReturnValue) for StepNodes.\n");
	return FALSE;
}

// Initialise node
bool StepNode::initialise()
{
	printf("Internal Error: A StepNode cannot be initialised.\n");
	return FALSE;
}

// Static function to search accessors of type represented by this path step
StepNode *StepNode::findAccessor(const char *s, TreeNode *arrayindex)
{
	msg.enter("StepNode::findAccessor");
	// From the return type of the node, determine which (static) function to call
	StepNode *result = NULL;
	switch (returnType_)
	{
		case (VTypes::NoData):
			printf("Internal Error: StepNode was expecting NoData.\n");
			break;
		case (VTypes::AtomData):
			result = AtomVariable::accessorSearch(s, arrayindex);
			break;
		case (VTypes::BondData):
			result = BondVariable::accessorSearch(s, arrayindex);
			break;
		case (VTypes::CellData):
			result = CellVariable::accessorSearch(s, arrayindex);
			break;
		case (VTypes::ElementsData):
			result = ElementsVariable::accessorSearch(s, arrayindex);
			break;
		case (VTypes::ForcefieldData):
			result = ForcefieldVariable::accessorSearch(s, arrayindex);
			break;
		case (VTypes::ForcefieldAtomData):
			result = ForcefieldAtomVariable::accessorSearch(s, arrayindex);
			break;
		case (VTypes::ForcefieldBoundData):
			result = ForcefieldBoundVariable::accessorSearch(s, arrayindex);
			break;
		case (VTypes::ModelData):
			result = ModelVariable::accessorSearch(s, arrayindex);
			break;
		case (VTypes::VectorData):
			result = VectorVariable::accessorSearch(s, arrayindex);
			break;
		default:
			printf("Internal Error: StepNode doesn't know how to search for accessors in type '%s'.\n", VTypes::dataType(returnType_));
			break;
	}
	msg.exit("StepNode::findAccessor");
	return result;
}
