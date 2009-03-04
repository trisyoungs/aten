/*
	*** Variable Node
	*** src/parser/variablenode.cpp
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

#include "parser/accessnode.h"
#include "parser/vector.h"
#include <string.h>

// Constructor
AccessNode::AccessNode(int id, NuVTypes::DataType prevtype, NuVTypes::DataType rtntype) : accessor_(id), previousType_(prevtype)
{
	// Private variables
	readOnly_ = FALSE;
	returnType_ = rtntype;
	printf("Return type of AccessNode is %s\n", NuVTypes::dataType(returnType_));
}

// Destructor
AccessNode::~AccessNode()
{
}

// Execute command
bool AccessNode::execute(NuReturnValue &rv)
{
	msg.enter("AccessNode::execute");
	// Check that the ReturnValue contains the type that we are expecting
	if (rv.type() != previousType_)
	{
		printf("Internal Error: AccessNode was expecting a type of '%s' but was given type '%s'\n", NuVTypes::dataType(previousType_), NuVTypes::dataType(rv.type()));
		msg.exit("AccessNode::execute");
		return FALSE;
	}
	// Retrieve a value from the relevant class
	bool result = FALSE;
	switch (previousType_)
	{
		case (NuVTypes::NoData):
			printf("Internal Error: AccessNode was expecting NoData.\n");
			break;
		case (NuVTypes::VectorData):
			result = NuVectorVariable::retrieveAccessor(accessor_, rv, FALSE);
			break;
		default:
			printf("Internal Error: AccessNode doesn't recognise this type (%s)\n", NuVTypes::dataType(previousType_));
			break;
	}
	msg.exit("AccessNode::execute");
	return result;
}

// Print node contents
void AccessNode::nodePrint(int offset, const char *prefix)
{
	printf("Cannot print the contents of an AccessNode (yet).\n");
}

// Set from returnvalue node
bool AccessNode::set(NuReturnValue &rv)
{
	printf("Cannot SET an AccessNode (yet).\n");
	return FALSE;
}

// Reset node
void AccessNode::reset()
{
	return;
}
