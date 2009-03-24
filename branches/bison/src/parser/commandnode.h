/*
	*** Command Node
	*** src/parser/commandnode.h
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

#ifndef ATEN_NUCOMMANDNODE_H
#define ATEN_NUCOMMANDNODE_H

#include "templates/reflist.h"
#include "templates/list.h"
#include "templates/vector3.h"
#include "nucommand/commands.h"
#include "parser/treenode.h"
#include "parser/returnvalue.h"
#include "parser/format.h"
#include "base/vtypes.h"

// Forward Declarations
class Tree;

// Command Node
class NuCommandNode : public TreeNode
{
	public:
	// Constructor / Destructor
	NuCommandNode(NuCommand::Function func = NuCommand::NoFunction);
	~NuCommandNode();

	/*
	// Command Data
	*/
	protected:
	// Command that this node performs
	NuCommand::Function function_;
	// Associated format node (if any)
	NuFormat *format_;
	
	public:
	// Set command function
	void setFunction(NuCommand::Function ca);
	// Get command function
	NuCommand::Function function();
	// Check validity of supplied arguments
	bool checkArguments();
	// Create format node (if necessary) from supplied argument id
	NuFormat *createFormat(int fmtargid, int firstargid);
	// Execute command
	bool execute(NuReturnValue &rv);
	// Print node contents
	void nodePrint(int offset, const char *prefix = "");
	// Set from returnvalue node
	bool set(NuReturnValue &rv);
	// Initialise node
	bool initialise();
	// Create, run, and free a single command with simple arguments
	static bool run(NuCommand::Function func, const char *arglist, ...);
};

#endif
