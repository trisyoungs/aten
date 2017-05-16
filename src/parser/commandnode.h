/*
	*** Command Node
	*** src/parser/commandnode.h
	Copyright T. Youngs 2007-2017

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

#ifndef ATEN_COMMANDNODE_H
#define ATEN_COMMANDNODE_H

#include "templates/reflist.h"
#include "templates/list.h"
#include "templates/vector3.h"
#include "command/commands.h"
#include "parser/treenode.h"
#include "parser/returnvalue.h"
#include "parser/format.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Tree;
class Aten;

// Command Node
class CommandNode : public TreeNode
{
	public:
	// Constructors / Destructor
	CommandNode(Commands::Function func = Commands::NoFunction);
	CommandNode(TreeNode* source);
	~CommandNode();


	/*
	 * Link to Aten
	 */
	private:
	// Pointer to Aten
	static Aten* aten_;

	public:
	// Set pointer to Aten
	static void setAten(Aten* aten);
	// Return pointer to Aten
	Aten* aten();


	/*
	 * Command Data
	 */
	protected:
	// Command that this node performs
	Commands::Function function_;
	// Associated format node (if any)
	Format* format_;
	
	public:
	// Prepare the stored command function, initialising any data and running any commands
	bool prepFunction();
	// Get command function
	Commands::Function function();
	// Create format node (if necessary) from supplied argument id
	Format* createFormat(int fmtargid, int firstargid);
	// Create a 'delimited' writeable format (if necessary) from supplied argument id
	Format* createFormat(const char* delimiter);
	// Execute command
	bool execute(ReturnValue& rv);
	// Print node contents
	void nodePrint(int offset, const char* prefix = "");
	// Set from returnvalue node
	bool set(ReturnValue& rv);
	// Initialise node
	bool initialise();
	// Create, run, and free a single command with simple arguments
	static ReturnValue run(Commands::Function func, const char* argList = NULL, ...);
	// Create, run, and free a single command with simple arguments and specified bundle
	static ReturnValue run(Commands::Function func, Bundle& bundle, const char* argList = NULL, ...);
	// Execute command with specified bundle
	bool execute(Bundle& bundle, ReturnValue& rv);
};

ATEN_END_NAMESPACE

#endif
