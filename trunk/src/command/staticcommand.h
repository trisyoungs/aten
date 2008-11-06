/*
	*** Static Command
	*** src/command/staticcommand.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_STATICCOMMAND_H
#define ATEN_STATICCOMMAND_H

#include "command/command.h"

// Static (Independent) Command node
class StaticCommandNode
{
	public:
	// Constructor / Destructor
	StaticCommandNode(Command::Function func, const char *args, ...);

	private:
	// Standard Command
	CommandNode cmd_;
	// Permanent variablelist for command
	static VariableList staticVariableList_;
	// Permanent CommandList to own the command
	static CommandList staticCommandList_;

	public:
	// Set (overwrite) command
	void setFunction(Command::Function cf);
	// Set variable arguments
	void pokeArguments(const char *args, ...);
	// Execute command node
	int execute();
};

#endif
