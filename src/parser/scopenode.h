/*
	*** Scoped Command Node
	*** src/parser/scopenode.h
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

#ifndef ATEN_SCOPENODE_H
#define ATEN_SCOPENODE_H

#include "parser/commandnode.h"
#include "parser/variablelist.h"

// Scoped Command Node
class ScopeNode : public NuCommandNode
{
	public:
	// Constructor / Destructor
	ScopeNode(NuCommand::Function func = NuCommand::NoFunction);
	~ScopeNode();

	/*
	// Variable List
	*/
	NuVariableList variables;
};

#endif
