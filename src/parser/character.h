/*
	*** Character Variable
	*** src/parser/character.h
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

#ifndef ATEN_CHARACTERVARIABLE_H
#define ATEN_CHARACTERVARIABLE_H

#include "parser/variable.h"
#include "parser/accessor.h"
#include "base/dnchar.h"

// Character Variable
class NuCharacterVariable : public NuVariable
{
	public:
	// Constructor / Destructor
	NuCharacterVariable(const char *s, bool constant = FALSE);
	~NuCharacterVariable();

	/*
	// Set / Get
	*/
	public:
	// Set value of variable (character)
	bool set(const char *s);
	// Return value of node
	bool execute(NuReturnValue &rv);
	// Set from returnvalue node
	bool set(NuReturnValue &rv);
	// Reset variable
	void reset();

	/*
	// Variable Data
	*/
	private:
	// Character data
	Dnchar characterData_;
	// Print node contents
	void nodePrint(int offset, const char *prefix = "");

	/*
	// Access Data
	*/
	public:
	// Accessor list
	enum Accessors { nAccessors };
	// Search variable access list for provided accessor
	TreeNode *findAccessor(const char *s);

	private:
	// Accessor data
 	static Accessor accessorData[nAccessors];
};

#endif
