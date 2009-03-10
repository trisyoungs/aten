/*
	*** Bond Variable
	*** src/parser/bond.h
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

#ifndef ATEN_BONDVARIABLE_H
#define ATEN_BONDVARIABLE_H

#include "parser/variable.h"
#include "parser/accessor.h"

// Forward Declarations
class Bond;

// Bond Variable
class BondVariable : public NuVariable
{
	public:
	// Constructor / Destructor
	BondVariable(Bond *i = NULL, bool constant = FALSE);
	~BondVariable();

	/*
	// Set / Get
	*/
	public:
	// Return value of node
	bool execute(NuReturnValue &rv);
	// Set from returnvalue node
	bool set(NuReturnValue &rv);
	// Reset node
	void reset();

	/*
	// Variable Data
	*/
	private:
	// Bond data
	void *bondData_;
	// Print node contents
	void nodePrint(int offset, const char *prefix = "");

	/*
	// Access Data
	*/
	public:
	// Accessor list
        enum Accessors { I, J, Order, Type, nAccessors };
	// Search variable access list for provided accessor
	StepNode *findAccessor(const char *s, bool array);
	// Static function to search accessors
	static StepNode *accessorSearch(const char *s, bool array);
	// Retrieve desired value
	static bool retrieveAccessor(int i, NuReturnValue &rv, bool hasarrayindex, int arrayIndex = -1);
	// Accessor data
	static Accessor accessorData[nAccessors];
};

#endif

