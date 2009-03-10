/*
	*** Cell Variable
	*** src/parser/cell.h
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

#ifndef ATEN_CELLVARIABLE_H
#define ATEN_CELLVARIABLE_H

#include "parser/variable.h"
#include "parser/accessor.h"

// Forward Declarations
class Cell;

// Cell Variable
class CellVariable : public NuVariable
{
	public:
	// Constructor / Destructor
	CellVariable(Cell *i = NULL, bool constant = FALSE);
	~CellVariable();

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
	// Cell data
	void *cellData_;
	// Print node contents
	void nodePrint(int offset, const char *prefix = "");

	/*
	// Access Data
	*/
	public:
	// Accessor list
	enum Accessors { A, B, C, Alpha, Beta, Gamma, AX, AY, AZ, BX, BY, BZ, CX, CY, CZ, CentreX, CentreY, CentreZ, Density, Matrix, SpacegroupId, SpacegroupName, SpacegroupSetting, Type, Volume, nAccessors };
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

