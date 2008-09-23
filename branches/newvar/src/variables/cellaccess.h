/*
	*** Cell Accessors
	*** src/variables/cellaccess.h
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

#ifndef ATEN_CELLACCESS_H
#define ATEN_CELLACCESS_H

#include "variables/vaccess.h"

// Cell Accessor
class CellAccessors : public VAccess
{
	public:
	// Constructor
	CellAccessors();
	// Accessor list
	enum Accessors { A, B, C, Alpha, Beta, Gamma, AX, AY, AZ, BX, BY, BZ, CX, CY, CZ, CentreX, CentreY, CentreZ, Type, nAccessors };

	private:
	// Array of acessor pointers for look-up
	Variable *accessorPointers[CellAccessors::nAccessors];

	public:
	// Retrieve specified data
	bool retrieve(void *classptr, int vid, ReturnValue &rv);
	// Set specified data
	bool set(void *classptr, int vid, Variable *sourcevar);
};

extern CellAccessors cellAccessors;

#endif
