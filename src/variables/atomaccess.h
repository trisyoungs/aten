/*
	*** Atom Accessors
	*** src/variables/atomaccess.h
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

#ifndef ATEN_ATOMACCESS_H
#define ATEN_ATOMACCESS_H

#include "variables/vaccess.h"

// Atom Accessor
class AtomAccessors : public VAccess
{
	public:
	// Constructor
	AtomAccessors();
	// Accessor list
	enum Accessors { FX, FY, FZ, Id, Mass, Name, Q, RX, RY, RZ, Symbol, Type, VX, VY, VZ, Z, nAccessors };

	private:
	// Array of acessor pointers for look-up
	Variable *accessorPointers[AtomAccessors::nAccessors];

	public:
	// Retrieve specified data
	bool retrieve(void *classptr, AccessStep *step, ReturnValue &rv);
	// Set specified data
	bool set(void *classptr, AccessStep *step, Variable *sourcevar);
};

extern AtomAccessors atomAccessors;

#endif
