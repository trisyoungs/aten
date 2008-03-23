/*
	*** Crystal spacegroups
	*** src/base/spacegroup.h
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

#ifndef ATEN_SPACEGROUP_H
#define ATEN_SPACEGROUP_H

#include "templates/vector3.h"
#include "templates/reflist.h"
#include "classes/cell.h"

// Spacegroup
class Spacegroup
{
	public:
	// Destructor
	~Spacegroup();

	public:
	// Name of the spacegroup (plaintext)
	const char *name;
	// Name of the spacegroup (formatted)
	const char *displayname;
	// Number of symmetry generators for this spacegroup
	int nGenerators;
	// List of symmetry generators for this spacegroup
	int generators[192];
};

#endif
