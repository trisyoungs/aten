/*
	*** Crystal spacegroups / generators
	*** src/classes/spacegroup.h
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

#ifndef H_SPACEGROUP_H
#define H_SPACEGROUP_H

#include "templates/vector3.h"
#include "templates/reflist.h"
#include "classes/cell.h"

// Forward Declarations
class generator;

#define NSPACEGROUPS 231

// Spacegroup
class spacegroup
{
	public:
	// Destructor
	~spacegroup();

	public:
	// Name of the spacegroup (plaintext)
	const char *name;
	// Name of the spacegroup (formatted)
	const char *displayname;
	// Number of symmetry generators for this spacegroup
	int ngenerators;
	// List of symmetry generators for this spacegroup
	int generators[192];
};

#endif
