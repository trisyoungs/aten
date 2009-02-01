/*
	*** Crystal spacegroups
	*** src/base/spacegroup.h
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

#ifndef ATEN_SPACEGROUP_H
#define ATEN_SPACEGROUP_H

#include "templates/vector3.h"
#include "templates/reflist.h"
#include "base/cell.h"

// Spacegroup
class Spacegroup
{
	public:
	// Name of the spacegroup (plaintext)
	const char *name;
	// Name of the spacegroup (formatted)
	const char *displayName;
	// Number of symmetry generators for this spacegroup
	int nGenerators;
	// List of symmetry generators for this spacegroup
	int generators[192];
};

// Spacegroup Map
class SpacegroupMap
{
	public:
	// Constructor
	SpacegroupMap();

	private:
	// Spacegroup definitions
	static Spacegroup spacegroups_[];
	// Number of defined generators
	int nGenerators_;

	public:
	// Return id for the named spacegroup
	int spacegroup(const char *name) const;
	// Return plaintext name for the specified spacegroup
	const char *name(int sg) const;
	// Return richtext name for the specified spacegroup
	const char *displayName(int sg) const;
	// Return number of generators for spacegroup
	int nGenerators(int sg) const;
	// Return id of nth generator for spacegroup
	int generator(int sg, int gen) const;
	// Returns cell type of specified spacegroup id
	Cell::CellType cellType(int sg) const;
};

extern SpacegroupMap spacegroups;

#endif
