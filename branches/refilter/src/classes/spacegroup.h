/*
	*** Crystal spacegroups / generators
	*** src/classes/spacegroup.h
	Copyright T. Youngs 2007

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
/*

*/

#ifndef H_SPACEGROUP_H
#define H_SPACEGROUP_H

#include "templates/vector3.h"
#include "templates/list.h"
#include "classes/cell.h"

#define NSPACEGROUPS 231

// Symmetry operator
class symmop
{
	private:
	// Rotation matrix
	mat3<double> rotation;
	// Translation vector
	vec3<double> translation;
	// Whether this is the identity operator (x,y,z,0,0,0)
	bool identity;
	public:
	// Constructor
	symmop();
	// Destructor
	~symmop();
	// List pointers
	symmop *next, *prev;
	// Set the matrix and translation from string of format "xvec,yvec,zvec,tx,ty,tz"
	void set(const char*);
	// Set the matrix and translation from string of format "xvec+tx,yvec+ty,zvec+tz"
	void set_from_xyz(const char*);
	// Returns the rotation matrix
	mat3<double> get_rotation() { return rotation; };
	// Returns the translation vector
	vec3<double> get_translation() { return translation; };
	// Returns whether the operator is the identity operator
	bool is_identity() { return identity; };
};

// Spacegroup
class spacegroup
{
	private:
	// Name of the spacegroup
	char name[20];
	// List of symmetry operations for the spacegroup
	list<symmop> symmops;
	public:
	// Constructor
	spacegroup();
	// Destructor
	~spacegroup();
	// Returns the name of the spacegroup
	const char *get_name() { return name; };
	// Returns the first symmop in the list
	symmop *get_symmops() { return symmops.first(); };
	friend class spacegroup_list;
};

// Spacegroups
class spacegroup_list
{
	public:
	// Constructor / Destructor
	spacegroup_list();
	~spacegroup_list();

	/*
	// Spacegroup List
	*/
	private:
	// List of crystal spacegroups (0=no spgrp)
	spacegroup groups[NSPACEGROUPS];

	public:
	// Load in spacegroup definitions from file
	void load(const char*);
	// Returns the spacegroup structure with index i
	spacegroup *get_spacegroup(int i) { return &groups[i]; }
	// Returns the name of spacegroup i
	const char *get_name(int i) { return groups[i].name; }
	// Returns the symmetry operators of spacegroup i
	symmop *get_symmops(int i) { return groups[i].symmops.first(); }
	// Searches for the named spacegroup
	int find_by_name(const char*);
	// Returns cell type of specified spacegroup id
	cell_type get_cell_type(int);
};

// Spacegroup definitions
extern spacegroup_list spacegroups;
	

#endif
