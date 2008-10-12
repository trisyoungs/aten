/*
	*** Molecular restraint
	*** src/classes/restraint.h
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

#ifndef ATEN_RESTRAINT_H
#define ATEN_RESTRAINT_H

#include "base/atom.h"
#include "templates/list.h"

// Distance restraint
class restraint_ij
{
	public:
	// Atoms in distance restraint
	Atom *i,*j;
	// Distance
	double rij;
	// List pointers
	restraint_ij *next, *prev;
	// Constructor
	restraint_ij();
	// Destructor
	~restraint_ij();
};

// Geometry Constraint (used in model class)
class restraints
{
	public:
	// Constructor / Destructor
	restraints();
	~restraints();

	private:
	// Parent model
	Model *ownermodel;
	// List of distance restraints
	List<restraint_ij> ijs;
	// List of angle restraints
	//List<restraint_ijk> ijks;
	// List of torsion restraints
	//List<restraint_ijkl> ijkls;

	public:
	// Add distance restraint
	void add_ij(Reflist<Atom,int>&);
	// Add angle restraint
	//void add_ijk(Reflist<atom>&);
	// Add torsion restraint
	//void add_ijkl(Reflist<atom>&);
	// Search for distance restraint in list
	restraint_ij *does_ij_exist(Reflist<Atom,int>&);
	//restraint_ijk *does_ijk_exist(Reflist<atom>&);// Search for angle restraint in list
	//restraint_ijkl *does_ijkl_exist(Reflist<atom>&);// Search for torsion restraint in list
	// Clear all restraints
	void clear_all();
	// Delete all restraints involving specific atom
	void prune_atom(Atom*);
};

#endif
