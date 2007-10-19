/*
	*** Atom location
	*** src/classes/atomaddress.h
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

#ifndef H_ATOMADDRESS_H
#define H_ATOMADDRESS_H

// Forward declarations
class pattern;

// Atom Address
class atomaddress
{
	private:
	// Molecule in which the atom exists
	int molecule;
	// Local (molecule) atom offset
	int offset;
	// Pattern node in which the atom lies (must be set by model before site is used)
	pattern *sourcepattern;

	public:
	// Constructor / Destructor
	atomaddress();
	~atomaddress();
	// List pointers
	atomaddress *prev, *next;	

	/*
	// Variable Access
	*/
	public:
	// Set the local molecule offset of the atom
	void set_offset(int i) { offset = i; }
	// Returns the local molecule offset of the atom
	int get_offset() { return offset; }
	// Set the molecule id of the atom
	void set_molecule(int i) { molecule = i; }
	// Returns the molecule the atom is in
	int get_molecule() { return molecule; }
	// Set the pattern pointer for the atom
	void set_pattern(pattern *p) { sourcepattern = p; }
	// Returns the current pattern for the atom
	pattern *get_pattern() { return sourcepattern; }
};

#endif
