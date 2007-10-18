/*
	*** Element definitions
	*** src/base/elements.h

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

#ifndef H_ELEMAP_H
#define H_ELEMAP_H

#include "classes/atom.h"
#include "base/prefs.h"

// Number of elements (120 until Ellipsoid is removed, 119 thereafter)
#define NELEMENTS 120

// Element
struct element
{
	// Mass of element
        double mass;
	// Element name
	char name[20];		
	// Element symbol
	char symbol[10];	
	// Uppercase Element symbol
	char ucsymbol[10];	
	// Rough elemental radius (for bond calculation etc.)
        double radius;
	// Drawing colour
        GLint colour[4];
	// Maximal bond order about the element 
	int valency;

	// Constructor / Destructor
	element();
	~element();
};

// Element map
class element_map
{
	private:
	// Element data array
	element el[NELEMENTS];
	// Convert string from Z to element number
	int number_to_z(const char*);
	// Convert string from alpha to element number
	int alpha_to_z(const char*);
	// Convert string from fftype to element number
	int ff_to_z(const char*);

	public:
	// Constructor / Destructor
	element_map();
	~element_map();
	// Returns atomic number of element in string
	int find(const char*);
	// Returns atomic number of element in string, specifying algorithm
	int find(const char*, zmap_type);
	// Initialise element data
	void initialise();

	/*
	// Data by Z
	*/
	public:
	// Return atomic mass of atomic number 'i'
	double mass(int i) { return el[i].mass; }
	// Return name of atomic number 'i'
	const char *name(int i) { return el[i].name; }
	// Return symbol of atomic number 'i'
	const char *symbol(int i) { return el[i].symbol; }
	// Return effective radius of atomic number 'i'
	double radius(int i) { return el[i].radius; }
	// Return valency of atomic number 'i'
	int valency(int i) { return el[i].valency; }
	// Return the drawing colour of the element
	GLint *colour(int i) { return el[i].colour; }
	// Set colour component of element
	void set_colour(int i, int rgb, int value) { el[i].colour[rgb] = value; }

	/*
	// Data by atom*
	*/
	public:
	// Returns atomic mass of atomic number 'i'
	double mass(atom *i) { return mass(i->get_element()); }
	// Returns name of atomic number 'i'
	const char *name(atom *i) { return name(i->get_element()); }
	// Returns symbol of atomic number 'i'
	const char *symbol(atom *i) { return symbol(i->get_element()); }
	// Returns effective radius of atomic number 'i'
	double radius(atom *i) { return radius(i->get_element()); }
	// Returns valency of atomic number 'i'
	int valency(atom *i) { return valency(i->get_element()); }
	// Returns the drawing colour of the element
	GLint *colour(atom *i) { return colour(i->get_element()); }
};

extern element_map elements;

#endif
