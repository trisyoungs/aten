/*
	*** Element definitions
	*** src/base/elements.h
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

#ifndef H_ELEMAP_H
#define H_ELEMAP_H

#include "classes/atom.h"
#include "base/prefs.h"

// Number of elements (120 until Ellipsoid is removed, 119 thereafter)
#define NELEMENTS 120

// Element
struct element
{
	// Constructor / Destructor
	element();
	~element();

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
	// Ambient colour
        GLint ambient[4];
	// Diffuse colour
	GLint diffuse[4];
	// Maximal bond order about the element 
	int valency;
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
	// Return atomic number of element in string
	int find(const char*);
	// Return atomic number of element in string, specifying algorithm
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
	// Return the ambient colour of the element
	GLint *ambient(int i) { return el[i].ambient; }
	// Copy the ambient colour of the element in the array provided
	void ambient(int i, GLint *v);
	// Set ambient colour component of element
	void set_ambient(int i, int rgb, int value) { el[i].ambient[rgb] = value; }
	// Return the diffuse colour of the element
	GLint *diffuse(int i) { return el[i].diffuse; }
	// Copy the diffuse colour of the element in the array provided
	void diffuse(int i, GLint *v);
	// Set ambient colour component of element
	void set_diffuse(int i, int rgb, int value) { el[i].diffuse[rgb] = value; }

	/*
	// Data by atom*
	*/
	public:
	// Return atomic mass of atomic number 'i'
	double mass(atom *i) { return mass(i->get_element()); }
	// Return name of atomic number 'i'
	const char *name(atom *i) { return name(i->get_element()); }
	// Return symbol of atomic number 'i'
	const char *symbol(atom *i) { return symbol(i->get_element()); }
	// Return effective radius of atomic number 'i'
	double radius(atom *i) { return radius(i->get_element()); }
	// Return valency of atomic number 'i'
	int valency(atom *i) { return valency(i->get_element()); }
	// Return the ambient colour of the element
	GLint *ambient(atom *i) { return ambient(i->get_element()); }
	// Return the diffuse colour of the element
	GLint *diffuse(atom *i) { return diffuse(i->get_element()); }
};

extern element_map elements;

#endif
