/*
	*** Element definitions
	*** src/base/elements.h
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

#ifndef ATEN_ELEMAP_H
#define ATEN_ELEMAP_H

#include "classes/atom.h"
#include "base/prefs.h"
#include "gui/canvas.h"

// Number of elements (120 until Ellipsoid is removed, 119 thereafter)
#define NELEMENTS 120

// Element
struct element
{
	// Mass of element
        double mass;
	// Element name
	const char *name;
	// Uppercase element name
	const char *ucname;
	// Element symbol
	const char *symbol;
	// Uppercase Element symbol
	const char *ucsymbol;
	// Rough elemental radius (for bond calculation etc.)
        double radius;
	// Maximal bond order about the element 
	int valency;
	// Ambient colour
        GLfloat ambient[4];
	// Diffuse colour
	GLfloat diffuse[4];
};

// Element map
class element_map
{
	private:
	// Element data array
	static element el[];
	// Convert string from Z to element number
	int number_to_z(const char*);
	// Convert string from alpha to element number
	int alpha_to_z(const char*);
	// Convert string from first alpha (up to non-AZ inc 09) to element number
	int alphafirst_to_z(const char*);
	// Convert string from name to element number
	int name_to_z(const char*);
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
	// Set radius of atomic number 'i'
	void set_radius(int i, double r) { el[i].radius = r; }
	// Return effective radius of atomic number 'i'
	double radius(int i) { return el[i].radius; }
	// Return valency of atomic number 'i'
	int valency(int i) { return el[i].valency; }
	// Return the ambient colour of the element
	GLfloat *ambient(int i) { return el[i].ambient; }
	// Copy the ambient colour of the element into the array provided
	void ambient(int i, GLfloat *v);
	// Set ambient colour component of element
	void set_ambient(int i, int rgb, GLfloat value) { el[i].ambient[rgb] = value; }
	void set_ambient(int i, GLfloat r, GLfloat g, GLfloat b) { el[i].ambient[0] = r; el[i].ambient[1] = g; el[i].ambient[2] = b; }
	// Return the diffuse colour of the element
	GLfloat *diffuse(int i) { return el[i].diffuse; }
	// Copy the diffuse colour of the element into the array provided
	void diffuse(int i, GLfloat *v);
	// Set diffuse colour component of element
	void set_diffuse(int i, int rgb, GLfloat value) { el[i].diffuse[rgb] = value; }
	void set_diffuse(int i, GLfloat r, GLfloat g, GLfloat b) { el[i].diffuse[0] = r; el[i].diffuse[1] = g; el[i].diffuse[2] = b; }

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
	GLfloat *ambient(atom *i) { return ambient(i->get_element()); }
	// Return the diffuse colour of the element
	GLfloat *diffuse(atom *i) { return diffuse(i->get_element()); }
};

extern element_map elements;

#endif
