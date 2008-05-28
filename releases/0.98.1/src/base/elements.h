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

#include "base/prefs.h"
#include <QtOpenGL/QtOpenGL>

// Forward Declarations
class Atom;

// Element
class Element
{
	public:
	// Mass of element
	double atomicMass;
	// Element name
	const char *name;
	// Uppercase element name
	const char *ucName;
	// Element symbol
	const char *symbol;
	// Uppercase Element symbol
	const char *ucSymbol;
	// Rough elemental radius (for bond calculation etc.)
	double atomicRadius;
	// Maximal bond order about the element 
	int valency;
	// Ambient colour
	GLfloat ambientColour[4];
	// Diffuse colour
	GLfloat diffuseColour[4];
};

// Element map
class ElementMap
{
	private:
	// Element data array
	static Element el_[];
	// Convert string from Z to element number
	int numberToZ(const char*);
	// Convert string from alpha to element number
	int alphaToZ(const char*);
	// Convert string from first alpha (up to non-AZ inc 09) to element number
	int firstAlphaToZ(const char*);
	// Convert string from name to element number
	int nameToZ(const char*);
	// Convert string from fftype to element number
	int ffToZ(const char*);
	// Number of defined elements
	int nElements_;

	public:
	// Constructor / Destructor
	ElementMap();
	~ElementMap();
	// Return atomic number of element in string
	int find(const char*);
	// Return atomic number of element in string, specifying algorithm
	int find(const char*, Prefs::ZmapType);
	// Return number of defined elements
	int nElements();

	/*
	// Data by Z
	*/
	public:
	// Return atomic mass of atomic number 'i'
	double atomicMass(int i);
	// Return name of atomic number 'i'
	const char *name(int i);
	// Return symbol of atomic number 'i'
	const char *symbol(int i);
	// Set radius of atomic number 'i'
	void setAtomicRadius(int i, double r);
	// Return effective radius of atomic number 'i'
	double atomicRadius(int i);
	// Return valency of atomic number 'i'
	int valency(int i);
	// Return the ambient colour of the element
	GLfloat *ambientColour(int i);
	// Copy the ambient colour of the element into the array provided
	void copyAmbientColour(int i, GLfloat *v);
	// Set ambient colour component of element
	void setAmbientColour(int i, int rgb, GLfloat value);
	void setAmbientColour(int i, GLfloat r, GLfloat g, GLfloat b);
	// Return the diffuse colour of the element
	GLfloat *diffuseColour(int i);
	// Copy the diffuse colour of the element into the array provided
	void copyDiffuseColour(int i, GLfloat *v);
	// Set diffuse colour component of element
	void setDiffuseColour(int i, int rgb, GLfloat value);
	void setDiffuseColour(int i, GLfloat r, GLfloat g, GLfloat b);

	/*
	// Data by atom*
	*/
	public:
	// Return atomic mass of atomic number 'i'
	double atomicMass(Atom *i);
	// Return name of atomic number 'i'
	const char *name(Atom *i);
	// Return symbol of atomic number 'i'
	const char *symbol(Atom *i);
	// Return effective radius of atomic number 'i'
	double atomicRadius(Atom *i);
	// Return valency of atomic number 'i'
	int valency(Atom *i);
	// Return the ambient colour of the element
	GLfloat *ambientColour(Atom *i);
	// Return the diffuse colour of the element
	GLfloat *diffuseColour(Atom *i);
};

extern ElementMap elements;

#endif
