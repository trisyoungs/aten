/*
	*** Element definitions
	*** src/base/elements.h
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

#ifndef ATEN_ELEMENTMAP_H
#define ATEN_ELEMENTMAP_H

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
	// Group position in periodic table
	int group;
	// Rough elemental radius (for bond calculation etc.)
	double atomicRadius;
	// Ambient colour
	GLfloat ambientColour[4];
	// Diffuse colour
	GLfloat diffuseColour[4];
	// Numeric measure of 'penalties' for total bond orders 0 - 8
	int bondOrderPenalty[9];
	// Formal charges for bond orders 0 - 8
	int formalCharges[9];
};

// Element map
class ElementMap
{
	public:
	// Constructor
	ElementMap();
	// Name->Z mapping methods
	enum ZMapType { AlphaZMap, FirstAlphaZMap, SingleAlphaZMap, NameZMap, NumericZMap, ForcefieldZMap, AutoZMap, nZMapTypes };
	static ElementMap::ZMapType zMapType(const char *s);

	private:
	// Element data array
	static Element el_[];
	// Convert string from Z to element number
	int numberToZ(const char*);
	// Convert string from alpha to element number
	int alphaToZ(const char*);
	// Convert string from alpha (up to non-AZ inc 09) to element number
	int firstAlphaToZ(const char*);
	// Convert string from first alpha (up to non-AZ inc 09) to element number
	int singleAlphaToZ(const char*);
	// Convert string from name to element number
	int nameToZ(const char*);
	// Convert string from fftype to element number
	int ffToZ(const char*);
	// Number of defined elements
	int nElements_;

	public:
	// Return atomic number of element in string
	int find(const char*);
	// Return atomic number of element in string, specifying algorithm
	int find(const char*, ElementMap::ZMapType);
	// Return atomic number of element provided using standard Alpha mapping
	int findAlpha(const char *);
	// Return number of defined elements
	int nElements();

	/*
	// Data by Z
	*/
	public:
	// Return periodic table group number
	int group(int i);
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
	// Return bond order penalty for TBO 'bo' of atomic number 'i'
	int bondOrderPenalty(int i, int bo);
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
	// Return periodic table group number
	int group(Atom *i);
	// Return atomic mass of atomic number 'i'
	double atomicMass(Atom *i);
	// Return name of atomic number 'i'
	const char *name(Atom *i);
	// Return symbol of atomic number 'i'
	const char *symbol(Atom *i);
	// Return effective radius of atomic number 'i'
	double atomicRadius(Atom *i);
	// Return bond order penalty for TBO 'bo' of atomic number 'i'
	int bondOrderPenalty(Atom *i, int bo);
	// Return the ambient colour of the element
	GLfloat *ambientColour(Atom *i);
	// Return the diffuse colour of the element
	GLfloat *diffuseColour(Atom *i);
};

extern ElementMap &elements();
extern ElementMap *elementsAsPointer();

#endif
