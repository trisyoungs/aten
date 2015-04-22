/*
	*** Element Map
	*** src/base/elementmap.h
	Copyright T. Youngs 2007-2015

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

#ifdef _MAC
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#include "base/element.h"
#include "templates/vector4.h"
#include "templates/list.h"
#include "templates/namemap.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Atom;
class Forcefield;

// Element map
class ElementMap
{
	public:
	// Constructor / Destructor
	ElementMap();
	~ElementMap();
	// Name->Z mapping methods
	enum ZMapType { AlphaZMap, FirstAlphaZMap, SingleAlphaZMap, NameZMap, NumericZMap, ForcefieldZMap, AutoZMap, nZMapTypes };
	static ElementMap::ZMapType zMapType(QString s, bool reportError = false);
	static const char* zMapType(ElementMap::ZMapType zm);

	/*
	 * Element Data
	 */
	private:
	// Default element data
	static const Element defaultElements_[];
	// Current element data array
	Element* elements_;
	// Number of defined elements
	int nElements_;
	// Storage for copy of element data
	Element* backupElements_;
	// Element map name conversions to apply
	NameMapList<int> mappings_;

	public:
	// Backup current data
	void backupData();
	// Restore default element values
	void restoreData();
	// Return number of defined elements
	int nElements() const;
	// Return pointer to specified element
	Element* element(int z);
	// Clear all name mappings
	void clearMappings();
	// Add name mapping
	void addMapping(int element, QString names);
	// Return Z of specified element symbol
	int z(QString symbol) const;
	// Convert string from Z to element number
	int numberToZ(QString number) const;
	// Convert string from alpha to element number
	int alphaToZ(QString alpha) const;
	// Convert string from alpha (up to non-AZ inc 09) to element number
	int firstAlphaToZ(QString alpha) const;
	// Convert string from first alpha (up to non-AZ inc 09) to element number
	int singleAlphaToZ(QString alpha) const;
	// Convert string from name to element number
	int nameToZ(QString name) const;
	// Convert string from fftype to element number
	int ffToZ(QString s, Forcefield* firstFF) const;
	// Return atomic number of element in string using supplied method (if specified)
	int find(QString query, ElementMap::ZMapType zmt = ElementMap::AutoZMap, Forcefield* firstFF = NULL) const;


	/*
	 * Data by Z
	 */
	public:
	// Return periodic table group number
	int group(int z) const;
	// Return atomic mass of atomic number 'z'
	double atomicMass(int z) const;
	// Return name of atomic number 'z'
	const char* name(int z) const;
	// Return symbol of atomic number 'z'
	const char* symbol(int z) const;
	// Set radius of atomic number 'z'
	void setAtomicRadius(int z, double r);
	// Return effective radius of atomic number 'z'
	double atomicRadius(int z) const;
	// Return whether radius has changed for ztomic number 'z'
	bool radiusHasChanged(int z) const;
	// Return bond order penalty for TBO 'bo' of atomic number 'z'
	int bondOrderPenalty(int z, int bo) const;
	// Return the colour of the element
	double* colour(int z);
	// Set colour component of element
	void setColour(int z, int rgba, double value);
	// Set colour of element
	void setColour(int z, double r, double g, double b, double a);
	// Copy the colour of the element into the GLfloat array provided
	void copyColour(int z, GLfloat* v) const;
	// Copy the colour of the element into the double array provided
	void copyColour(int z, double* v) const;
	// Copy the colour of the element into the Vec4 provided
	void copyColour(int z, Vec4<GLfloat>& v) const;
	// Return whether colour of specified element has changed from the default
	bool colourHasChanged(int z) const;


	/*
	 * Data by Atom*
	 */
	public:
	// Return periodic table group number
	int group(Atom* i);
	// Return atomic mass of atomic number 'i'
	double atomicMass(Atom* i);
	// Return name of atomic number 'i'
	const char* name(Atom* i);
	// Return symbol of atomic number 'i'
	const char* symbol(Atom* i);
	// Return effective radius of atomic number 'i'
	double atomicRadius(Atom* i);
	// Return bond order penalty for TBO 'bo' of atomic number 'i'
	int bondOrderPenalty(Atom* i, int bo);
	// Return the colour of the element
	double* colour(Atom* i);
};

extern ElementMap &Elements();

ATEN_END_NAMESPACE

#endif
