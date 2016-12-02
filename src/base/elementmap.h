/*
	*** Element Map
	*** src/base/elementmap.h
	Copyright T. Youngs 2007-2016

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
#include <QIcon>

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Atom;
class Aten;
class ForcefieldAtom;

// Element map
class ElementMap
{
	public:
	// Name->Z mapping methods
	enum ZMapType { AlphaZMap, FirstAlphaZMap, SingleAlphaZMap, NameZMap, NumericZMap, ForcefieldZMap, AutoZMap, nZMapTypes };
	static ElementMap::ZMapType zMapType(QString s, bool reportError = false);
	static const char* zMapType(ElementMap::ZMapType zm);


	/*
	 * Pointer to Aten
	 */
	private:
	// Pointer to main Aten object
	static Aten* aten_;


	/*
	 * Element Data
	 */
	private:
	// Default element data
	static const Element defaultElements_[];
	// Current element data array
	static Element* elements_;
	// Number of defined elements
	static int nElements_;
	// Storage for copy of element data
	static Element* backupElements_;
	// Element map name conversions to apply
	static NameMapList<int> mappings_;

	public:
	// Initialise data before first use
	static void initialise(Aten* aten);
	// Delete data after final use
	static void finalise();
	// Backup current data
	static void backupData();
	// Restore default element values
	static void restoreData();
	// Return number of defined elements
	static int nElements();
	// Return pointer to specified element
	static Element* element(int z);
	// Clear all name mappings
	static void clearMappings();
	// Add name mapping
	static void addMapping(int element, QString names);
	// Return Z of specified element symbol
	static int z(QString symbol);
	// Return Z of element with corresponding mass (within tolerance)
	static int z(double targetMass, double tolerance = 0.01);
	// Convert string from Z to element number
	static int numberToZ(QString number);
	// Convert string from alpha to element number
	static int alphaToZ(QString alpha);
	// Convert string from alpha (up to non-AZ inc 09) to element number
	static int firstAlphaToZ(QString alpha);
	// Convert string from first alpha (up to non-AZ inc 09) to element number
	static int singleAlphaToZ(QString alpha);
	// Convert string from name to element number
	static int nameToZ(QString name);
	// Convert string from fftype to element number
	static int ffToZ(QString s);
	// Return atomic number of element in string using supplied method (if specified)
	static int find(QString query, ElementMap::ZMapType zmt = ElementMap::AutoZMap);
	// Return first forcefield atom type matching supplied name
	static ForcefieldAtom* forcefieldAtom(QString name);


	/*
	 * Data by Z
	 */
	public:
	// Return periodic table group number
	static int group(int z);
	// Return atomic mass of atomic number 'z'
	static double atomicMass(int z);
	// Return name of atomic number 'z'
	static const char* name(int z);
	// Return symbol of atomic number 'z'
	static const char* symbol(int z);
	// Set radius of atomic number 'z'
	static void setAtomicRadius(int z, double r);
	// Return effective radius of atomic number 'z'
	static double atomicRadius(int z);
	// Return whether radius has changed for atomic number 'z'
	static bool radiusHasChanged(int z);
	// Return bond order penalty for TBO 'bo' of atomic number 'z'
	static int bondOrderPenalty(int z, int bo);
	// Return the colour of the element
	static double* colour(int z);
	// Set colour component of element
	static void setColour(int z, int rgba, double value);
	// Set colour of element
	static void setColour(int z, double r, double g, double b, double a);
	// Copy the colour of the element into the GLfloat array provided
	static void copyColour(int z, GLfloat* v);
	// Copy the colour of the element into the double array provided
	static void copyColour(int z, double* v);
	// Copy the colour of the element into the Vec4 provided
	static void copyColour(int z, Vec4<GLfloat>& v);
	// Return whether colour of specified element has changed from the default
	static bool colourHasChanged(int z);
	// Return QIcon for the given element
	static QIcon icon(int z);


	/*
	 * Data by Atom*
	 */
	public:
	// Return periodic table group number
	static int group(Atom* i);
	// Return atomic mass of atomic number 'i'
	static double atomicMass(Atom* i);
	// Return name of atomic number 'i'
	static const char* name(Atom* i);
	// Return symbol of atomic number 'i'
	static const char* symbol(Atom* i);
	// Return effective radius of atomic number 'i'
	static double atomicRadius(Atom* i);
	// Return bond order penalty for TBO 'bo' of atomic number 'i'
	static int bondOrderPenalty(Atom* i, int bo);
	// Return the colour of the element
	static double* colour(Atom* i);
};

ATEN_END_NAMESPACE

#endif
