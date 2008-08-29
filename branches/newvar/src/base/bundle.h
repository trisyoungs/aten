/*
	*** Pointer Bundle
	*** src/classes/Bundle.h
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

#ifndef ATEN_BUNDLE_H
#define ATEN_BUNDLE_H

// Forward declarations
class Atom;
class Model;
class Site;
class Forcefield;
class Grid;
class Pattern;
class Glyph;

// Bundle Pointers
enum BundlePointer { BP_ATOM=1, BP_PATTERN=2, BP_MODEL=4, BP_FF=8, BP_GRID=16, BP_SITE=32, BP_GLYPH=64 };

// Pointer Bundle
// Convenience structure to hold/pass a Bundle of current object pointers
class Bundle
{
	public:
	// Constructor
	Bundle();

	public:
	// Model pointer
	Model *m;
	// Render source pointer
	Model *rs;
	// Pattern pointer
	Pattern *p;
	// Atom pointer
	Atom *i;
	//Forcefield pointer
	Forcefield *ff;
	// Grid pointer
	Grid *g;
	// Site pointer
	Site *s;
	// Glyph
	Glyph *gl;

	/*
	// Member Functions
	*/
	public:
	// Check for null pointers
	bool isNull(int);
	// Check and notify of null pointers
	bool notifyNull(int);
};

#endif
