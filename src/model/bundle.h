/*
	*** Pointer Bundle
	*** src/model/bundle.h
	Copyright T. Youngs 2007-2017

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

#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Atom;
class Model;
class Site;
class Forcefield;
class Grid;
class Pattern;
class Glyph;

// Pointer Bundle
// Convenience structure to hold/pass a Bundle of current object pointers
class Bundle
{
	public:
	// Constructors
	Bundle();
	Bundle(Forcefield* ptr);
	Bundle(Model* ptr);
	Bundle(Pattern* ptr);
	Bundle(Atom* ptr);
	Bundle(Grid* ptr);
	Bundle(Glyph* ptr);
	// Bundle Pointer Types
	enum BundlePointer { AtomPointer=1, PatternPointer=2, ModelPointer=4, ForcefieldPointer=8, GridPointer=16, SitePointer=32, GlyphPointer=64 };

	public:
	// Model pointer
	Model* m;
	// Pattern pointer
	Pattern* p;
	// Atom pointer
	Atom* i;
	//Forcefield pointer
	Forcefield* ff;
	// Grid pointer
	Grid* g;
	// Site pointer
	Site* s;
	// Glyph
	Glyph* gl;
	// Return render source pointer for stored model
	Model* rs();

	/*
	 * Member Functions
	 */
	public:
	// Clear pointers
	void clear();
	// Check for null pointers
	bool isNull(int ptrs) const;
	// Check and notify of null pointers
	bool notifyNull(int ptrs) const;
};

ATEN_END_NAMESPACE

#endif
