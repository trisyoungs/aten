/*
	*** Primitive Group
	*** src/render/primitivegroup.cpp
	Copyright T. Youngs 2007-2013

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

#include "render/primitivegroup.h"
#include "classes/prefs.h"

// Constructor
PrimitiveGroup::PrimitiveGroup()
{
	primitives_ = NULL;
	nLevelsOfDetail_ = 0;
	name_ = "<UnnamedGroup>";
	clear();
}

PrimitiveGroup::~PrimitiveGroup()
{
	if (primitives_ != NULL) delete[] primitives_;
	nLevelsOfDetail_ = 0;
}

// Clear old primitives array and allocate new one
void PrimitiveGroup::clear()
{
	if (primitives_ != NULL) delete[] primitives_;
	nLevelsOfDetail_ = prefs.levelsOfDetail();
	primitives_ = new Primitive[nLevelsOfDetail_];
	// Set new names of primitives
	Dnchar text;
	for (int n=0; n<nLevelsOfDetail_; ++n)
	{
		text.sprintf("%s, lod=%i", name_.get(), n);
		primitives_[n].setName(text);
	}
}

// Push new instance of all primitives in group
void PrimitiveGroup::pushInstance(const QGLContext *context)
{
	for (int n=0; n<nLevelsOfDetail_; ++n) primitives_[n].pushInstance(context);
}

// Pop topmost instance for all primitives in group
void PrimitiveGroup::popInstance(const QGLContext *context)
{
	for (int n=0; n<nLevelsOfDetail_; ++n) primitives_[n].popInstance(context);
}

// Return primitive corresponding to level of detail specified
Primitive &PrimitiveGroup::primitive(int lod)
{
	// Clamp LOD to allowable range
	int clampedLod = lod;
	if (clampedLod <= 0) clampedLod = 0;
	else if (clampedLod >= nLevelsOfDetail_) clampedLod = nLevelsOfDetail_-1;
	return primitives_[clampedLod];
	
// 	if (lod <= 0) return primitives_[0];
// 	else if (lod >= nLevelsOfDetail_) return primitives_[nLevelsOfDetail_-1];
// 	else return primitives_[lod];
}

// Set name of primitive group
void PrimitiveGroup::setName(const char *s)
{
	name_ = s;
}

// Send to OpenGL (i.e. render) at specified level of detail
void PrimitiveGroup::sendToGL(int lod)
{
	// Clamp lod to allowable range for this PrimitiveGroup
	int clampedLod = lod;
	if (clampedLod <= 0) clampedLod = 0;
	else if (clampedLod >= nLevelsOfDetail_) clampedLod = nLevelsOfDetail_-1;
	primitives_[clampedLod].sendToGL();
}
