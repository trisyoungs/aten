/*
	*** Rendering glyph
	*** src/classes/glyph.cpp
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

#include "classes/glyph.h"
#include "base/debug.h"
#include "base/sysfunc.h"

// Glyph styles
const char *GS_keywords[GS_NITEMS] = { "arrow", "vector", "sphere", "cube", "triangle", "ellipsoid" };
const char *text_from_GS(glyph_style gs)
	{ return GS_keywords[gs]; }
glyph_style GS_from_text(const char *s)
	{ return (glyph_style) int(pow(2,enum_search("glyph style",GS_NITEMS,GS_keywords,s))); }

// Constructors
glyphdata::glyphdata()
{
	i = NULL;
	atomsetlast = FALSE;
	#ifdef MEMDEBUG
		memdbg.create[MD_GLYPHDATA] ++;
	#endif
}

glyph::glyph()
{
	prev = NULL;
	next = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_GLYPH] ++;
	#endif
}

// Destructor
glyph::~glyph()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_GLYPH] ++;
	#endif
}
