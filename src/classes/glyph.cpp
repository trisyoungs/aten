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
const char *GS_keywords[GS_NITEMS] = { "arrow", "vector", "sphere", "cube", "triangle", "ellipsoid", "tetrahedron" };
const char *text_from_GS(glyph_style gs)
	{ return GS_keywords[gs]; }
glyph_style GS_from_text(const char *s)
	{ return (glyph_style) enum_search("glyph style",GS_NITEMS,GS_keywords,s); }

// Constructors
glyphdata::glyphdata()
{
	i = NULL;
	idata = AV_R;
	atomsetlast = FALSE;
	set = FALSE;
	#ifdef MEMDEBUG
		memdbg.create[MD_GLYPHDATA] ++;
	#endif
}

glyph::glyph()
{
	prev = NULL;
	next = NULL;
	solid = TRUE;
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

// Set the vector data
void glyphdata::set_vector(double x, double y, double z)
{
	vec.set(x,y,z);
	atomsetlast = FALSE;
	set = TRUE;
}

// Set the atom pointer
void glyphdata::set_atom(atom *target, atom_vector av)
{
	i = target;
	idata = av;
	atomsetlast = TRUE;
	set = TRUE;
}

// Return the vector data
vec3<double> glyphdata::get_vector()
{
	if (atomsetlast)
	{
		if (i == NULL)
		{
			printf("Atom was apparently set last in glyph, but pointer is NULL.\n");
			return vec;
		}
		switch (idata)
		{
			case (AV_R): return i->r();
			case (AV_F): return i->f();
			case (AV_V): return i->v();
		}
	}
	else return vec;
}

// Set style of glyph (and set data vectors to default values)
void glyph::set_type(glyph_style gt)
{
	// Add default values, provided they have not already been set...
	switch (gt)
	{
		case (GS_ARROW):
		case (GS_VECTOR):
			if (!data[1].is_set()) data[1].set_vector(0.0,1.0,0.0);
			break;
		case (GS_SPHERE):
		case (GS_CUBE):
			if (!data[1].is_set()) data[1].set_vector(1.0,1.0,1.0);
			break;
		case (GS_TRIANGLE):
			break;
		case (GS_ELLIPSOID):
			break;
	}
	type = gt;
}
