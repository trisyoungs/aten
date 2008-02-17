/*
	*** Glyph rendering
	*** src/render/glyph.cpp
	Copyright T. Youngs 2007

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

#include "model/model.h"
#include "base/elements.h"		// TEMPORARY
#include "gui/canvas.h"
#ifdef IS_MAC
	#include <GLUT/glut.h>
#else
	#include <GL/glut.h>
#endif

// Render model glyphs
void canvas::render_model_glyphs()
{
	dbg_begin(DM_CALLS,"canvas::render_model_glyphs");
	// Render other elemental objects in the model
	int el;
	for (atom *i = displaymodel->get_atoms(); i != NULL; i = i->next)
	{
		el = i->get_element();
		if (el <= 118) continue;
		glMaterialiv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, elements.ambient(i->get_element()));
		switch (el)
		{
			case (119): 	// Ellipsoid - coords = coords, velocities = lookat, forces = scaling
				gl_ellipsoid(i->r(),i->v(),i->f());
				break;
		}
	}
	dbg_end(DM_CALLS,"canvas::render_model_glyphs");
}
