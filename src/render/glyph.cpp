/*
	*** Glyph rendering
	*** src/render/glyph.cpp
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

#include "model/model.h"
#include "base/elements.h"		// TEMPORARY
#include "gui/canvas.h"

// Render model glyphs
void canvas::render_model_glyphs()
{
	dbg_begin(DM_CALLS,"canvas::render_model_glyphs");
	static vec3<double> vec[MAXGLYPHDATA], avg, normal;
	GLfloat col[4] = { 0.0f, 0.0f, 0.9f, 0.5f };

	// Render other elemental objects in the model
	for (glyph *g = displaymodel->get_glyphs(); g != NULL; g = g->next)
	{
		// Set relevant polygon mode
		glPolygonMode(GL_FRONT_AND_BACK, (g->is_solid() ? GL_FILL : GL_LINE));
		glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, col);
		switch (g->get_type())
		{
			// Arrow - tail = data[0], head = data[1]
			case (GS_ARROW):
				vec[0] = g->data[0].get_vector();
				gl_arrow(vec[0], g->data[1].get_vector() - vec[0] );
				break;
			case (GS_VECTOR):
				break;
			// Sphere - centre = data[0], scale = data[1]
			case (GS_SPHERE):
				vec[0] = g->data[0].get_vector();
				vec[1] = g->data[1].get_vector();
				glPushMatrix();
				  glTranslated(vec[0].x, vec[0].y, vec[0].z);
				  glScaled(vec[1].x, vec[1].y, vec[1].z);
				  glCallList(g->is_solid() ? GLOB_UNITATOM : GLOB_WIREUNITATOM);
				glPopMatrix();
				break;
			// Cube - centre = data[0], scale = data[1]
			case (GS_CUBE):
				vec[0] = g->data[0].get_vector();
				vec[1] = g->data[1].get_vector();
				glPushMatrix();
				  glTranslated(vec[0].x, vec[0].y, vec[0].z);
				  glScaled(vec[1].x, vec[1].y, vec[1].z);
				  glCallList(g->is_solid() ? GLOB_UNITCUBE : GLOB_WIREUNITCUBE);
				glPopMatrix();
				break;
			// Ellipsoid - vertex 1 = data[0], vertex 2 = data[1], vertex 3 = data[2]
			case (GS_TRIANGLE):
				vec[0] = g->data[0].get_vector();
				vec[1] = g->data[1].get_vector();
				vec[2] = g->data[2].get_vector();
				glBegin(GL_TRIANGLES);
				  glVertex3d(vec[0].x, vec[0].y, vec[0].z);
				  glVertex3d(vec[1].x, vec[1].y, vec[1].z);
				  glVertex3d(vec[2].x, vec[2].y, vec[2].z);
				glPopMatrix();
				break;
			// Ellipsoid - centre = data[0], edge vector = data[1], face vector = data[2]
			case (GS_ELLIPSOID):
				gl_ellipsoid(g->data[0].get_vector(), g->data[1].get_vector(), g->data[2].get_vector());
				break;
			// Tetrahedron - four vertices in data[0] to data[3]
			case (GS_TETRAHEDRON):
				vec[0] = g->data[0].get_vector();
				vec[1] = g->data[1].get_vector();
				vec[2] = g->data[2].get_vector();
				vec[3] = g->data[3].get_vector();
				avg = (vec[0] + vec[1] + vec[2] + vec[3]) / 4.0;
				glBegin(GL_TRIANGLE_STRIP);
				  normal = avg - vec[0];
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[0].x, vec[0].y, vec[0].z);
				  normal = avg - vec[1];
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[1].x, vec[1].y, vec[1].z);
				  normal = avg - vec[2];
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[2].x, vec[2].y, vec[2].z);
				  normal = avg - vec[3];
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[3].x, vec[3].y, vec[3].z);
				  normal = avg - vec[0];
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[0].x, vec[0].y, vec[0].z);
				  normal = avg - vec[1];
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[1].x, vec[1].y, vec[1].z);
				glEnd();
				break;
		}

			//case (119): 	// Ellipsoid - coords = coords, velocities = lookat, forces = scaling
			//	gl_ellipsoid(i->r(),i->v(),i->f());
			//	break;
	}
	dbg_end(DM_CALLS,"canvas::render_model_glyphs");
}
