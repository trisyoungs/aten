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
#include "gui/canvas.h"
#include "base/glyph.h"

// Render model glyphs
void Canvas::renderModelGlyphs()
{
	msg.enter("Canvas::renderModelGlyphs");
	static Vec3<double> vec[4], avg, normal;

	glEnable(GL_LIGHTING);
	// Render other elemental objects in the model
	for (Glyph *g = displayModel_->glyphs(); g != NULL; g = g->next)
	{
		// Set relevant polygon mode
		glPolygonMode(GL_FRONT_AND_BACK, (g->isSolid() ? GL_FILL : GL_LINE));
		switch (g->type())
		{
			// Arrow - tail = data[0], head = data[1]
			case (Glyph::ArrowGlyph):
				vec[0] = g->vector(0);
				glLineWidth(g->lineWidth());
				glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				glArrow(vec[0], g->vector(1) - vec[0] );
				break;
			// Vector - centroid = data[0], direction = data[1]
			case (Glyph::VectorGlyph):
				vec[0] = g->vector(0);
				vec[1] = g->vector(1);
				glLineWidth(g->lineWidth());
				glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				glArrow( vec[0] - (vec[1] * 0.5), vec[1] );
				break;
			// Sense Vector - end1 = data[0], direction = data[1], length = data[2].x
			case (Glyph::SenseVectorGlyph):
				vec[0] = g->vector(0);
				vec[2] = g->vector(2);
				if (g->atom(1) != NULL)
				{
					vec[1] = g->vector(1) - vec[0];
					vec[1].normalise();
				}
				else vec[1] = g->vector(1);
				vec[1] *= vec[2].x;
				glLineWidth(g->lineWidth());
				glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				glArrow( vec[0], vec[1], g->vector(2).y < 0.0 ? TRUE : FALSE);
				break;
			// Sphere - centre = data[0], scale = data[1]
			case (Glyph::SphereGlyph):
				vec[0] = g->vector(0);
				vec[1] = g->vector(1);
				glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				glPushMatrix();
				  glTranslated(vec[0].x, vec[0].y, vec[0].z);
				  glScaled(vec[1].x, vec[1].y, vec[1].z);
				  glCallList(g->isSolid() ? GLOB_UNITATOM : GLOB_WIREUNITATOM);
				glPopMatrix();
				break;
			// Cube - centre = data[0], scale = data[1]
			case (Glyph::CubeGlyph):
				vec[0] = g->vector(0);
				vec[1] = g->vector(1);
				glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				glPushMatrix();
				  glTranslated(vec[0].x, vec[0].y, vec[0].z);
				  glScaled(vec[1].x, vec[1].y, vec[1].z);
				  glCallList(g->isSolid() ? GLOB_UNITCUBE : GLOB_WIREUNITCUBE);
				glPopMatrix();
				break;
			// Line - vertex 1 = data[0], vertex 2 = data[1]
			case (Glyph::LineGlyph):
				vec[0] = g->vector(0);
				vec[1] = g->vector(1);
				glLineWidth(g->lineWidth());
				glBegin(GL_LINES);
				  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				  glVertex3d(vec[0].x, vec[0].y, vec[0].z);
				  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(1));
				  glVertex3d(vec[1].x, vec[1].y, vec[1].z);
				glEnd();
				break;
			// Triangle - vertex 1 = data[0], vertex 2 = data[1], vertex 3 = data[2]
			case (Glyph::TriangleGlyph):
				vec[0] = g->vector(0);
				vec[1] = g->vector(1);
				vec[2] = g->vector(2);
				glLineWidth(g->lineWidth());
				glBegin(GL_TRIANGLES);
				  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				  glVertex3d(vec[0].x, vec[0].y, vec[0].z);
				  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(1));
				  glVertex3d(vec[1].x, vec[1].y, vec[1].z);
				  glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(2));
				  glVertex3d(vec[2].x, vec[2].y, vec[2].z);
				glEnd();
				break;
			// Quad - vertex 1 = data[0], vertex 2 = data[1], vertex 3 = data[2], vertex 4 = data[3]
			case (Glyph::QuadGlyph):
				vec[0] = g->vector(0);
				vec[1] = g->vector(1);
				vec[2] = g->vector(2);
				vec[3] = g->vector(3);
				glLineWidth(g->lineWidth());
				glBegin(GL_QUADS);
				  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				  glVertex3d(vec[0].x, vec[0].y, vec[0].z);
				  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, g->colour(1));
				  glVertex3d(vec[1].x, vec[1].y, vec[1].z);
				  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, g->colour(2));
				  glVertex3d(vec[2].x, vec[2].y, vec[2].z);
				  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, g->colour(3));
				  glVertex3d(vec[3].x, vec[3].y, vec[3].z);
				glEnd();
				break;
			// Ellipsoid - centre = data[0], edge vector = data[1], face vector = data[2]
			case (Glyph::EllipsoidGlyph):
				glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				glEllipsoid(g->vector(0), g->vector(1), g->vector(2));
				break;
			// Ellipsoid - centre = data[0], X = data[1], Y = data[2], Z= data[3]
			case (Glyph::EllipsoidXYZGlyph):
				glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				glEllipsoid(g->vector(0), g->vector(1), g->vector(2), g->vector(3));
				break;
			// Tetrahedron - four vertices in data[0] to data[3]
			case (Glyph::TetrahedronGlyph):
				vec[0] = g->vector(0);
				vec[1] = g->vector(1);
				vec[2] = g->vector(2);
				vec[3] = g->vector(3);
				avg = (vec[0] + vec[1] + vec[2] + vec[3]) / 4.0;
				glBegin(GL_TRIANGLE_STRIP);
				  normal = avg - vec[0];
				  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[0].x, vec[0].y, vec[0].z);
				  normal = avg - vec[1];
				  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, g->colour(1));
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[1].x, vec[1].y, vec[1].z);
				  normal = avg - vec[2];
				  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, g->colour(2));
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[2].x, vec[2].y, vec[2].z);
				  normal = avg - vec[3];
				  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, g->colour(3));
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[3].x, vec[3].y, vec[3].z);
				  normal = avg - vec[0];
				  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, g->colour(0));
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[0].x, vec[0].y, vec[0].z);
				  normal = avg - vec[1];
				  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, g->colour(1));
				  glNormal3d(normal.x, normal.y, normal.z);
				  glVertex3d(vec[1].x, vec[1].y, vec[1].z);
				glEnd();
				break;
		}

			//case (119): 	// Ellipsoid - coords = coords, velocities = lookat, forces = scaling
			//	gl_ellipsoid(i->r(),i->v(),i->f());
			//	break;
	}
	msg.exit("Canvas::renderModelGlyphs");
}

// Render model text glyphs
void Canvas::renderModelTextGlyphs()
{
	msg.enter("Canvas::renderModelTextGlyphs");
	static Vec3<double> vec[2], avg, normal;
	GLfloat col[4] = { 0.0f, 0.0f, 0.9f, 0.5f };
	TextObject *to;

	// Render other elemental objects in the model
	for (Glyph *g = displayModel_->glyphs(); g != NULL; g = g->next)
	{
		// Set relevant polygon mode
		glPolygonMode(GL_FRONT_AND_BACK, (g->isSolid() ? GL_FILL : GL_LINE));
		glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, col);
		switch (g->type())
		{
			// Text in 2D coordinates - left-hand origin = data[0]
			case (Glyph::TextGlyph):
				vec[0] = g->vector(0);
				// Add text object to list
				to = new TextObject((int)vec[0].x, int(height_ - vec[0].y), FALSE, g->text());
				textObjects_.own(to);
				break;
			// Text in 3D coordinates - left-hand origin = data[0]
			case (Glyph::TextGlyph3D):
				vec[0] = g->vector(0);
				// Add text object to list
				vec[1] = displayModel_->modelToScreen(vec[0]);
				if (vec[1].z < 1.0)
				{
					to = new TextObject((int)vec[1].x, int(height_ - vec[1].y), FALSE, g->text());
					textObjects_.own(to);
				}
				break;
		}
	}
	msg.exit("Canvas::renderModelTextGlyphs");
}
