/*
	*** Glyph Rendering
	*** src/render/rendergroup_glyph.cpp
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

#include "render/rendergroup.h"
#include "render/primitiveset.h"
#include "model/model.h"
#include "base/prefs.h"

ATEN_USING_NAMESPACE

// Render glyphs
void RenderGroup::createGlyphs(PrimitiveSet& primitiveSet, Model* source)
{
	Messenger::enter("Viewer::renderGlyphs");
	Matrix A, B;
	Vec3<double> r[4], u;
	Vec4<GLfloat> colour[4], textColour;
	double phi, rij;
	int n, i;
	double arrowBodyLength = 0.8, arrowHeadLength = 1.0-arrowBodyLength;

	// Copy foreground colour (for highlighting selected glyphs)
	prefs.copyColour(prefs.currentForegroundColour(), textColour);
	
	for (Glyph* g = source->glyphs(); g != NULL; g = g->next)
	{
		// Check if glyph is visible
		if (!g->isVisible()) continue;
		
		// Grab first coordinate (always used)
		r[0] = g->data(0)->vector();
		
		switch (g->type())
		{
			// Arrow - tail = data[0], head = data[1]
			case (Glyph::ArrowGlyph):
				r[1] = g->data(1)->vector();
				g->data(0)->copyColour(colour[0]);
				// Draw simple line from tail to head points
				extraNormalLines_.defineVertex(r[0].x, r[0].y, r[0].z, 0.0, 0.0, 1.0, colour[0]);
				extraNormalLines_.defineVertex(r[1].x, r[1].y, r[1].z, 0.0, 0.0, 1.0, colour[0]);
				// Draw cylinder arrowhead in wireframe
				A.createTranslation(r[0]);
				r[2] = r[1]-r[0];
				rij = r[2].magnitude();
				phi = DEGRAD * acos(r[2].z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r[2].y, r[2].x, 0.0, phi, true);
				// Move to endpoint
				A.applyTranslation(0.0, 0.0, rij*arrowBodyLength);
				A.applyScaling(0.2,0.2,rij*arrowHeadLength/arrowBodyLength);
				addTriangles(primitiveSet.cone(), A, colour[0], GL_LINE);
				break;
			// Line - start = data[0], end = data[1]
			case (Glyph::LineGlyph):
				r[1] = g->data(1)->vector();
				g->data(0)->copyColour(colour[0]);
				extraNormalLines_.defineVertex(r[0].x, r[0].y, r[0].z, 0.0, 0.0, 1.0, colour[0]);
				extraNormalLines_.defineVertex(r[1].x, r[1].y, r[1].z, 0.0, 0.0, 1.0, colour[0]);
				break;
			// Sphere - centre = data[0], scale = data[1]
			case (Glyph::SphereGlyph):
				r[1] = g->data(1)->vector();
				g->data(0)->copyColour(colour[0]);
				A.createTranslation(r[0]);
				g->applyRotation(A);
				A.applyScaling(r[1]);
				if (g->isSolid())
				{
					addTriangles(primitiveSet.sphere(), A, colour[0]);
					if (g->isSelected()) addTriangles(primitiveSet.sphere(), A, textColour, GL_LINE);
				}
				else
				{
					if (g->isSelected()) addTriangles(primitiveSet.sphere(), A, textColour, GL_LINE);
					else addTriangles(primitiveSet.sphere(), A, colour[0], GL_LINE);
				}
				break;
			// Cube - centre = data[0], scale = data[1]
			case (Glyph::CubeGlyph):
				r[1] = g->data(1)->vector();
				g->data(0)->copyColour(colour[0]);
				A.createTranslation(r[0]);
				g->applyRotation(A);
				A.applyScaling(r[1]);
				if (g->isSolid())
				{
					addTriangles(primitiveSet.cube(), A, colour[0], GL_FILL);
					if (g->isSelected()) addTriangles(primitiveSet.cube(), A, textColour, GL_LINE);
				}
				else
				{
					if (g->isSelected()) addTriangles(primitiveSet.cube(), A, textColour, GL_LINE);
					else addTriangles(primitiveSet.cube(), A, colour[0], GL_LINE);
				}
				break;
			// Triangle - vertex 1 = data[0], vertex 2 = data[1], vertex 3 = data[2]
			case (Glyph::TriangleGlyph):
				r[1] = g->data(1)->vector();
				r[2] = g->data(2)->vector();
				g->data(0)->copyColour(colour[0]);
				g->data(1)->copyColour(colour[1]);
				g->data(2)->copyColour(colour[2]);
				// Work out normal
				u = (r[1] - r[0]) * (r[2] - r[0]);
				u.normalise();
				if (g->isSolid())
				{
					extraSolidTriangles_.defineVertex(r[0], u, colour[0]);
					extraSolidTriangles_.defineVertex(r[1], u, colour[1]);
					extraSolidTriangles_.defineVertex(r[2], u, colour[2]);
				}
				else if (!g->isSelected())
				{
					extraWireTriangles_.defineVertex(r[0], u, colour[0]);
					extraWireTriangles_.defineVertex(r[1], u, colour[1]);
					extraWireTriangles_.defineVertex(r[2], u, colour[2]);
				}
				else
				{
					extraWireTriangles_.defineVertex(r[0], u, textColour);
					extraWireTriangles_.defineVertex(r[1], u, textColour);
					extraWireTriangles_.defineVertex(r[2], u, textColour);
				}
				break;
			// Quad - vertex 1 = data[0], vertex 2 = data[1], vertex 3 = data[2], vertex 4 = data[3]
			case (Glyph::QuadGlyph):
				r[1] = g->data(1)->vector();
				r[2] = g->data(2)->vector();
				r[3] = g->data(3)->vector();
				g->data(0)->copyColour(colour[0]);
				g->data(1)->copyColour(colour[1]);
				g->data(2)->copyColour(colour[2]);
				g->data(3)->copyColour(colour[3]);
				for (n = 0; n<2; ++n)
				{
					// Work out normal
					u = (r[n+1] - r[n]) * (r[n+2] - r[n]);
					u.normalise();
					if (g->isSolid())
					{
						extraSolidTriangles_.defineVertex(r[n], u, colour[n]);
						extraSolidTriangles_.defineVertex(r[n+1], u, colour[n+1]);
						extraSolidTriangles_.defineVertex(r[n+2], u, colour[n+2]);
					}
					else if (!g->isSelected())
					{
						extraWireTriangles_.defineVertex(r[n], u, colour[n]);
						extraWireTriangles_.defineVertex(r[n+1], u, colour[n+1]);
						extraWireTriangles_.defineVertex(r[n+2], u, colour[n+2]);
					}
					else
					{
						extraWireTriangles_.defineVertex(r[n], u, textColour);
						extraWireTriangles_.defineVertex(r[n+1], u, textColour);
						extraWireTriangles_.defineVertex(r[n+2], u, textColour);
					}
				}
				break;
			// Tetrahedron - vertex 1 = data[0], vertex 2 = data[1], vertex 3 = data[2], vertex 4 = data[3]
			case (Glyph::TetrahedronGlyph):
				r[1] = g->data(1)->vector();
				r[2] = g->data(2)->vector();
				r[3] = g->data(3)->vector();
				g->data(0)->copyColour(colour[0]);
				g->data(1)->copyColour(colour[1]);
				g->data(2)->copyColour(colour[2]);
				g->data(3)->copyColour(colour[3]);
				for (n = 0; n<4; ++n)
				{
					// Work out normal
					u = (r[(n+1)%4] - r[n]) * (r[(n+2)%4] - r[n]);
					u.normalise();
					if (g->isSolid())
					{
						extraSolidTriangles_.defineVertex(r[n], u, colour[n]);
						extraSolidTriangles_.defineVertex(r[(n+1)%4], u, colour[(n+1)%4]);
						extraSolidTriangles_.defineVertex(r[(n+2)%4], u, colour[(n+2)%4]);
					}
					else if (!g->isSelected())
					{
						extraWireTriangles_.defineVertex(r[n], u, colour[n]);
						extraWireTriangles_.defineVertex(r[(n+1)%4], u, colour[(n+1)%4]);
						extraWireTriangles_.defineVertex(r[(n+2)%4], u, colour[(n+2)%4]);
					}
					else
					{
						extraWireTriangles_.defineVertex(r[n], u, textColour);
						extraWireTriangles_.defineVertex(r[(n+1)%4], u, textColour);
						extraWireTriangles_.defineVertex(r[(n+2)%4], u, textColour);
					}
				}
				break;
			// Ellipsoid - centre = data[0], edge vector = data[1], face vector = data[2]
			// EllipsoidXYZ - centre = data[0], X = data[1], Y = data[2], Z= data[3]
			case (Glyph::EllipsoidGlyph):
			case (Glyph::EllipsoidXYZGlyph):
				g->data(0)->copyColour(colour[0]);
				A.createTranslation(r[0].x, r[0].y, r[0].z);
				r[1] = g->data(1)->vector() - r[0];
				r[2] = g->data(2)->vector() - r[0];
				if (g->type() == Glyph::EllipsoidXYZGlyph) r[3] = g->data(3)->vector() - r[0];
				else
				{
					r[3] = r[1] * r[2];
					r[3].normalise();
				}
				B.setColumn(0, r[1], 0.0);
				B.setColumn(1, r[2], 0.0);
				B.setColumn(2, r[3], 0.0);
				B.setColumn(3, 0.0, 0.0, 0.0, 1.0);
				A.multiplyRotation(B);
				if (g->isSolid())
				{
					addTriangles(primitiveSet.sphere(), A, colour[0], GL_FILL);
					if (g->isSelected()) addTriangles(primitiveSet.sphere(), A, textColour, GL_LINE);
				}
				else
				{
					if (g->isSelected()) addTriangles(primitiveSet.sphere(), A, textColour, GL_LINE);
					else addTriangles(primitiveSet.sphere(), A, colour[0], GL_LINE, 1.0);
				}
				break;
			// Text (2D)
			case (Glyph::TextGlyph):
				r[0] = g->data(0)->vector();
// 				addText2D(r[0].x, r[0].y, g->text()); // ATEN2 TODO
				break;
			case (Glyph::Text3DGlyph):
// 				addText3D(g->data(0)->vector(), g->text()); // ATEN2 TODO
				break;
			// Tube arrow - tail = data[0], head = data[1]
			case (Glyph::TubeArrowGlyph):
				r[1] = g->data(1)->vector();
				g->data(0)->copyColour(colour[0]);
				// Draw cylinder body and arrowhead
				A.createTranslation(r[0]);
				r[2] = r[1]-r[0];
				rij = r[2].magnitude();
				phi = DEGRAD * acos(r[2].z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r[2].y, r[2].x, 0.0, phi, true);
				// Draw cylinder
				A.applyScaling(0.1,0.1,rij*arrowBodyLength);
				addTriangles(primitiveSet.cylinder(), A, colour[0], g->isSolid() ? GL_FILL : GL_LINE);
				// Move to endpoint
				A.applyTranslation(0.0, 0.0, 1.0);
				A.applyScaling(2.0,2.0,arrowHeadLength/arrowBodyLength);
				addTriangles(primitiveSet.cone(), A, colour[0], g->isSolid() ? GL_FILL : GL_LINE);
				break;
			// Tube vector - centre = data[0], vector = data[1]
			case (Glyph::TubeVectorGlyph):
				r[1] = g->data(1)->vector();
				r[0] -= r[1]*0.5;
				g->data(0)->copyColour(colour[0]);
				// Draw cylinder body and arrowhead
				A.createTranslation(r[0]);
				rij = r[1].magnitude();
				phi = DEGRAD * acos(r[1].z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r[1].y, r[1].x, 0.0, phi, true);
				// Draw cylinder
				A.applyScaling(0.1,0.1,rij*arrowBodyLength);
				addTriangles(primitiveSet.cylinder(), A, colour[0], g->isSolid() ? GL_FILL : GL_LINE);
				// Move to endpoint
				A.applyTranslation(0.0, 0.0, 1.0);
				A.applyScaling(2.0,2.0,arrowHeadLength/arrowBodyLength);
				addTriangles(primitiveSet.cone(), A, colour[0], g->isSolid() ? GL_FILL : GL_LINE);
				break;
			// Vector - center = data[0], vector = data[1]
			case (Glyph::VectorGlyph):
				r[1] = g->data(1)->vector();
				r[0] -= r[1]*0.5;
				g->data(0)->copyColour(colour[0]);
				// Draw simple line from tail to head points, since we have adjusted along half the vector above
				extraNormalLines_.defineVertex(r[0].x, r[0].y, r[0].z, 0.0, 0.0, 1.0, colour[0]);
				extraNormalLines_.defineVertex(r[0].x+r[1].x, r[0].y+r[1].y, r[0].z+r[1].z, 0.0, 0.0, 1.0, colour[0]);
				// Draw cylinder arrowhead in wireframe
				A.createTranslation(r[0]);
				rij = r[1].magnitude();
				phi = DEGRAD * acos(r[1].z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r[1].y, r[1].x, 0.0, phi, true);
				// Move to endpoint
				A.applyTranslation(0.0, 0.0, rij*arrowBodyLength);
				A.applyScaling(0.2, 0.2, rij*arrowHeadLength/arrowBodyLength);
				addTriangles(primitiveSet.cone(), A, colour[0], GL_LINE);
				break;
      default:
        break;  
		}
	}

	Messenger::exit("Viewer::renderGlyphs");
}
