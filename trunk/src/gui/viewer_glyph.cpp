/*
	*** Glyph Rendering
	*** src/gui/viewer_glyph.cpp
	Copyright T. Youngs 2007-2015

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

// #include "classes/prefs.h"
#include "model/model.h"
#include "gui/viewer.uih"

// Render glyphs
void Viewer::renderGlyphs(Model* source)
{
	Messenger::enter("Viewer::renderGlyphs");
	Matrix A, B;
	Vec3<double> r[4], u;
	Viewer::GlyphTriangleStyle ts;
	Vec4<GLfloat> colour[4], textColour;
	double phi, rij;
	int n, i;
	double arrowBodyLength = 0.8, arrowHeadLength = 1.0-arrowBodyLength;

	// Copy text colour (for highlighting selected glyphs)
	prefs.copyColour(Prefs::TextColour, textColour);
	
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
				glyphLines_.defineVertex(r[0].x, r[0].y, r[0].z, 0.0, 0.0, 1.0, colour[0]);
				glyphLines_.defineVertex(r[1].x, r[1].y, r[1].z, 0.0, 0.0, 1.0, colour[0]);
				// Draw cylinder arrowhead in wireframe
				A.setIdentity();
				A.applyTranslation(r[0]);
				r[2] = r[1]-r[0];
				rij = r[2].magnitude();
				phi = DEGRAD * acos(r[2].z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r[2].y, r[2].x, 0.0, phi, TRUE);
				// Move to endpoint
				A.applyTranslationZ(rij*arrowBodyLength);
				A.applyScaling(0.2,0.2,rij*arrowHeadLength/arrowBodyLength);
				renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].cone(), colour[0], A, GL_LINE);
				break;
			// Line - start = data[0], end = data[1]
			case (Glyph::LineGlyph):
				r[1] = g->data(1)->vector();
				g->data(0)->copyColour(colour[0]);
				glyphLines_.defineVertex(r[0].x, r[0].y, r[0].z, 0.0, 0.0, 1.0, colour[0]);
				glyphLines_.defineVertex(r[1].x, r[1].y, r[1].z, 0.0, 0.0, 1.0, colour[0]);
				break;
			// Sphere - centre = data[0], scale = data[1]
			case (Glyph::SphereGlyph):
				r[1] = g->data(1)->vector();
				g->data(0)->copyColour(colour[0]);
				A.setIdentity();
				A.applyTranslation(r[0]);
				if (g->rotated()) A *= (*g->matrix());
				A.applyScaling(r[1]);
				if (g->isSolid())
				{
					renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].sphere(), colour[0], A, GL_FILL);
					if (g->isSelected()) renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].sphere(), textColour, A, GL_LINE, 2.0);
				}
				else
				{
					if (g->isSelected()) renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].sphere(), textColour, A, GL_LINE, 3.0);
					else renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].sphere(), colour[0], A, GL_LINE, 1.0);
				}
				break;
			// Cube - centre = data[0], scale = data[1]
			case (Glyph::CubeGlyph):
				r[1] = g->data(1)->vector();
				g->data(0)->copyColour(colour[0]);
				A.setIdentity();
				A.applyTranslation(r[0]);
				if (g->rotated()) A *= (*g->matrix());
				A.applyScaling(r[1]);
				if (g->isSolid())
				{
					renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].cube(), colour[0], A, GL_FILL);
					if (g->isSelected()) renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].cube(), textColour, A, GL_LINE, 2.0);
				}
				else
				{
					if (g->isSelected()) renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].cube(), textColour, A, GL_LINE, 3);
					else renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].cube(), colour[0], A, GL_LINE, 1.0);
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
				if (g->isSolid()) ts = Viewer::SolidTriangle;
				else ts = Viewer::WireTriangle;
				glyphTriangles_[ts].defineVertex(r[0], u, colour[0]);
				glyphTriangles_[ts].defineVertex(r[1], u, colour[1]);
				glyphTriangles_[ts].defineVertex(r[2], u, colour[2]);
				if (g->isSelected())
				{
					glyphTriangles_[Viewer::WireTriangle].defineVertex(r[0], u, textColour);
					glyphTriangles_[Viewer::WireTriangle].defineVertex(r[1], u, textColour);
					glyphTriangles_[Viewer::WireTriangle].defineVertex(r[2], u, textColour);
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
					if (g->isSolid()) ts = Viewer::SolidTriangle;
					else ts = Viewer::WireTriangle;
					i = n;
					glyphTriangles_[ts].defineVertex(r[i], u, colour[i]);
					++i;
					glyphTriangles_[ts].defineVertex(r[i], u, colour[i]);
					++i;
					glyphTriangles_[ts].defineVertex(r[i], u, colour[i]);
					if (g->isSelected())
					{
						glyphTriangles_[Viewer::WireTriangle].defineVertex(r[n], u, textColour);
						glyphTriangles_[Viewer::WireTriangle].defineVertex(r[n+1], u, textColour);
						glyphTriangles_[Viewer::WireTriangle].defineVertex(r[n+2], u, textColour);
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
					if (g->isSolid()) ts = Viewer::SolidTriangle;
					else ts = Viewer::WireTriangle;
					glyphTriangles_[ts].defineVertex(r[n], u, colour[n]);
					i = (n+1)%4;
					glyphTriangles_[ts].defineVertex(r[i], u, colour[i]);
					i = (n+2)%4;
					glyphTriangles_[ts].defineVertex(r[i], u, colour[i]);
					if (g->isSelected())
					{
						glyphTriangles_[Viewer::WireTriangle].defineVertex(r[n], u, textColour);
						glyphTriangles_[Viewer::WireTriangle].defineVertex(r[(n+1)%4], u, textColour);
						glyphTriangles_[Viewer::WireTriangle].defineVertex(r[(n+2)%4], u, textColour);
					}
				}
				break;
			// Ellipsoid - centre = data[0], edge vector = data[1], face vector = data[2]
			// EllipsoidXYZ - centre = data[0], X = data[1], Y = data[2], Z= data[3]
			case (Glyph::EllipsoidGlyph):
			case (Glyph::EllipsoidXYZGlyph):
				g->data(0)->copyColour(colour[0]);
				A.setIdentity();
				A.applyTranslation(r[0].x, r[0].y, r[0].z);
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
					renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].sphere(), colour[0], A, GL_FILL);
					if (g->isSelected()) renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].sphere(), textColour, A, GL_LINE, 2.0);
				}
				else
				{
					if (g->isSelected()) renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].sphere(), textColour, A, GL_LINE, 3.0);
					else renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].sphere(), colour[0], A, GL_LINE, 1.0);
				}
				break;
			// Text - handled in Viewer::renderTextGlyphs()
			case (Glyph::TextGlyph):
			case (Glyph::Text3DGlyph):
				break;
			// Tube arrow - tail = data[0], head = data[1]
			case (Glyph::TubeArrowGlyph):
				r[1] = g->data(1)->vector();
				g->data(0)->copyColour(colour[0]);
				// Draw cylinder body and arrowhead
				A.setIdentity();
				A.applyTranslation(r[0]);
				r[2] = r[1]-r[0];
				rij = r[2].magnitude();
				phi = DEGRAD * acos(r[2].z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r[2].y, r[2].x, 0.0, phi, TRUE);
				// Draw cylinder
				A.applyScaling(0.1,0.1,rij*arrowBodyLength);
				renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].cylinder(), colour[0], A, g->isSolid() ? GL_FILL : GL_LINE);
				// Move to endpoint
				A.applyTranslationZ(1.0);
				A.applyScaling(2.0,2.0,arrowHeadLength/arrowBodyLength);
				renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].cone(), colour[0], A, g->isSolid() ? GL_FILL : GL_LINE);
				break;
			// Tube vector - centre = data[0], vector = data[1]
			case (Glyph::TubeVectorGlyph):
				r[1] = g->data(1)->vector();
				r[0] -= r[1]*0.5;
				g->data(0)->copyColour(colour[0]);
				// Draw cylinder body and arrowhead
				A.setIdentity();
				A.applyTranslation(r[0]);
				rij = r[1].magnitude();
				phi = DEGRAD * acos(r[1].z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r[1].y, r[1].x, 0.0, phi, TRUE);
				// Draw cylinder
				A.applyScaling(0.1,0.1,rij*arrowBodyLength);
				renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].cylinder(), colour[0], A, g->isSolid() ? GL_FILL : GL_LINE);
				// Move to endpoint
				A.applyTranslationZ(1.0);
				A.applyScaling(2.0,2.0,arrowHeadLength/arrowBodyLength);
				renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].cone(), colour[0], A, g->isSolid() ? GL_FILL : GL_LINE);
				break;
			// Vector - center = data[0], vector = data[1]
			case (Glyph::VectorGlyph):
				r[1] = g->data(1)->vector();
				r[0] -= r[1]*0.5;
				g->data(0)->copyColour(colour[0]);
				// Draw simple line from tail to head points, since we have adjusted along half the vector above
				glyphLines_.defineVertex(r[0].x, r[0].y, r[0].z, 0.0, 0.0, 1.0, colour[0]);
				glyphLines_.defineVertex(r[0].x+r[1].x, r[0].y+r[1].y, r[0].z+r[1].z, 0.0, 0.0, 1.0, colour[0]);
				// Draw cylinder arrowhead in wireframe
				A.setIdentity();
				A.applyTranslation(r[0]);
				rij = r[1].magnitude();
				phi = DEGRAD * acos(r[1].z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r[1].y, r[1].x, 0.0, phi, TRUE);
				// Move to endpoint
				A.applyTranslationZ(rij*arrowBodyLength);
				A.applyScaling(0.2, 0.2, rij*arrowHeadLength/arrowBodyLength);
				renderPrimitive(Viewer::GlyphObject, primitives_[primitiveSet_].cone(), colour[0], A, GL_LINE);
				break;
		}
	}
	
	A.setIdentity();
	renderPrimitive(Viewer::GlyphObject, glyphTriangles_[Viewer::SolidTriangle], colour[0], A);	// ATEN2 TODO
	renderPrimitive(Viewer::GlyphObject, glyphTriangles_[Viewer::WireTriangle], colour[0], A, GL_LINE);
	renderPrimitive(Viewer::GlyphObject, glyphLines_, colour[0], A, GL_LINE);
	Messenger::exit("Viewer::renderGlyphs");
}

// Render text glyphs
void Viewer::renderTextGlyphs(Model* source)
{
	Vec3<double> r1, r2;
	Vec4<double> screenr;
	GLfloat textcolour[4];
	Glyph* g;

	// Copy text colour
	prefs.copyColour(Prefs::TextColour, textcolour);
	
	for (Refitem<Glyph,int>* ri = source->textGlyphs(); ri != NULL; ri = ri->next)
	{
		// Get glyph pointer
		g = ri->item;

		// Check if glyph is visible
		if (!g->isVisible()) continue;
		
		// Grab first coordinate (always used)
		r1 = g->data(0)->vector();
		
		if (g->type() == Glyph::TextGlyph) renderTextPrimitive(r1.x, r1.y, g->text());
		else if (g->type() == Glyph::Text3DGlyph)
		{
			r2 = source->modelToWorld(r1, &screenr);
			if (r2.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, g->text());
		}
		else printf("Internal Error: Found non-text glyph in textglyphs list...\n");
	}
}
