/*
	*** Rendering Engine Renderer!
	*** src/render/engine_render.cpp
	Copyright T. Youngs 2007-2010

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

#include "render/engine.h"
#include "classes/prefs.h"
#include "classes/forcefieldatom.h"
#include "classes/grid.h"
#include "model/model.h"
#include "model/fragment.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"
#include "gui/fragment.h"
#include "base/sysfunc.h"
#include <math.h>

// Set OpenGL options ready for drawing
void RenderEngine::initialiseGL()
{
	msg.enter("RenderEngine::initialiseGL");
	// Clear colour
	GLfloat col[4];
	prefs.copyColour(Prefs::BackgroundColour, col);
	glClearColor(col[0],col[1],col[2],col[3]);
	//glClearDepth(1.0);
	// Perspective hint
	glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_FASTEST);
	// Enable depth buffer
	glEnable(GL_DEPTH_TEST);
	// Smooth shading
	glShadeModel(GL_SMOOTH);
	// Auto-calculate surface normals
	glEnable(GL_NORMALIZE);
	// Set alpha-blending function
	glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
	//glBlendFunc(GL_SRC_ALPHA_SATURATE, GL_ONE);
	// Set up the light model
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glEnable(GL_LIGHTING);
	prefs.copySpotlightColour(Prefs::AmbientComponent, col);
	glLightfv(GL_LIGHT0, GL_AMBIENT, col);
	prefs.copySpotlightColour(Prefs::DiffuseComponent, col);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, col);
	prefs.copySpotlightColour(Prefs::SpecularComponent, col);
	glLightfv(GL_LIGHT0, GL_SPECULAR, col);
	prefs.copySpotlightPosition(col);
	glLightfv(GL_LIGHT0, GL_POSITION, col);
	prefs.spotlightActive() ? glEnable(GL_LIGHT0) : glDisable(GL_LIGHT0);
	// Set specular reflection colour
	prefs.copyColour(Prefs::SpecularColour, col);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, col);
	glMateriali(GL_FRONT_AND_BACK, GL_SHININESS, prefs.shininess());
	glDisable(GL_BLEND);
	glDisable(GL_LINE_SMOOTH);
	glDisable(GL_POLYGON_SMOOTH);
	glDisable(GL_MULTISAMPLE);
	// Configure antialiasing
	if (prefs.multiSampling()) glEnable(GL_MULTISAMPLE);
	if (prefs.lineAliasing())
	{
		glEnable(GL_BLEND);
		glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);
		glEnable(GL_LINE_SMOOTH);
	}
	if (prefs.polygonAliasing())
	{
		glEnable(GL_BLEND);
		glHint(GL_POLYGON_SMOOTH_HINT,GL_NICEST);
		glEnable(GL_POLYGON_SMOOTH);
	}
	// Configure fog effects
	if (prefs.depthCue())
	{
		glFogi(GL_FOG_MODE, GL_LINEAR);
		prefs.copyColour(Prefs::BackgroundColour, col);
		glFogfv(GL_FOG_COLOR, col);
		glFogf(GL_FOG_DENSITY, 0.35f);
		glHint(GL_FOG_HINT, GL_NICEST);
		glFogi(GL_FOG_START, prefs.depthNear());
		glFogi(GL_FOG_END, prefs.depthFar());
		glEnable(GL_FOG);
	}
	else glDisable(GL_FOG);
	// Configure face culling
	glCullFace(GL_BACK);
	prefs.backfaceCulling() ? glEnable(GL_CULL_FACE) : glDisable(GL_CULL_FACE);
	// Test
	glDisable(GL_DITHER);
	glDisable(GL_LOGIC_OP);
	msg.exit("RenderEngine::initialiseGL");
}

// Render bond
void RenderEngine::renderBond(Matrix A, Vec3<double> vij, Atom *i, Atom::DrawStyle style_i, GLfloat *colour_i, double radius_i, Atom *j, Atom::DrawStyle style_j, GLfloat *colour_j, double radius_j, Bond::BondType bt, int lod, double selscale, Bond *b)
{
	double dvisible, factor, rij, phi, dp;
	Vec3<double> ijk, ri, rj;
	GLfloat alpha_i, alpha_j;
	
	// Store copies of alpha values
	alpha_i = colour_i[3];
	alpha_j = colour_j[3];
	
	rij = vij.magnitude();
	
	// Don't bother calculating transformation if both atom styles are Stick
	if ((style_i == Atom::StickStyle) && (style_j == Atom::StickStyle))
	{
		ri = i->r();
		rj = j->r();
		glNormal3d(0.0,0.0,1.0);
		glLineWidth( i->isSelected() ? 3.0 : 1.0 );
		glColor4fv(colour_i);
		glBegin(GL_LINES);
		glVertex3d(ri.x, ri.y, ri.z);
		ri += vij*0.5;
		glVertex3d(ri.x, ri.y, ri.z);
		glEnd();
		glLineWidth( j->isSelected() ? 3.0 : 1.0 );
		glColor4fv(colour_j);
		glBegin(GL_LINES);
		glVertex3d(ri.x, ri.y, ri.z);
		glVertex3d(rj.x, rj.y, rj.z);
		glEnd();
	}
	else
	{
		// If bond is not visible, don't bother drawing it...
		dvisible = 0.5 * (rij - 0.85*(radius_i + radius_j));
		if (dvisible < 0.0) return;
		factor = (selscale*radius_i - 0.85*radius_i) / dvisible;
		
		// Calculate angle out of XZ plane
		// OPTIMISE - Precalculate acos()
		phi = DEGRAD * acos(vij.z/rij);
		
		// Special case where the bond is exactly in the XY plane.
		if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
		else A.applyRotationAxis(-vij.y, vij.x, 0.0, phi, TRUE);
		
		// We can perform an initial translation to the 'edge' of atom i, and scale to visible bond length
		A.applyTranslation(0.0, 0.0, 0.85*radius_i);
		A.applyScalingZ(dvisible);
		
		// Determine bond plane rotation if a multiple bond
		if ((bt > Bond::Single) && (b != NULL))
		{
			if (i > j) ijk = i->findBondPlane(j,b,vij);
			else ijk = j->findBondPlane(i,b,vij);
			// Calculate projected bond plane vector
			ijk = A.rotateVector(ijk);
			// 					printf("Transformed bond-plane vector is :"); ijk.print();
			// 					A.print();
			// 					printf("DP = %f,  acos = %f\n", ijk.dp(Vec3<double>(A[0],A[1],A[2])), DEGRAD*acos(ijk.dp(Vec3<double>(A[0],A[1],A[2]))));
			// Rotate into bond plane if necessary
			dp = ijk.x*A[0] + ijk.y*A[1] + ijk.z*A[2];
			phi = 180.0 - DEGRAD*acos(dp);
			if (phi > 0.01)
			{
				A.applyRotationAxis(0.0,0.0,1.0,phi-180.0,FALSE);  // BROKEN
			}
		}
		
		// Draw first bond half
		switch (style_i)
		{
			case (Atom::StickStyle):
				glLineWidth( i->isSelected() ? 4.0 : 2.0 );
				glBegin(GL_LINES);
				glVertex3d(0.0, 0.0, 0.0);
				glVertex3d(0.5*vij.x, 0.5*vij.y, 0.5*vij.z);
				glEnd();
				break;
			case (Atom::TubeStyle):
				renderPrimitive(bond_[style_i][bt], lod, colour_i, A);
				if (i->isSelected())
				{
					colour_i[3] = 0.5f;
					renderPrimitive(selectedBond_[style_i][bt], lod, colour_i, A);
					colour_i[3] = alpha_i;
				}
				// Move to centre of visible bond, ready for next bond half
				A.applyTranslation(0.0, 0.0, 1.0);
				break;
			case (Atom::SphereStyle):
			case (Atom::ScaledStyle):
				renderPrimitive(bond_[style_i][bt], lod, colour_i, A);
				if (i->isSelected())
				{
					// Move to bond centre and apply 'reverse' Z-scaling
					A.applyTranslation(0.0, 0.0, factor);
					A.applyScalingZ(1.0-factor);
					colour_i[3] = 0.5f;
					renderPrimitive(selectedBond_[style_i][bt], lod, colour_i, A);
					colour_i[3] = alpha_i;
					// Move to centrepoint and reverse scaling back to 'dvisible'
					A.applyTranslation(0.0, 0.0, 1.0);	// OPTIMIZE - Separate funcs for X, Y, or Z translation
					A.applyScalingZ(1.0/(1.0-factor));
				}
				else A.applyTranslation(0.0, 0.0, 1.0);
				break;
		}
		
		// Draw second bond half
		switch (style_j)
		{
			case (Atom::StickStyle):
				glLineWidth( j->isSelected() ? 3.0 : 1.0 );
				glBegin(GL_LINES);
				glVertex3d(0.5*vij.x, 0.5*vij.y, 0.5*vij.z);
				glVertex3d(vij.x, vij.y, vij.z);
				glEnd();
				break;
			case (Atom::TubeStyle):
				renderPrimitive(bond_[style_i][bt], lod, colour_j, A);
				if (j->isSelected())
				{
					colour_j[3] = 0.5f;
					renderPrimitive(selectedBond_[style_j][bt], lod, colour_j, A);
					colour_j[3] = alpha_j;
				}
				break;
			case (Atom::SphereStyle):
			case (Atom::ScaledStyle):
				renderPrimitive(bond_[style_j][bt], lod, colour_j, A);
				if (j->isSelected())
				{
					colour_j[3] = 0.5f;
					A.applyScalingZ(1.0-factor);
					renderPrimitive(selectedBond_[style_j][bt], lod, colour_j, A);
					colour_j[3] = alpha_j;
				}
				break;
		}
	}
}

// Render basic model information (atoms, bonds, labels, and glyphs)
void RenderEngine::renderModel(Model *source, Matrix baseTransform, TCanvas *canvas)
{
	GLfloat colour_i[4], colour_j[4], alpha_i, colour_k[4], colour_l[4], textcolour[4];
	GLenum style;
	int lod, id_i, labels;
	Dnchar text;
	double selscale, z, radius_i, radius_j, rij, phi;
	Atom *i, *j;
	Vec3<double> pos, v, ijk, r1, r2, r3, r4;
	Vec4<double> transformZ, screenr;
	Matrix atomtransform, A;
	Refitem<Bond,int> *ri;
	Atom::DrawStyle style_i, style_j, globalstyle;
	Prefs::ColouringScheme scheme;
	ForcefieldAtom *ffa;
	RenderEngine::TriangleStyle ts;
	
	// Grab global style values and text colour
	scheme = prefs.colourScheme();
	globalstyle = prefs.renderStyle();
	selscale = prefs.selectionScale();
	prefs.copyColour(Prefs::TextColour, textcolour);
	
	// Gram z-transform vector
	transformZ.set(baseTransform[2], baseTransform[6], baseTransform[10], baseTransform[14]);
	
	// Render cell and cell axes
	if (source->cell()->type() != Cell::NoCell)
	{
		prefs.copyColour(Prefs::UnitCellColour, colour_i);
		glColor4fv(colour_i);
		A = source->modelViewMatrix() * source->cell()->axes();
		glMultMatrixd(A.matrix());
		if (prefs.isVisibleOnScreen(Prefs::ViewCell)) wireCube_.sendToGL();
		glTranslated(-0.5, -0.5, -0.5);
		v = source->cell()->lengths();
		glScaled(1.0 / v.x, 1.0 / v.y, 1.0 / v.z);
		prefs.copyColour(Prefs::UnitCellAxesColour, colour_i);
		glColor4fv(colour_i);
		if (prefs.isVisibleOnScreen(Prefs::ViewCellAxes)) cellAxes_.sendToGL();
	}

	// We will apply the basic transformation matrix to OpenGL here, so we can draw simple line primitives using just model coordinates
	// Consequently, glMultMatrixd() and glLoadIdentity(), as well as all other OpenGL matrix manipulators, should not be used past this point!
	glLoadIdentity();
	glMultMatrixd(baseTransform.matrix());
	
	// Atoms and Bonds // OPTIMIZE - use atom array instead
	if (prefs.isVisibleOnScreen(Prefs::ViewAtoms)) for (i = source->atoms(); i != NULL; i = i->next)
	{
		// Skip hidden atoms
		if (i->isHidden()) continue;
		
		// Grab atom coordinate - we'll need it a lot
		pos = i->r();
		
		// Calculate projected Z distance from viewer
		// If z is less than 0, don't even bother continuing since its behind the viewer
		z = -(pos.x*transformZ.x + pos.y*transformZ.y + pos.z*transformZ.z + transformZ.w);
		if (z < 0) continue;
		
		// Determine level of detail to use for primitives
		if (z < prefs.levelOfDetailStartZ()) lod = 0;
		else lod = int((z-prefs.levelOfDetailStartZ()) / prefs.levelOfDetailWidth());
		// 		printf("lod = %i, projected z = %f, nlevels = %i\n", lod, z, prefs.levelsOfDetail());
		
		// Move to local atom position
		atomtransform = baseTransform;
		atomtransform.applyTranslation(pos.x, pos.y, pos.z);
		
		// Select colour
		if (i->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, colour_i);
		else switch (scheme)
		{
			case (Prefs::ElementScheme):
				elements().copyColour(i->element(), colour_i);
				break;
			case (Prefs::ChargeScheme):
				prefs.colourScale[0].colour(i->charge(), colour_i);
				break;
			case (Prefs::VelocityScheme):
				prefs.colourScale[1].colour(i->v().magnitude(), colour_i);
				break;
			case (Prefs::ForceScheme):
				prefs.colourScale[2].colour(i->f().magnitude(), colour_i);
				break;
			case (Prefs::CustomScheme):
				i->copyColour(colour_i);
				break;
			default:
				break;
		}
		// Store copy of alpha value
		alpha_i = colour_i[3];
		
		// Get atom style
		style_i = (globalstyle == Atom::IndividualStyle ? i->style() : globalstyle);
		
		switch (style_i)
		{
			case (Atom::StickStyle):
				if (i->nBonds() == 0) renderPrimitive(atom_[style_i], lod, colour_i, atomtransform, GL_LINE, i->isSelected() ? 3.0 : 1.0);
				break;
			case (Atom::TubeStyle):
			case (Atom::SphereStyle):
				renderPrimitive(atom_[style_i], lod, colour_i, atomtransform);
				if (i->isSelected())
				{
					colour_i[3] = 0.5f;
					renderPrimitive(selectedAtom_[style_i], lod, colour_i, atomtransform);
					colour_i[3] = alpha_i;
				}
				break;
			case (Atom::ScaledStyle):
				renderPrimitive(scaledAtom_[i->element()], lod, colour_i, atomtransform);
				if (i->isSelected())
				{
					colour_i[3] = 0.5f;
					renderPrimitive(selectedScaledAtom_[i->element()], lod, colour_i, atomtransform);
					colour_i[3] = alpha_i;
				}
				break;
		}
		
		// Labels
		labels = i->labels();
		if ((labels != 0) && (prefs.isVisibleOnScreen(Prefs::ViewLabels)))
		{
			ffa = i->type();
			
			// Blank label string
			text.clear();
			// Now add on all parts of the label that are required
			if (labels&(1 << Atom::IdLabel)) text.strcatf("%i ", i->id()+1);
			if (labels&(1 << Atom::ElementLabel)) text.strcatf("%s ", elements().symbol(i));
			if (labels&(1 << Atom::TypeLabel))
			{
				if (ffa == NULL) text.strcat("[None] ");
				else text.strcatf("[%i %s] ", ffa->typeId(), ffa->name());
			}
			if (labels&(1 << Atom::EquivLabel)) text.strcatf("[=%s] ", ffa == NULL ? "None" : ffa->equivalent());
			if (labels&(1 << Atom::ChargeLabel)) text.strcatf("(%f e)", i->charge());
			
			// Add text object
			renderTextPrimitive(i->r(), text.get());
		}
		
		// Bonds
		// Grab some useful values from atom i
		id_i = i->id();
		radius_i = (style_i == Atom::TubeStyle ? 0.0 : prefs.screenRadius(i)*0.85);
		
		for (ri = i->bonds(); ri != NULL; ri = ri->next)
		{
			j = ri->item->partner(i);
			if (j->id() > id_i) continue;
			
			// Grab colour of second atom
			if (j->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, colour_j);
			else switch (scheme)
			{
				case (Prefs::ElementScheme):
					elements().copyColour(j->element(), colour_j);
					break;
				case (Prefs::ChargeScheme):
					prefs.colourScale[0].colour(j->charge(), colour_j);
					break;
				case (Prefs::VelocityScheme):
					prefs.colourScale[1].colour(j->v().magnitude(), colour_j);
					break;
				case (Prefs::ForceScheme):
					prefs.colourScale[2].colour(j->f().magnitude(), colour_j);
					break;
				case (Prefs::CustomScheme):
					j->copyColour(colour_j);
					break;
				default:
					break;
			}
			
			// Get atom style and radius
			style_j = (globalstyle == Atom::IndividualStyle ? j->style() : globalstyle);
			radius_j = (style_j == Atom::TubeStyle ? 0.0 : prefs.screenRadius(j)*0.85);
			
			// Calculate vector i->j
			v = source->cell()->mimd(j, i);
			
			// Render bond
			renderBond(atomtransform, v, i, style_i, colour_i, radius_i, j, style_j, colour_j, radius_j, ri->item->type(), lod, selscale);
		}
		
	}
	
	// Surfaces
	// Cycle over grids stored in current model
	for (Grid *g = source->grids(); g != NULL; g = g->next)
	{
		// Check visibility
		if (!g->isVisible()) continue;
		// Does a GridPrimitive already exist?
		GridPrimitive *gp = findGridPrimitive(g);
		// If the GridPrimitive exists and is 'in date', no need to regenerate it...
		if ((gp == NULL) || g->shouldRerender())
		{
			// Generate new GridPrimitive if required
			if (gp == NULL)
			{
				gp = gridPrimitives_.add();
				gp->setSource(g);
			}
			if (g->type() == Grid::RegularXYZData) gp->createSurfaceMarchingCubes();
			else if (g->type() == Grid::FreeXYZData)
			{
				// Construct the Delaunay triangulization of the surface
				// 				DelaunaySurface D(g);
			}
			else gp->createSurface2D();
			g->updateRenderPoint();
		}
		
		// Grid primitive now exists (or has been updated) so create transformation and render it
		A = baseTransform;
		A.applyTranslation(g->origin());
		A.multiplyRotation(g->axes());
		if (g->style() == Grid::TriangleSurface) style = GL_LINE;
		else if (g->style() == Grid::SolidSurface) style = GL_FILL;
		else style = GL_POINTS;
		if (g->useColourScale()) renderPrimitive(&gp->primaryPrimitive(), gp->primaryIsTransparent(), NULL, A, style);
		else
		{
			g->copyPrimaryColour(colour_i);
			renderPrimitive(&gp->primaryPrimitive(), gp->primaryIsTransparent(), colour_i, A, style);
		}
		if (g->useSecondary())
		{
			if (g->useColourScale()) renderPrimitive(&gp->secondaryPrimitive(), gp->secondaryIsTransparent(), NULL, A, style);
			else
			{
				g->copySecondaryColour(colour_i);
				renderPrimitive(&gp->secondaryPrimitive(), gp->secondaryIsTransparent(), colour_i, A, style);
			}
		}
	}
	
	// Glyphs
	for (Glyph *g = source->glyphs(); g != NULL; g = g->next)
	{
		// Check if glyph is visible
		if (!g->isVisible()) continue;
		
		// Determine level of detail for glyph
		r1 = g->data(0)->vector();
		// If z is less than 0, don't even bother continuing since its behind the viewer
		z = -(r1.x*transformZ.x + r1.y*transformZ.y + r1.z*transformZ.z + transformZ.w);
		if (z < 0) continue;
		if (z < prefs.levelOfDetailStartZ()) lod = 0;
		else lod = int((z-prefs.levelOfDetailStartZ()) / prefs.levelOfDetailWidth());
		
		switch (g->type())
		{
			// Arrow - tail = data[0], head = data[1]
			case (Glyph::ArrowGlyph):
				r2 = g->data(1)->vector();
				g->data(0)->copyColour(colour_i);
				glLineWidth(g->lineWidth());
				glColor4fv(colour_i);
				// Draw simple line from tail to head points
				glBegin(GL_LINES);
				glVertex3d(r1.x, r1.y, r1.z);
				glVertex3d(r2.x, r2.y, r2.z);
				glEnd();
				// Draw cylinder arrowhead in wireframe
				A = baseTransform;
				A.applyTranslation(r1);
				r3 = r2-r1;
				rij = r3.magnitude();
				phi = DEGRAD * acos(r3.z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r3.y, r3.x, 0.0, phi, TRUE);
				// Move to endpoint
				A.applyTranslation(0.0,0.0,rij*0.9);
				A.applyScaling(0.2,0.2,rij*0.1);
				renderPrimitive(cones_, lod, colour_i, A, GL_LINE);
				break;
			// Line - start = data[0], end = data[1]
			case (Glyph::LineGlyph):
				r2 = g->data(1)->vector();
				g->data(0)->copyColour(colour_i);
				glLineWidth(g->lineWidth());
				glColor4fv(colour_i);
				// Draw simple line from tail to head points
				glBegin(GL_LINES);
				glVertex3d(r1.x, r1.y, r1.z);
				glVertex3d(r2.x, r2.y, r2.z);
				glEnd();
				break;
			// Sphere or Cube - centre = data[0], scale = data[1]
			case (Glyph::SphereGlyph):
			case (Glyph::CubeGlyph):
				r2 = g->data(1)->vector();
				g->data(0)->copyColour(colour_i);
				A = baseTransform;
				A.applyTranslation(r1.x, r1.y, r1.z);
				if (g->rotated()) A *= (*g->matrix());
				A.applyScaling(r2.x, r2.y, r2.z);
				if (g->isSolid())
				{
					if (g->type() == Glyph::SphereGlyph) renderPrimitive(spheres_, lod, colour_i, A, GL_FILL);
					else renderPrimitive(cubes_, lod, colour_i, A, GL_FILL);
					if (g->isSelected())
					{
						if (g->type() == Glyph::SphereGlyph) renderPrimitive(spheres_, lod, textcolour, A, GL_LINE, 3.0);
						else renderPrimitive(cubes_, lod, textcolour, A, GL_LINE, 3.0);
					}
				}
				else
				{
					if (g->isSelected())
					{
						if (g->type() == Glyph::SphereGlyph) renderPrimitive(spheres_, lod, textcolour, A, GL_LINE, g->lineWidth()+2);
						else renderPrimitive(cubes_, lod, textcolour, A, GL_LINE, g->lineWidth()+2);
					}
					else
					{
						if (g->type() == Glyph::SphereGlyph) renderPrimitive(spheres_, lod, colour_i, A, GL_LINE, g->lineWidth());
						else renderPrimitive(cubes_, lod, colour_i, A, GL_LINE, g->lineWidth());
					}
				}
				break;
			// Triangle/Quad - vertex 1 = data[0], vertex 2 = data[1], vertex 3 = data[2]
			case (Glyph::TriangleGlyph):
			case (Glyph::QuadGlyph):
				r2 = g->data(1)->vector();
				r3 = g->data(2)->vector();
				g->data(0)->copyColour(colour_i);
				g->data(1)->copyColour(colour_j);
				g->data(2)->copyColour(colour_k);
				// Work out normal
				r4 = (r2 - r1) * (r3 - r1);
				r4.normalise();
				if (g->isSolid())
				{
					if ((colour_i[3] < 0.99f) || (colour_j[3] < 0.99f) || (colour_k[3] < 0.99f)) ts = RenderEngine::TransparentTriangle;
					else ts = RenderEngine::SolidTriangle;
				}
				else ts = RenderEngine::WireTriangle;
				glyphTriangles_[ts].defineVertex(r1.x, r1.y, r1.z, r4.x, r4.y, r4.z, colour_i);
				glyphTriangles_[ts].defineVertex(r2.x, r2.y, r2.z, r4.x, r4.y, r4.z, colour_j);
				glyphTriangles_[ts].defineVertex(r3.x, r3.y, r3.z, r4.x, r4.y, r4.z, colour_k);
				if (g->isSelected())
				{
					glyphTriangles_[RenderEngine::WireTriangle].defineVertex(r1.x, r1.y, r1.z, r4.x, r4.y, r4.z, textcolour);
					glyphTriangles_[RenderEngine::WireTriangle].defineVertex(r2.x, r2.y, r2.z, r4.x, r4.y, r4.z, textcolour);
					glyphTriangles_[RenderEngine::WireTriangle].defineVertex(r3.x, r3.y, r3.z, r4.x, r4.y, r4.z, textcolour);
				}
				if (g->type() == Glyph::QuadGlyph)
				{
					r4 = g->data(3)->vector();
					g->data(3)->copyColour(colour_l);
					// Work out normal
					r2 = -(r1 - r3) * (r4 - r3);
					r2.normalise();
					if (g->isSolid())
					{
						if ((colour_i[3] < 0.99f) || (colour_k[3] < 0.99f) || (colour_l[3] < 0.99f)) ts = RenderEngine::TransparentTriangle;
						else ts = RenderEngine::SolidTriangle;
					}
					else ts = RenderEngine::WireTriangle;
					glyphTriangles_[ts].defineVertex(r1.x, r1.y, r1.z, r2.x, r2.y, r2.z, colour_i);
					glyphTriangles_[ts].defineVertex(r3.x, r3.y, r3.z, r2.x, r2.y, r2.z, colour_k);
					glyphTriangles_[ts].defineVertex(r4.x, r4.y, r4.z, r2.x, r2.y, r2.z, colour_l);
					if (g->isSelected())
					{
						glyphTriangles_[RenderEngine::WireTriangle].defineVertex(r1.x, r1.y, r1.z, r2.x, r2.y, r2.z, textcolour);
						glyphTriangles_[RenderEngine::WireTriangle].defineVertex(r3.x, r3.y, r3.z, r2.x, r2.y, r2.z, textcolour);
						glyphTriangles_[RenderEngine::WireTriangle].defineVertex(r4.x, r4.y, r4.z, r2.x, r2.y, r2.z, textcolour);
					}
				}
				break;
			// Text in 2D coordinates - left-hand origin = data[0]
			case (Glyph::TextGlyph):
				renderTextPrimitive(r1.x, canvas->contextHeight()-r1.y, g->text());
				break;
			// Text in 3D coordinates - left-hand origin = data[0]
			case (Glyph::Text3DGlyph):
				renderTextPrimitive(r1, g->text());
				break;
			// Tube arrow - tail = data[0], head = data[1]
			case (Glyph::TubeArrowGlyph):
				r2 = g->data(1)->vector();
				g->data(0)->copyColour(colour_i);
				// Draw cylinder body and arrowhead
				A = baseTransform;
				A.applyTranslation(r1);
				r3 = r2-r1;
				rij = r3.magnitude();
				phi = DEGRAD * acos(r3.z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r3.y, r3.x, 0.0, phi, TRUE);
				// Draw cylinder
				A.applyScaling(0.1,0.1,rij*0.9);
				renderPrimitive(cylinders_, lod, colour_i, A, g->isSolid() ? GL_FILL : GL_LINE);
				// Move to endpoint
				A.applyTranslation(0.0,0.0,1.0);
				A.applyScaling(2.0,2.0,0.1/0.9);	// BROKEN Not sure this is right!
				renderPrimitive(cones_, lod, colour_i, A, g->isSolid() ? GL_FILL : GL_LINE);
				break;
			// Vector - tail = data[0], head = data[1]
			case (Glyph::VectorGlyph):
				r2 = g->data(1)->vector();
				r1 -= r2*0.5;
				g->data(0)->copyColour(colour_i);
				glLineWidth(g->lineWidth());
				glColor4fv(colour_i);
				// Draw simple line from tail to head points
				glBegin(GL_LINES);
				glVertex3d(r1.x, r1.y, r1.z);
				glVertex3d(r1.x+r2.x, r1.y+r2.y, r1.z+r2.z);
				glEnd();
				// Draw cylinder arrowhead in wireframe
				A = baseTransform;
				A.applyTranslation(r1);
				rij = r2.magnitude();
				phi = DEGRAD * acos(r2.z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r2.y, r2.x, 0.0, phi, TRUE);
				// Move to endpoint
				A.applyTranslation(0.0,0.0,rij*0.9);
				A.applyScaling(0.2, 0.2, rij*0.1);
				renderPrimitive(cones_, lod, colour_i, A, GL_LINE);
				break;
		}
	}
}

// Render additional model information (measurements etc.)
void RenderEngine::renderModelOverlays(Model *source, Matrix baseTransform, TCanvas *canvas)
{
	Vec3<double> r1, r2, r3, r4, pos, rji, rjk;
	Vec4<double> screenr;
	Matrix A;
	double gamma, t;
	Atom **atoms;

	// Clear depth buffer to force lines on top of existing primitives,
	glDisable(GL_DEPTH_TEST);
	
	// Measurements
	// Apply standard transformation matrix to OpenGL so we may just use local atom positions for vertices
	if (prefs.isVisibleOnScreen(Prefs::ViewMeasurements))
	{
		// Load model transformation matrix
		glLoadIdentity();
		glMultMatrixd(baseTransform.matrix());
		
		// Distances
		for (Measurement *m = source->distanceMeasurements(); m != NULL; m = m->next)
		{
			atoms = m->atoms();
			// Check that all atoms involved in the measurement are visible (i.e. not hidden)
			if (atoms[0]->isHidden() || atoms[1]->isHidden()) continue;
			r1 = atoms[0]->r();
			r2 = atoms[1]->r();
			glBegin(GL_LINES);
			glVertex3d(r1.x, r1.y, r1.z);
			glVertex3d(r2.x, r2.y, r2.z);
			glEnd();
			renderTextPrimitive((r1+r2)*0.5, ftoa(m->value(), prefs.distanceLabelFormat()), 0x212b);
		}
		
		// Angles
		for (Measurement *m = source->angleMeasurements(); m != NULL; m = m->next)
		{
			atoms = m->atoms();
			// Check that all atoms involved in the measurement are visible (i.e. not hidden)
			if (atoms[0]->isHidden() || atoms[1]->isHidden() || atoms[2]->isHidden()) continue;
			r1 = atoms[0]->r();
			r2 = atoms[1]->r();
			r3 = atoms[2]->r();
			glBegin(GL_LINE_STRIP);
			glVertex3d(r1.x, r1.y, r1.z);
			glVertex3d(r2.x, r2.y, r2.z);
			glVertex3d(r3.x, r3.y, r3.z);
			glEnd();
			// Curved angle marker
			rji = (r1 - r2);
			rjk = (r3 - r2);
			rji.normalise();
			rjk.normalise();
			gamma = acos(rji.dp(rjk));
			// Draw segments
			t = 0.0;
			glBegin(GL_LINES);
			for (int n=0; n<11; n++)
			{
				pos = rji * (sin((1.0-t)*gamma) / sin(gamma)) + rjk * (sin(t*gamma) / sin(gamma));  // OPTIMIZE!
				pos *= 0.2;
				pos += r2;
				glVertex3d(pos.x, pos.y, pos.z);
				t += 0.1;
				// Store text position
				if (n == 5) r4 = pos;
			}
			glEnd();
			// Determine left or right-alignment of text
			// OPTIMIZE - can we just multiply by modelTransformationMatrix_ here? We don't care about the exact projection....
			modelToWorld(r2, &screenr);
			gamma = screenr.x;
			modelToWorld(r4, &screenr);
			renderTextPrimitive(r4, ftoa(m->value(), prefs.angleLabelFormat()), 176, gamma > screenr.x);
		}
		
		// Torsions
		for (Measurement *m = source->torsionMeasurements(); m != NULL; m = m->next)
		{
			atoms = m->atoms();
			// Check that all atoms involved in the measurement are visible (i.e. not hidden)
			if (atoms[0]->isHidden() || atoms[1]->isHidden() || atoms[2]->isHidden() || atoms[3]->isHidden()) continue;
			r1 = atoms[0]->r();
			r2 = atoms[1]->r();
			r3 = atoms[2]->r();
			r4 = atoms[3]->r();
			glBegin(GL_LINE_STRIP);
			glVertex3d(r1.x, r1.y, r1.z);
			glVertex3d(r2.x, r2.y, r2.z);
			glVertex3d(r3.x, r3.y, r3.z);
			glVertex3d(r4.x, r4.y, r4.z);
			glEnd();
			renderTextPrimitive((r2+r3)*0.5, ftoa(m->value(), prefs.angleLabelFormat()), 176);
		}
	}
	
	// If glyphs window is visible, highlight selected glyph(s)
	
	// Re-enable depth buffer
	glEnable(GL_DEPTH_TEST);
}

// Render text objects (with supplied QPainter)
void RenderEngine::renderText(QPainter &painter, TCanvas *canvas)
{
	textPrimitives_.renderAll(painter, canvas);
}

// Render 3D
void RenderEngine::render3D(Model *source, TCanvas *canvas)
{
	Matrix A;
	Atom *i, *j, tempj;
	GLfloat colour[4], colour_j[4];
	Atom::DrawStyle style_i, style_j;
	Vec3<double> pos, rmouse, v;
	double radius_i, radius_j;
	Dnchar text;
	Fragment *frag;
	
	// Clear filtered primitive lists
	solidPrimitives_.clear();
	transparentPrimitives_.clear();
	textPrimitives_.forgetAll();
	glyphTriangles_[RenderEngine::SolidTriangle].forgetAll();
	glyphTriangles_[RenderEngine::TransparentTriangle].forgetAll();
	glyphTriangles_[RenderEngine::WireTriangle].forgetAll();
	
	// Set initial transformation matrix, including any translation occurring from cell...
	setTransformationMatrix(source->modelViewMatrix(), source->cell()->centre());
	
	// Set target matrix mode and reset it, and set colour mode
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);
	
	// Render rotation globe in small viewport in lower right-hand corner
	if (prefs.isVisibleOnScreen(Prefs::ViewGlobe))
	{
		int n = prefs.globeSize();
		glViewport(canvas->contextWidth()-n,0,n,n);
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glMultMatrixd(globeProjectionMatrix_.matrix());
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		A = modelTransformationMatrix_;
		A.removeTranslationAndScaling();
		A[14] = -1.2;
		glMultMatrixd(A.matrix());
		prefs.copyColour(Prefs::GlobeColour, colour);
		glColor4fv(colour);
		rotationGlobe_.sendToGL();
		prefs.copyColour(Prefs::GlobeAxesColour, colour);
		glColor4fv(colour);
		rotationGlobeAxes_.sendToGL();
	}
	
	// Prepare for model rendering
	glViewport(0, 0, canvas->contextWidth(), canvas->contextHeight());
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glMultMatrixd(modelProjectionMatrix_.matrix());
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	
	// Draw main model
	renderModel(source, modelTransformationMatrix_, canvas);
	
	// Draw on the selection highlights (for atoms in canvas.subsel)
	for (Refitem<Atom,int> *ri = canvas->pickedAtoms(); ri != NULL; ri = ri->next)
	{
		i = ri->item;
		// Move to local atom position
		A = modelTransformationMatrix_;
		A.applyTranslation(i->r());
		prefs.copyColour(Prefs::TextColour, colour);
		// Draw a wireframe sphere at the atoms position
		style_i = (prefs.renderStyle() == Atom::IndividualStyle ? i->style() : prefs.renderStyle());
		switch (style_i)
		{
			case (Atom::StickStyle):
			case (Atom::TubeStyle):
				renderPrimitive(selectedAtom_[Atom::TubeStyle], 0, colour, A, GL_LINE);
				break;
			case (Atom::SphereStyle):
				renderPrimitive(selectedAtom_[Atom::SphereStyle], 0, colour, A, GL_LINE);
				break;
			case (Atom::ScaledStyle):
				renderPrimitive(selectedScaledAtom_[i->element()], 0, colour, A, GL_LINE);
				break;
		}
	}
	
	
	// Active user actions
	i = canvas->atomClicked();
	switch (canvas->activeMode())
	{
		// Draw on bond and new atom for chain drawing (if mode is active)
		case (UserAction::DrawChainAction):
			if (i == NULL) break;
			pos = i->r();
			rmouse = canvas->rMouseLast();
			style_i = (prefs.renderStyle() == Atom::IndividualStyle ? i->style() : prefs.renderStyle());
			radius_i = (style_i == Atom::TubeStyle ? 0.0 : prefs.screenRadius(i)*0.85);
			// We need to project a point from the mouse position onto the canvas plane, unless the mouse is over an existing atom in which case we snap to its position instead
			j = source->atomOnScreen(rmouse.x, rmouse.y);
			if (j == NULL)
			{
				j = &tempj;
				v = screenToModel(rmouse.x, rmouse.y, canvas->currentDrawDepth());
				style_j = (prefs.renderStyle() == Atom::IndividualStyle ? Atom::StickStyle : prefs.renderStyle());
				radius_j = prefs.bondStyleRadius(prefs.renderStyle());
			}
			else
			{
				v = j->r();
				style_j = (prefs.renderStyle() == Atom::IndividualStyle ? j->style() : prefs.renderStyle());
				radius_j = (style_i == Atom::TubeStyle ? 0.0 : prefs.screenRadius(j)*0.85);
			}
			v -= pos;
			
			// Select colour
			if (i->isPositionFixed()) prefs.copyColour(Prefs::FixedAtomColour, colour);
			else switch (prefs.colourScheme())
			{
				case (Prefs::ElementScheme):
					elements().copyColour(i->element(), colour);
					break;
				case (Prefs::ChargeScheme):
					prefs.colourScale[0].colour(i->charge(), colour);
					break;
				case (Prefs::VelocityScheme):
					prefs.colourScale[1].colour(i->v().magnitude(), colour);
					break;
				case (Prefs::ForceScheme):
					prefs.colourScale[2].colour(i->f().magnitude(), colour);
					break;
				case (Prefs::CustomScheme):
					i->copyColour(colour);
					break;
				default:
					break;
			}
			elements().copyColour(canvas->sketchElement(), colour_j);
			
			// Construct transformation matrix to centre on original (first) atom
			A = modelTransformationMatrix_;
			A.applyTranslation(pos);
			
			// Render new (temporary) bond
			renderBond(A, v, i, style_i, colour, radius_i, j, style_j, colour_j, radius_j, Bond::Single, 0, prefs.selectionScale());
			
			// Draw text showing distance
			text.sprintf("r = %f ", v.magnitude());
			renderTextPrimitive(rmouse.x, canvas->contextHeight()-rmouse.y, text.get(), 0x212b);
			break;
	}
	
	// Selected user mode actions
	i = canvas->atomClicked();
	switch (canvas->selectedMode())
	{
		// Draw on fragment (as long as mode is selected)
		case (UserAction::DrawFragmentAction):
			if (gui.fragmentWindow->currentFragment() == NULL) break;
			frag = gui.fragmentWindow->currentFragment();
			j = source->atomOnScreen(canvas->rMouseLast().x, canvas->rMouseLast().y);
			if ((i != NULL) || (j != NULL))
			{
				// Atom is now fragment anchor point - make sure we select a non-null atom i or j
				if (i != NULL) j = i;
				pos = j->r();
				Model *m = frag->anchoredModel(j, canvas->keyModifier(Prefs::ShiftKey), gui.fragmentWindow->bondId());

				A = modelTransformationMatrix_;
				A.applyTranslation(pos);	
				// Did we find a valid anchor point?
				if (m != NULL) renderModel(m, A, canvas);
				else
				{
					prefs.copyColour(Prefs::TextColour, colour);
					renderPrimitive(&crossedCube_, FALSE, colour, A, GL_LINE, 2.0);
				}
			}
			else
			{
				// No atom under the moust pointer, so draw on at the prefs drawing depth in its current orientation
				// Get drawing point origin, translate to it, and render the stored model
				if (canvas->activeMode() == UserAction::DrawFragmentAction) pos = screenToModel(canvas->rMouseDown().x, canvas->rMouseDown().y, prefs.drawDepth());
				else pos = screenToModel(canvas->rMouseLast().x, canvas->rMouseLast().y, prefs.drawDepth());
				A = modelTransformationMatrix_;
				A.applyTranslation(pos);
				renderModel(frag->orientedModel(), A, canvas);
			}
			break;
	}

	// All 3D primitive objects have now been filtered, so add triangles, then sort and send to GL
	renderPrimitive(&glyphTriangles_[RenderEngine::SolidTriangle], FALSE, NULL, modelTransformationMatrix_);
	renderPrimitive(&glyphTriangles_[RenderEngine::WireTriangle], FALSE, NULL, modelTransformationMatrix_, GL_LINE);
	renderPrimitive(&glyphTriangles_[RenderEngine::TransparentTriangle], TRUE, NULL, modelTransformationMatrix_);
	sortAndSendGL();
	
	// Render overlays
	renderModelOverlays(source, modelTransformationMatrix_, canvas);
}
