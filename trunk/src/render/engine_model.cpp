/*
	*** Model Rendering
	*** src/render/engine_model.cpp
	Copyright T. Youngs 2007-2011

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
#include "base/pattern.h"
#include "gui/tcanvas.uih"
#include "base/sysfunc.h"

// Render bond
void RenderEngine::renderBond(Matrix A, Vec3<double> vij, Atom *i, Atom::DrawStyle style_i, GLfloat *colour_i, double radius_i, Atom *j, Atom::DrawStyle style_j, GLfloat *colour_j, double radius_j, Bond::BondType bt, double selscale, Bond *b)
{
	double dvisible, selvisible, factor, rij, phi;
	Vec3<double> ri, rj, localx, localy, localz, stickpos;
	GLfloat alpha_i, alpha_j;

	// Store copies of alpha values
	alpha_i = colour_i[3];
	alpha_j = colour_j[3];
	
	localz = vij;
	rij = localz.magAndNormalise();
	
	// If bond is not visible, don't bother drawing it...
	dvisible = 0.5 * (rij - radius_i - radius_j);
	if (dvisible < 0.0) return;
	selvisible = 0.5 * (rij - selscale*radius_i - selscale*radius_j);

	// Determine bond plane rotation if a multiple bond
	if (((bt == Bond::Double) || (bt == Bond::Triple)) && (b != NULL))
	{
		// Desired z-axis is already known ( = vijn)
		// X axis is bond plane vector
		// Y axis is xp of the two
		localx = i->findBondPlane(j,b,localz);
		localx = A.rotateVector(localx);
		localz = A.rotateVector(localz);
		localy = localx * localz;
		A[0] = localx.x;
		A[1] = localx.y;
		A[2] = localx.z;
		A[4] = localy.x;
		A[5] = localy.y;
		A[6] = localy.z;
		A[8] = localz.x;
		A[9] = localz.y;
		A[10] = localz.z;
	}
	else
	{
		// Calculate dot product with vector {0,0,1}
		phi = DEGRAD * acos(localz.z);
		
		// Special case where the bond is exactly along Z already
		if (phi > 179.99) A.applyRotationX(phi);
		else if (phi >= 0.01) A.applyRotationAxis(-vij.y, vij.x, 0.0, phi, TRUE);
	}
	// We can perform an initial translation to the 'edge' of atom i, and scale to visible bond length
	A.applyTranslationZ(radius_i);
	A.applyScalingZ(dvisible);
	
	// Draw first bond half
	switch (style_i)
	{
		case (Atom::StickStyle):
			if (!rebuildSticks_) break;
			// First vertex is at 0,0,0 (i.e. translation elements of A). Second is vij * (0,0,1)
			stickpos = A * Vec3<double>(0.0,0.0,1.0);
			if (i->isSelected())
			{
				stickSelectedLines_.defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_i, FALSE);
				stickSelectedLines_.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_i, FALSE);
			}
			else 
			{
				stickLines_.defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_i, FALSE);
				stickLines_.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_i, FALSE);
			}
			// Move to centre of visible bond, ready for next bond half
			A.applyTranslationZ(1.0);
			break;
		case (Atom::TubeStyle):
			renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].bonds_[style_i][bt], colour_i, A);
			if (i->isSelected() && (selvisible > 0.0))
			{
				colour_i[3] = 0.5f;
				renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].selectedBonds_[style_i][bt], colour_i, A);
				colour_i[3] = alpha_i;
			}
			// Move to centre of visible bond, ready for next bond half
			A.applyTranslationZ(1.0);
			break;
		case (Atom::SphereStyle):
		case (Atom::ScaledStyle):
			renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].bonds_[style_i][bt], colour_i, A);
			if (i->isSelected() && (selvisible > 0.0))
			{
				// Move to edge of selected atom and apply selection bond scaling
				A.applyTranslationZ((selscale*radius_i-radius_i) / dvisible);
				A.applyScalingZ(selvisible/dvisible);
				colour_i[3] = 0.5f;
				renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].selectedBonds_[style_i][bt], colour_i, A);
				colour_i[3] = alpha_i;
				// Move to centrepoint and reverse scaling back to 'dvisible'
				A.applyTranslationZ(1.0);
				A.applyScalingZ(dvisible/selvisible);
			}
			else A.applyTranslationZ(1.0);
			break;
	}
	
	// Draw second bond half
	switch (style_j)
	{
		case (Atom::StickStyle):
			if (!rebuildSticks_) break;
			// First vertex is *still* at 0,0,0 (i.e. translation elements of A). Second is vij * (0,0,1)
			stickpos = A * Vec3<double>(0.0,0.0,1.0);
			if (j->isSelected())
			{
				stickSelectedLines_.defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_j, FALSE);
				stickSelectedLines_.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_j, FALSE);
			}
			else 
			{
				stickLines_.defineVertex(A[12], A[13], A[14], 0.0,0.0,1.0, colour_j, FALSE);
				stickLines_.defineVertex(stickpos.x, stickpos.y, stickpos.z, 0.0,0.0,1.0, colour_j, FALSE);
			}
			break;
		case (Atom::TubeStyle):
			renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].bonds_[style_j][bt], colour_j, A);
			if (j->isSelected())
			{
				colour_j[3] = 0.5f;
				renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].selectedBonds_[style_j][bt], colour_j, A);
				colour_j[3] = alpha_j;
			}
			break;
		case (Atom::SphereStyle):
		case (Atom::ScaledStyle):
			renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].bonds_[style_j][bt], colour_j, A);
			if (j->isSelected() && (selvisible > 0.0))
			{
				A.applyScalingZ(selvisible / dvisible);
				colour_j[3] = 0.5f;
				renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].selectedBonds_[style_j][bt], colour_j, A);
				colour_j[3] = alpha_j;
			}
			break;
	}
}

// Render basic model information (atoms, bonds, labels, and glyphs)
void RenderEngine::renderModel(Model *source, Matrix basetransform)
{
	msg.enter("RenderEngine::renderModel");
	GLfloat colour_i[4], colour_j[4], alpha_i, colour_k[4], colour_l[4];
	GLenum style;
	int id_i, labels, m, n;
	Dnchar text;
	double selscale, z, radius_i, radius_j, phi, mag, best;
	Atom *i, *j, **atoms;
	Vec3<double> pos, v, ijk, r1, r2, r3, r4;
	Vec4<double> screenr;
	Matrix atomtransform, A, B;
	Refitem<Bond,int> *rb;
	Refitem<Atom,int> *ra;
	Atom::DrawStyle style_i, style_j, globalstyle;
	Prefs::ColouringScheme scheme;
	ForcefieldAtom *ffa;
	
	// Grab global style values
	scheme = prefs.colourScheme();
	globalstyle = prefs.renderStyle();
	selscale = prefs.selectionScale();
	
	// Atoms and Bonds
	atoms = source->atomArray();
	for (n = 0; n<source->nAtoms(); ++n)
	{
		// Get atom pointer
		i = atoms[n];
		
		// Skip hidden atoms
		if (i->isHidden()) continue;
		
		// Grab atom coordinate - we'll need it a lot
		pos = i->r();

		// Move to local atom position
		atomtransform = basetransform;
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
				if (i->nBonds() == 0)
				{
					if (i->isSelected()) renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].atoms_[style_i], colour_i, atomtransform, 3.0);
					else renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].atoms_[style_i], colour_i, atomtransform, GL_LINE, 1.0);
				}
				break;
			case (Atom::TubeStyle):
			case (Atom::SphereStyle):
				renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].atoms_[style_i], colour_i, atomtransform);
				if (i->isSelected())
				{
					colour_i[3] = 0.5f;
					renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].selectedAtoms_[style_i], colour_i, atomtransform);
					colour_i[3] = alpha_i;
				}
				break;
			case (Atom::ScaledStyle):
				renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].scaledAtoms_[i->element()], colour_i, atomtransform);
				if (i->isSelected())
				{
					colour_i[3] = 0.5f;
					renderPrimitive(RenderEngine::AtomSelectionObject, primitives_[Q_].selectedScaledAtoms_[i->element()], colour_i, atomtransform);
					colour_i[3] = alpha_i;
				}
				break;
		}

		// Labels
		labels = i->labels();
		if (labels != 0)
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
			
			// primitives_[Q_]. text object
			r2 = source->modelToWorld(i->r(), &screenr);
			if (r2.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, text.get());
		}
		
		// Bonds
		// Grab some useful values from atom i
		id_i = i->id();
		if (style_i <= Atom::TubeStyle) radius_i = 0.0;
		else if (style_i == Atom::ScaledStyle) radius_i = prefs.styleRadius(i) - scaledAtomAdjustments_[i->element()];
		else radius_i = prefs.styleRadius(i) - sphereAtomAdjustment_;
		
		for (rb = i->bonds(); rb != NULL; rb = rb->next)
		{
			j = rb->item->partner(i);
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
			if (style_j <= Atom::TubeStyle) radius_j = 0.0;
			else if (style_j == Atom::ScaledStyle) radius_j = prefs.styleRadius(j) - scaledAtomAdjustments_[j->element()];
			else radius_j = prefs.styleRadius(j) - sphereAtomAdjustment_;
			
			// Calculate vector i->j
			v = source->cell()->mimd(j, i);
			
			// Render bond
			renderBond(atomtransform, v, i, style_i, colour_i, radius_i, j, style_j, colour_j, radius_j, rb->item->type(), selscale, rb->item);
		}
		
	}
	
	// Aromatic rings (needs valid pattern description)
	if (source->arePatternsValid())
	{
		prefs.copyColour(Prefs::AromaticRingColour, colour_i);

		atoms = source->atomArray();
		for (Pattern *p = source->patterns(); p != NULL; p = p->next)
		{
			// Continue if no rings present in pattern
			if (p->rings() == NULL) continue;

			// Cycle over rings
			for (Ring *r = p->rings(); r != NULL; r = r->next)
			{
				if (r->type() != Ring::AromaticRing) continue;
				
				// Loop over pattern molecules
				id_i = 0;
				for (m=0; m<p->nMolecules(); ++m)
				{
					// Determine ring centroid
					ra = r->atoms();
					i = atoms[id_i+ra->item->id()];
					r1 = i->r();
					pos = r1;
					for (ra = ra->next; ra != NULL; ra = ra->next)
					{
						j = atoms[id_i+ra->item->id()];
						pos += source->cell()->mim(j->r(), r1);
					}
					pos /= r->nAtoms();

					// Determine average vector magnitude of ring atoms and y-axis vector (closest 90deg vector)
					mag = 0.0;
					r1 -= pos;
					r1.normalise();
					best = PI/2.0;
					r2.zero();
					radius_i = 0.0;
					
					for (ra = r->atoms(); ra != NULL; ra = ra->next)
					{
						// Grab atom pointer and get minimum image vector with centroid 'v'
						i = atoms[id_i+ra->item->id()];
						if (prefs.styleRadius(i) > radius_i) radius_i = prefs.styleRadius(i);
						v = source->cell()->mimd(i->r(), pos);
						// Accumulate magnitude
						mag += v.magnitude();
						v.normalise();
						phi = fabs(PI/2.0 - acos(r1.dp(v)));
						if (phi < best)
						{
							r2 = v;
							best = phi;
						}
					}

					// Finalise values and create z-vector from cross product
					r2.orthogonalise(r1);
					r2.normalise();
					r3 = r1*r2;
					mag /= r->nAtoms();
					mag -= radius_i*0.9;
					mag *= 0.75;
					// Construct transformation matrix
					atomtransform = basetransform;
					atomtransform.applyTranslation(pos.x, pos.y, pos.z);
					A.setColumn(0, r1*mag, 0.0);
					A.setColumn(1, r2*mag, 0.0);
					A.setColumn(2, r3*mag, 0.0);
					A.setColumn(3, 0.0, 0.0, 0.0, 1.0);
					atomtransform *= A;

					// Render ring
					if (prefs.renderDashedAromatics())
					{
						if (globalstyle == Atom::StickStyle) renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].segmentedLineRings_, colour_i, atomtransform);
						else renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].segmentedTubeRings_, colour_i, atomtransform);
					}
					else
					{
						if (globalstyle == Atom::StickStyle) renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].lineRings_, colour_i, atomtransform);
						else renderPrimitive(RenderEngine::BasicObject, primitives_[Q_].tubeRings_, colour_i, atomtransform);
					}

					id_i += p->nAtoms();
				}
			}
		}
	}
	
	// Hydrogen bonds
	atoms = source->atomArray();
	for (n = 0; n<source->nAtoms()-1; ++n)
	{
		// Get atom pointer
		i = atoms[n];
		
		// Skip hidden atoms
		if (i->isHidden()) continue;
		
		// Skip non-hydrogen atoms
		if (i->element() != 1) continue;
		
		// Grab atom coordinate - we'll need it a lot
		pos = i->r();

		// Move to local atom position
		atomtransform = basetransform;
		atomtransform.applyTranslation(pos.x, pos.y, pos.z);
		
		// Loop over other atoms
		for (m = n; m<source->nAtoms(); ++m)
		{
			j = atoms[m];

			// Skip hidden atoms
			if (i->isHidden()) continue;
			
			// Get distance i-j
// 			v = source->mimd(i,j);
			
			// Get element types
		}
	}
	msg.exit("RenderEngine::renderModel");
}

// Render model cell
void RenderEngine::renderCell(Model *source)
{
	if (source->cell()->type() == UnitCell::NoCell) return;
	msg.enter("RenderEngine::renderCell");
	GLfloat colour[4];
	prefs.copyColour(Prefs::UnitCellColour, colour);
	glColor4fv(colour);
	Matrix A = source->modelViewMatrix() * source->cell()->axes();
	glMultMatrixd(A.matrix());
	primitives_[Q_].wireCube_.sendToGL();
	glTranslated(-0.5, -0.5, -0.5);
	Vec3<double> v = source->cell()->lengths();
	glScaled(1.0 / v.x, 1.0 / v.y, 1.0 / v.z);
	prefs.copyColour(Prefs::UnitCellAxesColour, colour);
	glColor4fv(colour);
	primitives_[Q_].cellAxes_.sendToGL();
	msg.exit("RenderEngine::renderCell");
}

// Render grids
void RenderEngine::renderGrids(Model *source)
{
	msg.enter("RenderEngine::renderGrids");
	Matrix A;
	GLenum style;
	GLfloat colour[4], textcolour[4];
	
	// Copy text colour
	prefs.copyColour(Prefs::TextColour, textcolour);
	
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
		A.setIdentity();
		A.applyTranslation(g->origin());
		A.multiplyRotation(g->axes());
		if (g->style() == Grid::TriangleSurface) style = GL_LINE;
		else if (g->style() == Grid::SolidSurface) style = GL_FILL;
		else style = GL_POINTS;
		if (g->useColourScale()) renderPrimitive(RenderEngine::GridObject, &gp->primaryPrimitive(), gp->primaryIsTransparent(), NULL, A, style);
		else
		{
			g->copyPrimaryColour(colour);
			renderPrimitive(RenderEngine::GridObject, &gp->primaryPrimitive(), gp->primaryIsTransparent(), colour, A, style);
		}
		if (g->useSecondary())
		{
			if (g->useColourScale()) renderPrimitive(RenderEngine::GridObject, &gp->secondaryPrimitive(), gp->secondaryIsTransparent(), NULL, A, style);
			else
			{
				g->copySecondaryColour(colour);
				renderPrimitive(RenderEngine::GridObject, &gp->secondaryPrimitive(), gp->secondaryIsTransparent(), colour, A, style);
			}
		}
		// Render volume outline
		if (g->outlineVolume())
		{
			A.columnMultiply(0, g->nPoints().x);
			A.columnMultiply(1, g->nPoints().y);
			A.columnMultiply(2, g->nPoints().z);
			renderPrimitive(RenderEngine::GridObject, primitives_[Q_].originCubes_, textcolour, A, GL_LINE, 1.0);
		}
	}
	msg.exit("RenderEngine::renderGrids");
}

// Render glyphs
void RenderEngine::renderGlyphs(Model *source)
{
	msg.enter("RenderEngine::renderGlyphs");
	Matrix A, B;
	Vec3<double> r1, r2, r3, r4;
	RenderEngine::TriangleStyle ts;
	GLfloat colour_i[4], colour_j[4], colour_k[4], colour_l[4], textcolour[4];
	double phi, rij;

	// Copy text colour (for highlighint selected glyphs)
	prefs.copyColour(Prefs::TextColour, textcolour);
	
	for (Glyph *g = source->glyphs(); g != NULL; g = g->next)
	{
		// Check if glyph is visible
		if (!g->isVisible()) continue;
		
		// Grab first coordinate (always used)
		r1 = g->data(0)->vector();
		
		switch (g->type())
		{
			// Arrow - tail = data[0], head = data[1]
			case (Glyph::ArrowGlyph):
				r2 = g->data(1)->vector();
				g->data(0)->copyColour(colour_i);
				glColor4fv(colour_i);
				// Draw simple line from tail to head points
				glBegin(GL_LINES);
				glVertex3d(r1.x, r1.y, r1.z);
				glVertex3d(r2.x, r2.y, r2.z);
				glEnd();
				// Draw cylinder arrowhead in wireframe
				A.setIdentity();
				A.applyTranslation(r1);
				r3 = r2-r1;
				rij = r3.magnitude();
				phi = DEGRAD * acos(r3.z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r3.y, r3.x, 0.0, phi, TRUE);
				// Move to endpoint
				A.applyTranslationZ(rij*0.9);
				A.applyScaling(0.2,0.2,rij*0.1);
				renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].cones_, colour_i, A, GL_LINE);
				break;
			// Line - start = data[0], end = data[1]
			case (Glyph::LineGlyph):
				r2 = g->data(1)->vector();
				g->data(0)->copyColour(colour_i);
				glyphLines_.defineVertex(r1.x, r1.y, r1.z, 0.0, 0.0, 1.0, colour_i[0], colour_i[1], colour_i[2], colour_i[3], FALSE);
				glyphLines_.defineVertex(r2.x, r2.y, r2.z, 0.0, 0.0, 1.0, colour_i[0], colour_i[1], colour_i[2], colour_i[3], FALSE);
				break;
			// Sphere or Cube - centre = data[0], scale = data[1]
			case (Glyph::SphereGlyph):
			case (Glyph::CubeGlyph):
				r2 = g->data(1)->vector();
				g->data(0)->copyColour(colour_i);
				A.setIdentity();
				A.applyTranslation(r1.x, r1.y, r1.z);
				if (g->rotated()) A *= (*g->matrix());
				A.applyScaling(r2.x, r2.y, r2.z);
				if (g->isSolid())
				{
					if (g->type() == Glyph::SphereGlyph) renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].spheres_, colour_i, A, GL_FILL);
					else renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].cubes_, colour_i, A, GL_FILL);
					if (g->isSelected())
					{
						if (g->type() == Glyph::SphereGlyph) renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].spheres_, textcolour, A, GL_LINE, 2.0);
						else renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].cubes_, textcolour, A, GL_LINE, 2.0);
					}
				}
				else
				{
					if (g->isSelected())
					{
						if (g->type() == Glyph::SphereGlyph) renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].spheres_, textcolour, A, GL_LINE, 3.0);
						else renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].cubes_, textcolour, A, GL_LINE, 3);
					}
					else
					{
						if (g->type() == Glyph::SphereGlyph) renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].spheres_, colour_i, A, GL_LINE, 1.0);
						else renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].cubes_, colour_i, A, GL_LINE, 1.0);
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
			// Ellipsoid - centre = data[0], edge vector = data[1], face vector = data[2]
			// EllipsoidXYZ - centre = data[0], X = data[1], Y = data[2], Z= data[3]
			case (Glyph::EllipsoidGlyph):
			case (Glyph::EllipsoidXYZGlyph):
				g->data(0)->copyColour(colour_i);
				A.setIdentity();
				A.applyTranslation(r1.x, r1.y, r1.z);
				r2 = g->data(1)->vector() - r1;
				r3 = g->data(2)->vector() - r1;
				if (g->type() == Glyph::EllipsoidXYZGlyph) r4 = g->data(3)->vector() - r1;
				else
				{
					r4 = r2 * r3;
					r4.normalise();
				}
				B.setColumn(0, r2, 0.0);
				B.setColumn(1, r3, 0.0);
				B.setColumn(2, r4, 0.0);
				A.multiplyRotation(B);
				if (g->isSolid())
				{
					renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].spheres_, colour_i, A, GL_FILL);
					if (g->isSelected()) renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].spheres_, textcolour, A, GL_LINE, 2.0);
				}
				else
				{
					if (g->isSelected()) renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].spheres_, textcolour, A, GL_LINE, 3.0);
					else renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].spheres_, colour_i, A, GL_LINE, 1.0);
				}
				break;
			// Text - handled in RenderEngine::renderTextGlyphs()
			case (Glyph::TextGlyph):
			case (Glyph::Text3DGlyph):
				break;
			// Tube arrow - tail = data[0], head = data[1]
			case (Glyph::TubeArrowGlyph):
				r2 = g->data(1)->vector();
				g->data(0)->copyColour(colour_i);
				// Draw cylinder body and arrowhead
				A.setIdentity();
				A.applyTranslation(r1);
				r3 = r2-r1;
				rij = r3.magnitude();
				phi = DEGRAD * acos(r3.z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r3.y, r3.x, 0.0, phi, TRUE);
				// Draw cylinder
				A.applyScaling(0.1,0.1,rij*0.9);
				renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].cylinders_, colour_i, A, g->isSolid() ? GL_FILL : GL_LINE);
				// Move to endpoint
				A.applyTranslationZ(1.0);
				A.applyScaling(2.0,2.0,0.1/0.9);
				renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].cones_, colour_i, A, g->isSolid() ? GL_FILL : GL_LINE);
				break;
			// Vector - tail = data[0], head = data[1]
			case (Glyph::VectorGlyph):
				r2 = g->data(1)->vector();
				r1 -= r2*0.5;
				g->data(0)->copyColour(colour_i);
				glColor4fv(colour_i);
				// Draw simple line from tail to head points
				glBegin(GL_LINES);
				glVertex3d(r1.x, r1.y, r1.z);
				glVertex3d(r1.x+r2.x, r1.y+r2.y, r1.z+r2.z);
				glEnd();
				// Draw cylinder arrowhead in wireframe
				A.setIdentity();
				A.applyTranslation(r1);
				rij = r2.magnitude();
				phi = DEGRAD * acos(r2.z/rij);
				// Special case where the bond is exactly in the XY plane.
				if ((fabs(phi) < 0.01) || (phi > 179.99)) A.applyRotationX(phi);
				else A.applyRotationAxis(-r2.y, r2.x, 0.0, phi, TRUE);
				// Move to endpoint
				A.applyTranslationZ(rij*0.9);
				A.applyScaling(0.2, 0.2, rij*0.1);
				renderPrimitive(RenderEngine::GlyphObject, primitives_[Q_].cones_, colour_i, A, GL_LINE);
				break;
		}
	}
	
	A.setIdentity();
	renderPrimitive(RenderEngine::GlyphObject, &glyphTriangles_[RenderEngine::SolidTriangle], FALSE, NULL, A);
	renderPrimitive(RenderEngine::GlyphObject, &glyphTriangles_[RenderEngine::WireTriangle], FALSE, NULL, A, GL_LINE);
	renderPrimitive(RenderEngine::GlyphObject, &glyphTriangles_[RenderEngine::TransparentTriangle], TRUE, NULL, A);
	msg.exit("RenderEngine::renderGlyphs");
}

// Render text glyphs
void RenderEngine::renderTextGlyphs(Model *source, TCanvas *canvas)
{
	Vec3<double> r1, r2;
	Vec4<double> screenr;
	GLfloat textcolour[4];
	Glyph *g;

	// Copy text colour
	prefs.copyColour(Prefs::TextColour, textcolour);
	
	for (Refitem<Glyph,int> *ri = source->textGlyphs(); ri != NULL; ri = ri->next)
	{
		// Get glyph pointer
		g = ri->item;

		// Check if glyph is visible
		if (!g->isVisible()) continue;
		
		// Grab first coordinate (always used)
		r1 = g->data(0)->vector();
		
		if (g->type() == Glyph::TextGlyph) renderTextPrimitive(r1.x, canvas->contextHeight()-r1.y, g->text());
		else if (g->type() == Glyph::Text3DGlyph)
		{
			r2 = source->modelToWorld(r1, &screenr);
			if (r2.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, g->text());
		}
		else printf("Internal Error: Found non-text glyph in textglyphs list...\n");
	}
}

// Render additional model information (measurements etc.)
void RenderEngine::renderModelOverlays(Model *source)
{
	Vec3<double> r1, r2, r3, r4, rji, rjk, pos;
	Vec4<double> screenr;
	GLfloat colour[4];
	double gamma, t;
	int labels;
	Dnchar text(512);
	Atom **atoms, *i;
	ForcefieldAtom *ffa;

	// Clear depth buffer to force lines on top of existing primitives,
	glDisable(GL_DEPTH_TEST);
	
	// Set colour
	prefs.copyColour(Prefs::TextColour, colour);
	glColor4fv(colour);
	
	// Atoms and Bonds
	atoms = source->atomArray();
	for (int n = 0; n<source->nAtoms(); ++n)
	{
		// Get atom pointer
		i = atoms[n];
		
		// Skip hidden atoms
		if (i->isHidden()) continue;
		
		// Labels
		labels = i->labels();
		if (labels == 0) continue;
		
		// Grab forcefield atom pointer
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
		r2 = source->modelToWorld(i->r(), &screenr);
		if (r2.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, text.get());
	}
	
	// Measurements
	// Apply standard transformation matrix to OpenGL so we may just use local atom positions for vertices
	// Load model transformation matrix
	glLoadIdentity();
	glMultMatrixd(modelTransformationMatrix_.matrix());
	
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
		r4 = (r1+r2)*0.5;
		r3 = source->modelToWorld(r4, &screenr);
		if (r3.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, ftoa(m->value(), prefs.distanceLabelFormat()), 0x212b);
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
			pos = (rji * sin((1.0-t)*gamma) + rjk * sin(t*gamma)) / sin(gamma);
			pos *= 0.2;
			pos += r2;
			glVertex3d(pos.x, pos.y, pos.z);
			t += 0.1;
			// Store text position
			if (n == 5) r4 = pos;
		}
		glEnd();
		// Determine left or right-alignment of text
		source->modelToWorld(r2, &screenr);
		gamma = screenr.x;
		r3 = source->modelToWorld(r4, &screenr);
		if (r3.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, ftoa(m->value(), prefs.angleLabelFormat()), 176, gamma > screenr.x);
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
		r1 = (r2+r3)*0.5;
		r4 = source->modelToWorld(r1, &screenr);
		if (r4.z < -1.0) renderTextPrimitive(screenr.x, screenr.y, ftoa(m->value(), prefs.angleLabelFormat()), 176);
	}
	
	// Re-enable depth buffer
	glEnable(GL_DEPTH_TEST);
}
