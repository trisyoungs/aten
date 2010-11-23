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
#include "model/model.h"
#include "model/fragment.h"
#include "gui/tcanvas.uih"
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

// Render 3D
void RenderEngine::render3D(Model *source, TCanvas *canvas)
{
	GLfloat colour_i[4], colour_j[4], alpha_i, alpha_j;
	int lod, id_i;
	Dnchar text;
	double selscale, z, phi, radius_i, radius_j, dvisible, rij, dp;
	Atom *i, *j;
	Vec3<double> pos, v, ijk;
	Vec4<double> transformZ, screenr;
	GLMatrix atomtransform, bondtransform, A;
	Refitem<Bond,int> *ri;
	Atom::DrawStyle style_i, style_j, globalstyle;
	Bond::BondType bt;
	Prefs::ColouringScheme scheme;
	Fragment *frag;

	// Clear filtered primitives list
	solidPrimitives_.clear();
	transparentPrimitives_.clear();

	// Set initial transformation matrix, including any translation occurring from cell...
	setTransformationMatrix(source->viewMatrix(), source->cell()->centre());
	transformZ.set(transformationMatrix_[2], transformationMatrix_[6], transformationMatrix_[10], transformationMatrix_[14]);
	
	// Grab global style values
	scheme = prefs.colourScheme();
	globalstyle = prefs.renderStyle();
	selscale = prefs.selectionScale();

	// Set target matrix mode and reset it, and set colour mod
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);
	
	// Render cell and cell axes
	if (source->cell()->type() != Cell::NoCell)
	{
		// Setup pen colour
		prefs.copyColour(Prefs::ForegroundColour, colour_i);
		glColor4fv(colour_i);
		A = source->viewMatrix() * source->cell()->axes();
		glMultMatrixd(A.matrix());
		wireCube_.sendToGL();
		glTranslated(-0.5, -0.5, -0.5);
		v = source->cell()->lengths();
		glScaled(1.0 / v.x, 1.0 / v.y, 1.0 / v.z);
		glColor4f(0.8f, 0.8f, 0.8f, 1.0f);
		cellAxes_.sendToGL();
		glLoadIdentity();
	}

	// Atoms and Bonds // OPTIMIZE - use atom array instead
	for (i = source->atoms(); i != NULL; i = i->next)
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
		atomtransform = transformationMatrix_;
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
				if (i->nBonds() == 0) renderPrimitive(atom_[style_i], lod, colour_i, atomtransform);
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

		// Bonds
		// Set initial transformation matrix to centre on atom i
		bondtransform = atomtransform;
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
			bt = ri->item->type();

			// Store copy of alpha value
			alpha_j = colour_j[3];

			// Calculate vector i->j
			v = source->cell()->mimd(j, i);
			rij = v.magnitude();

			// Don't bother calculating transformation if both atom styles are Stick
			if ((style_i == Atom::StickStyle) && (style_j == Atom::StickStyle))
			{
			        glMultMatrixd(atomtransform.matrix());
				glNormal3d(0.0,0.0,1.0);
				glLineWidth( i->isSelected() ? 3.0 : 1.0 );
				glColor4fv(colour_i);
				glBegin(GL_LINES);
				glVertex3d(0.0, 0.0, 0.0);
				glVertex3d(0.5*v.x, 0.5*v.y, 0.5*v.z);
				glEnd();
				glLineWidth( j->isSelected() ? 3.0 : 1.0 );
				glColor4fv(colour_j);
				glBegin(GL_LINES);
				glVertex3d(0.5*v.x, 0.5*v.y, 0.5*v.z);
				glVertex3d(v.x, v.y, v.z);
				glEnd();
				glLoadIdentity();
			}
			else
			{
				// If bond is not visible, don't bother drawing it...
				dvisible = 0.5 * (rij - 0.85*(radius_i + radius_j));
				if (dvisible < 0.0) continue;

				// Calculate angle out of XZ plane
				// OPTIMISE - Precalculate acos()
				phi = DEGRAD * acos(v.z/rij);
	
				// Special case where the bond is exactly in the XY plane.
				bondtransform = atomtransform;
				if ((fabs(phi) < 0.01) || (phi > 179.99)) bondtransform.applyRotationX(phi);
				else bondtransform.applyRotationAxis(-v.y, v.x, 0.0, phi, TRUE);

				// We can perform an initial translation to the 'edge' of atom i, and scale to visible bond length
				bondtransform.applyTranslation(0.0, 0.0, 0.85*radius_i);
				bondtransform.applyScalingZ(dvisible);

				// Determine bond plane rotation if a multiple bond
				if (ri->item->type() > Bond::Single)
				{
					if (i > j) ijk = i->findBondPlane(j,ri->item,v);
					else ijk = j->findBondPlane(i,ri->item,v);
					// Calculate projected bond plane vector
					ijk = bondtransform.rotateVector(ijk);
// 					printf("Transformed bond-plane vector is :"); ijk.print();
// 					bondtransform.print();
// 					printf("DP = %f,  acos = %f\n", ijk.dp(Vec3<double>(bondtransform[0],bondtransform[1],bondtransform[2])), DEGRAD*acos(ijk.dp(Vec3<double>(bondtransform[0],bondtransform[1],bondtransform[2]))));
					// Rotate into bond plane if necessary
					dp = ijk.x*bondtransform[0] + ijk.y*bondtransform[1] + ijk.z*bondtransform[2];
					phi = 180.0 - DEGRAD*acos(dp);
					if (phi > 0.01)
					{
						bondtransform.applyRotationAxis(0.0,0.0,1.0,phi-180.0,FALSE);  // BROKEN
					}
				}
				
				// Draw first bond half
				switch (style_i)
				{
					case (Atom::StickStyle):
						glLineWidth( i->isSelected() ? 4.0 : 2.0 );
						glBegin(GL_LINES);
						glVertex3d(pos.x, pos.y, pos.z);
						glVertex3d(pos.x+0.5*v.x, pos.y+0.5*v.y, pos.z+0.5*v.z);
						glEnd();
						break;
					case (Atom::TubeStyle):
						renderPrimitive(bond_[style_i][bt], lod, colour_i, bondtransform);
						if (i->isSelected())
						{
							colour_i[3] = 0.5f;
							renderPrimitive(selectedBond_[style_i][bt], lod, colour_i, bondtransform);
							colour_i[3] = alpha_i;
						}
						// Move to centre of visible bond, ready for next bond half
						bondtransform.applyTranslation(0.0, 0.0, 1.0);
						break;
					case (Atom::SphereStyle):
					case (Atom::ScaledStyle):
						renderPrimitive(bond_[style_i][bt], lod, colour_i, bondtransform);
						if (i->isSelected())
						{
							// Move to bond centre and apply 'reverse' Z-scaling
							bondtransform.applyTranslation(0.0, 0.0, 1.0);
							z = -(1.0 - (dvisible - 0.85*(radius_i*selscale-radius_i))/dvisible);
							bondtransform.applyScalingZ(z);
							colour_i[3] = 0.5f;
							renderPrimitive(selectedBond_[style_i][bt], lod, colour_i, bondtransform);
							colour_i[3] = alpha_i;
							// Reverse scaling back to 'dvisible'
							bondtransform.applyScalingZ(1.0/z);
						}
						else bondtransform.applyTranslation(0.0, 0.0, 1.0);
						break;
				}

				// Draw second bond half
				switch (style_j)
				{
					case (Atom::StickStyle):
						glLineWidth( j->isSelected() ? 3.0 : 1.0 );
						glBegin(GL_LINES);
						glVertex3d(pos.x+0.5*v.x, pos.y+0.5*v.y, pos.z+0.5*v.z);
						glVertex3d(pos.x+v.x, pos.y+v.y, pos.z+v.z);
						glEnd();
						break;
					case (Atom::TubeStyle):
						renderPrimitive(bond_[style_i][bt], lod, colour_j, bondtransform);
						if (j->isSelected())
						{
							colour_j[3] = 0.5f;
							renderPrimitive(selectedBond_[style_j][bt], lod, colour_j, bondtransform);
							colour_j[3] = alpha_j;
						}
						break;
					case (Atom::SphereStyle):
					case (Atom::ScaledStyle):
						renderPrimitive(bond_[style_j][bt], lod, colour_j, bondtransform);
						if (j->isSelected())
						{
							colour_j[3] = 0.5f;
							z = 1.0 - (dvisible - 0.85*(radius_j*selscale-radius_j))/dvisible;
							bondtransform.applyScalingZ(z);
							renderPrimitive(selectedBond_[style_j][bt], lod, colour_j, bondtransform);
							colour_j[3] = alpha_j;
						}
						break;
				}
			}
		}

	}

	// Draw on the selection highlights (for atoms in canvas.subsel)
	for (Refitem<Atom,int> *ri = canvas->pickedAtoms(); ri != NULL; ri = ri->next)
	{
		i = ri->item;
		// Move to local atom position
		atomtransform = transformationMatrix_;
		atomtransform.applyTranslation(i->r());
		// Draw a wireframe sphere at the atoms position
		style_i = (globalstyle == Atom::IndividualStyle ? i->style() : globalstyle);
		switch (style_i)
		{
			case (Atom::StickStyle):
			case (Atom::TubeStyle):
				renderPrimitive(atom_[Atom::TubeStyle], lod, colour_i, atomtransform, GL_LINES);
				break;
			case (Atom::SphereStyle):
				renderPrimitive(atom_[Atom::SphereStyle], lod, colour_i, atomtransform, GL_LINES);
				break;
			case (Atom::ScaledStyle):
				renderPrimitive(scaledAtom_[i->element()], lod, colour_i, atomtransform, GL_LINES);
				break;
		}
	}


	// Active user actions
	i = canvas->atomClicked();
	switch (canvas->activeMode())
	{
		// Draw on bond and new atom for chain drawing (if mode is active)
		case (Canvas::DrawChainAction):
			if (i == NULL) break;
			pos = i->r();
			// We need to project a point from the mouse position onto the canvas plane, unless the mouse is over an existing atom in which case we snap to its position instead
			i = displayModel_->atomOnScreen(canvas->rMouseLast().x, canvas->rMouseLast().y);
			style_i = (globalstyle == Atom::IndividualStyle ? i->style() : globalstyle);
			radius_i = (style_i == Atom::TubeStyle ? 0.0 : prefs.screenRadius(i)*0.85);
			radius_j = prefs.bondStyleRadius(prefs.renderStyle());
			if (i == NULL) mouse = displayModel_->guideToModel(canvas->rMouseLast()., canvas->currentDrawDepth());
			else mouse = i->r();
			v = mouse - pos;
			rij = v.magnitude();
			
			// Determine how we'll draw the new bond / atom
			if (prefs.renderStyle() == Atom::StickStyle)
			{
				// Simple - draw line from atomClicked_ to mouse position
				glLoadIdentity();
				glMultMatrixd(transformationMatrix_.matrix();
				glBegin(GL_LINES);
				glVertex3d(pos.x, pos.y, pos.z);
				glVertex3d(mouse.x+pos.x,mouse.y+pos.y,mouse.z+pos.z);
				glEnd();
			}
			else
			{
				// Draw new bond and atom
				bondtransform = transformationMatrix_;
				bondtransform.applyTranslation(i->r());
				
				// If bond is not visible, don't bother drawing it...
				dvisible = 0.5 * (rij - 0.85*(radius_i + radius_j));
				if (dvisible < 0.0) continue;
				
				// Calculate angle out of XZ plane
				// OPTIMISE - Precalculate acos()
				phi = DEGRAD * acos(v.z/rij);
				
				// Special case where the bond is exactly in the XY plane.
				bondtransform = atomtransform;
				if ((fabs(phi) < 0.01) || (phi > 179.99)) bondtransform.applyRotationX(phi);
				else bondtransform.applyRotationAxis(-v.y, v.x, 0.0, phi, TRUE);
				
				// We can perform an initial translation to the 'edge' of atom i, and scale to visible bond length
				bondtransform.applyTranslation(0.0, 0.0, 0.85*radius_i);
				bondtransform.applyScalingZ(dvisible);
				// Draw first bond half
				switch (style_i)
				{
					case (Atom::StickStyle):
						glLineWidth( i->isSelected() ? 4.0 : 2.0 );
						glBegin(GL_LINES);
						glVertex3d(pos.x, pos.y, pos.z);
						glVertex3d(pos.x+0.5*v.x, pos.y+0.5*v.y, pos.z+0.5*v.z);
						glEnd();
						break;
					case (Atom::TubeStyle):
						renderPrimitive(bond_[style_i][bt], lod, colour_i, bondtransform);
						bondtransform.applyTranslation(0.0, 0.0, 1.0);
						break;
					case (Atom::SphereStyle):
					case (Atom::ScaledStyle):
						renderPrimitive(bond_[style_i][bt], lod, colour_i, bondtransform);
						bondtransform.applyTranslation(0.0, 0.0, 1.0);
						break;
				}
				
				// Draw second bond half
				switch (prefs.renderStyle())
				{
					case (Atom::StickStyle):
						glLineWidth( j->isSelected() ? 3.0 : 1.0 );
						glBegin(GL_LINES);
						glVertex3d(pos.x+0.5*v.x, pos.y+0.5*v.y, pos.z+0.5*v.z);
						glVertex3d(pos.x+v.x, pos.y+v.y, pos.z+v.z);
						glEnd();
						break;
					case (Atom::TubeStyle):
						renderPrimitive(bond_[style_i][bt], lod, colour_j, bondtransform);
						break;
					case (Atom::SphereStyle):
					case (Atom::ScaledStyle):
						renderPrimitive(bond_[style_j][bt], lod, colour_j, bondtransform);
						break;
				}
						
						
						
						
				radius = prefs.bondStyleRadius(prefs.renderStyle());
				glCylinder(mouse, mouse.magnitude(), radius, radius, FALSE, FALSE);
				glTranslated(mouse.x, mouse.y, mouse.z);
				switch (prefs.renderStyle())
				{
					case (Atom::TubeStyle):
						glCallList(glob(WireTubeAtomGlob));
						break;
					case (Atom::SphereStyle):
						glCallList(glob(WireSphereAtomGlob));
						break;
					case (Atom::ScaledStyle):
						glCallList(glob(WireSphereAtomGlob));
						break;
					default:
						break;
				}
			}
			// Draw text showing distance
			s.sprintf(" l = %f A",mouse.magnitude());
			glText(textpos,s);
			break;


	// All objects have now been filtered...
	sortAndSendGL();
	
// 	glDisable(GL_COLOR_MATERIAL);
	prefs.copyColour(Prefs::ForegroundColour, colour_i);
	glColor4fv(colour_i);
}

