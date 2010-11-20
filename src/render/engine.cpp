/*
	*** Rendering Engine
	*** src/render/engine.cpp
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
#include "base/messenger.h"
#include "classes/prefs.h"
#include "model/model.h"
#include "classes/forcefieldatom.h"
#include "gui/tcanvas.uih"
#include <math.h>

// Constructor
RenderEngine::RenderEngine()
{
	// Primitives
	scaledAtom_ = new PrimitiveGroup[elements().nElements()];
	selectedScaledAtom_ = new PrimitiveGroup[elements().nElements()];
	
	createPrimitives();
	triangleChopper_.initialise(0.0, 20.0, 0.1);
}

// Destructor
RenderEngine::~RenderEngine()
{
	delete[] scaledAtom_;
	delete[] selectedScaledAtom_;
}

/*
// Primitive Generation
*/

// (Re)Generate primitives
void RenderEngine::createPrimitives()
{
	msg.enter("RenderEngine::createPrimitives");
	double radius, lodratio, aradius[Atom::nDrawStyles], bradius[Atom::nDrawStyles], selscale;
	int n, m, lod, nstacks, nslices, quality = prefs.primitiveQuality();
	// Clear old primitive groups
	for (n=0; n<Atom::nDrawStyles; ++n)
	{
		atom_[n].clear();
		selectedAtom_[n].clear();	
		for (m=0; m<Bond::nBondTypes; ++m)
		{
			bond_[n][m].clear();
			selectedBond_[n][m].clear();
		}
	}
	for (n=0; n<elements().nElements(); ++n)
	{
		scaledAtom_[n].clear();
		selectedScaledAtom_[n].clear();
	}
	// To clean up following code, grab radii here
	for (n=0; n<Atom::nDrawStyles; ++n)
	{
		aradius[n] = prefs.atomStyleRadius( (Atom::DrawStyle) n);
		bradius[n] = prefs.bondStyleRadius( (Atom::DrawStyle) n);
	}
	// Loop over levels of detail
	for (lod=0; lod < prefs.levelsOfDetail(); ++lod)
	{
		// Calculate general level-of-detail ratio, which ranges from 1 (at lod=0) to 0 (at lod=nlevels)
		lodratio = 1.0 - (double (lod+1)/prefs.levelsOfDetail());
		// Atom Styles (Atom::StickStyle, Atom::TubeStyle, and Atom::SphereStyle)
		atom_[Atom::StickStyle].primitive(lod).createCross(0.5,3-lod);
		nstacks = max(3,(int) (quality*lodratio*0.75));
		nslices = max(3,(int) (quality*lodratio*1.5));
		atom_[Atom::TubeStyle].primitive(lod).createSphere(aradius[Atom::TubeStyle], nstacks, nslices);
		atom_[Atom::SphereStyle].primitive(lod).createSphere(aradius[Atom::SphereStyle], nstacks, nslices);
		selectedAtom_[Atom::TubeStyle].primitive(lod).createSphere(aradius[Atom::TubeStyle]*selscale, nstacks, nslices);
		selectedAtom_[Atom::SphereStyle].primitive(lod).createSphere(aradius[Atom::SphereStyle]*selscale, nstacks, nslices);
		// Atom Styles (Atom::ScaledStyle)
		for (n = 0; n<elements().nElements(); ++n)
		{
			radius = aradius[Atom::ScaledStyle] * elements().el[n].atomicRadius;
			scaledAtom_[n].primitive(lod).createSphere(radius, nstacks, nslices);
			selectedScaledAtom_[n].primitive(lod).createSphere(radius*selscale, nstacks, nslices);
		}
		// Bond Styles
		nstacks = max(1,(int) (quality*lodratio*0.25));
		nslices = max(3,(int) (quality*lodratio));
		// All styles - Single Bond
		bond_[Atom::TubeStyle][Bond::Single].primitive(lod).createCylinder(bradius[Atom::TubeStyle], bradius[Atom::TubeStyle], nstacks, nslices);
		bond_[Atom::SphereStyle][Bond::Single].primitive(lod).createCylinder(bradius[Atom::SphereStyle], bradius[Atom::SphereStyle], nstacks, nslices);
		bond_[Atom::ScaledStyle][Bond::Single].primitive(lod).createCylinder(bradius[Atom::ScaledStyle], bradius[Atom::ScaledStyle], nstacks, nslices);
		selectedBond_[Atom::TubeStyle][Bond::Single].primitive(lod).createCylinder(bradius[Atom::TubeStyle]*selscale, bradius[Atom::TubeStyle]*selscale, nstacks, nslices);
		selectedBond_[Atom::SphereStyle][Bond::Single].primitive(lod).createCylinder(bradius[Atom::SphereStyle]*selscale, bradius[Atom::SphereStyle]*selscale, nstacks, nslices);
		selectedBond_[Atom::ScaledStyle][Bond::Single].primitive(lod).createCylinder(bradius[Atom::ScaledStyle]*selscale, bradius[Atom::ScaledStyle]*selscale, nstacks, nslices);
		// All styles - Double Bond
// 		bond_[Atom::TubeStyle][Bond::Double].primitive(lod).createEmpty(GL_TRIANGLES, 2*nstacks*nslices*2, FALSE);
// 		bond_[Atom::TubeStyle][Bond::Double].primitive(lod).plotCylinder(-aradius[Atom::TubeStyle]*0.25,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);
// 		bond_[Atom::TubeStyle][Bond::Double].primitive(lod).plotCylinder(aradius[Atom::TubeStyle]*0.25,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);		
		// Cubes
		cubes_.primitive(lod).createCube(1.0, max(1, int(quality*lodratio)) );
		// Cones
		cones_.primitive(lod).createCylinder(0.2,0.0,nstacks,nslices);
	}
	// One-off objects
	wireCube_.createWireCube(1.0);
	cellAxes_.createCellAxes();
	msg.exit("RenderEngine::createPrimitives");
}

/*
// View Control
*/

// Set-up viewport and projection matrices
void RenderEngine::setupView(GLint x, GLint y, GLint w, GLint h)
{
	// Setup and store viewport matrix
	viewportMatrix_[0] = x;
	viewportMatrix_[1] = y;
	viewportMatrix_[2] = w;
	viewportMatrix_[3] = h;
	glViewport( x, y, w, h );
	// Create projection matrix
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	GLdouble top, bottom, aspect = (GLdouble) w / (GLdouble) h;
	if (prefs.hasPerspective())
	{
		// Use reversed top and bottom values so we get y-axis (0,1,0) pointing up
		bottom = tan(prefs.perspectiveFov() / DEGRAD) * prefs.clipNear();
		top = -bottom;
		glFrustum(aspect*top, aspect*bottom, top, bottom, prefs.clipNear(), prefs.clipFar());
	}
	else
	{
		top = tan(prefs.perspectiveFov() / DEGRAD) * 1.0; // TGAY (displayModel_ == NULL ? 1.0 : displayModel_->camera().z);
		bottom = -top;
		glOrtho(aspect*top, aspect*bottom, top, bottom, -prefs.clipFar(), prefs.clipFar());
	}
	GLdouble pmat[16];
	glGetDoublev(GL_PROJECTION_MATRIX, projectionMatrix_.matrix());
}

// Project given model coordinates into world coordinates (and screen coordinates if requested)
Vec3<double> &RenderEngine::modelToWorld(Vec3<double> &modelr, Vec4<double> *screenr, double screenradius)
{
	msg.enter("RenderEngine::modelToWorld");
	static Vec3<double> worldr;
	Vec4<double> pos, temp, tempscreen;
	// Projection formula is : worldr = P x M x modelr
	pos.set(modelr, 1.0);
	// Get the world coordinates of the atom - Multiply by modelview matrix 'view'
	temp = transformationMatrix_ * pos;
	worldr.set(temp.x, temp.y, temp.z);
	// Calculate 2D screen coordinates - Multiply world coordinates by P
	if (screenr != NULL)
	{
		*screenr = projectionMatrix_ * temp;
		screenr->x /= screenr->w;
		screenr->y /= screenr->w;
		screenr->x = viewportMatrix_[0] + viewportMatrix_[2]*(screenr->x+1)*0.5;
		screenr->y = viewportMatrix_[1] + viewportMatrix_[3]*(screenr->y+1)*0.5;
		screenr->z = screenr->z / screenr->w;
		// Calculate 2D 'radius' of the atom - Multiply world[x+delta] coordinates by P
		if (screenradius > 0.0)
		{
			temp.x += screenradius;
			tempscreen = projectionMatrix_ * temp;
			tempscreen.x /= tempscreen.w;
			screenr->w = fabs( (viewportMatrix_[0] + viewportMatrix_[2]*(tempscreen.x+1)*0.5) - screenr->x);
		}
	}
	msg.exit("RenderEngine::modelToWorld");
	return worldr;
}

// Project the specified world coordinates into 2D screen coords
Vec4<double> &RenderEngine::worldToScreen(const Vec3<double> &v)
{
	// The returned vec4's 'w' component is the unit 'radius' at that point.
	msg.enter("RenderEngine::worldToScreen");
	static Vec4<double> modelr, screenr, worldr, result;
	static double x1,x2,radius;
	// Projection formula is : worldr = P x M x modelr
	// Get the 3D coordinates of the atom - Multiply by modelview matrix 'view'
	modelr.set(v.x, v.y, v.z, 1.0);
	worldr = transformationMatrix_ * modelr;
	//viewMatrix_.print();
	// Calculate 2D 'radius' of the atom - Multiply worldr[x+delta] coordinates by P
	screenr = projectionMatrix_ * worldr;
	screenr.x /= screenr.w;
	screenr.y /= screenr.w;
	result = screenr;
	x1 = viewportMatrix_[0] + viewportMatrix_[2]*(screenr.x+1)/2.0;
	worldr.x += 1.0;
	screenr = projectionMatrix_ * worldr;
	screenr.x /= screenr.w;
	x2 = viewportMatrix_[0] + viewportMatrix_[2]*(screenr.x+1)/2.0;
	radius = fabs(x2 - x1);
	// Store info and return
	result.w = radius;
	msg.exit("RenderEngine::worldToScreen");
	return result;
}

/*
// Object Rendering
*/

// Render primitive in specified colour and level of detail (coords/transform used only if filtered)
void RenderEngine::renderPrimitive(PrimitiveGroup &pg, int lod, GLfloat *colour, GLMatrix &transform, bool transformInGL)
{
	if (colour[3] > 0.99f)
	{
		// Add primitive info to solid objects list
		PrimitiveInfo *pi = solidPrimitives_.add();
		pi->set(&pg.primitive(lod), colour, transform);
	}
	else
	{
		// Add primitive info to transparent objects list
		PrimitiveInfo *pi = transparentPrimitives_.add();
		pi->set(&pg.primitive(lod), colour, transform);
	}
}

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

// Update transformation matrix
void RenderEngine::setTransformationMatrix(Mat4<double> &mat, Vec3<double> cellcentre)
{
	transformationMatrix_ = mat;
	transformationMatrix_.applyTranslation(-cellcentre.x, -cellcentre.y, -cellcentre.z);
}

// Render specified model
void RenderEngine::renderModel(Model *source, TCanvas *canvas, QPainter &painter)
{
	GLfloat colour_i[4], colour_j[4], alpha_i, alpha_j;
	int lod, id_i, labels;
	Dnchar text;
	double selscale, z, phi, halfr, radius_i, radius_j, dvisible, rij;
	Atom *i, *j;
	Vec3<double> pos, v, ijk;
	Vec4<double> transformZ, screenr;
	GLMatrix atomtransform, bondtransform, A, B;
	Refitem<Bond,int> *ri;
	ForcefieldAtom *ffa;
	Atom::DrawStyle style_i, style_j, globalstyle;
	Bond::BondType bt;
	Prefs::ColouringScheme scheme;

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
		cones_.primitive(0).sendToGL();
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
				if (i->nBonds() == 0) renderPrimitive(atom_[style_i], lod, colour_i, atomtransform, FALSE);
				break;
			case (Atom::TubeStyle):
			case (Atom::SphereStyle):
				renderPrimitive(atom_[style_i], lod, colour_i, atomtransform, FALSE);
				if (i->isSelected())
				{
					colour_i[3] = 0.5f;
					renderPrimitive(selectedAtom_[style_i], lod, colour_i, atomtransform, FALSE);
					colour_i[3] = alpha_i;
				}
				break;
			case (Atom::ScaledStyle):
				renderPrimitive(scaledAtom_[i->element()], lod, colour_i, atomtransform, FALSE);
				if (i->isSelected())
				{
					colour_i[3] = 0.5f;
					renderPrimitive(selectedScaledAtom_[i->element()], lod, colour_i, atomtransform, FALSE);
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
					// Now what?
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
						if (i->isSelected())
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

	// All objects have now been filtered...
	sortAndSendGL();
}

// Sort and render filtered polygons by depth
void RenderEngine::sortAndSendGL()
{
	// Transform and render each solid primitive in the list
	Vec3<double> pos;
	for (PrimitiveInfo *pi = solidPrimitives_.first(); pi != NULL; pi = pi->next)
	{
		// If colour data is not present in the vertex data array, use the colour stored in the PrimitiveInfo object
		if (!pi->primitive()->colouredVertexData()) glColor4fv(pi->colour());
		glPushMatrix();
		glMultMatrixd(pi->localTransform().matrix());
		pi->primitive()->sendToGL();
		glPopMatrix();
	}
	
	// Transform and render each transparent primitive in the list, unless transparencyCorrect_ is off.
	if (prefs.transparencyCorrect())
	{
		triangleChopper_.emptyTriangles();
		for (PrimitiveInfo *pi = transparentPrimitives_.first(); pi != NULL; pi = pi->next) triangleChopper_.storeTriangles(pi);
		triangleChopper_.sendToGL();
	}
	else for (PrimitiveInfo *pi = transparentPrimitives_.first(); pi != NULL; pi = pi->next)
	{
		glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, pi->colour());
		glPushMatrix();
		glMultMatrixd(pi->localTransform().matrix());
		pi->primitive()->sendToGL();
		glPopMatrix();
	}
}
