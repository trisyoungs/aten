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
	selscale = prefs.selectionScale();
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
		bond_[Atom::TubeStyle][Bond::Double].primitive(lod).createEmpty(GL_TRIANGLES, 2*nstacks*nslices*2, FALSE);
		bond_[Atom::TubeStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);
		bond_[Atom::TubeStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);
		bond_[Atom::SphereStyle][Bond::Double].primitive(lod).createEmpty(GL_TRIANGLES, 2*nstacks*nslices*2, FALSE);
		bond_[Atom::SphereStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5, bradius[Atom::SphereStyle]*0.5, nstacks, nslices);
		bond_[Atom::SphereStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5, bradius[Atom::SphereStyle]*0.5, nstacks, nslices);
		bond_[Atom::ScaledStyle][Bond::Double].primitive(lod).createEmpty(GL_TRIANGLES, 2*nstacks*nslices*2, FALSE);
		bond_[Atom::ScaledStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5, bradius[Atom::ScaledStyle]*0.5, nstacks, nslices);
		bond_[Atom::ScaledStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5, bradius[Atom::ScaledStyle]*0.5, nstacks, nslices);
		// All styles - Triple Bond
		bond_[Atom::TubeStyle][Bond::Triple].primitive(lod).createEmpty(GL_TRIANGLES, 3*nstacks*nslices*2, FALSE);
		bond_[Atom::TubeStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::TubeStyle]*0.25,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);
		bond_[Atom::TubeStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::TubeStyle]*0.25,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);
		bond_[Atom::SphereStyle][Bond::Triple].primitive(lod).createEmpty(GL_TRIANGLES, 3*nstacks*nslices*2, FALSE);
		bond_[Atom::SphereStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5, bradius[Atom::SphereStyle]*0.5, nstacks, nslices);
		bond_[Atom::SphereStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5, bradius[Atom::SphereStyle]*0.5, nstacks, nslices);
		bond_[Atom::ScaledStyle][Bond::Triple].primitive(lod).createEmpty(GL_TRIANGLES, 3*nstacks*nslices*2, FALSE);
		bond_[Atom::ScaledStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5, bradius[Atom::ScaledStyle]*0.5, nstacks, nslices);
		bond_[Atom::ScaledStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5, bradius[Atom::ScaledStyle]*0.5, nstacks, nslices);
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
void RenderEngine::renderPrimitive(PrimitiveGroup &pg, int lod, GLfloat *colour, GLMatrix &transform)
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

// Update transformation matrix
void RenderEngine::setTransformationMatrix(Mat4<double> &mat, Vec3<double> cellcentre)
{
	transformationMatrix_ = mat;
	transformationMatrix_.applyTranslation(-cellcentre.x, -cellcentre.y, -cellcentre.z);
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
		glLoadIdentity();
		glMultMatrixd(pi->localTransform().matrix());
		pi->primitive()->sendToGL();
	}
	
	// Transform and render each transparent primitive in the list, unless transparencyCorrect_ is off.
	if (prefs.transparencyCorrect())
	{
		triangleChopper_.emptyTriangles();
		for (PrimitiveInfo *pi = transparentPrimitives_.first(); pi != NULL; pi = pi->next) triangleChopper_.storeTriangles(pi);
		glLoadIdentity();
		glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);
		triangleChopper_.sendToGL();
		glPopClientAttrib();
	}
	else for (PrimitiveInfo *pi = transparentPrimitives_.first(); pi != NULL; pi = pi->next)
	{
		if (!pi->primitive()->colouredVertexData()) glColor4fv(pi->colour());
		glLoadIdentity();
		glMultMatrixd(pi->localTransform().matrix());
		pi->primitive()->sendToGL();
	}
}
