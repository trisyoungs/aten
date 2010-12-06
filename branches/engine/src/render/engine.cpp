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
	primitiveQuality_ = -1;

	initialiseTransparency();
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
void RenderEngine::createPrimitives(int quality)
{
	msg.enter("RenderEngine::createPrimitives");
	double radius, lodratio, aradius[Atom::nDrawStyles], bradius[Atom::nDrawStyles], selscale;
	int n, m, lod, nstacks, nslices;
	if (primitiveQuality_ == quality)
	{
		msg.exit("RenderEngine::createPrimitives");
		return;
	}
	primitiveQuality_ = quality;
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
		atom_[Atom::TubeStyle].primitive(lod).plotSphere(aradius[Atom::TubeStyle], nstacks, nslices);
		atom_[Atom::SphereStyle].primitive(lod).plotSphere(aradius[Atom::SphereStyle], nstacks, nslices);
		selectedAtom_[Atom::TubeStyle].primitive(lod).plotSphere(aradius[Atom::TubeStyle]*selscale, nstacks, nslices);
		selectedAtom_[Atom::SphereStyle].primitive(lod).plotSphere(aradius[Atom::SphereStyle]*selscale, nstacks, nslices);
		// Atom Styles (Atom::ScaledStyle)
		for (n = 0; n<elements().nElements(); ++n)
		{
			radius = aradius[Atom::ScaledStyle] * elements().el[n].atomicRadius;
			scaledAtom_[n].primitive(lod).plotSphere(radius, nstacks, nslices);
			selectedScaledAtom_[n].primitive(lod).plotSphere(radius*selscale, nstacks, nslices);
		}
		// Bond Styles
		nstacks = max(1,(int) (quality*lodratio*0.25));
		nslices = max(3,(int) (quality*lodratio));
		// All styles - Single Bond
		bond_[Atom::TubeStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::TubeStyle], bradius[Atom::TubeStyle], nstacks, nslices);
		bond_[Atom::SphereStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::SphereStyle], bradius[Atom::SphereStyle], nstacks, nslices);
		bond_[Atom::ScaledStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::ScaledStyle], bradius[Atom::ScaledStyle], nstacks, nslices);
		selectedBond_[Atom::TubeStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::TubeStyle]*selscale, bradius[Atom::TubeStyle]*selscale, nstacks, nslices);
		selectedBond_[Atom::SphereStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::SphereStyle]*selscale, bradius[Atom::SphereStyle]*selscale, nstacks, nslices);
		selectedBond_[Atom::ScaledStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::ScaledStyle]*selscale, bradius[Atom::ScaledStyle]*selscale, nstacks, nslices);
		// All styles - Double Bond
		bond_[Atom::TubeStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);
		bond_[Atom::TubeStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);
		bond_[Atom::SphereStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5, bradius[Atom::SphereStyle]*0.5, nstacks, nslices);
		bond_[Atom::SphereStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5, bradius[Atom::SphereStyle]*0.5, nstacks, nslices);
		bond_[Atom::ScaledStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5, bradius[Atom::ScaledStyle]*0.5, nstacks, nslices);
		bond_[Atom::ScaledStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5, bradius[Atom::ScaledStyle]*0.5, nstacks, nslices);
		// All styles - Triple Bond
		bond_[Atom::TubeStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::TubeStyle]*0.25,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);
		bond_[Atom::TubeStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::TubeStyle]*0.25,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);
		bond_[Atom::SphereStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5, bradius[Atom::SphereStyle]*0.5, nstacks, nslices);
		bond_[Atom::SphereStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5, bradius[Atom::SphereStyle]*0.5, nstacks, nslices);
		bond_[Atom::ScaledStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5, bradius[Atom::ScaledStyle]*0.5, nstacks, nslices);
		bond_[Atom::ScaledStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5, bradius[Atom::ScaledStyle]*0.5, nstacks, nslices);
		// Cubes
		cubes_.primitive(lod).createCube(1.0, max(1, int(quality*lodratio)) );
		// Cones
		cones_.primitive(lod).plotCylinder(0,0,0,0,0,1,0.2,0.0,nstacks,nslices);
	}
	// One-off objects
	wireCube_.createWireCube(1.0);
	cellAxes_.createCellAxes();
	rotationGlobe_.plotSphere(0.75,10,13);
	rotationGlobeAxes_.createRotationGlobeAxes(8,10);
	msg.exit("RenderEngine::createPrimitives");
}

// (Re)initialise transparency filter
void RenderEngine::initialiseTransparency()
{
	triangleChopper_.initialise(prefs.transparencyBinStartZ(), prefs.transparencyNBins(), prefs.transparencyBinWidth());
}

/*
// View Control
*/

// Set-up viewport and projection matrices
void RenderEngine::setupView(GLint x, GLint y, GLint w, GLint h, double orthozoom)
{
	// Setup and store viewport matrix
	viewportMatrix_[0] = x;
	viewportMatrix_[1] = y;
	viewportMatrix_[2] = w;
	viewportMatrix_[3] = h;
	glViewport( x, y, w, h );
	glMatrixMode(GL_PROJECTION);
	
	// Create projection matrix for rotation globe
	glLoadIdentity();
	glOrtho(-1.0, 1.0, -1.0, 1.0, -10.0, 10.0);
	glGetDoublev(GL_PROJECTION_MATRIX, globeProjectionMatrix_.matrix());
	
	// Create projection matrix for model
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
		top = tan(prefs.perspectiveFov() / DEGRAD) * orthozoom;
		bottom = -top;
		glOrtho(aspect*top, aspect*bottom, top, bottom, -prefs.clipFar(), prefs.clipFar());
	}
	glGetDoublev(GL_PROJECTION_MATRIX, modelProjectionMatrix_.matrix());
	glMatrixMode(GL_MODELVIEW);
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
	temp = modelTransformationMatrix_ * pos;
	worldr.set(temp.x, temp.y, temp.z);
	// Calculate 2D screen coordinates - Multiply world coordinates by P
	if (screenr != NULL)
	{
		*screenr = modelProjectionMatrix_ * temp;
		screenr->x /= screenr->w;
		screenr->y /= screenr->w;
		screenr->x = viewportMatrix_[0] + viewportMatrix_[2]*(screenr->x+1)*0.5;
		screenr->y = viewportMatrix_[1] + viewportMatrix_[3]*(screenr->y+1)*0.5;
		screenr->z = screenr->z / screenr->w;
		// Calculate 2D 'radius' of the atom - Multiply world[x+delta] coordinates by P
		if (screenradius > 0.0)
		{
			temp.x += screenradius;
			tempscreen = modelProjectionMatrix_ * temp;
			tempscreen.x /= tempscreen.w;
			screenr->w = fabs( (viewportMatrix_[0] + viewportMatrix_[2]*(tempscreen.x+1)*0.5) - screenr->x);
		}
	}
	msg.exit("RenderEngine::modelToWorld");
	return worldr;
}

// Convert screen coordinates into modelspace coordinates
Vec3<double> &RenderEngine::screenToModel(double x, double y, double z)
{
	Vec3<double> screenr, modelr;
	// Unproject screen coordinates
	screenr.x = (x - viewportMatrix_[0]) / viewportMatrix_[2] * 2.0;
	screenr.y = (y - viewportMatrix_[1]) / viewportMatrix_[3] * 2.0;
	screenr.z = z;
	Matrix mat = modelProjectionMatrix_;
	mat.invert();
	screenr = mat * screenr;
	mat = modelTransformationMatrix_;
	mat.invert();
	modelr = mat * screenr;
	return modelr;
}

// Update transformation matrix
void RenderEngine::setTransformationMatrix(Matrix &mat, Vec3<double> cellcentre)
{
	modelTransformationMatrix_ = mat;
	modelTransformationMatrix_.applyTranslation(-cellcentre.x, -cellcentre.y, -cellcentre.z);
}

/*
// Object Rendering
*/

// Render primitive in specified colour and level of detail (coords/transform used only if filtered)
void RenderEngine::renderPrimitive(PrimitiveGroup &pg, int lod, GLfloat *colour, Matrix &transform, GLenum fillMode)
{
	if ((colour[3] > 0.99f) || (fillMode != GL_FILL))
	{
		// Add primitive info to solid objects list
		PrimitiveInfo *pi = solidPrimitives_.add();
		pi->set(&pg.primitive(lod), colour, transform, fillMode);
	}
	else
	{
		// Add primitive info to transparent objects list
		PrimitiveInfo *pi = transparentPrimitives_.add();
		pi->set(&pg.primitive(lod), colour, transform);
	}
}

// Render primitive in specified colour
void RenderEngine::renderPrimitive(Primitive* primitive, bool isTransparent, GLfloat *colour, Matrix& transform, GLenum fillMode)
{
	if ((!isTransparent) || (fillMode != GL_FILL) || ((colour != NULL) && (colour[3] > 0.99f)))
	{
		// Add primitive info to solid objects list
		PrimitiveInfo *pi = solidPrimitives_.add();
		pi->set(primitive, colour, transform, fillMode);
	}
	else
	{
		// Add primitive info to transparent objects list
		PrimitiveInfo *pi = transparentPrimitives_.add();
		pi->set(primitive, colour, transform);
	}
}

// Add text primitive for rendering later
void RenderEngine::renderTextPrimitive(int x, int y, const char *text, QChar addChar, bool rightalign)
{
	textPrimitives_.add(x, y, text, addChar, rightalign);
}

// Add text primitive for rendering later (screen position calculated from 3D model coordinates)
void RenderEngine::renderTextPrimitive(Vec3<double> vec, const char *text, QChar addChar, bool rightalign)
{
	// Project atom and render text
	Vec4<double> screenr;			// OPTIMIZE - Hardcode 'modelToWorld' here
	Vec3<double> r = modelToWorld(vec, &screenr);
	if (r.z < -1.0) textPrimitives_.add(screenr.x, screenr.y, text, addChar, rightalign);
}

// Search for primitive associated to specified Grid pointer
GridPrimitive *RenderEngine::findGridPrimitive(Grid *g)
{
	GridPrimitive *gp = NULL;
	for (gp = gridPrimitives_.first(); gp != NULL; gp = (GridPrimitive*) gp->next) if (gp->source() == g) break;
	return gp;
}

// Remove grid primitive from list (if it exists)
void RenderEngine::removeGridPrimitive(Grid *g)
{
	GridPrimitive *gp = NULL;
	for (gp = gridPrimitives_.first(); gp != NULL; gp = (GridPrimitive*) gp->next) if (gp->source() == g) break;
	if (gp != NULL) gridPrimitives_.remove(gp);
}

// Sort and render filtered polygons by depth
void RenderEngine::sortAndSendGL()
{
	// Transform and render each solid primitive in the list
	for (PrimitiveInfo *pi = solidPrimitives_.first(); pi != NULL; pi = pi->next)
	{
		// If colour data is not present in the vertex data array, use the colour stored in the PrimitiveInfo object
		if (!pi->primitive()->colouredVertexData()) glColor4fv(pi->colour());
		glPolygonMode(GL_FRONT_AND_BACK, pi->fillMode());
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
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
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
