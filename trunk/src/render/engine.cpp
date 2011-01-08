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
#include "gui/gui.h"
#include "gui/tcanvas.uih"
#include <math.h>

// Constructor
RenderEngine::RenderEngine()
{
	// Primitives
	scaledAtoms_ = new PrimitiveGroup[elements().nElements()];
	selectedScaledAtoms_ = new PrimitiveGroup[elements().nElements()];
	scaledAtomAdjustments_ = new double[elements().nElements()];
	primitiveQuality_ = -1;
	glyphTriangles_[RenderEngine::SolidTriangle].setColourData(TRUE);
	glyphTriangles_[RenderEngine::TransparentTriangle].setColourData(TRUE);
	glyphTriangles_[RenderEngine::WireTriangle].setColourData(TRUE);
	initialiseTransparency();
	calculateAdjustments();
}

// Destructor
RenderEngine::~RenderEngine()
{
	delete[] scaledAtoms_;
	delete[] selectedScaledAtoms_;
	delete[] scaledAtomAdjustments_;
}

/*
// Primitive Generation
*/

// (Re)Generate primitives
void RenderEngine::createPrimitives(int quality, bool force)
{
	msg.enter("RenderEngine::createPrimitives");
	double radius, lodratio, aradius[Atom::nDrawStyles], bradius[Atom::nDrawStyles], selscale;
	int n, m, lod, nstacks, nslices;
	if ((!force) && (primitiveQuality_ == quality))
	{
		msg.exit("RenderEngine::createPrimitives");
		return;
	}
	primitiveQuality_ = quality;
	// Clear old primitive groups
	for (n=0; n<Atom::nDrawStyles; ++n)
	{
		atoms_[n].clear();
		selectedAtoms_[n].clear();	
		for (m=0; m<Bond::nBondTypes; ++m)
		{
			bonds_[n][m].clear();
			selectedBonds_[n][m].clear();
		}
	}
	for (n=0; n<elements().nElements(); ++n)
	{
		scaledAtoms_[n].clear();
		selectedScaledAtoms_[n].clear();
	}
	tubeRings_.clear();
	segmentedTubeRings_.clear();
	lineRings_.clear();
	segmentedLineRings_.clear();
	spheres_.clear();
	cubes_.clear();
	cylinders_.clear();
	cones_.clear();
	wireCube_.clear();
	crossedCube_.clear();
	cellAxes_.clear();
	rotationGlobe_.clear();
	rotationGlobeAxes_.clear();
	
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
		nstacks = max(3,(int) (quality*lodratio*0.75));
		nslices = max(3,(int) (quality*lodratio*1.5));
		
		// Atom Styles (Atom::StickStyle, Atom::TubeStyle, and Atom::SphereStyle)
		atoms_[Atom::StickStyle].primitive(lod).createCross(0.5,3-lod);
		atoms_[Atom::TubeStyle].primitive(lod).plotSphere(aradius[Atom::TubeStyle], nstacks, nslices);
		atoms_[Atom::SphereStyle].primitive(lod).plotSphere(aradius[Atom::SphereStyle], nstacks, nslices);
		selectedAtoms_[Atom::TubeStyle].primitive(lod).plotSphere(aradius[Atom::TubeStyle]*selscale, nstacks, nslices);
		selectedAtoms_[Atom::SphereStyle].primitive(lod).plotSphere(aradius[Atom::SphereStyle]*selscale, nstacks, nslices);
		
		// Atom Styles (Atom::ScaledStyle)
		for (n = 0; n<elements().nElements(); ++n)
		{
			radius = aradius[Atom::ScaledStyle] * elements().el[n].atomicRadius;
			scaledAtoms_[n].primitive(lod).plotSphere(radius, nstacks, nslices);
			selectedScaledAtoms_[n].primitive(lod).plotSphere(radius*selscale, nstacks, nslices);
		}
		
		// Bond Styles
		nstacks = max(1,(int) (quality*lodratio*0.25));
		nslices = max(3,(int) (quality*lodratio));
		
		// All styles - Single and Aromatic Bonds
		bonds_[Atom::TubeStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::TubeStyle], bradius[Atom::TubeStyle], nstacks, nslices);
		bonds_[Atom::SphereStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::SphereStyle], bradius[Atom::SphereStyle], nstacks, nslices);
		bonds_[Atom::ScaledStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::ScaledStyle], bradius[Atom::ScaledStyle], nstacks, nslices);
		selectedBonds_[Atom::TubeStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::TubeStyle]*selscale, bradius[Atom::TubeStyle]*selscale, nstacks, nslices);
		selectedBonds_[Atom::SphereStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::SphereStyle]*selscale, bradius[Atom::SphereStyle]*selscale, nstacks, nslices);
		selectedBonds_[Atom::ScaledStyle][Bond::Single].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::ScaledStyle]*selscale, bradius[Atom::ScaledStyle]*selscale, nstacks, nslices);
		bonds_[Atom::TubeStyle][Bond::Aromatic].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::TubeStyle], bradius[Atom::TubeStyle], nstacks, nslices);
		bonds_[Atom::SphereStyle][Bond::Aromatic].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::SphereStyle], bradius[Atom::SphereStyle], nstacks, nslices);
		bonds_[Atom::ScaledStyle][Bond::Aromatic].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::ScaledStyle], bradius[Atom::ScaledStyle], nstacks, nslices);
		selectedBonds_[Atom::TubeStyle][Bond::Aromatic].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::TubeStyle]*selscale, bradius[Atom::TubeStyle]*selscale, nstacks, nslices);
		selectedBonds_[Atom::SphereStyle][Bond::Aromatic].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::SphereStyle]*selscale, bradius[Atom::SphereStyle]*selscale, nstacks, nslices);
		selectedBonds_[Atom::ScaledStyle][Bond::Aromatic].primitive(lod).plotCylinder(0,0,0,0,0,1,bradius[Atom::ScaledStyle]*selscale, bradius[Atom::ScaledStyle]*selscale, nstacks, nslices);
		
		// All styles - Double Bond
		bonds_[Atom::TubeStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);
		bonds_[Atom::TubeStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5, bradius[Atom::TubeStyle]*0.5, nstacks, nslices);
		bonds_[Atom::SphereStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5, bradius[Atom::SphereStyle]*0.5, nstacks, nslices);
		bonds_[Atom::SphereStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5, bradius[Atom::SphereStyle]*0.5, nstacks, nslices);
		bonds_[Atom::ScaledStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5, bradius[Atom::ScaledStyle]*0.5, nstacks, nslices);
		bonds_[Atom::ScaledStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5, bradius[Atom::ScaledStyle]*0.5, nstacks, nslices);
		selectedBonds_[Atom::TubeStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5*selscale, bradius[Atom::TubeStyle]*0.5*selscale, nstacks, nslices);
		selectedBonds_[Atom::TubeStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.5*selscale, bradius[Atom::TubeStyle]*0.5*selscale, nstacks, nslices);
		selectedBonds_[Atom::SphereStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::SphereStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5*selscale, bradius[Atom::SphereStyle]*0.5*selscale, nstacks, nslices);
		selectedBonds_[Atom::SphereStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::SphereStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.5*selscale, bradius[Atom::SphereStyle]*0.5*selscale, nstacks, nslices);
		selectedBonds_[Atom::ScaledStyle][Bond::Double].primitive(lod).plotCylinder(-bradius[Atom::ScaledStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5*selscale, bradius[Atom::ScaledStyle]*0.5*selscale, nstacks, nslices);
		selectedBonds_[Atom::ScaledStyle][Bond::Double].primitive(lod).plotCylinder(bradius[Atom::ScaledStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.5*selscale, bradius[Atom::ScaledStyle]*0.5*selscale, nstacks, nslices);
		
		// All styles - Triple Bond
		bonds_[Atom::TubeStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::TubeStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.33, bradius[Atom::TubeStyle]*0.33, nstacks, nslices);
		bonds_[Atom::TubeStyle][Bond::Triple].primitive(lod).plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.33, bradius[Atom::TubeStyle]*0.33, nstacks, nslices);
		bonds_[Atom::TubeStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::TubeStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.33, bradius[Atom::TubeStyle]*0.33, nstacks, nslices);
		bonds_[Atom::SphereStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::SphereStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.33, bradius[Atom::SphereStyle]*0.33, nstacks, nslices);
		bonds_[Atom::SphereStyle][Bond::Triple].primitive(lod).plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.33, bradius[Atom::SphereStyle]*0.33, nstacks, nslices);
		bonds_[Atom::SphereStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::SphereStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.33, bradius[Atom::SphereStyle]*0.33, nstacks, nslices);
		bonds_[Atom::ScaledStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::ScaledStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.33, bradius[Atom::ScaledStyle]*0.33, nstacks, nslices);
		bonds_[Atom::ScaledStyle][Bond::Triple].primitive(lod).plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.33, bradius[Atom::ScaledStyle]*0.33, nstacks, nslices);
		bonds_[Atom::ScaledStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::ScaledStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.33, bradius[Atom::ScaledStyle]*0.33, nstacks, nslices);
		selectedBonds_[Atom::TubeStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::TubeStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.33*selscale, bradius[Atom::TubeStyle]*0.33*selscale, nstacks, nslices);
		selectedBonds_[Atom::TubeStyle][Bond::Triple].primitive(lod).plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.33*selscale, bradius[Atom::TubeStyle]*0.33*selscale, nstacks, nslices);
		selectedBonds_[Atom::TubeStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::TubeStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::TubeStyle]*0.33*selscale, bradius[Atom::TubeStyle]*0.33*selscale, nstacks, nslices);
		selectedBonds_[Atom::SphereStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::SphereStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.33*selscale, bradius[Atom::SphereStyle]*0.33*selscale, nstacks, nslices);
		selectedBonds_[Atom::SphereStyle][Bond::Triple].primitive(lod).plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.33*selscale, bradius[Atom::SphereStyle]*0.33*selscale, nstacks, nslices);
		selectedBonds_[Atom::SphereStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::SphereStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::SphereStyle]*0.33*selscale, bradius[Atom::SphereStyle]*0.33*selscale, nstacks, nslices);
		selectedBonds_[Atom::ScaledStyle][Bond::Triple].primitive(lod).plotCylinder(-bradius[Atom::ScaledStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.33*selscale, bradius[Atom::ScaledStyle]*0.33*selscale, nstacks, nslices);
		selectedBonds_[Atom::ScaledStyle][Bond::Triple].primitive(lod).plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.33*selscale, bradius[Atom::ScaledStyle]*0.33*selscale, nstacks, nslices);
		selectedBonds_[Atom::ScaledStyle][Bond::Triple].primitive(lod).plotCylinder(bradius[Atom::ScaledStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bradius[Atom::ScaledStyle]*0.33*selscale, bradius[Atom::ScaledStyle]*0.33*selscale, nstacks, nslices);
		
		// Other primitives
		nstacks = max(3,(int) (quality*lodratio*0.75));
		nslices = max(3,(int) (quality*lodratio*1.5));
		cubes_.primitive(lod).createCube(1.0, max(1, int(quality*lodratio)) );
		spheres_.primitive(lod).plotSphere(1.0, nstacks, nslices);
		cylinders_.primitive(lod).plotCylinder(0,0,0,0,0,1,1.0,1.0,nstacks, nslices);
		cones_.primitive(lod).plotCylinder(0,0,0,0,0,1,1.0,0.0,nstacks,nslices);
		tubeRings_.primitive(lod).plotRing(1.0, 0.1, 10, nslices, 5);
		segmentedTubeRings_.primitive(lod).plotRing(1.0, 0.1, 20, nslices, 5, TRUE);
		lineRings_.primitive(lod).plotCircle(1.0, 10, 5);
		segmentedLineRings_.primitive(lod).plotCircle(1.0, 20, 5, TRUE);
	}

	// One-off objects
	wireCube_.createWireCube(1.0);
	crossedCube_.createCrossedCube(1.0);
	cellAxes_.createCellAxes();
	rotationGlobe_.plotSphere(0.75,10,13);
	rotationGlobeAxes_.createRotationGlobeAxes(8,10);
	// Recalculate adjustments for bond positioning
	calculateAdjustments();
	msg.exit("RenderEngine::createPrimitives");
}

// (Re)initialise transparency filter
void RenderEngine::initialiseTransparency()
{
	triangleChopper_.initialise(prefs.transparencyBinStartZ(), prefs.transparencyNBins(), prefs.transparencyBinWidth());
}

// Calculate atom/bond adjustments
void RenderEngine::calculateAdjustments()
{
	double atomradius, bondradius, theta;
	int i;

	// Triangle formed between atom radius (H), bond radius (O), and unknown (A)
	// Determine angle between H and O and calculate adjustment (=H-A)

	// Sphere Style
	atomradius = prefs.atomStyleRadius(Atom::SphereStyle);
	bondradius = prefs.bondStyleRadius(Atom::SphereStyle);
	theta = asin(bondradius / atomradius);
	sphereAtomAdjustment_ = atomradius - atomradius*cos(theta);

	// Scaled Style
	theta = asin(bondradius / atomradius);
	sphereAtomAdjustment_ = atomradius - atomradius*cos(theta);
	for (i = 0; i<elements().nElements(); ++i)
	{
		atomradius = prefs.atomStyleRadius(Atom::ScaledStyle) * elements().el[i].atomicRadius;
		theta = asin(bondradius / atomradius);
		scaledAtomAdjustments_[i] = (atomradius - atomradius*cos(theta));
	}
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
Vec3<double> &RenderEngine::screenToModel(int x, int y, double z)
{
	msg.enter("RenderEngine::screenToModel");
	static Vec3<double> modelr;
	Vec4<double> temp, worldr;
	int newx, newy;
	double dx, dy;
	
	// Grab transformation matrix and invert
	Matrix itransform = modelTransformationMatrix_;
	itransform.invert();
	
	// Mirror y-coordinate
	y = viewportMatrix_[3] - y;

	// Project points at guide z-position and two other points along literal x and y to get scaling factors for screen coordinates
	worldr.set(0.0,0.0,z, 1.0);
	temp = modelProjectionMatrix_ * worldr;
	newx = viewportMatrix_[0] + viewportMatrix_[2]*(temp.x / temp.w + 1.0)*0.5;
	newy = viewportMatrix_[1] + viewportMatrix_[3]*(temp.y / temp.w + 1.0)*0.5;
		
	for (int n=0; n<10; ++n)
	{
		// Determine new (better) coordinate from a yardstick centred at current world coordinates
		temp = modelProjectionMatrix_ * Vec4<double>(worldr.x+1.0, worldr.y+1.0, worldr.z, worldr.w);
		dx = viewportMatrix_[0] + viewportMatrix_[2]*(temp.x / temp.w + 1.0)*0.5 - newx;
		dy = viewportMatrix_[1] + viewportMatrix_[3]*(temp.y / temp.w + 1.0)*0.5 - newy;

		worldr.add((x-newx)/dx, (y-newy)/dy, 0.0, 0.0);
// 		printf ("N=%i", n); worldr.print();
		temp = modelProjectionMatrix_ * worldr;
		newx = viewportMatrix_[0] + viewportMatrix_[2]*(temp.x / temp.w + 1.0)*0.5;
		newy = viewportMatrix_[1] + viewportMatrix_[3]*(temp.y / temp.w + 1.0)*0.5;
// 		printf("NEW dx = %f, dy = %f, wantedxy = %f, %f\n", newx, newy, x, y);
		if ((x == newx) && (y == newy)) break;
	}
	
	// Finally, invert to model coordinates
	modelr = itransform * Vec3<double>(worldr.x, worldr.y, worldr.z);
	
	msg.exit("RenderEngine::screenToModel");
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

// Return level of detail for supplied coordinate (return -1 for 'behind viewer')
int RenderEngine::levelOfDetail(Vec3<double> &r, TCanvas *canvas)
{
	// If z is less than 0, don't even bother continuing since its behind the viewer
	double z = -(r.x*modelTransformationMatrix_[2] + r.y*modelTransformationMatrix_[6] + r.z*modelTransformationMatrix_[10] + modelTransformationMatrix_[14]);
	if (z < 0) return -1;
	// If we are rendering to an offscreen bitmap, don't bother with levelofdetal calculation (always return best quality)
	if (canvas->offScreenRendering()) return 0;
	// Determine level of detail to use for primitives
	if (z < prefs.levelOfDetailStartZ()) return 0;
	else return int((z-prefs.levelOfDetailStartZ()) / prefs.levelOfDetailWidth());

}

// Render primitive in specified colour and level of detail (coords/transform used only if filtered)
void RenderEngine::renderPrimitive(PrimitiveGroup &pg, int lod, GLfloat *colour, Matrix &transform, GLenum fillMode, GLfloat lineWidth)
{
	if ((colour[3] > 0.99f) || (fillMode != GL_FILL))
	{
		// Add primitive info to solid objects list
		PrimitiveInfo *pi = solidPrimitives_.add();
		pi->set(&pg.primitive(lod), colour, transform, fillMode, lineWidth);
	}
	else
	{
		// Add primitive info to transparent objects list
		PrimitiveInfo *pi = transparentPrimitives_.add();
		pi->set(&pg.primitive(lod), colour, transform);
	}
}

// Render primitive in specified colour
void RenderEngine::renderPrimitive(Primitive* primitive, bool isTransparent, GLfloat *colour, Matrix& transform, GLenum fillMode, GLfloat lineWidth)
{
	if ((!isTransparent) || (fillMode != GL_FILL) || ((colour != NULL) && (colour[3] > 0.99f)))
	{
		// Add primitive info to solid objects list
		PrimitiveInfo *pi = solidPrimitives_.add();
		pi->set(primitive, colour, transform, fillMode, lineWidth);
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
		if (pi->fillMode() == GL_LINE) glLineWidth(pi->lineWidth());
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

// Render text objects (with supplied QPainter)
void RenderEngine::renderText(QPainter &painter, TCanvas *canvas)
{
	textPrimitives_.renderAll(painter, canvas);
}

// Render 3D
void RenderEngine::render3D(Model *source, TCanvas *canvas)
{
	GLfloat colour[4];

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
		Matrix A = modelTransformationMatrix_;
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
	
	if (gui.exists())
	{
		// Render embellshments for current UserAction
		renderUserActions(source, modelTransformationMatrix_, canvas);	
		// Render extras arising from open tool windows
		renderWindowExtras(source, modelTransformationMatrix_, canvas);
	}

	// All 3D primitive objects have now been filtered, so add triangles, then sort and send to GL
	renderPrimitive(&glyphTriangles_[RenderEngine::SolidTriangle], FALSE, NULL, modelTransformationMatrix_);
	renderPrimitive(&glyphTriangles_[RenderEngine::WireTriangle], FALSE, NULL, modelTransformationMatrix_, GL_LINE);
	renderPrimitive(&glyphTriangles_[RenderEngine::TransparentTriangle], TRUE, NULL, modelTransformationMatrix_);
	sortAndSendGL();
	
	// Render overlays
	renderModelOverlays(source, modelTransformationMatrix_, canvas);
}
