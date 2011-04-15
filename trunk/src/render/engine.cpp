/*
	*** Rendering Engine
	*** src/render/engine.cpp
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
#include "base/messenger.h"
#include "classes/prefs.h"
#include "classes/forcefieldatom.h"
#include "model/model.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"
#include "main/aten.h"
#include <math.h>
#ifdef _WIN32
#include "glext.h"
#endif

/*
// Render Primitives
*/

// Constructor
RenderPrimitives::RenderPrimitives()
{
	// Primitives
	scaledAtoms_ = new PrimitiveGroup[elements().nElements()];
	selectedScaledAtoms_ = new PrimitiveGroup[elements().nElements()];
	primitiveQuality_ = -1;
}

// Destructor
RenderPrimitives::~RenderPrimitives()
{
	delete[] scaledAtoms_;
	delete[] selectedScaledAtoms_;
}

// (Re)Generate primitives
void RenderPrimitives::createPrimitives(int quality)
{
	msg.enter("RenderPrimitives::createPrimitives");
	double radius, lodratio, aradius[Atom::nDrawStyles], bradius[Atom::nDrawStyles], selscale;
	int n, m, lod, nstacks, nslices;
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
	originCubes_.clear();
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
		
		// Bond primitive accuracy
		nstacks = max(1,(int) (quality*lodratio*0.25));
		nslices = max(3,(int) (quality*lodratio));
		
		// All Stick styles, all bond types
		bonds_[Atom::StickStyle][Bond::Single].primitive(lod).plotLine(0,0,0,0,0,1);
		bonds_[Atom::StickStyle][Bond::Aromatic].primitive(lod).plotLine(0,0,0,0,0,1);
		bonds_[Atom::StickStyle][Bond::Double].primitive(lod).plotLine(-bradius[Atom::StickStyle]*0.5,0,0,-bradius[Atom::StickStyle]*0.5,0,1);
		bonds_[Atom::StickStyle][Bond::Double].primitive(lod).plotLine(bradius[Atom::StickStyle]*0.5,0,0,bradius[Atom::StickStyle]*0.5,0,1);
		bonds_[Atom::StickStyle][Bond::Triple].primitive(lod).plotLine(0,0,0,0,0,1);
		bonds_[Atom::StickStyle][Bond::Triple].primitive(lod).plotLine(-bradius[Atom::StickStyle]*0.66,0,0,-bradius[Atom::StickStyle]*0.66,0,1);
		bonds_[Atom::StickStyle][Bond::Triple].primitive(lod).plotLine(bradius[Atom::StickStyle]*0.66,0,0,bradius[Atom::StickStyle]*0.66,0,1);
		
		// All sphere styles - Single and Aromatic Bonds
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
		cubes_.primitive(lod).createCube(1.0, max(1, int(quality*lodratio)), -0.5, -0.5, -0.5);
		originCubes_.primitive(lod).createCube(1.0, max(1, int(quality*lodratio)), 0.0, 0.0, 0.0);
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

	msg.exit("RenderPrimitives::createPrimitives");
}

// Create instance for primitives
void RenderPrimitives::pushInstance(const QGLContext* context)
{
	msg.enter("RenderPrimitives::pushInstance");
	for (int n=0; n<Atom::nDrawStyles; ++n)
	{
		atoms_[n].pushInstance(context);
		selectedAtoms_[n].pushInstance(context);
		for (int m=0; m<Bond::nBondTypes; ++m)
		{
			bonds_[n][m].pushInstance(context);
			selectedBonds_[n][m].pushInstance(context);
		}
	}
	for (int n=0; n<elements().nElements(); ++n)
	{
		scaledAtoms_[n].pushInstance(context);
		selectedScaledAtoms_[n].pushInstance(context);
	}
	tubeRings_.pushInstance(context);
	segmentedTubeRings_.pushInstance(context);
	lineRings_.pushInstance(context);
	segmentedLineRings_.pushInstance(context);
	spheres_.pushInstance(context);
	cubes_.pushInstance(context);
	originCubes_.pushInstance(context);
	cylinders_.pushInstance(context);
	cones_.pushInstance(context);
	wireCube_.pushInstance(context);
	crossedCube_.pushInstance(context);
	cellAxes_.pushInstance(context);
	rotationGlobe_.pushInstance(context);
	rotationGlobeAxes_.pushInstance(context);
	msg.exit("RenderPrimitives::pushInstance");
}

// Pop topmost instance for primitives
void RenderPrimitives::popInstance()
{
	msg.enter("RenderPrimitives::popInstance");
	for (int n=0; n<Atom::nDrawStyles; ++n)
	{
		atoms_[n].popInstance();
		selectedAtoms_[n].popInstance();
		for (int m=0; m<Bond::nBondTypes; ++m)
		{
			bonds_[n][m].popInstance();
			selectedBonds_[n][m].popInstance();
		}
	}
	for (int n=0; n<elements().nElements(); ++n)
	{
		scaledAtoms_[n].popInstance();
		selectedScaledAtoms_[n].popInstance();
	}
	tubeRings_.popInstance();
	segmentedTubeRings_.popInstance();
	lineRings_.popInstance();
	segmentedLineRings_.popInstance();
	spheres_.popInstance();
	cubes_.popInstance();
	originCubes_.popInstance();
	cylinders_.popInstance();
	cones_.popInstance();
	wireCube_.popInstance();
	crossedCube_.popInstance();
	cellAxes_.popInstance();
	rotationGlobe_.popInstance();
	rotationGlobeAxes_.popInstance();
	msg.exit("RenderPrimitives::popInstance");
}

/*
// Render Engine
*/

// Constructor
RenderEngine::RenderEngine()
{
	// Primitives
	for (int n=0; n<nTriangleStyles; ++n)
	{
		glyphTriangles_[n].setColourData(TRUE);
		glyphTriangles_[n].setNoInstances();
	}
	stickLines_.setColourData(TRUE);
	stickLines_.setType(GL_LINES);
	stickSelectedLines_.setColourData(TRUE);
	stickSelectedLines_.setType(GL_LINES);
	glyphLines_.setColourData(TRUE);
	glyphLines_.setType(GL_LINES);
	glyphLines_.setNoInstances();
	initialiseTransparency();
	scaledAtomAdjustments_ = new double[elements().nElements()];
	primitives_[0].createPrimitives(prefs.primitiveQuality());
	primitives_[1].createPrimitives(prefs.imagePrimitiveQuality());
	lastSource_ = NULL;
	rebuildSticks_ = FALSE;
	Q_ = 0;
	calculateAdjustments();
}

// Destructor
RenderEngine::~RenderEngine()
{
	delete[] scaledAtomAdjustments_;
}

// Update transformation matrix
void RenderEngine::setTransformationMatrix(Matrix &mat, Vec3<double> cellcentre)
{
	modelTransformationMatrix_ = mat;
	modelTransformationMatrix_.applyTranslation(-cellcentre.x, -cellcentre.y, -cellcentre.z);
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

// Render primitive in specified colour and level of detail (coords/transform used only if filtered)
void RenderEngine::renderPrimitive(RenderEngine::RenderingObject obj, PrimitiveGroup& pg, GLfloat* colour, Matrix& transform, GLenum fillMode, GLfloat lineWidth)
{
	if (!activePrimitiveLists_[obj]) return;
	if ((colour[3] > 0.99f) || (fillMode != GL_FILL))
	{
		// Add primitive info to solid objects list
		PrimitiveInfo *pi = solidPrimitives_[obj].add();
		pi->set(&pg, colour, transform, fillMode, lineWidth);
	}
	else
	{
		// Add primitive info to transparent objects list
		PrimitiveInfo *pi = transparentPrimitives_[obj].add();
		pi->set(&pg, colour, transform);
	}
}

// Render primitive in specified colour
void RenderEngine::renderPrimitive(RenderEngine::RenderingObject obj, Primitive* primitive, bool isTransparent, GLfloat *colour, Matrix& transform, GLenum fillMode, GLfloat lineWidth)
{
	if (!activePrimitiveLists_[obj]) return;
	if ((!isTransparent) || (fillMode != GL_FILL) || ((colour != NULL) && (colour[3] > 0.99f)))
	{
		// Add primitive info to solid objects list
		PrimitiveInfo *pi = solidPrimitives_[obj].add();
		pi->set(primitive, colour, transform, fillMode, lineWidth);
	}
	else
	{
		// Add primitive info to transparent objects list
		PrimitiveInfo *pi = transparentPrimitives_[obj].add();
		pi->set(primitive, colour, transform);
	}
}

// Add text primitive for rendering later
void RenderEngine::renderTextPrimitive(int x, int y, const char *text, QChar addChar, bool rightalign)
{
	textPrimitives_.add(x, y, text, addChar, rightalign);
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
	Matrix A ;
	Primitive *prim;
	
	// Transform and render each solid primitive in each list
	for (int n=0; n<RenderEngine::nRenderingObjects; ++n)
	{
		for (PrimitiveInfo *pi = solidPrimitives_[n].first(); pi != NULL; pi = pi->next)
		{
			// If the info structure has a pointer to a primitive in it, use that.
			// Otherwise, work out a level of detail value to pass to the primitive group referenced.
			prim = pi->primitive();
			if (prim == NULL) prim = (Q_ == 1 ? pi->bestPrimitive() : pi->primitive(modelTransformationMatrix_));
			
			// If colour data is not present in the vertex data array, use the colour stored in the PrimitiveInfo object
			if (!prim->colouredVertexData()) glColor4fv(pi->colour());
			glPolygonMode(GL_FRONT_AND_BACK, pi->fillMode());
			if (pi->fillMode() == GL_LINE)
			{
				glLineWidth(pi->lineWidth());
				glDisable(GL_LIGHTING);
			}
			A = modelTransformationMatrix_ * pi->localTransform();
			glLoadMatrixd(A.matrix());
			prim->sendToGL();
			
			if (pi->fillMode() == GL_LINE)
			{
				glLineWidth(1.0);
				glEnable(GL_LIGHTING);
			}
		}
	}
	
	// Draw stick primitives
	glDisable(GL_LIGHTING);
	glLoadMatrixd(modelTransformationMatrix_.matrix());
	glLineWidth(2.0);
	stickLines_.sendToGL();
	glLineWidth(4.0);
	stickSelectedLines_.sendToGL();
	glEnable(GL_LIGHTING);
	glLineWidth(1.0);
	
	// Transform and render each transparent primitive in each list, unless transparencyCorrect_ is off.
	if (prefs.transparencyCorrect())
	{
		triangleChopper_.emptyTriangles();
		for (int n=0; n<RenderEngine::nRenderingObjects; ++n)
			for (PrimitiveInfo *pi = transparentPrimitives_[n].first(); pi != NULL; pi = pi->next) triangleChopper_.storeTriangles(pi, modelTransformationMatrix_);
		glLoadIdentity();
// 		glLoadMatrixd(modelTransformationMatrix_.matrix());
		glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
		triangleChopper_.sendToGL();
		glPopClientAttrib();
	}
	else for (int n=0; n<RenderEngine::nRenderingObjects; ++n)
	{
		for (PrimitiveInfo *pi = transparentPrimitives_[n].first(); pi != NULL; pi = pi->next)
		{
			// If the info structure has a pointer to a primitive in it, use that.
			// Otherwise, work out a level of detail value to pass to the primitive group referenced.
			prim = pi->primitive();
			if (prim == NULL) prim = (Q_ == 1 ? pi->bestPrimitive() : pi->primitive(modelTransformationMatrix_));
			if (!prim->colouredVertexData()) glColor4fv(pi->colour());
			A = modelTransformationMatrix_ * pi->localTransform();
			glLoadMatrixd(A.matrix());
			prim->sendToGL();
		}
	}
}

// (Re)initialise transparency filter
void RenderEngine::initialiseTransparency()
{
	triangleChopper_.initialise(prefs.transparencyBinStartZ(), prefs.transparencyNBins(), prefs.transparencyBinWidth());
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

// Push primitives instance (in specified quality)
void RenderEngine::pushInstance(bool highQuality, const QGLContext *context)
{
	msg.print(Messenger::Verbose, "Pushing primitive instance for context %p\n", context);
	if (context == gui.mainContext()) msg.print(Messenger::Verbose, "This instance is associated to the main context.\n");
	primitives_[highQuality].pushInstance(context);
	// Push separate instance of the stick primitives
	stickLines_.pushInstance(context);
	stickSelectedLines_.pushInstance(context);
}

// Pop topmost primitive instance
void RenderEngine::popInstance(bool highQuality)
{
	primitives_[highQuality].popInstance();
	stickLines_.popInstance();
	stickSelectedLines_.popInstance();
}

// Update all primitives (following prefs change, etc.)
void RenderEngine::updatePrimitives(const QGLContext *context)
{
	// Regenerate vertex information
	primitives_[0].createPrimitives(prefs.primitiveQuality());
	primitives_[1].createPrimitives(prefs.imagePrimitiveQuality());
	
	// Generate new VBOs / display lists - pop and push a context
	primitives_[0].popInstance();
	primitives_[1].popInstance();
	primitives_[0].pushInstance(context);
	primitives_[1].pushInstance(context);
	
	// Recalculate adjustments for bond positioning
	calculateAdjustments();
}

// Render text objects (with supplied QPainter)
void RenderEngine::renderText(QPainter &painter, TCanvas *canvas)
{
	textPrimitives_.renderAll(painter, canvas);
}

// Render 3D
void RenderEngine::render3D(bool highQuality, Model *source, TCanvas *canvas, bool currentModel)
{
	GLfloat colour[4];

	// Set initial transformation matrix, including any translation occurring from cell...
	setTransformationMatrix(source->modelViewMatrix(), source->cell()->centre());
	
	// Set target matrix mode and reset it, and set colour mode
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);

	// Store quality specifier
	Q_ = highQuality ? 1 : 0;

	// Grab model-specific viewport
	GLint *vp = source->viewportMatrix();

	// Render rotation globe in small viewport in lower right-hand corner
	if (prefs.viewRotationGlobe())
	{
		int n = prefs.globeSize();
		if (aten.nVisibleModels() > 2) n /= 2;
		glViewport(vp[0]+vp[2]-n,vp[1],n,n);
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glMultMatrixd(source->globeProjectionMatrix().matrix());
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		Matrix A = modelTransformationMatrix_;
		A.removeTranslationAndScaling();
		A[14] = -1.2;
		glMultMatrixd(A.matrix());
		prefs.copyColour(Prefs::GlobeColour, colour);
		glColor4fv(colour);
		glNormal3d(0.0,0.0,1.0);
		primitives_[Q_].rotationGlobe_.sendToGL();
		prefs.copyColour(Prefs::GlobeAxesColour, colour);
		glColor4fv(colour);
		primitives_[Q_].rotationGlobeAxes_.sendToGL();
	}
	
	// Prepare for model rendering
	glViewport(vp[0], vp[1], vp[2], vp[3]);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glMultMatrixd(source->modelProjectionMatrix().matrix());
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	
	// Clear the necessary triangle lists (i.e. only those for which the content may have changed)
	// By default, regenerate all lists (the case if the source model pointer has changed)
	int n;
	for (n=0; n<RenderEngine::nRenderingObjects; ++n) activePrimitiveLists_[n] = TRUE;
	if (lastSource_ == source)
	{
		// Check model logs against logs stored when the lists were last generated
		bool redobasic = !lastLog_.isSame(Log::Coordinates,source->changeLog);
		if (!redobasic) redobasic = !lastLog_.isSame(Log::Structure,source->changeLog);
		// If the model style has changed, need to rerender both basic and selection lists. Otherwise, depends on other logs
		if (redobasic || (!lastLog_.isSame(Log::Style,source->changeLog)))
		{
			activePrimitiveLists_[RenderEngine::BasicObject] = TRUE;
			activePrimitiveLists_[RenderEngine::AtomSelectionObject] = TRUE;
		}
		else
		{
			activePrimitiveLists_[RenderEngine::BasicObject] = FALSE;
			activePrimitiveLists_[RenderEngine::AtomSelectionObject] = !lastLog_.isSame(Log::Selection, source->changeLog);
		}
		
		// Stick style primitives
		if (redobasic || (!lastLog_.isSame(Log::Style,source->changeLog)) || (!lastLog_.isSame(Log::Selection,source->changeLog)))
		{
			stickLines_.forgetAll();
			stickSelectedLines_.forgetAll();
			rebuildSticks_ = TRUE;
		}
		else rebuildSticks_ = FALSE;
		
		// Glyphs must be redone on basic change (since they may follow atom coordinates
		if (redobasic || !lastLog_.isSame(Log::Glyphs, source->changeLog)) activePrimitiveLists_[RenderEngine::GlyphObject]  = TRUE;
		else activePrimitiveLists_[RenderEngine::GlyphObject]  = FALSE;
		
		// Grids only depend on their own log
		if (!lastLog_.isSame(Log::Grids, source->changeLog)) activePrimitiveLists_[RenderEngine::GridObject] = TRUE;
	}
	else
	{
		stickLines_.forgetAll();
		stickSelectedLines_.forgetAll();
	}
	
	// Clear flagged lists
	for (n=0; n<RenderEngine::nRenderingObjects; ++n) if (activePrimitiveLists_[n])
	{
		solidPrimitives_[n].clear();
		transparentPrimitives_[n].clear();
	}
	textPrimitives_.forgetAll();
	
	// Extra lists to clear for Glyphs
	if (activePrimitiveLists_[RenderEngine::GlyphObject])
	{
		glyphTriangles_[RenderEngine::SolidTriangle].forgetAll();
		glyphTriangles_[RenderEngine::TransparentTriangle].forgetAll();
		glyphTriangles_[RenderEngine::WireTriangle].forgetAll();
		glyphLines_.forgetAll();
	}
	
	// Always draw unit cell, regardless of list status
	renderCell(source);
	// Draw main model (atoms, bonds, etc.)
	if (activePrimitiveLists_[RenderEngine::BasicObject] || activePrimitiveLists_[RenderEngine::AtomSelectionObject]) renderModel(source);
	// Draw model glyphs
	if (activePrimitiveLists_[RenderEngine::GlyphObject]) renderGlyphs(source);
	renderTextGlyphs(source, gui.mainWidget());
	// Draw model grids
	if (activePrimitiveLists_[RenderEngine::GridObject]) renderGrids(source);
	
	lastSource_ = source;
	lastLog_ = source->changeLog;
	
	if (gui.exists())
	{
		// Render embellshments for current UserAction
		renderUserActions(source, gui.mainWidget());	
		// Render extras arising from open tool windows (current model only)
		if (currentModel) renderWindowExtras(source);
	}

	// If the stick primitives were regenerated, need to create a new instance (after popping the old one)
	if (rebuildSticks_)
	{
		stickLines_.popInstance();
		stickSelectedLines_.popInstance();
		stickLines_.pushInstance(gui.mainContext());
		stickSelectedLines_.pushInstance(gui.mainContext());
	}

	// All 3D primitive objects have now been filtered, so sort and send to GL
	sortAndSendGL();
	
	// Render overlays
	renderModelOverlays(source);
}
