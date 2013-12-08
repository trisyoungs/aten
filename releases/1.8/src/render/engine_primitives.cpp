/*
	*** Rendering Engine Primitives
	*** src/render/engine_primitives.cpp
	Copyright T. Youngs 2007-2013

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

// Constructor
RenderPrimitives::RenderPrimitives()
{
	// Primitives
	requestedQuality_ = -1;
	currentQuality_ = -1;
	stackSize_ = 0;
	
	// Set names of primitives (for bug-tracking)
	atom_.setName("Atom");
	selectedAtom_.setName("SelectedAtom");
	bonds_[Atom::TubeStyle][Bond::Single].setName("Bond[Tube,Single]");
	bonds_[Atom::SphereStyle][Bond::Single].setName("Bond[Sphere,Single]");
	bonds_[Atom::ScaledStyle][Bond::Single].setName("Bond[Scaled,Single]");
	selectedBonds_[Atom::TubeStyle][Bond::Single].setName("SelectedBond[Tube,Single]");
	selectedBonds_[Atom::SphereStyle][Bond::Single].setName("SelectedBond[Sphere,Single]");
	selectedBonds_[Atom::ScaledStyle][Bond::Single].setName("SelectedBond[Scaled,Single]");
	bonds_[Atom::TubeStyle][Bond::Double].setName("Bond[Tube,Double]");
	bonds_[Atom::SphereStyle][Bond::Double].setName("Bond[Sphere,Double]");
	bonds_[Atom::ScaledStyle][Bond::Double].setName("Bond[Scaled,Double]");
	selectedBonds_[Atom::TubeStyle][Bond::Double].setName("SelectedBond[Tube,Double]");
	selectedBonds_[Atom::SphereStyle][Bond::Double].setName("SelectedBond[Sphere,Double]");
	selectedBonds_[Atom::ScaledStyle][Bond::Double].setName("SelectedBond[Scaled,Double]");
	bonds_[Atom::TubeStyle][Bond::Triple].setName("Bond[Tube,Triple]");
	bonds_[Atom::SphereStyle][Bond::Triple].setName("Bond[Sphere,Triple]");
	bonds_[Atom::ScaledStyle][Bond::Triple].setName("Bond[Scaled,Triple]");
	selectedBonds_[Atom::TubeStyle][Bond::Triple].setName("SelectedBond[Tube,Triple]");
	selectedBonds_[Atom::SphereStyle][Bond::Triple].setName("SelectedBond[Sphere,Triple]");
	selectedBonds_[Atom::ScaledStyle][Bond::Triple].setName("SelectedBond[Scaled,Triple]");
	bonds_[Atom::TubeStyle][Bond::Aromatic].setName("Bond[Tube,Aromatic]");
	bonds_[Atom::SphereStyle][Bond::Aromatic].setName("Bond[Sphere,Aromatic]");
	bonds_[Atom::ScaledStyle][Bond::Aromatic].setName("Bond[Scaled,Aromatic]");
	selectedBonds_[Atom::TubeStyle][Bond::Aromatic].setName("SelectedBond[Tube,Aromatic]");
	selectedBonds_[Atom::SphereStyle][Bond::Aromatic].setName("SelectedBond[Sphere,Aromatic]");
	selectedBonds_[Atom::ScaledStyle][Bond::Aromatic].setName("SelectedBond[Scaled,Aromatic]");
	cubes_.setName("Cubes");
	originCubes_.setName("OriginCubes");
	spheres_.setName("Spheres");
	cylinders_.setName("Cylinders");
	cones_.setName("Cones");
	tubeRings_.setName("TubeRings");
	segmentedTubeRings_.setName("SegmentedTubeRings");
	lineRings_.setName("LineRings");
	segmentedLineRings_.setName("SegmentedLineRings");
	wireCube_.setName("WireCube");
	crossedCube_.setName("CrossedCube");
	cellAxes_.setName("CellAxes");
	rotationGlobe_.setName("RotationGlobe");
	rotationGlobeAxes_.setName("RotationGlobeAxes");
}

// Destructor
RenderPrimitives::~RenderPrimitives()
{
}

// Set the desired primitive quality
void RenderPrimitives::setQuality(int quality)
{
	requestedQuality_ = quality;
}
	
// Return current primitive instance stacksize
int RenderPrimitives::stackSize()
{
	return stackSize_;
}

// (Re)Generate primitives
void RenderPrimitives::recreatePrimitives(bool force)
{
	msg.enter("RenderPrimitives::recreatePrimitives");
	double radius, lodratio, aradius[Atom::nDrawStyles], bradius[Atom::nDrawStyles], selscale;
	int n, m, lod, nstacks, nslices;
	
	// If current quality is the same as the requested quality, do nothing
	if ((requestedQuality_ == currentQuality_) && (!force))
	{
		msg.exit("RenderPrimitives::recreatePrimitives");
		return;
	}

	currentQuality_ = requestedQuality_;

	// Clear old primitive groups
	atom_.clear();
	selectedAtom_.clear();
	for (n=0; n<Atom::nDrawStyles; ++n)
	{
		for (m=0; m<Bond::nBondTypes; ++m)
		{
			bonds_[n][m].clear();
			selectedBonds_[n][m].clear();
		}
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
		lodratio = 1.0 - (double (lod)/prefs.levelsOfDetail());
		nstacks = max(3,(int) (currentQuality_*lodratio*0.75));
		nslices = max(3,(int) (currentQuality_*lodratio*1.5));
		
		// Atom Styles (Atom::StickStyle, Atom::TubeStyle, and Atom::SphereStyle)
		atom_.primitive(lod).plotSphere(1.0, nstacks, nslices);
		selectedAtom_.primitive(lod).plotSphere(selscale, nstacks, nslices);
		
		// Bond primitive accuracy
		nstacks = max(1,(int) (currentQuality_*lodratio*0.25));
		nslices = max(3,(int) (currentQuality_*lodratio));
		
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
		nstacks = max(3,(int) (currentQuality_*lodratio*0.75));
		nslices = max(3,(int) (currentQuality_*lodratio*1.5));
		cubes_.primitive(lod).createCube(1.0, max(1, int(currentQuality_*lodratio)), -0.5, -0.5, -0.5);
		originCubes_.primitive(lod).createCube(1.0, max(1, int(currentQuality_*lodratio)), 0.0, 0.0, 0.0);
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

	msg.exit("RenderPrimitives::recreatePrimitives");
}

// Create instance for primitives
void RenderPrimitives::pushInstance(const QGLContext* context, bool forceRegenerate)
{
	msg.enter("RenderPrimitives::pushInstance");
	
	// Recreate primitives
	recreatePrimitives(forceRegenerate);
	
	// Push instances
	atom_.pushInstance(context);
	selectedAtom_.pushInstance(context);
	for (int n=0; n<Atom::nDrawStyles; ++n)
	{
		for (int m=0; m<Bond::nBondTypes; ++m)
		{
			bonds_[n][m].pushInstance(context);
			selectedBonds_[n][m].pushInstance(context);
		}
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

	// Increase stacksize
	++stackSize_;

	msg.exit("RenderPrimitives::pushInstance");
}

// Pop topmost instance for primitives
void RenderPrimitives::popInstance(const QGLContext *context)
{
	msg.enter("RenderPrimitives::popInstance");
	atom_.popInstance(context);
	selectedAtom_.popInstance(context);
	for (int n=0; n<Atom::nDrawStyles; ++n)
	{
		for (int m=0; m<Bond::nBondTypes; ++m)
		{
			bonds_[n][m].popInstance(context);
			selectedBonds_[n][m].popInstance(context);
		}
	}
	tubeRings_.popInstance(context);
	segmentedTubeRings_.popInstance(context);
	lineRings_.popInstance(context);
	segmentedLineRings_.popInstance(context);
	spheres_.popInstance(context);
	cubes_.popInstance(context);
	originCubes_.popInstance(context);
	cylinders_.popInstance(context);
	cones_.popInstance(context);
	wireCube_.popInstance(context);
	crossedCube_.popInstance(context);
	cellAxes_.popInstance(context);
	rotationGlobe_.popInstance(context);
	rotationGlobeAxes_.popInstance(context);

	// Decrease stacksize
	--stackSize_;

	msg.exit("RenderPrimitives::popInstance");
}
