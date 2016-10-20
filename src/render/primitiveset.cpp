/*
	*** Primitive Set
	*** src/render/primitiveset.cpp
	Copyright T. Youngs 2007-2016

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

#include "render/primitiveset.h"
#include "base/prefs.h"
#include <QOpenGLContext>

ATEN_USING_NAMESPACE

// Static Objects
RefList<Primitive,int> PrimitiveSet::dynamicPrimitives_;
int PrimitiveSet::logPoint_ = 0;
int PrimitiveSet::nInstances_;

// Constructor
PrimitiveSet::PrimitiveSet()
{
	// Adjustments
	sphereAtomAdjustment_ = 1.0;
	scaledAtomAdjustments_.createEmpty(ElementMap::nElements());

	// Primitives
	creationPoint_ = -1;
	requestedQuality_ = 0;
	currentQuality_ = -1;
	nInstances_ = 0;
}

// Destructor
PrimitiveSet::~PrimitiveSet()
{
}

/*
 * Adjustments for primitives
 */

// Recalculate adjustments
void PrimitiveSet::calculateAdjustments()
{
	// Recalculate adjustments for bond positioning
	double atomradius, bondradius, theta;
	int i;

	// Triangle formed between atom radius (H), bond radius (O), and unknown (A)
	// Determine angle between H and O and calculate adjustment (=H-A)

	// Sphere Style
	atomradius = prefs.atomStyleRadius(Prefs::SphereStyle);
	bondradius = prefs.bondStyleRadius(Prefs::SphereStyle);
	theta = asin(bondradius / atomradius);
	sphereAtomAdjustment_ = atomradius - atomradius*cos(theta);

	// Scaled Style
	theta = asin(bondradius / atomradius);
	sphereAtomAdjustment_ = atomradius - atomradius*cos(theta);
	for (i = 0; i<ElementMap::nElements(); ++i)
	{
		atomradius = prefs.atomStyleRadius(Prefs::ScaledStyle) * ElementMap::atomicRadius(i);
		theta = asin(bondradius / atomradius);
		scaledAtomAdjustments_[i] = (atomradius - atomradius*cos(theta));
	}
}

// Return current sphereAtomAdjustment
double PrimitiveSet::sphereAtomAdjustment()
{
	return sphereAtomAdjustment_;
}

// Return current sphereAtomAdjustment
double PrimitiveSet::scaledAtomAdjustment(int element)
{
	return scaledAtomAdjustments_[element];
}

/*
 * Primitives
 */

// Return atom primitive
Primitive& PrimitiveSet::atom()
{
	return atom_;
}

// Return selected atom primitive
Primitive& PrimitiveSet::selectedAtom()
{
	return selectedAtom_;
}

// Return bond primitive
Primitive& PrimitiveSet::bond(Prefs::DrawStyle drawStyle, Bond::BondType bondType)
{
	return bonds_[drawStyle][bondType];
}

// Return selected bond primitive
Primitive& PrimitiveSet::selectedBond(Prefs::DrawStyle drawStyle, Bond::BondType bondType)
{
	return selectedBonds_[drawStyle][bondType];
}

// Return line ring primitive
Primitive& PrimitiveSet::lineRing()
{
	return lineRing_;
}

// Return segmented line ring primitive
Primitive& PrimitiveSet::segmentedLineRing()
{
	return segmentedLineRing_;
}

// Return tube ring primitive
Primitive& PrimitiveSet::tubeRing()
{
	return tubeRing_;
}

// Return segmented tube ring primitive
Primitive& PrimitiveSet::segmentedTubeRing()
{
	return segmentedTubeRing_;
}

// Return cube primitive
Primitive& PrimitiveSet::cube()
{
	return cube_;
}

// Return wiret cube primitive
Primitive& PrimitiveSet::wireCube()
{
	return wireCube_;
}

// Return origin cube primitive
Primitive& PrimitiveSet::originCube()
{
	return originCube_;
}

// Return sphere primitive
Primitive& PrimitiveSet::sphere()
{
	return sphere_;
}

// Return cylinder primitive
Primitive& PrimitiveSet::cylinder()
{
	return cylinder_;
}

// Return cone primitive
Primitive& PrimitiveSet::cone()
{
	return cone_;
}

// Return crossed cube primitive
Primitive& PrimitiveSet::crossedCube()
{
	return crossedCube_;
}

// Return cell axes primitive
Primitive& PrimitiveSet::cellAxes(int axis)
{
	return cellAxes_[axis];
}

// Return rotation globe primitive
Primitive& PrimitiveSet::rotationGlobe()
{
	return rotationGlobe_;
}

// Return rotation globe axes primitive
Primitive& PrimitiveSet::rotationGlobeAxes(int axis)
{
	return rotationGlobeAxes_[axis];
}

// Return halo primitive
Primitive& PrimitiveSet::halo()
{
	return halo_;
}

// Return picked atom primitive
Primitive& PrimitiveSet::pickedAtom()
{
	return pickedAtom_;
}

// Register the specified primitive as dynamic
void PrimitiveSet::registerDynamicPrimitive(Primitive* primitive)
{
	dynamicPrimitives_.add(primitive);
	primitive->setRegisteredAsDynamic(true);

	// Push an instance if the other primitives already have one?
	if (nInstances_ > 0) primitive->pushInstance(QOpenGLContext::currentContext());
}

// Release and destroy specified dynamic primitive
void PrimitiveSet::releaseDynamicPrimitive(Primitive* primitive)
{
	if (!dynamicPrimitives_.contains(primitive)) Messenger::error("Internal Error: Tried to release a dynamic primitive that wasn't registered in the first place.");
	else
	{
		dynamicPrimitives_.remove(primitive);
		primitive->setRegisteredAsDynamic(false);
	}
}

/*
 * Creation / Instantiation
 */

// Set the desired primitive quality
void PrimitiveSet::setQuality(int quality)
{
	requestedQuality_ = quality;
}

// Flag the primitive set for regeneration
void PrimitiveSet::flagForReCreation()
{
	++logPoint_;
}

// (Re)Generate primitives
void PrimitiveSet::recreatePrimitives()
{
	Messenger::enter("PrimitiveSet::recreatePrimitives");

	double radius, aRadius[Prefs::nDrawStyles], bRadius[Prefs::nDrawStyles], selScale;
	int n, m, nStacks, nSlices;
	
	// If current quality is the same as the requested quality, do nothing
	if ((requestedQuality_ == currentQuality_) && (logPoint_ == creationPoint_))
	{
		Messenger::exit("PrimitiveSet::recreatePrimitives");
		return;
	}

	currentQuality_ = requestedQuality_;
	creationPoint_ = logPoint_;

	Task* task = Messenger::initialiseTask("(Re)generating primitives", 33);

	// Clear old primitives
	atom_.clear();
	selectedAtom_.clear();
	for (n=0; n<Prefs::nDrawStyles; ++n)
	{
		for (m=0; m<Bond::nBondTypes; ++m)
		{
			bonds_[n][m].clear();
			selectedBonds_[n][m].clear();
		}
	}
	tubeRing_.clear();
	segmentedTubeRing_.clear();
	lineRing_.clear();
	segmentedLineRing_.clear();
	sphere_.clear();
	cube_.clear();
	originCube_.clear();
	cylinder_.clear();
	cone_.clear();
	wireCube_.clear();
	crossedCube_.clear();
	cellAxes_[0].clear();
	cellAxes_[1].clear();
	cellAxes_[2].clear();
	rotationGlobe_.clear();
	rotationGlobeAxes_[0].clear();
	rotationGlobeAxes_[1].clear();
	rotationGlobeAxes_[2].clear();
	halo_.clear();
	pickedAtom_.clear();

	// Set chunk increment sizes
	atom_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	selectedAtom_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	for (n=0; n<Prefs::nDrawStyles; ++n)
	{
		for (m=0; m<Bond::nBondTypes; ++m)
		{
			bonds_[n][m].setArrayChunkIncrement(SMALLCHUNKSIZE);
			selectedBonds_[n][m].setArrayChunkIncrement(SMALLCHUNKSIZE);
		}
	}
	tubeRing_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	segmentedTubeRing_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	lineRing_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	segmentedLineRing_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	sphere_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	cube_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	originCube_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	cylinder_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	cone_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	wireCube_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	crossedCube_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	cellAxes_[0].setArrayChunkIncrement(SMALLCHUNKSIZE);
	cellAxes_[1].setArrayChunkIncrement(SMALLCHUNKSIZE);
	cellAxes_[2].setArrayChunkIncrement(SMALLCHUNKSIZE);
	rotationGlobe_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	rotationGlobeAxes_[0].setArrayChunkIncrement(SMALLCHUNKSIZE);
	rotationGlobeAxes_[1].setArrayChunkIncrement(SMALLCHUNKSIZE);
	rotationGlobeAxes_[2].setArrayChunkIncrement(SMALLCHUNKSIZE);
	halo_.setArrayChunkIncrement(SMALLCHUNKSIZE);
	pickedAtom_.setArrayChunkIncrement(SMALLCHUNKSIZE);

	Messenger::incrementTaskProgress(task);

	// To clean up following code, grab radii here
	for (n=0; n<Prefs::nDrawStyles; ++n)
	{
		aRadius[n] = prefs.atomStyleRadius( (Prefs::DrawStyle) n);
		bRadius[n] = prefs.bondStyleRadius( (Prefs::DrawStyle) n);
	}
	selScale = prefs.selectionScale();

	nStacks = std::max(3, (int) (currentQuality_*0.75));
	nSlices = std::max(3, (int) (currentQuality_*1.5));
	
	// Atom Styles (Prefs::LineStyle, Prefs::TubeStyle, and Prefs::SphereStyle)
	atom_.initialise(GL_TRIANGLE_STRIP, false);
	atom_.plotSphere(1.0, nStacks, nSlices);
	selectedAtom_.initialise(GL_LINES, false);
	selectedAtom_.plotLineSphere(selScale, nStacks, nSlices);
	Messenger::incrementTaskProgress(task);
	
	// Bond primitive accuracy
	nStacks = std::max(1, (int) (currentQuality_*0.25));
	nSlices = std::max(3, (int) currentQuality_);
	
	// All sphere styles - Single and Aromatic Bonds
	bonds_[Prefs::TubeStyle][Bond::Single].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::SphereStyle][Bond::Single].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::ScaledStyle][Bond::Single].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::TubeStyle][Bond::Single].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::TubeStyle], nSlices);
	bonds_[Prefs::SphereStyle][Bond::Single].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::SphereStyle], nSlices);
	bonds_[Prefs::ScaledStyle][Bond::Single].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::ScaledStyle], nSlices);
	Messenger::incrementTaskProgress(task);
	selectedBonds_[Prefs::TubeStyle][Bond::Single].initialise(GL_LINE_LOOP, false);
	selectedBonds_[Prefs::SphereStyle][Bond::Single].initialise(GL_LINE_LOOP, false);
	selectedBonds_[Prefs::ScaledStyle][Bond::Single].initialise(GL_LINE_LOOP, false);
	selectedBonds_[Prefs::TubeStyle][Bond::Single].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::TubeStyle]*selScale, nSlices);
	selectedBonds_[Prefs::SphereStyle][Bond::Single].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::SphereStyle]*selScale, nSlices);
	selectedBonds_[Prefs::ScaledStyle][Bond::Single].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::ScaledStyle]*selScale, nSlices);
	Messenger::incrementTaskProgress(task);
	bonds_[Prefs::TubeStyle][Bond::Aromatic].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::SphereStyle][Bond::Aromatic].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::ScaledStyle][Bond::Aromatic].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::TubeStyle][Bond::Aromatic].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::TubeStyle], nSlices);
	bonds_[Prefs::SphereStyle][Bond::Aromatic].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::SphereStyle], nSlices);
	bonds_[Prefs::ScaledStyle][Bond::Aromatic].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::ScaledStyle], nSlices);
	Messenger::incrementTaskProgress(task);
	selectedBonds_[Prefs::TubeStyle][Bond::Aromatic].initialise(GL_LINES, false);
	selectedBonds_[Prefs::SphereStyle][Bond::Aromatic].initialise(GL_LINES, false);
	selectedBonds_[Prefs::ScaledStyle][Bond::Aromatic].initialise(GL_LINES, false);
	selectedBonds_[Prefs::TubeStyle][Bond::Aromatic].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::TubeStyle]*selScale, nSlices);
	selectedBonds_[Prefs::SphereStyle][Bond::Aromatic].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::SphereStyle]*selScale, nSlices);
	selectedBonds_[Prefs::ScaledStyle][Bond::Aromatic].plotCylinder(0,0,0,0,0,1, bRadius[Prefs::ScaledStyle]*selScale, nSlices);
	Messenger::incrementTaskProgress(task);

	// All styles - Double Bond
	bonds_[Prefs::TubeStyle][Bond::Double].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::SphereStyle][Bond::Double].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::ScaledStyle][Bond::Double].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::TubeStyle][Bond::Double].plotCylinder(-bRadius[Prefs::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::TubeStyle]*0.5, nSlices);
	bonds_[Prefs::TubeStyle][Bond::Double].plotCylinder( bRadius[Prefs::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::TubeStyle]*0.5, nSlices);
	Messenger::incrementTaskProgress(task);
	bonds_[Prefs::SphereStyle][Bond::Double].plotCylinder(-bRadius[Prefs::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::SphereStyle]*0.5, nSlices);
	bonds_[Prefs::SphereStyle][Bond::Double].plotCylinder( bRadius[Prefs::SphereStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::SphereStyle]*0.5, nSlices);
	Messenger::incrementTaskProgress(task);
	bonds_[Prefs::ScaledStyle][Bond::Double].plotCylinder(-bRadius[Prefs::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::ScaledStyle]*0.5, nSlices);
	bonds_[Prefs::ScaledStyle][Bond::Double].plotCylinder( bRadius[Prefs::ScaledStyle]*0.50,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::ScaledStyle]*0.5, nSlices);
	Messenger::incrementTaskProgress(task);
	selectedBonds_[Prefs::TubeStyle][Bond::Double].initialise(GL_LINES, false);
	selectedBonds_[Prefs::SphereStyle][Bond::Double].initialise(GL_LINES, false);
	selectedBonds_[Prefs::ScaledStyle][Bond::Double].initialise(GL_LINES, false);
	selectedBonds_[Prefs::TubeStyle][Bond::Double].plotCylinder(-bRadius[Prefs::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::TubeStyle]*0.5* selScale, nSlices);
	selectedBonds_[Prefs::TubeStyle][Bond::Double].plotCylinder( bRadius[Prefs::TubeStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::TubeStyle]*0.5* selScale, nSlices);
	Messenger::incrementTaskProgress(task);
	selectedBonds_[Prefs::SphereStyle][Bond::Double].plotCylinder(-bRadius[Prefs::SphereStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::SphereStyle]*0.5* selScale, nSlices);
	selectedBonds_[Prefs::SphereStyle][Bond::Double].plotCylinder( bRadius[Prefs::SphereStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::SphereStyle]*0.5* selScale, nSlices);
	Messenger::incrementTaskProgress(task);
	selectedBonds_[Prefs::ScaledStyle][Bond::Double].plotCylinder(-bRadius[Prefs::ScaledStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::ScaledStyle]*0.5* selScale, nSlices);
	selectedBonds_[Prefs::ScaledStyle][Bond::Double].plotCylinder( bRadius[Prefs::ScaledStyle]*0.5,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::ScaledStyle]*0.5* selScale, nSlices);
	Messenger::incrementTaskProgress(task);

	// All styles - Triple Bond
	bonds_[Prefs::TubeStyle][Bond::Triple].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::SphereStyle][Bond::Triple].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::ScaledStyle][Bond::Triple].initialise(GL_TRIANGLE_STRIP, false);
	bonds_[Prefs::TubeStyle][Bond::Triple].plotCylinder(-bRadius[Prefs::TubeStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::TubeStyle]*0.33, nSlices);
	bonds_[Prefs::TubeStyle][Bond::Triple].plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::TubeStyle]*0.33, nSlices);
	bonds_[Prefs::TubeStyle][Bond::Triple].plotCylinder( bRadius[Prefs::TubeStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::TubeStyle]*0.33, nSlices);
	Messenger::incrementTaskProgress(task);
	bonds_[Prefs::SphereStyle][Bond::Triple].plotCylinder(-bRadius[Prefs::SphereStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::SphereStyle]*0.33, nSlices);
	bonds_[Prefs::SphereStyle][Bond::Triple].plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::SphereStyle]*0.33, nSlices);
	bonds_[Prefs::SphereStyle][Bond::Triple].plotCylinder( bRadius[Prefs::SphereStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::SphereStyle]*0.33, nSlices);
	Messenger::incrementTaskProgress(task);
	bonds_[Prefs::ScaledStyle][Bond::Triple].plotCylinder(-bRadius[Prefs::ScaledStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::ScaledStyle]*0.33, nSlices);
	bonds_[Prefs::ScaledStyle][Bond::Triple].plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::ScaledStyle]*0.33, nSlices);
	bonds_[Prefs::ScaledStyle][Bond::Triple].plotCylinder( bRadius[Prefs::ScaledStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::ScaledStyle]*0.33, nSlices);
	Messenger::incrementTaskProgress(task);
	selectedBonds_[Prefs::TubeStyle][Bond::Triple].initialise(GL_LINES, false);
	selectedBonds_[Prefs::SphereStyle][Bond::Triple].initialise(GL_LINES, false);
	selectedBonds_[Prefs::ScaledStyle][Bond::Triple].initialise(GL_LINES, false);
	selectedBonds_[Prefs::TubeStyle][Bond::Triple].plotCylinder(-bRadius[Prefs::TubeStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::TubeStyle]*0.33* selScale, nSlices);
	selectedBonds_[Prefs::TubeStyle][Bond::Triple].plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::TubeStyle]*0.33* selScale, nSlices);
	selectedBonds_[Prefs::TubeStyle][Bond::Triple].plotCylinder( bRadius[Prefs::TubeStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::TubeStyle]*0.33* selScale, nSlices);
	Messenger::incrementTaskProgress(task);
	selectedBonds_[Prefs::SphereStyle][Bond::Triple].plotCylinder(-bRadius[Prefs::SphereStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::SphereStyle]*0.33* selScale, nSlices);
	selectedBonds_[Prefs::SphereStyle][Bond::Triple].plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::SphereStyle]*0.33* selScale, nSlices);
	selectedBonds_[Prefs::SphereStyle][Bond::Triple].plotCylinder( bRadius[Prefs::SphereStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::SphereStyle]*0.33* selScale, nSlices);
	Messenger::incrementTaskProgress(task);
	selectedBonds_[Prefs::ScaledStyle][Bond::Triple].plotCylinder(-bRadius[Prefs::ScaledStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::ScaledStyle]*0.33* selScale, nSlices);
	selectedBonds_[Prefs::ScaledStyle][Bond::Triple].plotCylinder(0.0,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::ScaledStyle]*0.33* selScale, nSlices);
	selectedBonds_[Prefs::ScaledStyle][Bond::Triple].plotCylinder( bRadius[Prefs::ScaledStyle]*0.66,0.0,0.0,0.0, 0.0, 1.0, bRadius[Prefs::ScaledStyle]*0.33* selScale, nSlices);
	Messenger::incrementTaskProgress(task);

	// Other primitives
	nStacks = std::max(3, (int) (currentQuality_*0.75));
	nSlices = std::max(3, (int) (currentQuality_));
	cube_.initialise(GL_TRIANGLES, false);
	cube_.plotCube(1.0, 2, -0.5, -0.5, -0.5);
	originCube_.initialise(GL_TRIANGLES, false);
	originCube_.plotCube(1.0, 2, 0.0, 0.0, 0.0);
	Messenger::incrementTaskProgress(task);

	sphere_.initialise(GL_TRIANGLE_STRIP, false);
	sphere_.plotSphere(1.0, nStacks, nSlices);
	Messenger::incrementTaskProgress(task);

	cylinder_.initialise(GL_TRIANGLES, false);
	cylinder_.plotCylinder(0,0,0,0,0,1,1.0,1.0, nStacks, nSlices);
	Messenger::incrementTaskProgress(task);

	cone_.initialise(GL_TRIANGLES, false);
	cone_.plotCone(0,0,0,0,0,1,1.0, nStacks, nSlices);
	Messenger::incrementTaskProgress(task);

	tubeRing_.initialise(GL_TRIANGLES, false);
	tubeRing_.plotRing(1.0, 0.1, 10, nSlices, 5);
	Messenger::incrementTaskProgress(task);

	segmentedTubeRing_.initialise(GL_TRIANGLES, false);
	segmentedTubeRing_.plotRing(1.0, 0.1, 20, nSlices, 5, true);
	Messenger::incrementTaskProgress(task);

	lineRing_.initialise(GL_LINES, false);
	lineRing_.plotCircle(1.0, 10, 5);
	Messenger::incrementTaskProgress(task);

	segmentedLineRing_.initialise(GL_LINES, false);
	segmentedLineRing_.plotCircle(1.0, 20, 5, true);
	Messenger::incrementTaskProgress(task);

	wireCube_.initialise(GL_LINES, false);
	wireCube_.plotWireCube(1.0, 0.0, 0.0, 0.0);
	Messenger::incrementTaskProgress(task);

	crossedCube_.initialise(GL_LINES, false);
	crossedCube_.plotCrossedCube(1.0, 1, 0.0, 0.0, 0.0);
	Messenger::incrementTaskProgress(task);

	cellAxes_[0].initialise(GL_TRIANGLE_STRIP, true);
	cellAxes_[1].initialise(GL_TRIANGLE_STRIP, true);
	cellAxes_[2].initialise(GL_TRIANGLE_STRIP, true);
	cellAxes_[0].plotCylinder(0.0f, 0.0f, 0.0f, 0.65f, 0.0f, 0.0f, 0.1f, nSlices, true, false, true, Vec4<GLfloat>(1.0f, 0.0f, 0.0f, 1.0f));
	cellAxes_[0].plotCone(0.65f, 0.0f, 0.0f, 0.35f, 0.0f, 0.0f, 0.2f, nSlices, true, true, Vec4<GLfloat>(1.0f, 0.0f, 0.0f, 1.0f));
	cellAxes_[1].plotCylinder(0.0f, 0.0f, 0.0f, 0.0f, 0.65f, 0.0f, 0.1f, nSlices, true, false, true, Vec4<GLfloat>(0.0f, 1.0f, 0.0f, 1.0f));
	cellAxes_[1].addDegenerateTriangle();
	cellAxes_[1].plotCone(0.0f, 0.65f, 0.0f, 0.0f, 0.35f, 0.0f, 0.2f, nSlices, true, true, Vec4<GLfloat>(0.0f, 1.0f, 0.0f, 1.0f));
	cellAxes_[2].plotCylinder(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.65f, 0.1f, nSlices, true, false, true, Vec4<GLfloat>(0.0f, 0.0f, 1.0f, 1.0f));
	cellAxes_[2].addDegenerateTriangle();
	cellAxes_[2].plotCone(0.0f, 0.0f, 0.65f, 0.0f, 0.0f, 0.35f, 0.2f, nSlices, true, true, Vec4<GLfloat>(0.0f, 0.0f, 1.0f, 1.0f));
	Messenger::incrementTaskProgress(task);

	rotationGlobe_.initialise(GL_TRIANGLE_STRIP, true);
	rotationGlobe_.plotSphere(0.75, 8, 24, true, Vec4<GLfloat>(1.0f, 1.0f, 1.0f, 0.6f));
	Messenger::incrementTaskProgress(task);

	rotationGlobeAxes_[0].initialise(GL_TRIANGLE_STRIP, true);
	rotationGlobeAxes_[1].initialise(GL_TRIANGLE_STRIP, true);
	rotationGlobeAxes_[2].initialise(GL_TRIANGLE_STRIP, true);
	rotationGlobeAxes_[0].plotCone(0.7f, 0.0f, 0.0f, 0.3f, 0.0f, 0.0f, 0.2f, 10, false, true, Vec4<GLfloat>(1.0f, 0.0f, 0.0f, 1.0f));
	rotationGlobeAxes_[1].plotCone(0.0f, 0.7f, 0.0f, 0.0f, 0.3f, 0.0f, 0.2f, 10, false, true, Vec4<GLfloat>(0.0f, 1.0f, 0.0f, 1.0f));
	rotationGlobeAxes_[2].plotCone(0.0f, 0.0f, 0.7f, 0.0f, 0.0f, 0.3f, 0.2f, 10, false, true, Vec4<GLfloat>(0.0f, 0.0f, 1.0f, 1.0f));
	Messenger::incrementTaskProgress(task);

	halo_.initialise(GL_TRIANGLES, false);
	halo_.plotHalo(0.6f, 1.0f, 32);
	Messenger::incrementTaskProgress(task);

	pickedAtom_.initialise(GL_TRIANGLE_STRIP, false);
	pickedAtom_.plotCone(1.5f, 0.0f, 0.0f, -0.25f, 0.0f, 0.0f, 0.2f, 10, true);
	pickedAtom_.plotCone(-1.5f, 0.0f, 0.0f, 0.25f, 0.0f, 0.0f, 0.2f, 10, true);
	pickedAtom_.plotCone(0.0, 1.5f, 0.0f, 0.0f, -0.25f, 0.0f, 0.2f, 10, true);
	pickedAtom_.plotCone(0.0, -1.5f, 0.0f, 0.0f, 0.25f, 0.0f, 0.2f, 10, true);
	pickedAtom_.plotCone(0.0, 0.0f, 1.5f, 0.0f, 0.0f, -0.25f, 0.2f, 10, true);
	pickedAtom_.plotCone(0.0, 0.0f, -1.50f, 0.0f, 0.0f, 0.25f, 0.2f, 10, true);
	Messenger::incrementTaskProgress(task);

	Messenger::terminateTask(task);

	Messenger::exit("PrimitiveSet::recreatePrimitives");
}

// Create instance for primitives
void PrimitiveSet::pushInstance(const QOpenGLContext* context)
{
	Messenger::enter("PrimitiveSet::pushInstance");

	// Push instances
	atom_.pushInstance(context);
	selectedAtom_.pushInstance(context);
	for (int n=0; n<Prefs::nDrawStyles; ++n)
	{
		for (int m=0; m<Bond::nBondTypes; ++m)
		{
			bonds_[n][m].pushInstance(context);
			selectedBonds_[n][m].pushInstance(context);
		}
	}
	tubeRing_.pushInstance(context);
	segmentedTubeRing_.pushInstance(context);
	lineRing_.pushInstance(context);
	segmentedLineRing_.pushInstance(context);
	sphere_.pushInstance(context);
	cube_.pushInstance(context);
	originCube_.pushInstance(context);
	cylinder_.pushInstance(context);
	cone_.pushInstance(context);
	wireCube_.pushInstance(context);
	crossedCube_.pushInstance(context);
	cellAxes_[0].pushInstance(context);
	cellAxes_[1].pushInstance(context);
	cellAxes_[2].pushInstance(context);
	rotationGlobe_.pushInstance(context);
	rotationGlobeAxes_[0].pushInstance(context);
	rotationGlobeAxes_[1].pushInstance(context);
	rotationGlobeAxes_[2].pushInstance(context);
	halo_.pushInstance(context);
	pickedAtom_.pushInstance(context);

	// Dynamic objects
	for (RefListItem<Primitive,int>* ri = dynamicPrimitives_.first(); ri != NULL; ri = ri->next) ri->item->pushInstance(context);
	
	++nInstances_;

	Messenger::exit("PrimitiveSet::pushInstance");
}

// Pop topmost instance for primitives
void PrimitiveSet::popInstance(const QOpenGLContext* context)
{
	Messenger::enter("PrimitiveSet::popInstance");

	atom_.popInstance(context);
	selectedAtom_.popInstance(context);
	for (int n=0; n<Prefs::nDrawStyles; ++n)
	{
		for (int m=0; m<Bond::nBondTypes; ++m)
		{
			bonds_[n][m].popInstance(context);
			selectedBonds_[n][m].popInstance(context);
		}
	}
	tubeRing_.popInstance(context);
	segmentedTubeRing_.popInstance(context);
	lineRing_.popInstance(context);
	segmentedLineRing_.popInstance(context);
	sphere_.popInstance(context);
	cube_.popInstance(context);
	originCube_.popInstance(context);
	cylinder_.popInstance(context);
	cone_.popInstance(context);
	wireCube_.popInstance(context);
	crossedCube_.popInstance(context);
	cellAxes_[0].popInstance(context);
	cellAxes_[1].popInstance(context);
	cellAxes_[2].popInstance(context);
	rotationGlobe_.popInstance(context);
	rotationGlobeAxes_[0].popInstance(context);
	rotationGlobeAxes_[1].popInstance(context);
	rotationGlobeAxes_[2].popInstance(context);
	halo_.popInstance(context);
	pickedAtom_.popInstance(context);

	// Dynamic objects
	for (RefListItem<Primitive,int>* ri = dynamicPrimitives_.first(); ri != NULL; ri = ri->next) ri->item->popInstance(context);

	--nInstances_;

	Messenger::exit("PrimitiveSet::popInstance");
}

// Return number of instances currently pushed
int PrimitiveSet::nInstances()
{
	return nInstances_;
}
