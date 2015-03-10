/*
	*** Rendering Primitive Set
	*** src/render/primitiveset.h
	Copyright T. Youngs 2007-2015

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

#ifndef ATEN_PRIMITIVESET_H
#define ATEN_PRIMITIVESET_H

#ifdef _WIN32
#include <windows.h>
#include <GL/gl.h>
#include "glext.h"
#endif
#include "render/primitive.h"
#include "base/prefs.h"
#include "base/bond.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class QGLContext;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class GLExtensions;

// Primitive Set
class PrimitiveSet
{
	public:
	// Constructor / Destructor
	PrimitiveSet();
	~PrimitiveSet();


	/*
	 * Adjustments for primitives
	 */
	private:
	// Sphere atom bond adjustment distances
	double sphereAtomAdjustment_;
	// Scaled atom bond adjustment distances
	Array<double> scaledAtomAdjustments_;

	public:
	// Recalculate adjustments
	void calculateAdjustments();
	// Return current sphereAtomAdjustment
	double sphereAtomAdjustment();
	// Return current sphereAtomAdjustment
	double scaledAtomAdjustment(int element);


	/*
	// Primitives
	*/
	private:
	// Atom
	Primitive atom_;
	// Selected atom styles
	Primitive selectedAtom_;
	// Bond styles
	Primitive bonds_[Prefs::nDrawStyles][Bond::nBondTypes];
	// Selected bond styles
	Primitive selectedBonds_[Prefs::nDrawStyles][Bond::nBondTypes];
	// Rings
	Primitive lineRing_, segmentedLineRing_, tubeRing_, segmentedTubeRing_;
	// Geometric objects
	Primitive cube_, wireCube_, originCube_, sphere_, cylinder_, cone_;
	// Specialist objects
	Primitive crossedCube_, cellAxes_, rotationGlobe_;

	public:
	// Return atom primitive
	Primitive& atom();
	// Return selected atom primitive
	Primitive& selectedAtom();
	// Return bond primitive
	Primitive& bond(Prefs::DrawStyle drawStyle, Bond::BondType bondType);
	// Return selected bond primitive
	Primitive& selectedBond(Prefs::DrawStyle drawStyle, Bond::BondType bondType);
	// Return line ring primitive
	Primitive& lineRing();
	// Return segmented line ring primitive
	Primitive& segmentedLineRing();
	// Return tube ring primitive
	Primitive& tubeRing();
	// Return segmented tube ring primitive
	Primitive& segmentedTubeRing();
	// Return cube primitive
	Primitive& cube();
	// Return wiret cube primitive
	Primitive& wireCube();
	// Return origin cube primitive
	Primitive& originCube();
	// Return sphere primitive
	Primitive& sphere();
	// Return cylinder primitive
	Primitive& cylinder();
	// Return cone primitive
	Primitive& cone();
	// Return crossed cube primitive
	Primitive& crossedCube();
	// Return cell axes primitive
	Primitive& cellAxes();
	// Return rotation globe primitive
	Primitive& rotationGlobe();


	/*
	 * Creation / Instantiation
	 */
	private:
	// Number of instances of primitives
	int nInstances_;
	// Quality setting that primitives should be next generated at
	int requestedQuality_;
	// Quality setting that primitives were last generated at (if at all)
	int currentQuality_;

	public:
	// Set the desired primitive quality
	void setQuality(int quality);	
	// (Re)Generate primitive vertex arrays (if necessary)
	void recreatePrimitives();
	// Push instance layer for all primitives
	void pushInstance(const QGLContext* context, GLExtensions* extensions);
	// Pop last instance layer
	void popInstance(const QGLContext* context, GLExtensions* extensions);
	// Return number of instances currently pushed
	int nInstances();
};

ATEN_END_NAMESPACE

#endif
