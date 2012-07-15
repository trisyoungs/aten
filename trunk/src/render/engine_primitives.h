/*
	*** Rendering Engine Primitives
	*** src/render/engine_primitives.h
	Copyright T. Youngs 2007-2012

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

#ifndef ATEN_RENDERENGINEPRIMITIVES_H
#define ATEN_RENDERENGINEPRIMITIVES_H

#ifdef _WIN32
#include <windows.h>
#include <GL/gl.h>
#include "glext.h"
#endif
// #include "render/triangles.h"
#include "render/primitivegroup.h"
#include "render/primitiveinfo.h"
// #include "render/gridprimitive.h"
// #include "render/textprimitive.h"
// #include "base/log.h"
// #include "templates/vector3.h"
#include "base/atom.h"
#include "base/bond.h"
// #include "base/matrix.h"

// Forward declarations
// class Model;
// class TCanvas;
// class RenderEngine;
class QGLContext;

// Rendering Primitives
class RenderPrimitives
{
	public:
	// Constructor / Destructor
	RenderPrimitives();
	~RenderPrimitives();
	// Declare RenderEngine as a Friend
	friend class RenderEngine;


	/*
	// Primitives
	*/
	private:
	// Quality setting that primitives should be next generated at
	int requestedQuality_;
	// Quality setting that primitives were last generated at (if at all)
	int currentQuality_;
	// Stack size counter
	int stackSize_;
	// Atom
	PrimitiveGroup atom_;
	// Selected atom styles
	PrimitiveGroup selectedAtom_;
	// Bond styles
	PrimitiveGroup bonds_[Atom::nDrawStyles][Bond::nBondTypes];
	// Selected bond styles
	PrimitiveGroup selectedBonds_[Atom::nDrawStyles][Bond::nBondTypes];
	// Rings
	PrimitiveGroup lineRings_, segmentedLineRings_, tubeRings_, segmentedTubeRings_;
	// Primitive objects
	PrimitiveGroup cubes_, originCubes_, spheres_, cylinders_, cones_;
	// One-off objects
	Primitive wireCube_, crossedCube_, cellAxes_, rotationGlobe_, rotationGlobeAxes_;

	public:
	// Set the desired primitive quality
	void setQuality(int quality);	
	// Return current primitive instance stacksize
	int stackSize();
	// (Re)Generate primitive vertex arrays (if necessary)
	void recreatePrimitives(bool force = FALSE);
	// Push instance layer for all primitives
	void pushInstance(const QGLContext *context, bool forceRegenerate = FALSE);
	// Pop last instance layer
	void popInstance(const QGLContext *context);
};

#endif
