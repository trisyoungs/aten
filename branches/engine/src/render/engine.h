/*
	*** Rendering Engine
	*** src/render/engine.h
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

#ifndef ATEN_RENDERENGINE_H
#define ATEN_RENDERENGINE_H

#include "render/glmatrix.h"
#include "render/triangles.h"
#include "render/primitive.h"
#include "templates/vector3.h"
#include "base/atom.h"
#include "base/bond.h"
#include <GL/gl.h>

// Forward declarations
class Model;
class TCanvas;

// Render Engine
class RenderEngine
{
	public:
	// Constructor / Destructor
	RenderEngine();
	~RenderEngine();
	// Filter type (if any)
	enum FilterType { NoFilter, TransparencyFilter, CompleteFilter, nFilterTypes };

	
	/*
	// Primitives
	*/
	private:
	// Atom styles
	PrimitiveGroup atom_[Atom::nDrawStyles], *scaledAtom_;
	// Selected atom styles
	PrimitiveGroup selectedAtom_[Atom::nDrawStyles], *selectedScaledAtom_;
	// Bond styles
	PrimitiveGroup bond_[Atom::nDrawStyles][Bond::nBondTypes];
	// Selected bond styles
	PrimitiveGroup selectedBond_[Atom::nDrawStyles][Bond::nBondTypes];
	// Unit cube
	PrimitiveGroup cubes_;
	// Cones of length 1.0 and base radius 0.2
	PrimitiveGroup cones_;
	// One-off objects
	Primitive wireCube_, cellAxes_;

	public:
	// (Re)Generate primitive vertex arrays
	void createPrimitives();


	/*
	// View Control
	*/
	private:
	// View matrix
	GLMatrix transformationMatrix_;
	// Projection matrix
	GLMatrix projectionMatrix_;
	// Viewport matrix for canvas
	GLint viewportMatrix_[4];

	public:
	// Set-up viewport and projection matrices
	void setupView(GLint x, GLint y, GLint w, GLint h);
	// Project given model coordinates into world coordinates (and screen coordinates if Vec3 is supplied)
	Vec3<double> &modelToWorld(Vec3<double> &pos, Vec4<double> *screenr = NULL, double screenradius = 0.0);
	// Project the specified world coordinates into 2D screen coords
	Vec4<double> &worldToScreen(const Vec3<double> &vec);
	// Update transformation matrix
	void setTransformationMatrix(Mat4<double> &mat, Vec3<double> cellcentre);


	/*
	// Rendering Functions and Primitive Lists
	*/
	private:
	// List of filtered solid primitives
	List<PrimitiveInfo> solidPrimitives_;
	// List of filtered primitives
	List<PrimitiveInfo> transparentPrimitives_;
	// Text primitives
	TextPrimitiveList textPrimitives_;
	// Triangle 'sorter'
	TriangleChopper triangleChopper_;

	private:
	// Render primitive in specified colour and level of detail
	void renderPrimitive(PrimitiveGroup& pg, int lod, GLfloat* colour, GLMatrix& transform, GLenum fillMode = GL_FILL);
	// Add text primitive for rendering later
	void renderTextPrimitive(int x, int y, const char *text, bool rightalign = FALSE);
	// Add text primitive for rendering later (screen position calculated from 3D model coordinates)
	void renderTextPrimitive(Vec3<double> vec, const char *text, bool rightalign = FALSE);

	public:
	// Initialise GL
	void initialiseGL();
	// Render 3D elements with OpenGL
	void render3D(Model* source, TCanvas *canvas);
	// Render text objects (with supplied QPainter)
	void renderText(QPainter &painter, TCanvas *canvas);
	// Set filter type
	void setType(FilterType type);
	// Return filter type
	FilterType type();
	// Sort and render filtered polygons by depth
	void sortAndSendGL();
};

#endif
