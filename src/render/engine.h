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

#include "render/polygon.h"
#include "render/primitive.h"
#include "templates/vector3.h"
#include "base/atom.h"
#include <GL/gl.h>

// Forward declarations
class Model;

// Render Engine
class RenderEngine
{
	public:
	// Constructor
	RenderEngine();
	// Filter type (if any)
	enum FilterType { NoFilter, TransparencyFilter, nFilterTypes };

	/*
	// Primitives
	*/
	private:
	// Atom styles
	Primitive atomStyle_[Atom::nDrawStyles];

	public:
	// (Re)Generate primitive vertex arrays
	void createPrimitives();


	/*
	// View Control
	*/
	private:
	// View matrix
	Mat4<double> transformationMatrix_;
	// Projection matrix
	Mat4<double> projectionMatrix_;
	// Viewport matrix for canvas
	GLint viewportMatrix_[4];

	public:
	// Set-up viewport and projection matrices
	void setupView(GLint x, GLint y, GLint w, GLint h);
	// Set current transformation matrix
	void setTransformationMatrix(Mat4<double> &mat);


	/*
	// Rendering Functions
	*/
	private:
	// Render primitive at requested local position in specified colour, returning projected position
	Vec3<double> &renderPrimitive(Primitive *p, Vec3<double> pos, GLfloat *ambient, GLfloat *diffuse);
	// Render scaled primitive at requested local position in specified colour, returning projected position
	Vec3<double> &renderPrimitiveScaled(Primitive *p, GLfloat scale, Vec3<double> pos, GLfloat *ambient, GLfloat *diffuse);

	public:
	// Render specified model
	void renderModel(Model *source);


	/*
	// Filter Type and Filtered Polygon List
	*/
	private:
	// Filter type
	FilterType type_;
	// List of filtered polygons
	List<Polygon> filteredPolygons_;
	// Depth-sorted polygon index
	int *sortedPolygons_;

	public:
	// Set filter type
	void setType(FilterType type);
	// Return filter type
	FilterType type();
	// Sort filtered polygons by depth
};

#endif
