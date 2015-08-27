/*
	*** RenderGroup
	*** src/render/rendergroup.h
	Copyright T. Youngs 2013-2015

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

#ifndef ATEN_RENDERGROUP_H
#define ATEN_RENDERGROUP_H

#include "render/primitive.h"
#include "render/primitiveinfo.h"
#include "render/textprimitivelist.h"
#include "base/prefs.h"
#include "base/bond.h"
#include <QColor>

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Atom;
class Model;
class PrimitiveSet;

// RenderGroup
class RenderGroup
{
	public:
	// Constructor
	RenderGroup();


	/*
	 * Object Lists
	 */
	private:
	// Primitives to be rendered as solid triangles
	List<PrimitiveInfo> solidTrianglePrimitives_;
	// Primitives to be rendered as transparent triangles
	List<PrimitiveInfo> transparentTrianglePrimitives_;
	// Primitives to be rendered as wireframe triangles
	List<PrimitiveInfo> wireTrianglePrimitives_;
	// Primitives to be rendered as normal lines
	List<PrimitiveInfo> normalLinePrimitives_;
	// Primitives to be rendered as bold lines
	List<PrimitiveInfo> boldLinePrimitives_;
	// Normal lines
	Primitive extraNormalLines_;
	// Bold lines
	Primitive extraBoldLines_;
	// Overlay lines
	Primitive overlayLines_;
	// Additional solid triangles
	Primitive extraSolidTriangles_;
	// Additional wire triangles
	Primitive extraWireTriangles_;
	// Text primitives
	TextPrimitiveList textPrimitives_;
	// Overlay text primitives
	TextPrimitiveList overlayTextPrimitives_;

	public:
	// Clear lists
	void clear();
	// Add text primitive
	void addText(QString text, Vec3<double> pos, double textSize, TextPrimitive::TextAnchor anchor = TextPrimitive::CentralAnchor, Vec3<double> globalAdjustment = Vec3<double>(), bool flat = true);
	// Add overlay text primitive
	void addOverlayText(QString text, Vec3<double> pos, double textSize, TextPrimitive::TextAnchor anchor = TextPrimitive::CentralAnchor, Vec3<double> globalAdjustment = Vec3<double>(), bool flat = true);
	// Add triangle primitive in specified colour
	void addTriangles(Primitive& targetPrimitive, Matrix& transform, Vec4<GLfloat>& colour, GLenum fillMode = GL_FILL, GLfloat lineWidth = -1.0);
	// Add triangle primitive (which has it's own colour info)
	void addTriangles(Primitive& targetPrimitive, Matrix& transform, GLenum fillMode = GL_FILL, GLfloat lineWidth = -1.0);
	// Add line primitive in specified colour
	void addLines(Primitive& targetPrimitive, Matrix& transform, Vec4<GLfloat>& colour, bool bold = false);
	// Add line primitive (which has it's own colour info)
	void addLines(Primitive& targetPrimitive, Matrix& transform, bool bold = false);


	/*
	 * Creation
	 */
	public:
	// Render bond
	void createBond(PrimitiveSet& primitiveSet, Matrix A, Vec3<double> vij, Atom* i, Prefs::DrawStyle style_i, Vec4<GLfloat>& colour_i, double radius_i, Atom* j, Prefs::DrawStyle style_j, Vec4<GLfloat>& colour_j, double radius_j, Bond::BondType bt, double selscale, Bond* b = 0, bool transparentSel = false);
	// Generate primitive info for atoms and bonds of specified model
	void createAtomsAndBonds(PrimitiveSet& primitiveSet, Model* source, Matrix baseTransform);
	// Generate primitive data for model glyphs
	void createGlyphs(PrimitiveSet& primitiveSet, Model* source);
	// Generate overlays (labels and measurements)
	void createOverlays(Model* source, Matrix baseTransform);


	/*
	 * GL
	 */
	public:
	// Send to GL
	void sendToGL(Matrix& modelTransformationMatrix);
};

ATEN_END_NAMESPACE

#endif
