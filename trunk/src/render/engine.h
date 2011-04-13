/*
	*** Rendering Engine
	*** src/render/engine.h
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

#ifndef ATEN_RENDERENGINE_H
#define ATEN_RENDERENGINE_H

#include "render/triangles.h"
#include "render/primitive.h"
#include "render/textprimitive.h"
#include "base/log.h"
#include "templates/vector3.h"
#include "base/atom.h"
#include "base/bond.h"
#include "base/matrix.h"

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
	// Style enum for ease of coding
	enum TriangleStyle { SolidTriangle, TransparentTriangle, WireTriangle, nTriangleStyles };
	// Objects for rendering
	enum RenderingObject { BasicObject, AtomSelectionObject, GridObject, GlyphObject, MiscObject, nRenderingObjects };
	// Text types for rendering
	enum TextType { LabelText, GlyphText, MiscText, nTextTypes };

	
	/*
	// Primitives
	*/
	private:
	// Quality setting that primitives were last generated at
	int primitiveQuality_;
	// Atom styles
	PrimitiveGroup atoms_[Atom::nDrawStyles], *scaledAtoms_;
	// Atom bond adjustment distances
	double sphereAtomAdjustment_, *scaledAtomAdjustments_;
	// Selected atom styles
	PrimitiveGroup selectedAtoms_[Atom::nDrawStyles], *selectedScaledAtoms_;
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
	// (Re)Generate primitive vertex arrays with specified quality
	void createPrimitives(int quality, bool force = FALSE);
	// (Re)initialise transparency filter
	void initialiseTransparency();
	// Calculate atom/bond adjustments
	void calculateAdjustments();
	// Create VBOs for all standard primitives
	void createVBOs();

	
	/*
	// View Control
	*/
	private:
	// Local copy of model view matrix
	Matrix modelTransformationMatrix_;
	
	public:
	// Update transformation matrix
	void setTransformationMatrix(Matrix &mat, Vec3<double> cellcentre);


	/*
	// Rendering Functions and Primitive Lists
	*/
	private:
	// Last rendered model
	Model *lastSource_;
	// Logs for last rendered source model
	Log lastLog_;
	// List of filtered solid primitives
	List<PrimitiveInfo> solidPrimitives_[RenderEngine::nRenderingObjects];
	// List of filtered primitives
	List<PrimitiveInfo> transparentPrimitives_[RenderEngine::nRenderingObjects];
	// Text primitives
	TextPrimitiveList textPrimitives_[RenderEngine::nTextTypes];
	// Triangle overs33r
	TriangleChopper triangleChopper_;
	// Grid primitives (for all models)
	List<GridPrimitive> gridPrimitives_;
	// Glyph triangle primitives
	Primitive glyphTriangles_[RenderEngine::nTriangleStyles];
	// Glyph line primitives
	Primitive glyphLines_;
	// Flags indicating which primitive lists are open for rebuilding
	bool activePrimitiveLists_[RenderEngine::nRenderingObjects];
	// Flags indicating which text primitive lists are open for rebuilding
	bool activeTextPrimitiveLists_[RenderEngine::nTextTypes];

	private:
	// Render primitive from primitive group in specified colour and level of detail
	void renderPrimitive(RenderEngine::RenderingObject obj, PrimitiveGroup& pg, GLfloat* colour, Matrix& transform, GLenum fillMode = GL_FILL, GLfloat lineWidth = 1.0);
	// Render primitive in specified colour
	void renderPrimitive(RenderEngine::RenderingObject obj, Primitive *primitive, bool isTransparent, GLfloat *colour, Matrix& transform, GLenum fillMode = GL_FILL, GLfloat lineWidth = 1.0);
	// Add text primitive for rendering later (2D coordinates)
	void renderTextPrimitive(RenderEngine::TextType type, int x, int y, const char *text, QChar addChar = 0, bool rightalign = FALSE);
	// Add text primitive for rendering later (3D coordinates)
	void renderTextPrimitive(RenderEngine::TextType type, Vec3<double> v, const char *text, QChar addChar = 0, bool rightalign = FALSE);
	// Search for primitive associated to specified Grid pointer
	GridPrimitive *findGridPrimitive(Grid *g);
	// Remove grid primitive from list (if it exists)
	void removeGridPrimitive(Grid *g);
	// Sort and render filtered polygons by depth
	void sortAndSendGL();
	// Render bond
	void renderBond(Matrix A, Vec3<double> vij, Atom *i, Atom::DrawStyle style_i, GLfloat *colour_i, double radius_i, Atom *j, Atom::DrawStyle style_j, GLfloat *colour_j, double radius_j, Bond::BondType bt, double selscale, Bond *b = NULL);
	// Render basic model information (atoms, bonds, labels)
	void renderModel(Model *source, Matrix basetransform = Matrix());
	// Render model cell
	void renderCell(Model *source);
	// Render grids
	void renderGrids(Model *source);
	// Render glyphs
	void renderGlyphs(Model *source, TCanvas *canvas);
	// Render additional model information (measurements etc.) which need to appear on top of everything else
	void renderModelOverlays(Model *source);
	// Render addition elements related to selected/active UserActions
	void renderUserActions(Model *source, TCanvas *canvas);
	// Render addition elements related to visible windows
	void renderWindowExtras(Model *source);

	public:
	// Initialise GL
	void initialiseGL();
	// Render text objects (with supplied QPainter)
	void renderText(QPainter &painter, TCanvas *canvas);
	// Render 3D elements with OpenGL
	void render3D(Model* source, TCanvas* canvas, bool currentModel);
};

#endif
