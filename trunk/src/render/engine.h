/*
	*** Rendering Engine
	*** src/render/engine.h
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

#ifndef ATEN_RENDERENGINE_H
#define ATEN_RENDERENGINE_H

#ifdef _WIN32
#include <windows.h>
#include <GL/gl.h>
#include "glext.h"
#endif
#include "render/engine_primitives.h"
#include "render/triangles.h"
#include "render/gridprimitive.h"
#include "render/textprimitive.h"
#include "base/log.h"
#include "templates/vector3.h"
#include "base/atom.h"
#include "base/bond.h"
#include "base/matrix.h"

// Forward declarations
class Model;
class TCanvas;
class QGLContext;

// Render Engine
class RenderEngine
{
	public:
	// Constructor / Destructor
	RenderEngine();
	~RenderEngine();
	// Style enum for ease of coding
	enum TriangleStyle { SolidTriangle, TransparentTriangle, WireTriangle, nTriangleStyles };
	// Triangle-based Objects for rendering
	enum TriangleObject { BasicObject, AtomSelectionObject, GridObject, GlyphObject, GuiObject, nTriangleObjects };
	// Stick objects for rendering
	enum LineObject { NormalLineObject, SelectedLineObject, NormalGuiLineObject, SelectedGuiLineObject, nLineObjects };
	// Drawing Targets enum
	enum DrawingTarget { NoTarget, ScreenTarget, PixmapTarget };
	// Bitmap Formats
	enum BitmapFormat { BitmapBMP, BitmapPG, BitmapPNG, BitmapPPM, BitmapXBM, BitmapX11, nBitmapFormats };
	static BitmapFormat bitmapFormat(const char *name, bool reportError = 0);
	static BitmapFormat bitmapFormatFromFilter(const char *s);
	static const char *bitmapFormatFilter(BitmapFormat bf);
	static const char *bitmapFormatExtension(BitmapFormat bf);
	// Primitive Set
	enum PrimitiveSet { LowQuality, HighQuality, nPrimitiveSets };
	static const char *primitiveSet(PrimitiveSet ps);
	// Rendering Type
	enum RenderType { OnscreenScene, OffscreenScene, OffscreenModel };


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
	// Rendering Functions and Filtered Primitive Lists
	*/
	private:
	// Atom bond adjustment distances
	double sphereAtomAdjustment_, *scaledAtomAdjustments_;
	// Primitive sets for on and offscreen rendering
	RenderPrimitives primitives_[RenderEngine::nPrimitiveSets];
	// Flag indicating that next render should completely clear all filtered primitive lists
	bool clearListsFlag_;
	// Current source primitive set
	RenderEngine::PrimitiveSet set_;
	// Last rendered model
	Model *lastSource_;
	// Logs for last rendered source model
	Log lastLog_;
	// List of filtered solid primitives
	List<PrimitiveInfo> solidPrimitives_[RenderEngine::nTriangleObjects];
	// List of filtered primitives
	List<PrimitiveInfo> transparentPrimitives_[RenderEngine::nTriangleObjects];
	//  Line primitives
	Primitive linePrimitives_[RenderEngine::nLineObjects];
	// Text primitives
	TextPrimitiveList textPrimitives_;
	// Triangle
	TriangleChopper triangleChopper_;
	// Grid primitives (for all models)
	List<GridPrimitive> gridPrimitives_;
	// Glyph triangle primitives
	Primitive glyphTriangles_[RenderEngine::nTriangleStyles];
	// Glyph line primitives
	Primitive glyphLines_;
	// Flags indicating which primitive lists are open for rebuilding
	bool activePrimitiveLists_[RenderEngine::nTriangleObjects];
	// Flag stating whether to rebuild stick primitives
	bool rebuildSticks_;
	// Height of current rendering target
	int contextHeight_;

	private:
	// Calculate atom/bond adjustments
	void calculateAdjustments();
	// Render primitive from primitive group in specified colour and level of detail
	void renderPrimitive(RenderEngine::TriangleObject obj, PrimitiveGroup& pg, GLfloat* colour, Matrix& transform, GLenum fillMode = GL_FILL, GLfloat lineWidth = 1.0);
	// Render primitive in specified colour
	void renderPrimitive(RenderEngine::TriangleObject obj, Primitive *primitive, bool isTransparent, GLfloat *colour, Matrix& transform, GLenum fillMode = GL_FILL, GLfloat lineWidth = 1.0);
	// Add text primitive for rendering later
	void renderTextPrimitive(int x, int y, const char *text, QChar addChar = 0, bool rightalign = FALSE);
	// Search for primitive associated to specified Grid pointer
	GridPrimitive *findGridPrimitive(Grid *g);
	// Remove grid primitive from list (if it exists)
	void removeGridPrimitive(Grid *g);
	// Sort and render filtered polygons by depth
	void sortAndSendGL();
	// Render bond
	void renderBond(TriangleObject basicList, TriangleObject selectionList, Matrix A, Vec3<double> vij, Atom *i, Atom::DrawStyle style_i, GLfloat *colour_i, double radius_i, Atom *j, Atom::DrawStyle style_j, GLfloat *colour_j, double radius_j, Bond::BondType bt, double selscale, Bond *b = NULL, bool transparentSel = FALSE, GLfloat *penColour = NULL);
	// Setup and render full Model
	void renderModel(Model* source, bool currentModel, bool renderType);
	// Render atoms and bonds
	void renderAtomsAndBonds(Model* source, Matrix baseTransform = Matrix(), bool isFragment = FALSE);
	// Render model cell
	void renderCell(Model *source);
	// Render grids
	void renderGrids(Model *source);
	// Render text for grids
	void renderGridText(Model *source);
	// Render 3D glyphs
	void renderGlyphs(Model *source);
	// Render text glyphs
	void renderTextGlyphs(Model *source);
	// Render additional model information (labels, measurements etc.) which need to appear on top of everything else
	void renderModelOverlays(Model *source);
	// Render active mode embellishments
	void renderActiveModes(QPainter& painter, int width, int height);
	// Render addition elements related to selected/active UserActions
	void renderUserActions(Model *source);
	// Render addition elements related to visible windows
	void renderWindowExtras(Model *source);

	public:
	// (Re)initialise transparency filter
	void initialiseTransparency();
	// Initialise GL
	void initialiseGL();
	// Push primitives instance (in specified quality)
	void pushInstance(RenderEngine::PrimitiveSet set, const QGLContext *context);
	// Pop topmost primitive instance
	void popInstance(RenderEngine::PrimitiveSet set, const QGLContext *context);
	// Update all primitives (following prefs change, etc.)
	void updatePrimitives();


	/*
	// Rendering
	*/
	private:
	// QQLPixelBuffer for offscreen rendering
	QGLPixelBuffer *pixelBuffer_;
	// Size of QGLPixelBuffer at last creation
	int pixelBufferWidth_, pixelBufferHeight_;
	// Context of current pixelBuffer_
	const QGLContext *pixelBufferContext_;
	// Context for TCanvas
	QGLContext *canvasContext_;
	// ID of last trajectory frame displayed (if any)
	int displayFrameId_;

	public:
	// (Re)initialise pixelbuffer to desired size
	bool initialisePixelBuffer(int w, int h, bool forceRecreate = FALSE);
	// Return context for TCanvas
	QGLContext *canvasContext();
	// Check for GL error
	void checkGlError();
	// Render whole scene
	void renderScene(RenderEngine::PrimitiveSet set, int width, int height, const QGLContext* context, RenderType renderType, Model *iconSource = NULL);
	// Create pixmap of whole scene
	QPixmap renderSceneImage(RenderEngine::PrimitiveSet set, int w, int h);
	// Create icon pixmap for specific model
	QPixmap renderModelIcon(Model *source);
	// Save image of current view
	bool saveImage(const char *filename, BitmapFormat bf, int width, int height, int quality = 85);
};

// External Declaration
extern RenderEngine &engine();

#endif
