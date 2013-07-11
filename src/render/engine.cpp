/*
	*** Rendering Engine
	*** src/render/engine.cpp
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

#include "render/engine.h"
#include "base/messenger.h"
#include "classes/prefs.h"
#include "classes/forcefieldatom.h"
#include "model/model.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"
#include "main/aten.h"
#include "base/sysfunc.h"
#include <math.h>

// Class return function
RenderEngine &engine()
{
	static RenderEngine engine_;
	return engine_;
}

// Bitmap Image Formats (conform to allowable pixmap formats in Qt)
const char *bitmapFormatFilters[RenderEngine::nBitmapFormats] = { "Windows Bitmap (*.bmp)", "Joint Photographic Experts Group (*.jpg)", "Portable Network Graphics (*.png)", "Portable Pixmap (*.ppm)", "X11 Bitmap (*.xbm)", "X11 Pixmap (*.xpm)" };
const char *bitmapFormatExtensions[RenderEngine::nBitmapFormats] = { "bmp", "jpg", "png", "ppm", "xbm", "xpm" };
RenderEngine::BitmapFormat RenderEngine::bitmapFormat(const char *s, bool reportError)
{
	RenderEngine::BitmapFormat bf = (RenderEngine::BitmapFormat) enumSearch("bitmap format", RenderEngine::nBitmapFormats, bitmapFormatExtensions, s);
	if ((bf == RenderEngine::nBitmapFormats) && reportError) enumPrintValid(RenderEngine::nBitmapFormats, bitmapFormatExtensions);
	return bf;
}
RenderEngine::BitmapFormat RenderEngine::bitmapFormatFromFilter(const char *s)
{
	return (RenderEngine::BitmapFormat) enumSearch("bitmap format", RenderEngine::nBitmapFormats, bitmapFormatFilters,s);
}
const char *RenderEngine::bitmapFormatFilter(RenderEngine::BitmapFormat bf)
{
	return bitmapFormatFilters[bf];
}
const char *RenderEngine::bitmapFormatExtension(RenderEngine::BitmapFormat bf)
{
	return bitmapFormatExtensions[bf];
}

// Primitive Set
const char *primitiveSetKeywords[RenderEngine::nPrimitiveSets] = { "offscreen-lo", "offscreen-hi" };
const char *RenderEngine::primitiveSet(PrimitiveSet ps)
{
	return primitiveSetKeywords[ps];
}

// Constructor
RenderEngine::RenderEngine()
{
	// Render Target
	displayFrameId_ = -1;
	pixelBufferContext_ = NULL;
	pixelBuffer_ = NULL;
	pixelBufferHeight_ = 0;
	pixelBufferWidth_ = 0;

	// Check VBO capability, if requested.
	// There are some things to consider here, namely that we can *only* use and create VBOs on the mainWidget's context.
	// The major reason it that, since QGLWidget::renderPixmap() seems to fail regularly when reusing the main context (and
	// which cannot be done at all on Windows platforms) we either have to regenerate and manage a secondary set of 
	// VBOs when rendering to offscreen bitmaps, or simply not use VBOs when rendering offscreen bitmaps.
	// The latter is simpler, so this function and the main rendering call take note of which context is being used.
	if (prefs.instanceType() == PrimitiveInstance::VBOInstance)
	{
		// Get Extensions string and check for presence of 'GL_ARB_vertex_buffer_object'
		const GLubyte *glexts = NULL;
		glexts = glGetString( GL_EXTENSIONS );
		msg.print(Messenger::Verbose, "Available GL Extensions are : %s\n", glexts);
		if (strstr( (const char *) glexts, "GL_ARB_vertex_buffer_object") == 0)
		{
			printf("Error: VBOs requested but the extension is not available. Falling back to display lists.\n");
			prefs.setInstanceType(PrimitiveInstance::ListInstance);
		}
		else
		{
			// Store VBO function pointers (Windows only)
			#ifdef _WIN32
			Primitive::glGenBuffers = (PFNGLGENBUFFERSPROC) wglGetProcAddress("glGenBuffers");
			Primitive::glBindBuffer = (PFNGLBINDBUFFERPROC) wglGetProcAddress("glBindBuffer");
			Primitive::glBufferData = (PFNGLBUFFERDATAPROC) wglGetProcAddress("glBufferData");
			Primitive::glDeleteBuffers = (PFNGLDELETEBUFFERSPROC) wglGetProcAddress("glDeleteBuffers");
			if ((Primitive::glGenBuffers == NULL) || (Primitive::glBindBuffer == NULL) || (Primitive::glBufferData == NULL) || (Primitive::glDeleteBuffers == NULL))
			{
				printf("Error: VBOs requested but the relevant procedures could not be located. Falling back to display lists.\n");
				prefs.setInstanceType(PrimitiveInstance::ListInstance);
			}
			#endif
		}
	}

	// Primitives
	for (int n=0; n<nTriangleStyles; ++n)
	{
		glyphTriangles_[n].setColourData(TRUE);
		glyphTriangles_[n].setNoInstances();
	}
	glyphTriangles_[RenderEngine::SolidTriangle].setName("GlyphTriangle[Solid]");
	glyphTriangles_[RenderEngine::TransparentTriangle].setName("GlyphTriangle[Transparent]");
	glyphTriangles_[RenderEngine::WireTriangle].setName("GlyphTriangle[Wire]");
	linePrimitives_[RenderEngine::NormalLineObject].setColourData(TRUE);
	linePrimitives_[RenderEngine::NormalLineObject].setType(GL_LINES);
	linePrimitives_[RenderEngine::NormalLineObject].setName("LineLines");
	linePrimitives_[RenderEngine::SelectedLineObject].setColourData(TRUE);
	linePrimitives_[RenderEngine::SelectedLineObject].setType(GL_LINES);
	linePrimitives_[RenderEngine::SelectedLineObject].setName("LineSelectedLines");
	linePrimitives_[RenderEngine::NormalGuiLineObject].setColourData(TRUE);
	linePrimitives_[RenderEngine::NormalGuiLineObject].setType(GL_LINES);
	linePrimitives_[RenderEngine::NormalGuiLineObject].setName("GuiLineLines");
	linePrimitives_[RenderEngine::SelectedGuiLineObject].setColourData(TRUE);
	linePrimitives_[RenderEngine::SelectedGuiLineObject].setType(GL_LINES);
	linePrimitives_[RenderEngine::SelectedGuiLineObject].setName("GuiStickLines");
	glyphLines_.setColourData(TRUE);
	glyphLines_.setType(GL_LINES);
	glyphLines_.setNoInstances();
	glyphLines_.setName("glyphLines");
	initialiseTransparency();
	scaledAtomAdjustments_ = new double[elements().nElements()];
	primitives_[RenderEngine::LowQuality].setQuality(prefs.primitiveQuality());
	primitives_[RenderEngine::HighQuality].setQuality(prefs.imagePrimitiveQuality());
	lastSource_ = NULL;
	rebuildSticks_ = FALSE;
	set_ = RenderEngine::LowQuality;
	clearListsFlag_ = FALSE;
	calculateAdjustments();

	// Setup QGLFormat for all contexts
	QGLFormat::defaultFormat().setSampleBuffers(TRUE);

	#if QT_VERSION >= 0x040600
	QGL::setPreferredPaintEngine(QPaintEngine::OpenGL);
	#endif
	
	// Create a context for the TCanvas
	canvasContext_ = new QGLContext(QGLFormat::defaultFormat());

	// Attempt to initialise a pixelbuffer
	if (prefs.usePixelBuffers() && (!initialisePixelBuffer(2048,2048))) prefs.setUsePixelBuffers(FALSE);
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
void RenderEngine::renderPrimitive(RenderEngine::TriangleObject obj, PrimitiveGroup& pg, GLfloat* colour, Matrix& transform, GLenum fillMode, GLfloat lineWidth)
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
void RenderEngine::renderPrimitive(RenderEngine::TriangleObject obj, Primitive* primitive, bool isTransparent, GLfloat *colour, Matrix& transform, GLenum fillMode, GLfloat lineWidth)
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
	textPrimitives_.add(x, contextHeight_-y, text, addChar, rightalign);
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
	Matrix A;
	Primitive *prim;

	// Transform and render each solid primitive in each list
	for (int n=0; n<RenderEngine::nTriangleObjects; ++n)
	{
		for (PrimitiveInfo *pi = solidPrimitives_[n].first(); pi != NULL; pi = pi->next)
		{
			// If the info structure has a pointer to a primitive in it, use that.
			// Otherwise, retrieve a primitive with a suitable level of detail by passing the current model transformation matrix
			prim = pi->primitive();
			if (prim == NULL) prim = (set_ == 1 ? pi->bestPrimitive() : pi->primitive(modelTransformationMatrix_));
			
			// If colour data is not present in the vertex data array, use the colour stored in the PrimitiveInfo object
			if (!prim->colouredVertexData()) glColor4fv(pi->colour());
			glPolygonMode(GL_FRONT_AND_BACK, pi->fillMode());
			if (pi->fillMode() == GL_LINE)
			{
				glLineWidth(pi->lineWidth());
				glDisable(GL_LIGHTING);
			}
			else if (pi->fillMode() == GL_POINT)
			{
				glPointSize(1.0);
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
			else if (pi->fillMode() == GL_POINT) glEnable(GL_LIGHTING);
		}
	}

	// Draw stick primitives
	glDisable(GL_LIGHTING);
	glLoadMatrixd(modelTransformationMatrix_.matrix());
	glLineWidth(prefs.stickLineNormalWidth());
	linePrimitives_[NormalLineObject].sendToGL();
	linePrimitives_[NormalGuiLineObject].sendToGL();
	glLineWidth(prefs.stickLineSelectedWidth());
	linePrimitives_[SelectedLineObject].sendToGL();
	linePrimitives_[SelectedGuiLineObject].sendToGL();
	glEnable(GL_LIGHTING);
	glLineWidth(1.0);
	
	// Transform and render each transparent primitive in each list, unless transparencyCorrect_ is off.
	if (prefs.transparencyCorrect())
	{
		triangleChopper_.emptyTriangles();
		for (int n=0; n<RenderEngine::nTriangleObjects; ++n)
		{
			for (PrimitiveInfo *pi = transparentPrimitives_[n].first(); pi != NULL; pi = pi->next)
			{
				triangleChopper_.storeTriangles(pi, modelTransformationMatrix_);
			}
		}
		glLoadIdentity();
		glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
		triangleChopper_.sendToGL();
		glPopClientAttrib();
	}
	else for (int n=0; n<RenderEngine::nTriangleObjects; ++n)
	{
		for (PrimitiveInfo *pi = transparentPrimitives_[n].first(); pi != NULL; pi = pi->next)
		{
			// If the info structure has a pointer to a primitive in it, use that.
			// Otherwise, work out a level of detail value to pass to the primitive group referenced.
			prim = pi->primitive();
			if (prim == NULL) prim = (set_ == 1 ? pi->bestPrimitive() : pi->primitive(modelTransformationMatrix_));
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
// 	glClearDepth(1.0);
// 	glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
// 	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
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
void RenderEngine::pushInstance(RenderEngine::PrimitiveSet set, const QGLContext *context)
{
	msg.print(Messenger::Verbose, "Pushing %s primitive instance for context %p (current context is %p).\n", RenderEngine::primitiveSet(set), context, QGLContext::currentContext());
	primitives_[set].pushInstance(context);
	// Push separate instance of the line primitives
	for (int n = 0; n<RenderEngine::nLineObjects; ++n) linePrimitives_[n].pushInstance(context);
}

// Pop topmost primitive instance
void RenderEngine::popInstance(RenderEngine::PrimitiveSet set, const QGLContext *context)
{
	msg.print(Messenger::Verbose, "Popping %s quality primitive instance for context %p\n", RenderEngine::primitiveSet(set), context);
	primitives_[set].popInstance(context);
	// Pop separate instance of the line primitives
	for (int n = 0; n<RenderEngine::nLineObjects; ++n) linePrimitives_[n].popInstance(context);
}

// Update all primitives (following prefs change, etc.)
void RenderEngine::updatePrimitives()
{
	// Set (possibly new) quality
	primitives_[LowQuality].setQuality(prefs.primitiveQuality());
	primitives_[HighQuality].setQuality(prefs.imagePrimitiveQuality());
	
	// Recalculate adjustments for bond positioning
	calculateAdjustments();

	// Generate new VBOs / display lists - pop and push a context
	for (int n=0; n<nPrimitiveSets; ++n) if (primitives_[n].stackSize() != 0)
	{
		primitives_[n].popInstance(prefs.usePixelBuffers() ? pixelBufferContext_ : canvasContext_);
		primitives_[n].pushInstance(prefs.usePixelBuffers() ? pixelBufferContext_ : canvasContext_, true);
	}
}

// (Re)initialise pixelbuffer to desired size
bool RenderEngine::initialisePixelBuffer(int w, int h, bool forceRecreate)
{
	// Are pixelBuffers supported?
	if (!QGLPixelBuffer::hasOpenGLPbuffers())
	{
		msg.print("Error: System does not support pixel buffers.\nWill have to revert to QGLWidget::renderPixmap() for offscreen images.\n");
		return FALSE;
	}
	
	// Check new size against old size
	if (forceRecreate || (w > pixelBufferWidth_) || (h > pixelBufferHeight_))
	{
		int newWidth = 1, newHeight = 1;
		while (newWidth < w) { newWidth *= 2; };
		while (newHeight < h) { newHeight *= 2; };
		msg.print(Messenger::Verbose, "Creating new offscreen pixelbuffer - requested = %ix%i, old = %ix%i, new = %ix%i\n", w, h, pixelBufferWidth_, pixelBufferHeight_, newWidth, newHeight);
		pixelBufferHeight_ = newHeight;
		pixelBufferWidth_ = newWidth;
		
		// Clean up from previous instantiation
		if (pixelBuffer_ != NULL)
		{
			delete pixelBuffer_;
			popInstance(LowQuality, pixelBufferContext_);
			popInstance(HighQuality, pixelBufferContext_);
		}
		
		// Create new pixelbuffer and associated primitives
		pixelBuffer_ = new QGLPixelBuffer(pixelBufferWidth_, pixelBufferHeight_, QGLFormat::defaultFormat(), gui.mainCanvas());
		
		if (!pixelBuffer_->makeCurrent())
		{
			msg.print("Error: Couldn't make pixelBuffer_'s context current.\n");
			delete pixelBuffer_;
			pixelBuffer_ = NULL;
			pixelBufferHeight_ = 0;
			pixelBufferWidth_ = 0;
			pixelBufferContext_ = NULL;
			return FALSE;
		}
		pixelBufferContext_ = QGLContext::currentContext();
		pushInstance(LowQuality, pixelBufferContext_);
		pushInstance(HighQuality, pixelBufferContext_);
	}
	return TRUE;
}

// Return context for TCanvas
QGLContext *RenderEngine::canvasContext()
{
	return canvasContext_;
}

// Check for GL Error
void RenderEngine::checkGlError()
{
	// Do GL error check
	if (msg.isOutputActive(Messenger::GL))
	{
		GLenum glerr = GL_NO_ERROR;
		do
		{
			switch (glGetError())
			{
				case (GL_INVALID_ENUM): msg.print(Messenger::GL, "GLenum argument out of range\n"); break;
				case (GL_INVALID_VALUE): msg.print(Messenger::GL, "Numeric argument out of range\n"); break;
				case (GL_INVALID_OPERATION): msg.print(Messenger::GL, "Operation illegal in current state\n"); break;
				case (GL_STACK_OVERFLOW): msg.print(Messenger::GL, "Command would cause a stack overflow\n"); break;
				case (GL_STACK_UNDERFLOW): msg.print(Messenger::GL, "Command would cause a stack underflow\n"); break;
				case (GL_OUT_OF_MEMORY): msg.print(Messenger::GL, "Not enough memory left to execute command\n"); break;
				case (GL_NO_ERROR): msg.print(Messenger::GL, "No GL error\n"); break;
				default:
					msg.print(Messenger::GL, "Unknown GL error?\n");
					break;
			}
		} while (glerr != GL_NO_ERROR);
	}
}

// Render Main Scene
void RenderEngine::renderScene(RenderEngine::PrimitiveSet set, int width, int height, const QGLContext* context, RenderEngine::RenderType renderType, Model* iconSource)
{
	msg.enter("RenderEngine::renderScene");
	QColor color;
	QRect currentBox;
	Refitem<Model,int> *first, localri;
	int px, py, nperrow = prefs.nModelsPerRow(), nrows, col, row, nmodels;
	bool modelIsCurrentModel;
	Model *m;

	// Store set specifier
	set_ = set;
	
	// Store full context height
	contextHeight_ = height;

	// Setup basic GL stuff
	initialiseGL();

	// Note: An internet source suggests that the QPainter documentation is incomplete, and that
	// all OpenGL calls should be made after the QPainter is constructed, and before the QPainter
	// is destroyed. However, this results in mangled graphics on the Linux (and other?) versions,
	// so here it is done in the 'wrong' order.
	
	// Set the first item to consider - set localri to the passed iconSource (if there was one)
	localri.item = iconSource;
	nmodels = aten.nVisibleModels();
	if ((nmodels == 0) || (renderType == OffscreenModel))
	{
		if (renderType != OffscreenModel) localri.item = aten.currentModel();
		nmodels = 1;
		first = &localri;
	}
	else first = aten.visibleModels();

	msg.print(Messenger::GL, " --> Target context is %p (canvas context = %p)\n", context, canvasContext_);

	// Clear view
	msg.print(Messenger::GL, " --> Clearing context, background, and setting pen colour\n");
	glViewport(0,0,width,height);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	textPrimitives_.forgetAll();
	
	// Set up some useful values
	nrows = nmodels/nperrow + (nmodels%nperrow == 0 ? 0 : 1);
	py = height / nrows;
	px = (nmodels == 1 ? width : width / nperrow);

	// Loop over model refitems in list (or single refitem)
	col = 0;
	row = 0;
	for (Refitem<Model,int> *ri = first; ri != NULL; ri = ri->next)
	{
		// Grab model pointer
		m = ri->item;
		if (m == NULL) continue;

		// Store coordinates for box if this is the current model
		if (m == aten.currentModel())
		{
			modelIsCurrentModel = TRUE;
			currentBox.setRect(col*px, row*py, px, py);
		}
		else modelIsCurrentModel = FALSE;

		// Vibration frame?
		m = m->renderSourceModel();
		if (m->renderFromVibration() && (m->vibrationCurrentFrame() != NULL)) m = m->vibrationCurrentFrame();

		// Determine desired pixel range and set up view(port)
		checkGlError();
		m->setupView(col*px, height-(row+1)*py, px, py);

		// Render the 3D parts of the model
		renderModel(m, modelIsCurrentModel, renderType);

		// Increase counters
		++col;
		if (col%nperrow == 0)
		{
			col = 0;
			++row;
		}
	}

	// Select QPaintDevice* for QPainter
	QPaintDevice* targetDevice = NULL;
	if ((renderType != OnscreenScene) && prefs.usePixelBuffers()) targetDevice = pixelBuffer_;
	else if (context != canvasContext_) targetDevice = QGLContext::currentContext()->device();
	else targetDevice = gui.mainCanvas();
	
	// Start of QPainter code
	static QFont font;
	static QBrush nobrush(Qt::NoBrush), solidbrush(Qt::SolidPattern);
	static QPen pen;
	QPainter painter(targetDevice);

	// Need to offset QPainter vertically if using pixelbuffers
	if ((renderType != OnscreenScene) && (prefs.usePixelBuffers()))
	{
		painter.translate(0, painter.viewport().height()-height);
	}

	// Text Primitives
	GLfloat colour[4];
	double fontSize = (prefs.labelSize()/100.0)*height;
	font.setPointSizeF(fontSize);
	painter.setFont(font);
	painter.setRenderHint(QPainter::Antialiasing);
	prefs.copyColour(Prefs::TextColour, colour);
	color.setRgbF(colour[0], colour[1], colour[2], colour[3]);
	solidbrush.setColor(color);
	painter.setBrush(solidbrush);
	painter.setPen(Qt::SolidLine);
	painter.setPen(color);
	textPrimitives_.renderAll(painter, fontSize);

	// Draw box around current model
	color.setRgbF(0.0,0.0,0.0,1.0);
	pen.setColor(color);
	pen.setWidth(2);
	painter.setBrush(nobrush);
	painter.setPen(Qt::SolidLine);
	painter.setPen(pen);

	if (prefs.frameCurrentModel()) painter.drawRect(currentBox);
	if (prefs.frameWholeView())
	{
		currentBox.setRect(0,0,width,height);
		painter.drawRect(currentBox);
	}

	// Render active user modes
	if (renderType == OnscreenScene) renderActiveModes(painter, width, height);

	// Done
	painter.end();

	msg.print(Messenger::GL, " --> RENDERING END\n");
	
	msg.exit("RenderEngine::renderScene");
}

// Render or grab image
QPixmap RenderEngine::renderSceneImage(RenderEngine::PrimitiveSet set, int w, int h)
{
	if (prefs.usePixelBuffers())
	{
		// Check that we can create a pixelbuffer object
		initialisePixelBuffer(w, h);
		if (!pixelBuffer_->makeCurrent())
		{
			msg.print("Error: QGLPixelBuffer could not be made current.\n");
			return QPixmap();
		}

		// Clear lists in engine
		clearListsFlag_ = TRUE;
		
		// Generate offscreen bitmap
		renderScene(RenderEngine::HighQuality, w, h, pixelBufferContext_, OffscreenScene);
		pixelBuffer_->doneCurrent();
		
		// Flag for rendering list regeneration again
		clearListsFlag_ = TRUE;
		
		QImage image = pixelBuffer_->toImage().copy(0,pixelBufferHeight_-h,w,h);
		
		return QPixmap::fromImage(image);
	}
	else
	{
		// Clear lists in engine
		clearListsFlag_ = TRUE;

		// Request high-quality primitives for next render
		gui.mainCanvas()->requestHighQuality();

		// Store current size
		int oldWidth = gui.mainCanvas()->contextWidth();
		int oldHeight = gui.mainCanvas()->contextHeight();

		// Generate offscreen bitmap (a temporary context will be created)
		gui.mainCanvas()->setRenderType(OffscreenScene);
		QPixmap pixmap = gui.mainCanvas()->renderPixmap(w,h);
		gui.mainCanvas()->setRenderType(OnscreenScene);
		
		// Flag for rendering list regeneration again
		clearListsFlag_ = TRUE;
		
		// Reset view size and refresh
		gui.mainCanvas()->resize(oldWidth, oldHeight);

		return pixmap;
	}
}

// Create icon pixmap for specific model
QPixmap RenderEngine::renderModelIcon(Model *source)
{
	msg.print(Messenger::Verbose, "Rendering model icon for '%s'....\n", source == NULL ? "??NULL Model??" : source->name());
	if (prefs.usePixelBuffers())
	{
		// Check that we can use create a pixelbuffer object
		initialisePixelBuffer(100, 100);
		if (!pixelBuffer_->makeCurrent())
		{
			msg.print("Error: QGLPixelBuffer could not be made current.\n");
			return QPixmap();
		}

		// Clear lists in engine
		clearListsFlag_ = TRUE;
		
		// Generate offscreen bitmap
		renderScene(RenderEngine::LowQuality, 100, 100, pixelBufferContext_, OffscreenModel, source);
		pixelBuffer_->doneCurrent();
		
		// Flag for rendering list regeneration again
		clearListsFlag_ = TRUE;
		
		QImage image = pixelBuffer_->toImage().copy(0,pixelBufferHeight_-100,100,100);
		
		return QPixmap::fromImage(image);
	}
	else
	{
		// Clear lists in engine
		clearListsFlag_ = TRUE;

		// Store current size
		int w = gui.mainCanvas()->contextWidth();
		int h = gui.mainCanvas()->contextHeight();

		// Generate offscreen bitmap (a temporary context will be created)
		gui.mainCanvas()->setRenderType(OffscreenModel, source);
		QPixmap pixmap = gui.mainCanvas()->renderPixmap(100,100);
		gui.mainCanvas()->setRenderType(OnscreenScene);
		
		// Flag for rendering list regeneration again
		clearListsFlag_ = TRUE;
		
		// Reset view size and refresh
		gui.mainCanvas()->resize(w, h);

		return pixmap;
	}
}

// Save image of current view
bool RenderEngine::saveImage(const char *filename, BitmapFormat bf, int width, int height, int quality)
{
	msg.enter("RenderEngine::saveImage");
	if (bf == RenderEngine::nBitmapFormats)
	{
		msg.print("Invalid bitmap format given to Gui::saveImage().\n");
		msg.exit("RenderEngine::saveImage");
		return FALSE;
	}

	QPixmap pixmap;
	// Get current mainCanvas_ geometry if none was specified
	if (width == 0) width = pixelBufferWidth_;
	if (height == 0) height = pixelBufferHeight_;

	pixmap = renderSceneImage(RenderEngine::HighQuality, width, height);

	pixmap.save(filename, RenderEngine::bitmapFormatExtension(bf), quality);
	msg.print("Saved current view as '%s' [%ix%i %s]\n", filename, width, height, RenderEngine::bitmapFormatFilter(bf));

	msg.exit("RenderEngine::saveImage");
	return TRUE;
}
