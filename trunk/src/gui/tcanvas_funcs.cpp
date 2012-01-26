/*
	*** TCanvas Functions
	*** src/gui/tcanvas_funcs.cpp
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

#include "main/aten.h"
#include "gui/tcanvas.uih"
#include "gui/gui.h"

// Local variables
bool DONTDRAW = FALSE;

// Constructor
TCanvas::TCanvas(QGLContext *ctxt, QWidget *parent) : QGLWidget(ctxt, parent)
{
	// Character / Setup
	contextWidth_ = 0;
	contextHeight_ = 0;
	valid_ = FALSE;

	// Render Target
	displayFrameId_ = -1;
	useCurrentModel_ = TRUE;
	renderSource_ = NULL;
	redrawActiveModel_ = FALSE;
	noPixelData_ = FALSE;

	// Rendering
	drawingTarget_ = TCanvas::NoTarget;
	noPixelData_ = FALSE;
	drawing_ = FALSE;
	highQuality_ = FALSE;
	mouseMoveCounter_.start();

	// Atom Selection
	atomClicked_ = NULL;
	pickEnabled_ = FALSE;
	actionBeforePick_ = UserAction::NoAction;
	pickAtomsCallback_ = NULL;
	nAtomsToPick_ = -1;

	// Mouse Input
	for (int i=0; i<3; i++) mouseButton_[i] = FALSE;

	// Key Input
	for (int i=0; i<3; i++) keyModifier_[i] = FALSE;

	// User Actions
	activeMode_ = UserAction::NoAction;
	selectedMode_ = UserAction::SelectAction;
	currentDrawDepth_ = -5.0;
	sketchElement_ = 6;
	editable_ = TRUE;

	// Prevent QPainter from autofilling widget background
	setAutoFillBackground(FALSE);
}

// Destructor
TCanvas::~TCanvas()
{
// 	printf("Destroying TCanvas\n");
}

/*
// Character
*/

// Return the current height of the drawing area
GLsizei TCanvas::contextHeight() const
{
	return contextHeight_;
}

// Return the current width of the drawing area
GLsizei TCanvas::contextWidth() const
{
	return contextWidth_;
}

// Probe features
void TCanvas::probeFeatures()
{
	if (msg.isOutputActive(Messenger::GL))
	{
		QGLFormat fmt = context()->format();
		// Probe this format!
		printf(" QGLFormat: Alpha buffer is %s.\n", fmt.alpha() ? "enabled" : "disabled");
		printf(" QGLFormat: Accumulation buffer is %s.\n", fmt.accum() ? "enabled" : "disabled");
		printf(" QGLFormat: Depth buffer is %s.\n", fmt.depth() ? "enabled" : "disabled");
		printf(" QGLFormat: Double-buffering is %s.\n", fmt.doubleBuffer() ? "enabled" : "disabled");
		printf(" QGLFormat: Direct rendering is %s.\n", fmt.directRendering() ? "enabled" : "disabled");
		printf(" QGLFormat: RGBA colour mode is %s.\n", fmt.rgba() ? "enabled" : "disabled");
		printf(" QGLFormat: Multisample buffer is %s.\n", fmt.sampleBuffers() ? "enabled" : "disabled");
		printf(" QGLFormat: Stencil buffer is %s.\n", fmt.stencil() ? "enabled" : "disabled");
		printf(" QGLWidget: Autoswap buffers is %s.\n", autoBufferSwap() ? "enabled" : "disabled");
	}
}

/*
// Render Target
*/

// Return the current display model
Model *TCanvas::displayModel() const
{
	Model *source = (useCurrentModel_ ? aten.currentModelOrFrame() : renderSource_);
	return source;
}

// Set the rendering source to the supplied model (reverts to useCurrentModel_ if a NULL pointer is supplied)
void TCanvas::setRenderSource(Model *source)
{
	if (source == NULL)
	{
		useCurrentModel_ = TRUE;
		renderSource_ = NULL;
	}
	else
	{
		useCurrentModel_ = FALSE;
		renderSource_ = source;
	}
}

// Determine target model based on clicked position on TCanvas
Model *TCanvas::modelAt(int x, int y)
{
	int nrows, py, px, id;
	
	// Is only one model displayed?
	if (aten.nVisibleModels() <= 1) return aten.currentModel();

	// Determine whether we need to change Aten's currentmodel based on click position on the canvas
	nrows = aten.nVisibleModels()/prefs.nModelsPerRow() + (aten.nVisibleModels()%prefs.nModelsPerRow() == 0 ? 0 : 1);
	py = contextHeight_ / nrows;
	px = (aten.nVisibleModels() == 1 ? contextWidth_ : contextWidth_ / prefs.nModelsPerRow());

	// Work out model index...
	id = (y/py)*prefs.nModelsPerRow() + x/px;

	// In the case of clicking in a blank part of the canvas with no model (i.e. bottom-right corner) return a safe model pointer
	return (id >= aten.nVisibleModels() ? aten.currentModel() : aten.visibleModel(id));
}

/*
// Rendering Functions
*/

// Initialise context widget (when created by Qt)
void TCanvas::initializeGL()
{
	// Initialize GL
	msg.enter("TCanvas::initializeGL");
	valid_ = TRUE;
	
	// Check VBO capability, if requested
	// There are some things to consider here, namely that we can *only* use and create VBOs on the mainWidget's context.
	// The major reason it that, since QGLWidget::renderPixmap() seems to fail regularly when reusing the main context (and
	// which cannot be done at all on Windows platforms) we either have to regenerate and manage a secondary set of 
	// VBOs when rendering to offscreen bitmaps, or simply not use VBOs when rendering offscreen bitmaps.
	// The latter is simpler, so this function and the main rendering call take note of which context is being used.
	static int count = 0;
	if ((count == 0) && (prefs.instanceType() == PrimitiveInstance::VBOInstance))
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
		++count;
	}

	// Push a primitives instance in the rendering engine
	engine_.pushInstance(highQuality_, context());

	msg.exit("TCanvas::initializeGL");
}

void TCanvas::paintGL()
{
	if (drawingTarget_ == TCanvas::PixmapTarget) paintEvent(NULL);
}

// General repaint callback
void TCanvas::paintEvent(QPaintEvent *event)
{
	msg.enter("TCanvas::paintGL");

	QColor color;
	QRect currentBox;
	Refitem<Model,int> *first, localri;
	int px, py, nperrow = prefs.nModelsPerRow(), nrows, col, row, nmodels;
	bool usepixels, modelIsCurrentModel;
	Model *m;

	// Do nothing if the canvas is not valid, or we are still drawing from last time.
	if ((!valid_) || drawing_ || (drawingTarget_ == TCanvas::NoTarget) || (aten.currentModel() == NULL))
	{
		msg.exit("TCanvas::paintGL");
		return;
	}

	// Setup basic GL stuff
	engine_.initialiseGL();

	// Note: An internet source suggests that the QPainter documentation is incomplete, and that
	// all OpenGL calls should be made after the QPainter is constructed, and before the QPainter
	// is destroyed. However, this results in mangled graphics on the Linux (and other?) versions,
	// so here it is done in the 'wrong' order.
	
	// Set the first item to consider - if we are rendering from a specific model (useCurrentModel_ == FALSE) use the local listitem
	if (useCurrentModel_)
	{
		nmodels = aten.nVisibleModels();
		if (nmodels == 0)
		{
			localri.item = aten.currentModel();
			nmodels = 1;
			first = &localri;
		}
		else first = aten.visibleModels();
	}
	else
	{
		// Quick check for NULL pointer
		if (renderSource_ == NULL)
		{
			msg.exit("TCanvas::paintGL");
			return;
		}
		localri.item = renderSource_;
		first = &localri;
		nmodels = 1;
	}
	if (first == NULL)
	{
		msg.exit("TCanvas::paintGL");
		return;
	}
	
	// Begin the GL commands
	if (!beginGl())
	{
		msg.print(Messenger::GL, " --> RENDERING END (BAD BEGIN)\n");
		msg.exit("TCanvas::paintGL");
		return;
	}
	
	// Clear view
	msg.print(Messenger::GL, " --> Clearing context, background, and setting pen colour\n");
	glViewport(0,0,contextWidth_,contextHeight_);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	engine_.forgetTextPrimitives();
	
	// Set up some useful values
	nrows = nmodels/nperrow + (nmodels%nperrow == 0 ? 0 : 1);
	py = contextHeight_ / nrows;
	px = (nmodels == 1 ? contextWidth_ : contextWidth_ / nperrow);
	
	// Loop over model refitems in list (or single refitem)
	col = 0;
	row = 0;
	for (Refitem<Model,int> *ri = first; ri != NULL; ri = ri->next)
	{
		// Grab model pointer
		m = ri->item;
		if (m == NULL) continue;

		// Store coordinates for box if this is the current model
		if ((m == aten.currentModel()) && useCurrentModel_)
		{
			modelIsCurrentModel = TRUE;
			currentBox.setRect(col*px, row*py, px, py);
		}
		else modelIsCurrentModel = FALSE;

		// Vibration frame?
		m = m->renderSourceModel();
		if (m->renderFromVibration() && (m->vibrationCurrentFrame() != NULL)) m = m->vibrationCurrentFrame();

		// If the stored model pixel data is out of date or rerendering has specifically been requested, redraw the model
		usepixels = TRUE;
// 		printf("For Model %s, global noPixelData_ and redrawActive flags are %i and %i, logpoint is %i, and pixel data validity is %i\n", ri->item->name(), noPixelData_, redrawActiveModel_, m->changeLog.log(Log::Total), m->pixelDataIsValid(px,py,m,m->changeLog.log(Log::Total)));
		if (redrawActiveModel_ && (ri->item == aten.currentModel())) usepixels = FALSE;
		else if (noPixelData_) usepixels = FALSE;
		else if (!m->pixelDataIsValid(px,py,m,m->changeLog.log(Log::Total))) usepixels = FALSE;

		if (usepixels)
		{
			// Setup flat projection for pixel rendering
			glViewport(0,0,contextWidth_,contextHeight_);
			glMatrixMode(GL_PROJECTION);
			glLoadIdentity();
			glOrtho(0.0, (double)contextWidth_, 0.0, (double)contextHeight_, -1.0, 1.0);
			glMatrixMode(GL_MODELVIEW);
			glLoadIdentity();
			glRasterPos2i(col*px, contextHeight_-(row+1)*py);
			glDrawPixels(px, py, GL_RGBA, GL_UNSIGNED_BYTE, m->pixelData());
		}
		else
		{
			// Determine desired pixel range and set up view(port)
			checkGlError();
			m->setupView(col*px, contextHeight_-(row+1)*py, px, py);
		
			// Render the 3D parts of the model
			render3D(m, modelIsCurrentModel);
		}

		// Increase counters
		++col;
		if (col%nperrow == 0)
		{
			col = 0;
			++row;
		}
	}
	endGl();
	
	// Start of QPainter code
	static QFont font;
	font.setPointSize(prefs.labelSize());
	QBrush nobrush(Qt::NoBrush);
	QPen pen;
	QPainter painter(this);
	
	// Render text elements for all models (with QPainter)
	painter.setFont(font);
	painter.setRenderHint(QPainter::Antialiasing);
	engine_.renderText(painter, this);

	// Render 2D mode embellishments for current model (with QPainter)
	m = useCurrentModel_ ? aten.currentModel() : renderSource_;
	if (m != NULL)
	{
		m = m->renderFromVibration() ? m->vibrationCurrentFrame() : m->renderSourceModel();
		render2D(painter, m);
	}

	// Draw box around current model
	color.setRgbF(0.0,0.0,0.0,1.0);
	pen.setColor(color);
	pen.setWidth(2);
	painter.setBrush(nobrush);
	painter.setPen(Qt::SolidLine);
	painter.setPen(pen);
	//printf("CurrentBox =  %i %i %i %i\n", currentBox.x(), currentBox.y(), currentBox.width(), currentBox.height());

	if (prefs.frameCurrentModel()) painter.drawRect(currentBox);
	if (prefs.frameWholeView())
	{
		currentBox.setRect(0,0,contextWidth_,contextHeight_);
		painter.drawRect(currentBox);
	}
	painter.end();

	// Store pixel data for models that need it
	col = 0;
	row = 0;
	glReadBuffer(GL_BACK);
	glViewport(0,0,contextWidth_,contextHeight_);
	for (Refitem<Model,int> *ri = first; ri != NULL; ri = ri->next)
	{
		// Grab model pointer
		m = ri->item;
		if (m == NULL) continue;
		
		// Vibration frame?
		m = m->renderSourceModel();
		if (m->renderFromVibration() && (m->vibrationCurrentFrame() != NULL)) m = m->vibrationCurrentFrame();
		
		// If the stored model pixel data is out of date re-copy pixel data
// 		if (!m->pixelDataIsValid(px,py,m,m->changeLog.log(Log::Total)))
// 		{
// 			m->preparePixelData(px,py,m,m->changeLog.log(Log::Total));
// 			glReadPixels(col*px, contextHeight_-(row+1)*py, px, py, GL_RGBA, GL_UNSIGNED_BYTE, m->pixelData());
// 		}
		
		// Increase counters
		++col;
		if (col%nperrow == 0)
		{
			col = 0;
			++row;
		}
	}
	
	// Swap buffers if necessary
	if (prefs.manualSwapBuffers()) swapBuffers();
	swapBuffers();
	
	// Special case when rendering to Pixmap - must delete associated context
	if (drawingTarget_ == TCanvas::PixmapTarget) engine_.popInstance(highQuality_, context());

	msg.exit("TCanvas::paintGL");
}

// Resize function
void TCanvas::resizeGL(int newwidth, int newheight)
{
	// Store the new width and height of the widget
	contextWidth_ = (GLsizei) newwidth;
	contextHeight_ = (GLsizei) newheight;
}

// Begin GL
bool TCanvas::beginGl()
{
	if (!valid_) return FALSE;
	drawing_ = TRUE;
	return TRUE;
}

// Finalize GL commands
void TCanvas::endGl()
{
	drawing_ = FALSE;
}

void TCanvas::checkGlError()
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

// Set current drawing target (none, screen, or pixmap)
void TCanvas::setDrawingTarget(TCanvas::DrawingTarget dt)
{
	drawingTarget_ = dt;
}

// Return current drawing target (none, screen, or pixmap)
TCanvas::DrawingTarget TCanvas::drawingTarget()
{
	return drawingTarget_;
}

// Return whether rendering should use high quality primitives
bool TCanvas::highQuality() const
{
	return highQuality_;
}

// Refresh widget
void TCanvas::postRedisplay(bool noImages, bool redrawActive)
{
	if ((!valid_) || drawing_) return;
	noPixelData_ = noImages;
	redrawActiveModel_ = redrawActive;
	update();
}

// Update view matrix stored in RenderEngine
void TCanvas::updateTransformation(Matrix& mat, Vec3<double> cellcentre)
{
	engine_.setTransformationMatrix(mat, cellcentre);
}

// Render or grab image
QPixmap TCanvas::generateImage(int w, int h, bool highQuality)
{
	// Store current quality value
	bool oldquality = highQuality_;
	highQuality_ = highQuality;
	if (prefs.useFrameBuffer() == FALSE)
	{
		// Set some flags so that the main view is redrawn properly, clearing lists and preventing image use
		noPixelData_ = TRUE;
		engine_.flagClearLists();
		
		// Specify that we are about to render offscreen (so instance can be automatically removed)
		TCanvas::DrawingTarget oldTarget = drawingTarget_;
		drawingTarget_ = TCanvas::PixmapTarget;

		// Generate offscreen bitmap (a temporary context will be created)
		QPixmap pixmap = renderPixmap(w, h, FALSE);
		
		// Return to last rendering mode, and flag for rendering list regeneration again
		drawingTarget_ = oldTarget;
		engine_.flagClearLists();
		
		// Ensure correct widget context size is stored
		contextWidth_ = (GLsizei) width();
		contextHeight_ = (GLsizei) height();
		// Revert to old quality setting
		highQuality_ = oldquality;
		return pixmap;
	}
	else
	{
		drawingTarget_ = TCanvas::PixmapTarget;
		noPixelData_ = TRUE;
		engine_.flagClearLists();
		if (highQuality != oldquality) postRedisplay();
		QImage image = grabFrameBuffer();
		// Revert to old quality setting
		highQuality_ = oldquality;
		return QPixmap::fromImage(image);
	}
}

// Reinitialise primitives
void TCanvas::updatePrimitives(bool force)
{
	engine_.updatePrimitives(context(), force);
}

// Reinitialise transparency correction
void TCanvas::reinitialiseTransparency()
{
	engine_.initialiseTransparency();
}

/*
// Other Qt Virtuals
*/

void TCanvas::focusOutEvent(QFocusEvent *event)
{
	gui.update(GuiQt::StatusBarTarget);
}
