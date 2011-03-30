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
TCanvas::TCanvas(QGLContext *context, QWidget *parent) : QGLWidget(context, parent)
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
	noPixelData_ = FALSE;
	drawing_ = FALSE;
	noDraw_ = TRUE;
	renderOffScreen_ = FALSE;
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

// Return if the canvas is valid
bool TCanvas::isValid() const
{
	return valid_;
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
	if (source != NULL) source = (source->renderFromVibration() ? source->vibrationCurrentFrame() : source);
	return source;
}

// Set the rendering source to the supplied model (reverts to useCurrentModel_ if a NULL pointer is supplied)
void TCanvas::setRenderSource(Model *source)
{
	if (source == NULL)
	{
		useCurrentModel_ = TRUE;
		renderSource_ = NULL ;
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
	// Image quality to use depends on current drawing target...
	if (renderOffScreen_ && (!prefs.reusePrimitiveQuality())) engine_.createPrimitives(prefs.imagePrimitiveQuality());
	else engine_.createPrimitives(prefs.primitiveQuality());
	engine_.initialiseGL();
	msg.exit("TCanvas::initializeGL");
}

// General repaint callback
void TCanvas::paintGL()
{
	static QFont font;
	static QBrush solidbrush(Qt::NoBrush);
	QPen pen;
	QColor color;
	QRect currentBox;
	Refitem<Model,int> *first, localri;
	int px, py, nperrow = prefs.nModelsPerRow(), nrows, col, row, nmodels;
	bool usepixels, modelIsCurrentModel;
	Model *m;

	// Do nothing if the canvas is not valid, or we are still drawing from last time.
	if ((!valid_) || drawing_ || (aten.currentModel() == NULL)) return;
	
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
		if (renderSource_ == NULL) return;
		localri.item = renderSource_;
		first = &localri;
		nmodels = 1;
	}
	if (first == NULL) return;
	
	// Begin the GL commands
	if (!beginGl())
	{
		msg.print(Messenger::GL, " --> RENDERING END (BAD BEGIN)\n");
		return;
	}
	
	// Clear view
	msg.print(Messenger::GL, " --> Clearing context, background, and setting pen colour\n");
	glViewport(0,0,contextWidth_,contextHeight_);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	// Set up some useful values
	nrows = nmodels/nperrow + (nmodels%nperrow == 0 ? 0 : 1);
	py = contextHeight_ / nrows;
	px = (nmodels == 1 ? contextWidth_ : contextWidth_ / nperrow);
	
	// Clear text lists in renderengine before we start the model loop
	engine_.clearTextLists();

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
		if (m->renderFromVibration()) m = m->vibrationCurrentFrame();
		else m = m->renderSourceModel();

		// If the stored model pixel data is out of date or rerendering has specifically been requested, redraw the model
		usepixels = TRUE;
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
		
			// Clear triangle lists and render the 3D parts of the model
			engine_.clearTriangleLists();
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
	
	// Render text elements for all models (with QPainter)
	QPainter painter(this);
	font.setPointSize(prefs.labelSize());
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
		if (m->renderFromVibration()) m = m->vibrationCurrentFrame();
		else m = m->renderSourceModel();
		
		// If the stored model pixel data is out of date re-copy pixel data
		if (!m->pixelDataIsValid(px,py,m,m->changeLog.log(Log::Total)))
		{
			m->preparePixelData(px,py,m,m->changeLog.log(Log::Total));
			glReadPixels(col*px, contextHeight_-(row+1)*py, px, py, GL_RGBA, GL_UNSIGNED_BYTE, m->pixelData());
		}

		// Increase counters
		++col;
		if (col%nperrow == 0)
		{
			col = 0;
			++row;
		}
	}

	// Draw box around current model
	color.setRgbF(0.0,0.0,0.0,1.0);
	pen.setColor(color);
	pen.setWidth(2);
	painter.setBrush(solidbrush);
	painter.setPen(Qt::SolidLine);
	painter.setPen(pen);
	if (prefs.frameCurrentModel()) painter.drawRect(currentBox);
	if (prefs.frameWholeView())
	{
		currentBox.setRect(0,0,contextWidth_,contextHeight_);
		painter.drawRect(currentBox);
	}
	painter.end();
	
	// Finally, swap buffers if necessary
	if (prefs.manualSwapBuffers()) swapBuffers();
}

// Render 3D objects for current displayModel_
void TCanvas::render3D(Model *source, bool currentModel)
{	
	// Valid pointer set?
	if (source == NULL) return;

	// Render model
	msg.print(Messenger::GL, " --> RENDERING BEGIN\n");
	
	// If the canvas is still restricted, don't draw anything
	if (noDraw_)
	{
		msg.print(Messenger::GL, " --> RENDERING END (NODRAW)\n");
		return;
	}
	checkGlError();

	// Check the supplied model against the previous one rendered to see if we must outdate the display list
	// 	if (lastDisplayed_ != source)
// 	{
// 		// Clear the picked atoms list
// 		pickedAtoms_.clear();
// 	}
	msg.print(Messenger::GL, "Begin rendering pass : source model pointer = %p, renderpoint = %d\n", source, source->changeLog.log(Log::Total));
	
	// If this is a trajectory frame, check its ID against the last one rendered
	if (source->parent() != NULL)
	{
		displayFrameId_ = source->parent()->trajectoryFrameIndex();
		msg.print(Messenger::GL, " --> Source model is a trajectory frame - index = %i\n", displayFrameId_);
	}
	
	// Render 3D elements (with OpenGL)
	msg.print(Messenger::GL, " --> Preparing lights, shading, aliasing, etc.\n");
	checkGlError();
	engine_.render3D(source, this, currentModel);
	//glFlush();
	checkGlError();

	msg.print(Messenger::GL, " --> RENDERING END\n");
}

// Resize function
void TCanvas::resizeGL(int newwidth, int newheight)
{
	// Store the new width and height of the widget and re-do projection
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

// Enable drawing
void TCanvas::enableDrawing()
{
	noDraw_ = FALSE;
}

// Disable drawing
void TCanvas::disableDrawing()
{
	noDraw_ = TRUE;
}

// Set whether offscreen rendering is being performed
void TCanvas::setOffScreenRendering(bool b)
{
	renderOffScreen_ = b;
	// Make sure here that the current (correct) context width and height are stored
	if (!b) 
	{
		contextWidth_ = (GLsizei) gui.mainWidget->width();
		contextHeight_ = (GLsizei) gui.mainWidget->height();
	}
}

// Return whether offscreen rendering is being performed
bool TCanvas::offScreenRendering() const
{
	return renderOffScreen_;
}

// Refresh widget
void TCanvas::postRedisplay(bool noImages, bool redrawActive)
{
	if ((!valid_) || drawing_) return;
	noPixelData_ = noImages;
	redrawActiveModel_ = redrawActive;
	updateGL();
}

// Update view matrix stored in RenderEngine
void TCanvas::updateTransformation(Matrix& mat, Vec3< double > cellcentre)
{
	engine_.setTransformationMatrix(mat, cellcentre);
}

// Reinitialise primitives
void TCanvas::reinitialisePrimitives(bool force)
{
	// Image quality to use depends on current drawing target...
	if (renderOffScreen_ && (!prefs.reusePrimitiveQuality())) engine_.createPrimitives(prefs.imagePrimitiveQuality(), force);
	else engine_.createPrimitives(prefs.primitiveQuality(), force);
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
