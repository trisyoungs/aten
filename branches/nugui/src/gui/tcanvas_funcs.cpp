/*
	*** Qt canvas functions
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
	displayModel_ = NULL;
	displayFrameId_ = -1;
	useCurrentModel_ = TRUE;
	renderSource_ = NULL;
	// Rendering
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
	return displayModel_;
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
	msg.exit("TCanvas::initializeGL");
}

// General repaint callback
void TCanvas::paintGL()
{


	// Do nothing if the canvas is not valid, or we are still drawing from last time.
	if ((!valid_) || drawing_) return;
	
	// Note: An internet source suggests that the QPainter documentation is incomplete, and that
	// all OpenGL calls should be made after the QPainter is constructed, and before the QPainter
	// is destroyed. However, this results in mangled graphics on the Linux (and other?) versions,
	// so here it is done in the 'wrong' order.
	
	printf("There are %i visible models\n", aten.nVisibleModels());

			// Need to get view z-depth (zoom) from current model

	// TEST - Render single model as usual
	
	if (useCurrentModel_) displayModel_ = aten.currentModelOrFrame();
	else displayModel_ = renderSource_;
			
	// First, setup view for specified pixel range
	if (displayModel_ != NULL)  /* engine_.setupView(0, 0, contextWidth_, contextHeight_, 1.0);*/
	{
		displayModel_->setupView(0, 0, contextWidth_, contextHeight_);
		// Vibration frame?
		if (displayModel_->renderFromVibration()) displayModel_ = displayModel_->vibrationCurrentFrame();
		else displayModel_ = displayModel_->renderSourceModel();
		render3D();
	}
		
	// Finally, swap buffers if necessary
	if (prefs.manualSwapBuffers()) swapBuffers();
}

// Render 3D objects for current displayModel_
void TCanvas::render3D()
{	
	static QFont font;
	
	// Valid pointer set?
	if (displayModel_ == NULL) return;

	// Vibration frame?
	if (displayModel_->renderFromVibration()) displayModel_ = displayModel_->vibrationCurrentFrame();
	else displayModel_ = displayModel_->renderSourceModel();
	
	// Render model
	msg.print(Messenger::GL, " --> RENDERING BEGIN\n");
	
	// If the canvas is still restricted, don't draw anything
	if (noDraw_)
	{
		msg.print(Messenger::GL, " --> RENDERING END (NODRAW)\n");
		return;
	}
	checkGlError();
	
	// Begin the GL commands
	if (!beginGl())
	{
		msg.print(Messenger::GL, " --> RENDERING END (BAD BEGIN)\n");
		return;
	}
	checkGlError();
	
	// Check the supplied model against the previous one rendered to see if we must outdate the display list
// 	if (lastDisplayed_ != displayModel_)
// 	{
// 		// Clear the picked atoms list
// 		pickedAtoms_.clear();
// 	}
	msg.print(Messenger::GL, "Begin rendering pass : source model pointer = %p, renderpoint = %d\n", displayModel_, displayModel_->changeLog.log(Log::Total));
	
	// If this is a trajectory frame, check its ID against the last one rendered
	if (displayModel_->parent() != NULL)
	{
		displayFrameId_ = displayModel_->parent()->trajectoryFrameIndex();
		msg.print(Messenger::GL, " --> Source model is a trajectory frame - index = %i\n", displayFrameId_);
	}
	
	// Render 3D elements (with OpenGL)
	msg.print(Messenger::GL, " --> Preparing lights, shading, aliasing, etc.\n");
	engine_.initialiseGL();
	checkGlError();
	msg.print(Messenger::GL, " --> Clearing context, background, and setting pen colour\n");
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	engine_.render3D(displayModel_, this);
	//glFlush();
	endGl();
	checkGlError();

	// Render 2D elements (with QPainter)
	QPainter painter(this);
	font.setPointSize(prefs.labelSize());
	painter.setFont(font);
	painter.setRenderHint(QPainter::Antialiasing);
	engine_.renderText(painter, this);
	render2D(painter);
	painter.end();

	msg.print(Messenger::GL, " --> RENDERING END\n");
}

// Resize function
void TCanvas::resizeGL(int newwidth, int newheight)
{
	// Store the new width and height of the widget and re-do projection
	contextWidth_ = (GLsizei) newwidth;
	contextHeight_ = (GLsizei) newheight;
	doProjection(contextWidth_, contextHeight_);
	// Flag that render source needs to be reprojected
	if (displayModel_ != NULL) displayModel_->changeLog.add(Log::Visual);
	if (prefs.manualSwapBuffers()) swapBuffers();
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
}

// Return whether offscreen rendering is being performed
bool TCanvas::offScreenRendering() const
{
	return renderOffScreen_;
}

// Refresh widget
void TCanvas::postRedisplay()
{
	if ((!valid_) || drawing_) return;
	updateGL();
// 	if (prefs.manualSwapBuffers()) swapBuffers();
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

// Calculate Projection
void TCanvas::doProjection(int newwidth, int newheight)
{
	// (Re)Create the projection and viewport matrix from the current geometry of the rendering widget / pixmap
	if (!valid_) return;
	msg.enter("Canvas::doProjection");
	// Check source
	if (beginGl())
	{
		printf("Doing projection\n");
		// Set the viewport size to the whole area and grab the matrix
		contextWidth_ = (GLsizei) (newwidth == -1 ? width() : newwidth);
		contextHeight_ = (GLsizei) (newheight == -1 ? height() : newheight);
// 		// Need to get view z-depth (zoom) from current model
// 		Model *source;
// 		if (useCurrentModel_) source = aten.currentModelOrFrame();
// 		else source = renderSource_;
// 		if (source != NULL) source->setupView(0, 0, contextWidth_, contextHeight_);
// 		else
// 		{
// 			// Vibration frame?
// 			if (source->renderFromVibration()) source = source->vibrationCurrentFrame();
// 			else source = source->renderSourceModel();
// 			engine_.setupView(0, 0, contextWidth_, contextHeight_, source->modelViewMatrix()[14] );
// 		}
		endGl();
	}
	else printf("Canvas::doProjection <<<< Failed to reset projection matrix >>>>\n");
	msg.exit("Canvas::doProjection");
}

/*
// Other Qt Virtuals
*/
void TCanvas::focusOutEvent(QFocusEvent *event)
{
	gui.updateStatusBar(TRUE);
}

void TCanvas::timerEvent(QTimerEvent *event)
{
	// Move on to the next frame in the trajectory
	// Check that we're not still drawing the last frame from the last timerEvent
	if (DONTDRAW) printf("Still drawing previous frame.\n");
	else
	{
		DONTDRAW = TRUE;
		Model *m = aten.currentModel();
		m->seekNextTrajectoryFrame();
		if (m->trajectoryFrameIndex() == m->nTrajectoryFrames()-1) gui.stopTrajectoryPlayback();
		gui.update();
		DONTDRAW = FALSE;
	}
}
