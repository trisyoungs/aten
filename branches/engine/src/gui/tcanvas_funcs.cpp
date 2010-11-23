/*
	*** Qt canvas functions
	*** src/gui/tcanvas_funcs.cpp
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
	// Atom Selection
	atomClicked_ = NULL;
	pickEnabled_ = FALSE;
	actionBeforePick_ = NULL;
	pickAtomsCallback_ = NULL;
	nAtomsToPick_ = -1;
	// Mouse Input
	for (int i=0; i<3; i++) mouseButton_[i] = FALSE;
	// Key Input
	for (int i=0; i<3; i++) keyModifier_[i] = FALSE;
	// User Actions
	activeMode_ = UserAction::NoAction;
	selectedMode_ = UserAction::SelectAction;
	currentDrawDepth_ = 0.0;
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
	engine_.createPrimitives();
	msg.exit("TCanvas::initializeGL");
}

// General repaint callback
void TCanvas::paintGL()
{
	static Model *lastDisplayed_ = NULL;
	
	// Note: An internet source suggests that the QPainter documentation is incomplete, and that
	// all OpenGL calls should be made after the QPainter is constructed, and befor the QPainter
	// is destroyed. However, this results in mangled graphics on the Linux (and other?) versions,
	// so here it is done in the 'wrong' order.
	
	if (useCurrentModel_) displayModel_ = aten.currentModelOrFrame();
	else displayModel_ = renderSource_;
	
	if (displayModel_ != NULL)
	{
		// Vibration frame?
		if (displayModel_->renderFromVibration()) displayModel_ = displayModel_->vibrationCurrentFrame();
		else displayModel_ = displayModel_->renderSourceModel();
		
		// Render model
		msg.print(Messenger::GL, " --> RENDERING BEGIN\n");
		
		// If the canvas is stil restricted, don't draw anything
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
		if ((lastDisplayed_ != displayModel_) || (displayModel_ == NULL)) renderPoint_.reset();
		msg.print(Messenger::GL, "Begin rendering pass : source model pointer = %p, renderpoint = %d\n", displayModel_, renderPoint_.log(Log::Total));
		
		// If this is a trajectory frame, check its ID against the last one rendered
		if (displayModel_->parent() != NULL)
		{
			if (displayModel_->parent()->trajectoryFrameIndex() != displayFrameId_) renderPoint_.reset();
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
		render2D();
		
		msg.print(Messenger::GL, " --> RENDERING END\n");
		lastDisplayed_ = displayModel_;
	}
	else
	{
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		printf("TCanvas has no model to render.\n");
	}
	if (prefs.manualSwapBuffers()) swapBuffers();
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
	// 	if (canvas_->displayModel() != NULL) canvas_->displayModel()->changeLog.add(Log::Camera);  //BROKEN?
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
	if (!valid_) return;
	updateGL();
	if (prefs.manualSwapBuffers()) swapBuffers();
}

// Update view matrix stored in RenderEngine
void TCanvas::updateTransformation(Mat4<double> &mat, Vec3<double> cellcentre)
{
	engine_.setTransformationMatrix(mat, cellcentre);
}

// Reinitialise GL (because of change in setup, lighting etc.)
void TCanvas::reinitialiseGL()
{
	engine_.initialiseGL();
	engine_.createPrimitives();
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
		// Set the viewport size to the whole area and grab the matrix
		contextWidth_ = (GLsizei) (newwidth == -1 ? width() : newwidth);
		contextHeight_ = (GLsizei) (newheight == -1 ? height() : newheight);
		engine_.setupView(0, 0, contextWidth_, contextHeight_);
		// Rotation globe projection matrix (square)
		/*		glLoadIdentity();
		glFrustum(-1.0, 1.0, -1.0, 1.0, 0.0, 10.0);
		glGetDoublev(GL_PROJECTION_MATRIX,pmat);
		GlobePMAT.setFromColumnMajor(pmat);*/    // TGAY  BROKEN
// 		glMatrixMode(GL_MODELVIEW);
		endGl();
	}
	else printf("Canvas::doProjection <<<< Failed to reset projection matrix >>>>\n");
	msg.exit("Canvas::doProjection");
}

// Project given model coordinates into world coordinates (and screen coordinates if Vec3 is supplied)
Vec3<double> &TCanvas::modelToWorld(Vec3<double> pos, Vec4< double >* screenr, double screenradius)
{
	return engine_.modelToWorld(pos, screenr, screenradius);
}

// Project the specified world coordinates into 2D screen coords
Vec4<double> &TCanvas::worldToScreen(const Vec3<double> &v)
{
	return engine_.worldToScreen(v);
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
		gui.update(FALSE,FALSE,FALSE);
		DONTDRAW = FALSE;
	}
}
