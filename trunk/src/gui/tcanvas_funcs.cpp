/*
	*** TCanvas Functions
	*** src/gui/tcanvas_funcs.cpp
	Copyright T. Youngs 2007-2015

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

	// Rendering
	drawing_ = FALSE;
	mouseMoveCounter_.start();
	primitiveSet_ = RenderEngine::LowQuality;
	renderType_ = RenderEngine::OnscreenScene;
	renderIconSource_ = NULL;

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

// Request a high-quality rendering pass on next redraw (for image saving, etc.)
void TCanvas::requestHighQuality()
{
	primitiveSet_ = RenderEngine::HighQuality;
}

// Set renderType to pass to RenderEngine::renderScene()
void TCanvas::setRenderType(RenderEngine::RenderType type, Model *iconSource)
{
	renderType_ = type;
	renderIconSource_ = iconSource;
}

/*
// Rendering Functions
*/

// Initialise context widget (when created by Qt)
void TCanvas::initializeGL()
{
	// Initialize GL
	msg.enter("TCanvas::initializeGL");
	
	// Push primitive instance for the TCanvas *if* we are not using pixelbuffers
	if (!prefs.usePixelBuffers())
	{
		engine().pushInstance(primitiveSet_, context());
// 		engine().pushInstance(RenderEngine::HighQuality, context());
	}

	msg.exit("TCanvas::initializeGL");
}

void TCanvas::paintGL()
{
	renderScene(contextWidth_, contextHeight_);
}

// General repaint callback
void TCanvas::paintEvent(QPaintEvent *event)
{
	// Do nothing if the canvas is not valid, or we are still drawing from last time.
	renderScene(contextWidth_, contextHeight_);
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
	drawing_ = TRUE;
	return TRUE;
}

// Finalize GL commands
void TCanvas::endGl()
{
	drawing_ = FALSE;
}

// Refresh widget
void TCanvas::postRedisplay()
{
	if (drawing_) return;
	update();
}

/*
// Other Qt Virtuals
*/

void TCanvas::focusOutEvent(QFocusEvent *event)
{
	gui.update(GuiQt::StatusBarTarget);
}
