/*
	*** Viewer Functions
	*** src/gui/viewer_funcs.cpp
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

#include "gui/mainwindow.h"
#include "main/aten.h"
#include <QPainter>
#include <QOpenGLContext>
#include <QPixmap>

ATEN_USING_NAMESPACE

// Constructor
Viewer::Viewer(QWidget* parent) : QOpenGLWidget(parent)
{
	Messenger::enter("Viewer::Viewer()");

	// Character / Setup
	atenWindow_ = NULL;
	contextWidth_ = 0;
	contextHeight_ = 0;

	// Rendering
	valid_ = false;
	drawing_ = false;
	renderingOffScreen_ = false;
	primitiveSet_ = Viewer::LowQuality;
	fontPixelHeight_ = 1;

	// Atom Selection
	atomClicked_ = NULL;
	pickEnabled_ = false;
	actionBeforePick_ = UserAction::NoAction;
	pickAtomsCallback_ = NULL;
	nAtomsToPick_ = -1;

	// Mouse Input
	for (int i=0; i<3; i++) mouseButton_[i] = false;

	// Key Input
	for (int i=0; i<3; i++) keyModifier_[i] = false;

	// User Actions
	activeMode_ = UserAction::NoAction;
	selectedMode_ = UserAction::SelectAction;
	currentDrawDepth_ = -5.0;
	buildElement_ = 6;
	buildGeometry_ = Atom::TetrahedralGeometry;
	editable_ = true;

	// Prevent QPainter from autofilling widget background
	setAutoFillBackground(false);

	Messenger::exit("Viewer::Viewer()");
}

// Destructor
Viewer::~Viewer()
{
}

/*
 * Character / Setup
 */

// Initialise context widget (when created by Qt)
void Viewer::initializeGL()
{
	Messenger::enter("Viewer::initializeGL");
	
        valid_ = true;

	// Setup function pointers to OpenGL extension functions
	initializeOpenGLFunctions();

	// Setup offscreen context
	Messenger::print(Messenger::Verbose, "Setting up offscreen context and surface...");
        offscreenContext_.setShareContext(context());
        offscreenContext_.setFormat(context()->format());
        offscreenContext_.create();
        offscreenSurface_.setFormat(context()->format());
	offscreenSurface_.create();
	Messenger::print(Messenger::Verbose, "Done.");

	// Make sure primitives are up-to-date
	updatePrimitives(Viewer::LowQuality);
	updatePrimitives(Viewer::HighQuality);

        // Check for vertex buffer extensions
        if ((!hasOpenGLFeature(QOpenGLFunctions::Buffers)) && (PrimitiveInstance::globalInstanceType() == PrimitiveInstance::VBOInstance))
        {
		// ATEN2 TODO If this is called for an offscreen render, and VBOs are not available (but *are* in the GUI), won't this break rendering?
                printf("VBO extension is requested but not available, so reverting to display lists instead.\n");
                PrimitiveInstance::setGlobalInstanceType(PrimitiveInstance::ListInstance);
        }

	Messenger::exit("Viewer::initializeGL");
}

// void Viewer::paintEvent(QPaintEvent* event)
void Viewer::paintGL()
{
	// Do nothing if the canvas is not valid, or we are still drawing from last time, or the Aten pointer has not been set
	if ((!valid_) || drawing_ || (!atenWindow_)) return;

	Messenger::enter("Viewer::paintGL");

	// Set the drawing flag so we don't have any rendering clashes
	drawing_ = true;

	atenWindow_->updateMessagesWidgets();

	// Create a QPainter
	QPainter painter(this);
	
	// Setup GL and clear view
	GLfloat col[4];
	prefs.copyColour(Prefs::BackgroundColour, col);
	glClearColor(col[0], col[1], col[2], col[3]);

	glViewport(0, 0, contextWidth_, contextHeight_);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// Store all GL state variables, since they will be modified by QPainter
	glPushAttrib(GL_ALL_ATTRIB_BITS);

	// Store height of current font (so the scrollbar can be set correctly)
	fontPixelHeight_ = painter.fontMetrics().height();

	// Grab message buffer
	QStringList& messages = Messenger::messageBuffer();
	int margin = 4;
	QRectF textRect(margin, margin, contextWidth_-2*margin, contextHeight_-2*margin), actualRect;
	for (int n=atenWindow_->messagesScrollPosition(); n<messages.count(); ++n)
	{
		textRect.setWidth(contextWidth_-2*margin);
		textRect.setHeight(contextHeight_-2*margin);
		actualRect = QRectF();
		painter.drawText(textRect, Qt::AlignBottom | Qt::TextWordWrap, messages.at(n), &actualRect);

		// Translate bounding rectangle upwarsd and check to make sure we are still on-screen
		textRect.translate(0.0, -actualRect.height());
		if (textRect.bottom() < margin) break;
	}

	painter.beginNativePainting();

	// Restore all GL state variables, and setup GL
	glPopAttrib();
	setupGL();

	// Set colour mode
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);
	glEnable(GL_TEXTURE_2D);
	glClear(GL_DEPTH_BUFFER_BIT);

	// Render all models
	renderModels();

	painter.endNativePainting();
	
	// Done!
	painter.end();

	// Set the rendering flag to false
	drawing_ = false;

	// Always revert to lower quality for next pass
	primitiveSet_ = Viewer::LowQuality;

	Messenger::exit("Viewer::paintGL");
}

// Resize function
void Viewer::resizeGL(int newwidth, int newheight)
{
	// Store the new width and height of the widget
	contextWidth_ = (GLsizei) newwidth;
	contextHeight_ = (GLsizei) newheight;
}

void Viewer::focusOutEvent(QFocusEvent* event)
{
	atenWindow_->updateWidgets(AtenWindow::StatusBarTarget);
}

// Set pointer to Aten's main structure
void Viewer::setAten(Aten* aten)
{
	aten_ = aten;
}


// Set pointer to AtenWindow
void Viewer::setAtenWindow(AtenWindow* atenWindow)
{
	atenWindow_ = atenWindow;
}

// Return the current height of the drawing area
GLsizei Viewer::contextHeight() const
{
	return contextHeight_;
}

// Return the current width of the drawing area
GLsizei Viewer::contextWidth() const
{
	return contextWidth_;
}

// Probe features
void Viewer::probeFeatures()
{
	// Probe this format!
	QSurfaceFormat format = context()->format();
	Messenger::print(Messenger::Verbose, "QGLFormat: Alpha buffer is %s.", format.hasAlpha() ? "enabled" : "disabled");
	// ATEN2 TODO
// 	Messenger::print(Messenger::Verbose, "QGLFormat: Accumulation buffer is %s.", format.a() ? "enabled" : "disabled");
// 	Messenger::print(Messenger::Verbose, "QGLFormat: Depth buffer is %s.", format.depth() ? "enabled" : "disabled");
// 	Messenger::print(Messenger::Verbose, "QGLFormat: Double-buffering is %s.", format.doubleBuffer() ? "enabled" : "disabled");
// 	Messenger::print(Messenger::Verbose, "QGLFormat: Direct rendering is %s.", format.directRendering() ? "enabled" : "disabled");
// 	Messenger::print(Messenger::Verbose, "QGLFormat: RGBA colour mode is %s.", format.rgba() ? "enabled" : "disabled");
// 	Messenger::print(Messenger::Verbose, "QGLFormat: Multisample buffer is %s.", format.sampleBuffers() ? "enabled" : "disabled");
// 	Messenger::print(Messenger::Verbose, "QGLFormat: Stencil buffer is %s.", format.stencil() ? "enabled" : "disabled");
// 	Messenger::print(Messenger::Verbose, "QGLWidget: Autoswap buffers is %s.", autoBufferSwap() ? "enabled" : "disabled");
}

// Check for GL error
void Viewer::checkGlError()
{
	GLenum glerr = GL_NO_ERROR;
	do
	{
		switch (glGetError())
		{
			case (GL_INVALID_ENUM): Messenger::print(Messenger::Verbose, "GLenum argument out of range"); break;
			case (GL_INVALID_VALUE): Messenger::print(Messenger::Verbose, "Numeric argument out of range"); break;
			case (GL_INVALID_OPERATION): Messenger::print(Messenger::Verbose, "Operation illegal in current state"); break;
			case (GL_STACK_OVERFLOW): Messenger::print(Messenger::Verbose, "Command would cause a stack overflow"); break;
			case (GL_STACK_UNDERFLOW): Messenger::print(Messenger::Verbose, "Command would cause a stack underflow"); break;
			case (GL_OUT_OF_MEMORY): Messenger::print(Messenger::Verbose, "Not enough memory left to execute command"); break;
			case (GL_NO_ERROR): Messenger::print(Messenger::Verbose, "No GL error"); break;
			default:
				Messenger::print(Messenger::Verbose, "Unknown GL error?");
				break;
		}
	} while (glerr != GL_NO_ERROR);
}

// Set whether we are currently rendering offscreen
void Viewer::setRenderingOffScreen(bool b)
{
	renderingOffScreen_ = b;
}

// Set line width and text scaling to use
void Viewer::setObjectScaling(double scaling)
{
	lineWidthScaling_ = scaling;

	// Pass this value on to those that depend on it
// 	LineStyle::setLineWidthScale(scaling);
	TextPrimitive::setTextSizeScale(scaling);
}

// Determine target model based on clicked position on Viewer
Model* Viewer::modelAt(int x, int y)
{
	int nrows, py, px, id;
	
	// Is only one model displayed?
	if (aten_->nVisibleModels() <= 1) return aten_->currentModel();

	// Determine whether we need to change Aten's currentmodel based on click position on the canvas
	nrows = aten_->nVisibleModels()/prefs.nModelsPerRow() + (aten_->nVisibleModels()%prefs.nModelsPerRow() == 0 ? 0 : 1);
	py = contextHeight_ / nrows;
	px = (aten_->nVisibleModels() == 1 ? contextWidth_ : contextWidth_ / prefs.nModelsPerRow());

	// Work out model index...
	id = (y/py)*prefs.nModelsPerRow() + x/px;

	// In the case of clicking in a blank part of the canvas with no model (i.e. bottom-right corner) return a safe model pointer
	return (id >= aten_->nVisibleModels() ? aten_->currentModel() : aten_->visibleModel(id));
}

// Request a high-quality rendering pass on next redraw (for image saving, etc.)
void Viewer::requestHighQuality()
{
	primitiveSet_ = Viewer::HighQuality;
}

// Return height, in pixels, of single line of text
int Viewer::fontPixelHeight()
{
	return fontPixelHeight_;
}
