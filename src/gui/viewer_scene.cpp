/*
	*** Viewer - Main scene render function
	*** src/gui/viewer_scene.cpp
	Copyright T. Youngs 2007-2016

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

#include "gui/viewer.hui"
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "base/sysfunc.h"
#include <QPen>
#include <QPainter>

// Setup OpenGL ready for drawing
void Viewer::setupGL()
{
	Messenger::enter("Viewer::setupGL");

	// Perspective hint
	glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_FASTEST);

	// Enable depth buffer
	glEnable(GL_DEPTH_TEST);

	// Smooth shading
	glShadeModel(GL_SMOOTH);

	// Auto-calculate surface normals
	glEnable(GL_NORMALIZE);

	// Set alpha-blending function
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

	// Set up the light model
	GLfloat col[4];
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

	Messenger::exit("Viewer::setupGL");
}

// Render messages
void Viewer::renderMessages(QPainter& painter, bool fade)
{
	// Set font for messages
	painter.setFont(prefs.messagesFont());

	// Grab message buffer
	QList<Message>& messages = Messenger::messageBuffer();
	int margin = 4;
	QColor textColour;
	QRectF textRect(margin, margin, contextWidth_-2*margin, contextHeight_-2*margin), actualRect;
	for (int n=atenWindow_->messagesScrollPosition(); n<messages.count(); ++n)
	{
		// Set brush colour to correspond to message type
		if (fade)
		{
			textColour = messages.at(n).colour();
			textColour.setAlphaF(0.5);
			painter.setPen(textColour);
		}
		else painter.setPen(messages.at(n).colour());

		// Set initial textRect
		textRect.setWidth(contextWidth_-2*margin);
		textRect.setHeight(contextHeight_-2*margin);
		actualRect = QRectF();
		painter.drawText(textRect, Qt::AlignBottom | Qt::TextWordWrap, messages.at(n).text(), &actualRect);

		// Translate bounding rectangle upwarsd and check to make sure we are still on-screen
		textRect.translate(0.0, -actualRect.height());
		if (textRect.bottom() < margin) break;
	}
}

// Render full scene
void Viewer::renderFullScene(int contextWidth, int contextHeight, int xOffset, int yOffset)
{
	Messenger::enter("Viewer::renderFullScene");
	QColor color;
	QRect currentBox;
	int nPerRow = prefs.nModelsPerRow(), nRows, col, row, nModels;
	int viewPortX, viewPortY, viewPortWidth, viewPortHeight;
	double viewPortAspect;
	Model* m;

	// Grab context dimensions if they were not provided
	if (contextWidth < 0) contextWidth = contextWidth_;
	if (contextHeight < 0) contextHeight = contextHeight_;

	// Restore all GL state variables, and setup GL
	glPopAttrib();
	setupGL();

	// Set colour mode
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);
	glEnable(GL_TEXTURE_2D);
	glClear(GL_DEPTH_BUFFER_BIT);

	// Set up some useful values
	nModels = aten_->nVisibleModels();
	nRows = nModels / nPerRow + (nModels %nPerRow == 0 ? 0 : 1);
	viewPortWidth = ( nModels == 1 ? contextWidth : contextWidth / nPerRow);
	viewPortHeight = contextHeight / nRows;
	viewPortAspect = double(viewPortWidth) / double(viewPortHeight);

	// Loop over model refitems in list (or single refitem)
	col = 0;
	row = 0;
	for (RefListItem<Model,int>* ri = aten_->visibleModels(); ri != NULL; ri = ri->next)
	{
		// Grab model pointer
		m = ri->item;
		if (m == NULL) continue;

		// Set viewport for this modsl
		viewPortX = col*viewPortWidth;
		viewPortY = contextHeight-(row+1)*viewPortHeight;

		// Draw on halo for current model
		if ((!renderingOffScreen_) && (m == aten_->currentModel()) && (nModels > 1))
		{
			glViewport(viewPortX, viewPortY, viewPortWidth, viewPortHeight);
			glMatrixMode(GL_PROJECTION);
			glLoadIdentity();
			glOrtho(-2.0*viewPortAspect, 2.0*viewPortAspect, -2.0, 2.0, -1.0, 1.0);
			glMatrixMode(GL_MODELVIEW);
			glLoadIdentity();
			glDisable(GL_DEPTH_TEST);
			glDisable(GL_LIGHTING);
			glColor4f(0.0f, 0.0f, 0.0f, 0.1f);
			primitives_[primitiveSet_].halo().sendToGL(QOpenGLContext::currentContext());
			glEnable(GL_LIGHTING);
			glEnable(GL_DEPTH_TEST);
		}

		// Reset local renderGroup_
		renderGroup_.clear();

		// Render the whole model
		renderModel(m, viewPortX+xOffset, viewPortY+yOffset, viewPortWidth, viewPortHeight, !renderingOffScreen_);

		// Render additional data for active model
		if (m == aten_->currentModel())
		{
			renderUserActions(m);
		}

		// Render extras from specific windows / popups
		renderExtras(m);

		// Send the local renderGroup
		renderGroup_.sendToGL(modelTransformationMatrix_);

		// Render active mode extras (in 2D) for current model
		if (m == aten_->currentModel())
		{
			glClear(GL_DEPTH_BUFFER_BIT);
			glViewport(viewPortX, viewPortY, viewPortWidth, viewPortHeight);
			glMatrixMode(GL_PROJECTION);
			glLoadIdentity();
			glOrtho(0, viewPortWidth, 0, viewPortHeight, -1.0, 1.0);
			glMatrixMode(GL_MODELVIEW);
			glLoadIdentity();
			glLineWidth(1.0);
			
			renderActiveModes(m);
		}

		// Increase counters
		++col;
		if (col%nPerRow == 0)
		{
			col = 0;
			++row;
		}
	}

	Messenger::exit("Viewer::renderFullScene");
}

// Initialise context widget (when created by Qt)
void Viewer::initializeGL()
{
	Messenger::enter("Viewer::initializeGL");

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

        // Check for vertex buffer extensions
        if ((!hasOpenGLFeature(QOpenGLFunctions::Buffers)) && (PrimitiveInstance::instanceType() == PrimitiveInstance::VBOInstance))
        {
                Messenger::warn("VBO extension is requested but not available, so reverting to display lists instead.\n");
                PrimitiveInstance::setInstanceType(PrimitiveInstance::ListInstance);
        }

	// Make sure low-quality primitives are up-to-date
	updatePrimitives(Viewer::LowQuality);

	valid_ = true;

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

	// Create and setup a QPainter
	QPainter painter(this);

	// Store all GL state variables, since they will be modified by QPainter
	glPushAttrib(GL_ALL_ATTRIB_BITS);

	// Setup GL and clear view
	GLfloat col[4];
	prefs.copyColour(Prefs::BackgroundColour, col);
	glClearColor(col[0], col[1], col[2], col[3]);

	glViewport(0, 0, contextWidth_, contextHeight_);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// Store height of current font (so the scrollbar can be set correctly)
	fontPixelHeight_ = painter.fontMetrics().height();

	switch (atenWindow_->messageDisplay())
	{
		case (AtenWindow::FullMessages):
			// Messages
			renderMessages(painter);
			break;
		case (AtenWindow::MessagesOverScene):
			// Scene
			painter.beginNativePainting();
			renderFullScene();
			painter.endNativePainting();
			painter.end();
			// Messages - Need a fresh QPainter here...
			painter.begin(this);
			painter.setBrush(QColor(255,255,255,128));
			painter.setPen(QPen(Qt::NoPen));
			painter.drawRect(0, 0, contextWidth_, contextHeight_);
			renderMessages(painter);
			break;
		case (AtenWindow::MessagesUnderScene):
			// Messages
			renderMessages(painter, true);
			// Scene
			painter.beginNativePainting();
			renderFullScene();
			painter.endNativePainting();
			break;
		case (AtenWindow::NoMessages):
			// Scene
			painter.beginNativePainting();
			renderFullScene();
			painter.endNativePainting();
			break;
	}

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

// Update all primitives (following prefs change, etc.)
void Viewer::updatePrimitives(Viewer::PrimitiveQuality targetQuality)
{
	Messenger::enter("Viewer::updatePrimitives");

	// Set (possibly new) quality
	int quality = (targetQuality == Viewer::LowQuality ? prefs.primitiveQuality() : prefs.imagePrimitiveQuality());
	primitives_[targetQuality].setQuality(quality);

	// Recalculate adjustments in PrimitiveSets
	primitives_[targetQuality].calculateAdjustments();

	// Recreate basic rendering objects
	primitives_[targetQuality].recreatePrimitives();

	// Pop and push a context
	if (primitives_[targetQuality].nInstances() != 0) primitives_[targetQuality].popInstance(context());
	primitives_[targetQuality].pushInstance(context());

	Messenger::exit("Viewer::updatePrimitives");
}
