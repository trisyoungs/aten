/*
	*** Viewer - Offscreen rendering
	*** src/gui/viewer_offscreen.cpp
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
#include <QOpenGLFramebufferObject>
#include <QPainter>
#include <QtWidgets/QProgressDialog>

// Grab current contents of framebuffer
QPixmap Viewer::frameBuffer()
{
        return QPixmap::fromImage(grabFramebuffer());
}

// Generate image for specified model
QPixmap Viewer::generateModelImage(Model* model, int width, int height)
{
	Messenger::enter("Viewer::generateModelImage");

	// Check validity of model and context
	if ((model == NULL) || (!valid_))
	{
		Messenger::exit("Viewer::generateModelImage");
		return QPixmap();
	}

	// Make the offscreen surface the current context
	offscreenContext_.makeCurrent(&offscreenSurface_);

	// Set icon size
	int iconWidth = width;
	int iconHeight = width;

	// Initialise framebuffer format and object
	QOpenGLFramebufferObjectFormat fboFormat;
// 	fboFormat.setMipmap(true);
// 	fboFormat.setSamples(4);
	QOpenGLFramebufferObject frameBufferObject(iconWidth, iconHeight, fboFormat);

	if (!frameBufferObject.bind())
	{
		Messenger::print("Failed to bind framebuffer object when generating image for single model.");
		Messenger::exit("Viewer::generateModelImage");
		return QPixmap();
	}

	// Set viewport
	glViewport(0, 0, iconWidth, iconHeight);

	// Grab clear colour and clear view / depth buffer
	GLfloat col[4];
	prefs.copyColour(Prefs::BackgroundColour, col);
	glClearColor(col[0], col[1], col[2], col[3]);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	// Set viewport, setupGL, and clear background / depth buffer
	setupGL();

	// Draw model
	renderModel(model, 0, 0, iconWidth, iconHeight, false);

        // Flush contents
        glFlush();

	// Grab image ready for return
	QImage fboImage(frameBufferObject.toImage());
	QImage image(fboImage.constBits(), fboImage.width(), fboImage.height(), QImage::Format_ARGB32);

	// Reset context back to main view
	makeCurrent();

	Messenger::exit("Viewer::generateModelImage");
	return QPixmap::fromImage(image, Qt::AutoColor);
}

// Render current scene at supplied size (or current widget size if none provided)
QPixmap Viewer::generateImage(int imageWidth, int imageHeight)
{
	Messenger::enter("Viewer::generateImage");

	// Make sure high quality primitives are up-to-date
	updatePrimitives(Viewer::HighQuality);

	// Check provided width/height
	if ((imageWidth == -1) || (imageHeight == -1))
	{
		imageWidth = contextWidth_;
		imageHeight = contextHeight_;
	}

	// Flag that we are rendering offscreen, and that we want high quality primitives
	renderingOffScreen_ = true;
	primitiveSet_ = Viewer::HighQuality;

	// Force regeneration of all rendergroups for models so that the high quality primitives are used
	aten_->globalLogChange(Log::Style);

	// Scale current line width and text scaling to reflect size of exported image
	setObjectScaling( double(imageHeight) / double(contextHeight()) );

	// Make the offscreen surface the current context
	offscreenContext_.makeCurrent(&offscreenSurface_);

	// Grab clear colour
	GLfloat col[4];
	prefs.copyColour(Prefs::BackgroundColour, col);

	// Set tile size
	int tileWidth = 512;
	int tileHeight = 512;

	// Initialise framebuffer format and object
	QOpenGLFramebufferObjectFormat fboFormat;
	fboFormat.setMipmap(true);
	fboFormat.setSamples(4);
	fboFormat.setAttachment(QOpenGLFramebufferObject::Depth);
	QOpenGLFramebufferObject frameBufferObject(tileWidth, tileHeight, fboFormat);

	if (!frameBufferObject.bind())
	{
		Messenger::print("Failed to bind framebuffer object when generating image.");
		Messenger::exit("Viewer::generateImage");
		return QPixmap();
	}

	// Create a QPixmap of the desired full size and a QPainter for it
	QPixmap pixmap = QPixmap(imageWidth, imageHeight);
	QPainter painter(&pixmap);
	painter.setPen(Qt::NoPen);
	painter.setBrush(Qt::white);
	painter.drawRect(0,0,imageWidth, imageHeight);

	// Calculate scale factors for ViewLayout, so that the context width/height is scaled to the desired image size
	int nX = imageWidth / tileWidth + ((imageWidth %tileWidth) ? 1 : 0);
	int nY = imageHeight / tileHeight + ((imageHeight %tileHeight) ? 1 : 0);

	// Loop over tiles in x and y
	Task* task = Messenger::initialiseTask("Generating tiled image", nX*nY);
	for (int x=0; x<nX; ++x)
	{
		for (int y=0; y<nY; ++y)
		{
			// Generate this tile
			frameBufferObject.bind();
			glClearColor(col[0], col[1], col[2], col[3]);
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			renderFullScene(imageWidth, imageHeight, -x*tileWidth, -y*tileHeight);

			// Generate this tile
			QImage fboImage(frameBufferObject.toImage());
			QImage tile(fboImage.constBits(), fboImage.width(), fboImage.height(), QImage::Format_ARGB32);

			// Paste this tile into the main image
			painter.drawImage(x*tileWidth, imageHeight-(y+1)*tileHeight, tile, Qt::ThresholdAlphaDither | Qt::ColorOnly);

			if (!Messenger::incrementTaskProgress(task)) break;
		}
		if (!Messenger::incrementTaskProgress(task)) break;
	}
	Messenger::terminateTask(task);

	// Finalise painter
	painter.end();

	// Reset line width and text size
	setObjectScaling(1.0);

	// Make sure the Viewer knows we no longer want offscreen rendering, and revert to normal quality primitives
	renderingOffScreen_ = false;
	primitiveSet_ = Viewer::LowQuality;

	// Force regeneration of all rendergroups for models so that the low quality primitives are used
	aten_->globalLogChange(Log::Style);

	// Reset context back to main view
	makeCurrent();

	return pixmap;
}
