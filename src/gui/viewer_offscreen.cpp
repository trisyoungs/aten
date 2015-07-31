/*
	*** Viewer - Offscreen rendering
	*** src/gui/viewer_offscreen.cpp
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

#include "gui/viewer.hui"
#include "gui/mainwindow.h"
#include <QOpenGLFramebufferObject>
#include <QPainter>
#include <QtWidgets/QProgressDialog>

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
	QSize iconSize(width, height);

	// Initialise framebiffer format and object
	QOpenGLFramebufferObject frameBufferObject(iconSize);

	if (!frameBufferObject.bind())
	{
		Messenger::print("Failed to bind framebuffer object when generating image for single model.");
		Messenger::exit("Viewer::generateModelImage");
		return QPixmap();
	}

	// Set viewport
	glViewport(0, 0, iconSize.width(), iconSize.height());

	// Grab clear colour and clear view / depth buffer
	GLfloat col[4];
	prefs.copyColour(Prefs::BackgroundColour, col);
	glClearColor(col[0], col[1], col[2], col[3]);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	// Set viewport, setupGL, and clear background / depth buffer
	setupGL();

	// Draw model
	renderModel(model, 0, 0, iconSize.width(), iconSize.height(), false);

        // Flush contents
        glFlush();

	// Grab image ready for return
	QImage image = frameBufferObject.toImage();

	// Reset context back to main view
	makeCurrent();

	Messenger::exit("Viewer::generateModelImage");
	return QPixmap::fromImage(image);
}

// Render current scene at supplied size
QPixmap Viewer::generateImage(int width, int height)
{
// 	renderingOffScreen_ = true;

	const int maxSize = 2000;
	bool useFrameBuffer = true;

	// Scale current line width and text scaling to reflect size of exported image
	setObjectScaling( double(height) / double(contextHeight()) );

	// If both image dimensions are less than some limiting size, get image in a single shot. If not, tile it...
	QPixmap pixmap;
	if ((width > maxSize) || (height > maxSize))
	{
		// If we are using the framebuffer, use the current Viewer size as our tile size
		int tileWidth = (useFrameBuffer ? contextWidth() : maxSize);
		int tileHeight = (useFrameBuffer ? contextHeight() : maxSize);

		// Create a QPixmap of the desired full size
		pixmap = QPixmap(width, height);
		QPainter painter(&pixmap);

		// Calculate scale factors for ViewLayout, so that the context width/height is scaled to the desired image size
		double xScale = double(width) / double(tileWidth);
		double yScale = double(height) / double(tileHeight);
		int nX = width / tileWidth + ((width%tileWidth) ? 1 : 0);
		int nY = height / tileHeight + ((height%tileHeight) ? 1 : 0);

		// Loop over tiles in x and y
		QProgressDialog progress("Saving tiled image", "Cancel", 0, nX*nY, atenWindow_);
		progress.setWindowTitle("Aten");
		progress.show();
		for (int x=0; x<nX; ++x)
		{
			for (int y=0; y<nY; ++y)
			{
				// Set progress value and check for cancellation
				if (progress.wasCanceled()) break;
				progress.setValue(x*nY+y);

				// Recalculate view pane sizes to reflect current tile position and tile size
// 				UChromaSession::viewLayout().setOffsetAndScale(-x*tileWidth, -y*tileHeight, xScale, yScale);  // ATEN2 TODO
// 				if (useFrameBuffer) UChromaSession::viewLayout().recalculate(tileWidth, tileHeight);  // ATEN2 TODO

				// Generate this tile
				if (useFrameBuffer) atenWindow_->ui.MainView->repaint();
// 				QPixmap tile = atenWindow_->ui.MainView->frameBuffer();	 // ATEN2 TODO

				// Paste this tile into the main image
// 				painter.drawPixmap(x*tileWidth, height-(y+1)*tileHeight, tile);
			}
			if (progress.wasCanceled()) break;
		}

		// Finalise painter
		painter.end();
	}
	else
	{
// 		pixmap = renderPixmap(width, height);
	}

	// Reset line width and text size
	setObjectScaling(1.0);

	// Make sure the Viewer knows we no longer want offscreen rendering
// 	renderingOffScreen_ = false;

	// The sizes of panes may now be incorrect, so reset everything
// 	UChromaSession::viewLayout().setOffsetAndScale(0, 0, 1.0, 1.0);  // ATEN2 TODO
// 	UChromaSession::viewLayout().recalculate(ui.MainView->contextWidth(), ui.MainView->contextHeight());  // ATEN2 TODO

	return pixmap;
}
