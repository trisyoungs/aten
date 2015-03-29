/*
	*** Aten image creation
	*** src/main/image.cpp
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
#include "gui/mainwindow.h"
#include "base/sysfunc.h"
#include <QtWidgets/QProgressDialog>

ATEN_USING_NAMESPACE

// Bitmap Image Formats (conform to allowable pixmap formats in Qt)
const char* bitmapFormatFilters[Aten::nBitmapFormats] = { "Windows Bitmap (*.bmp)", "Joint Photographic Experts Group (*.jpg)", "Portable Network Graphics (*.png)", "Portable Pixmap (*.ppm)", "X11 Bitmap (*.xbm)", "X11 Pixmap (*.xpm)" };
const char* bitmapFormatExtensions[Aten::nBitmapFormats] = { "bmp", "jpg", "png", "ppm", "xbm", "xpm" };
Aten::BitmapFormat Aten::bitmapFormat(QString s, bool reportError)
{
	Aten::BitmapFormat bf = (Aten::BitmapFormat) enumSearch("bitmap format", Aten::nBitmapFormats, bitmapFormatExtensions, s);
	if ((bf == Aten::nBitmapFormats) && reportError) enumPrintValid(Aten::nBitmapFormats, bitmapFormatExtensions);
	return bf;
}
Aten::BitmapFormat Aten::bitmapFormatFromFilter(const char* s)
{
	return (Aten::BitmapFormat) enumSearch("bitmap format", Aten::nBitmapFormats, bitmapFormatFilters,s);
}
const char* Aten::bitmapFormatFilter(Aten::BitmapFormat bf)
{
	return bitmapFormatFilters[bf];
}
const char* Aten::bitmapFormatExtension(Aten::BitmapFormat bf)
{
	return bitmapFormatExtensions[bf];
}

// Save image of current view
QPixmap Aten::currentViewAsPixmap(int width, int height)
{
	const int maxSize = 2000;
	bool useFrameBuffer = true;
	int contextWidth = atenWindow_->ui.MainView->contextWidth();
	int contextHeight = atenWindow_->ui.MainView->contextHeight();	

	// Scale current line width and text scaling to reflect size of exported image
	atenWindow_->ui.MainView->setObjectScaling( double(height) / double(contextHeight) );

	// If both image dimensions are less than some limiting size, get image in a single shot. If not, tile it...
	QPixmap pixmap;
	if ((width > maxSize) || (height > maxSize))
	{
		// If we are using the framebuffer, use the current Viewer size as our tile size
		int tileWidth = (useFrameBuffer ? contextWidth : maxSize);
		int tileHeight = (useFrameBuffer ? contextHeight : maxSize);

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
				QPixmap tile = atenWindow_->ui.MainView->frameBuffer();

				// Paste this tile into the main image
				painter.drawPixmap(x*tileWidth, height-(y+1)*tileHeight, tile);
			}
			if (progress.wasCanceled()) break;
		}

		// Finalise painter
		painter.end();
	}
	else
	{
		pixmap = atenWindow_->ui.MainView->generateImage(width, height);
	}

	// Reset line width and text size
	atenWindow_->ui.MainView->setObjectScaling(1.0);

	// Make sure the Viewer knows we no longer want offscreen rendering
	atenWindow_->ui.MainView->setRenderingOffScreen(false); 

	// The sizes of panes may now be incorrect, so reset everything
// 	UChromaSession::viewLayout().setOffsetAndScale(0, 0, 1.0, 1.0);  // ATEN2 TODO
// 	UChromaSession::viewLayout().recalculate(ui.MainView->contextWidth(), ui.MainView->contextHeight());  // ATEN2 TODO

	return pixmap;
}

// Return pixmap of specified model
QPixmap Aten::modelPixmap(Model* model, int width, int height)
{
	// ATEN2 TODO
}