/*
	*** MainWindow - Image Functions
	*** src/gui/mainwindow_image.cpp
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

// #include <QtWidgets/QMessageBox>
// #include "main/aten.h"
#include "gui/mainwindow.h"
// #include "gui/prefs.h"
// #include "gui/loadmodel.h"
// #include "gui/trajectory.h"
// #include "gui/ffeditor.h"
// #include "gui/selectpattern.h"
// #include "gui/about.h"
// #include "model/model.h"
// #include "model/clipboard.h"
// #include "model/undostate.h"
// #include "parser/commandnode.h"
// #include <QtWidgets/QFileDialog>
// #include <QKeyEvent>
// #include <QtWidgets/QProgressBar>
#include "base/sysfunc.h"
// #include "main/version.h"
// #include <iostream>
// #include <fstream>

// Bitmap Image Formats (conform to allowable pixmap formats in Qt)
const char* bitmapFormatFilters[AtenWindow::nBitmapFormats] = { "Windows Bitmap (*.bmp)", "Joint Photographic Experts Group (*.jpg)", "Portable Network Graphics (*.png)", "Portable Pixmap (*.ppm)", "X11 Bitmap (*.xbm)", "X11 Pixmap (*.xpm)" };
const char* bitmapFormatExtensions[AtenWindow::nBitmapFormats] = { "bmp", "jpg", "png", "ppm", "xbm", "xpm" };
AtenWindow::BitmapFormat AtenWindow::bitmapFormat(QString s, bool reportError)
{
	AtenWindow::BitmapFormat bf = (AtenWindow::BitmapFormat) enumSearch("bitmap format", AtenWindow::nBitmapFormats, bitmapFormatExtensions, s);
	if ((bf == AtenWindow::nBitmapFormats) && reportError) enumPrintValid(AtenWindow::nBitmapFormats, bitmapFormatExtensions);
	return bf;
}
AtenWindow::BitmapFormat AtenWindow::bitmapFormatFromFilter(const char* s)
{
	return (AtenWindow::BitmapFormat) enumSearch("bitmap format", AtenWindow::nBitmapFormats, bitmapFormatFilters,s);
}
const char* AtenWindow::bitmapFormatFilter(AtenWindow::BitmapFormat bf)
{
	return bitmapFormatFilters[bf];
}
const char* AtenWindow::bitmapFormatExtension(AtenWindow::BitmapFormat bf)
{
	return bitmapFormatExtensions[bf];
}

// Save image of current view
QPixmap AtenWindow::scenePixmap(int width, int height)
{
	return ui.MainView->generateImage(width, height);
}

// Return pixmap of specified model
QPixmap AtenWindow::modelPixmap(Model* model, int width, int height)
{
	return ui.MainView->generateModelImage(model, width, height);
}
