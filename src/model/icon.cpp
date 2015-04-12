/*
	*** Model icon functions
	*** src/model/icon.cpp
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

#include "model/model.h"
#include <QPainter>

ATEN_USING_NAMESPACE

// Return whether icon is currently valid
bool Model::iconIsValid()
{
	return (iconPoint_ == changeLog_.log(Log::Structure));  // ATEN2 TODO Make a general class to allow more than one log quantity to be compared?
}

// Set icon from supplied pixmap
void Model::setIcon(QPixmap pixmap)
{
	icon_ = QIcon();

	// Check to see if pixmap is valid
	if (pixmap.isNull())
	{
		iconPoint_ = -1;
		return;
	}

	// Take supplied pixmap and draw on black selection box
	QPixmap selectedPixmap = pixmap;
	QPainter painter(&selectedPixmap);
	QPen pen(Qt::black);
	pen.setWidth(3);
	painter.setPen(pen);
	painter.drawRect(0, 0, pixmap.width()-1, pixmap.height()-1);
	painter.end();

	// Set icons
	icon_.addPixmap(pixmap, QIcon::Normal, QIcon::On);
	icon_.addPixmap(selectedPixmap, QIcon::Selected, QIcon::On);

	// Store new logpoint
	iconPoint_ = changeLog_.log(Log::Structure);
}

// Return icon
QIcon& Model::icon()
{
	// Regenerate icon if necessary
	if (!iconIsValid()) Messenger::warn(Messenger::Verbose, "Model icon is out of date.");

	return icon_;
}
