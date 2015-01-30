/*
	*** TColourFrame Functions
	*** src/gui/tcolourframe_funcs.cpp
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

#include "gui/tcolourframe.uih"
#include <QtGui/QPainter>

// Constructor
TColourFrame::TColourFrame(QWidget *parent)
{
	setParent(parent);
	brush_.setStyle(Qt::SolidPattern);
}

// Paintevent to colour entire widget with the current colour
void TColourFrame::paintEvent(QPaintEvent *event)
{
	QPainter painter(this);
	painter.setBackgroundMode(Qt::OpaqueMode);
	painter.setBrush(brush_);
	painter.drawRect(0,0,width(),height());
	painter.end();
}

// Set colour to be displayed in frame (from QColor)
void TColourFrame::setColour(QColor &c)
{
	brush_.setColor(c);
}

// Set brush colour (with array of GLfloats)
void TColourFrame::setColour(double *col)
{
	QColor rgb;
	rgb.setRgbF(col[0],col[1],col[2],1.0f);
	brush_.setColor(rgb);
}

// Return current colour
QColor TColourFrame::colour()
{
	return brush_.color();
}
