/*
	*** Viewer Functions
	*** src/gui/viewer_funcs.cpp
	Copyright T. Youngs 2007-2018

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

	// Mouse Input
	for (int i=0; i<3; i++) mouseButton_[i] = false;

	// Key Input
	for (int i=0; i<3; i++) keyModifier_[i] = false;

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

// Set line width and text scaling to use
void Viewer::setObjectScaling(double scaling)
{
	lineWidthScaling_ = scaling;

	// Pass this value on to those that depend on it
// 	LineStyle::setLineWidthScale(scaling);
	TextPrimitive::setScalingFactor(scaling);
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

// Return height, in pixels, of single line of text
int Viewer::fontPixelHeight()
{
	return fontPixelHeight_;
}
