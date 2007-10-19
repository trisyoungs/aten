/*
	*** Qt canvas
	*** src/gui-qt/canvas-qt.h
	Copyright T. Youngs 2007

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

#ifndef H_CANVASQT_H
#define H_CANVASQT_H

#include "templates/vector3.h"
#include "gui/canvas.h"
#include "base/debug.h"

// Forward declarations
class atom;
class config;
class bond;
class geometry;
class subselection;
class unitcell;
class TCanvas;

/*
// Canvas Base Class
// Provides GL rendering functions for a context
*/
class widgetcanvas : public canvas_master
{
	private:
	// Source widget
	TCanvas *context_widget;

	public:
	// Constructor / Destructor
	widgetcanvas();
	~widgetcanvas();
	// Requests that the canvas contents be redrawn
	void postredisplay();


	/*
	// GTK-related callbacks / functions
	*/
	public:
	// Set up widget for OpenGL drawing
	bool set_widget(TCanvas*);
	// Called when widget is initialised and ready
	void realize();
	// Called when widget is resized
	void configure();
	// Called when widget needs to be redrawn
	void expose();
	// Swap buffers
	void swap_buffers();
	// Begin GL commands
	bool begin_gl();
	// End GL commands
	void end_gl();
};

#endif
