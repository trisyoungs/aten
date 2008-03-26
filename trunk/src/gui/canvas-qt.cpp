/*
	*** Qt canvas functions
	*** src/gui/canvas.cpp
	Copyright T. Youngs 2007,2008

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

#include "gui/canvas.h"
#include "base/master.h"
#include "base/prefs.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"

// Constructor
canvas::canvas()
{
	context_widget = NULL;
	subselect_enabled = FALSE;
	for (int i=0; i<3; i++)
	{
		mb[i] = FALSE;
		keymod[i] = FALSE;
	}
	#ifdef MEMDEBUG
		memdbg.create[MD_WIDGETCANVAS] ++;
	#endif
}

// Destructors
canvas::~canvas()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_WIDGETCANVAS] ++;
	#endif
}

// Begin GL
bool canvas::begin_gl()
{
	if (!valid) return FALSE;
	drawing = TRUE;
	return TRUE;
}

// Finalize GL commands
void canvas::end_gl()
{
	drawing = FALSE;
}

// Swap buffers
void canvas::swap_buffers()
{
}

/*
// Widget Canvas
*/

// Set widget
bool canvas::set_widget(TCanvas *w)
{
	context_widget = w;
	return TRUE;
}

// Widget realize
void canvas::realize()
{
	// Sets the canvas to use a widget for output.
	dbgBegin(Debug::Calls,"canvas::realize");
	valid = TRUE;
	init_gl();
	dbgEnd(Debug::Calls,"canvas::realize");
}

// Invalidate
void canvas::postRedisplay()
{
	dbgBegin(Debug::Calls,"canvas::postRedisplay");
	if (gui.exists()) context_widget->paintGL();
	dbgEnd(Debug::Calls,"canvas::postRedisplay");
}

// Widget Expose
void canvas::expose()
{
	if ((!gui.exists()) || gui.no_rendering() ) return;
	// Render from the current rendering source
	render_scene(master.currentModel()->get_render_source());
	#ifdef SPEEDTEST
		speedtest_numrenders ++;
		speedtest_totalrenders ++;
	#endif
}

// Widget configure
void canvas::configure()
{
	// Store the new width and height of the widget and re-do projection
	w = (float)context_widget->width();
	h = (float)context_widget->height();
	do_projection();
	// Flag that render source needs to be reprojected
	if (displaymodel != NULL) displaymodel->logChange(LOG_VISUAL);
}
