/*
	*** Qt canvas functions
	*** src/gui-qt/canvas-qt.cpp
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

#include "gui-qt/canvas-qt.h"
#include "base/master.h"
#include "base/prefs.h"
#include "gui/gui.h"
#include "gui-qt/tcanvas.uih"

// Constructor
widgetcanvas::widgetcanvas()
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
widgetcanvas::~widgetcanvas()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_WIDGETCANVAS] ++;
	#endif
}

// Begin GL
bool widgetcanvas::begin_gl()
{
	if (!valid) return FALSE;
	drawing = TRUE;
	return TRUE;
}

// Finalize GL commands
void widgetcanvas::end_gl()
{
	drawing = FALSE;
}

// Swap buffers
void widgetcanvas::swap_buffers()
{
}

/*
// Widget Canvas
*/

// Set widget
bool widgetcanvas::set_widget(TCanvas *w)
{
	context_widget = w;
	return TRUE;
}

// Widget realize
void widgetcanvas::realize()
{
	// Sets the canvas to use a widget for output.
	dbg_begin(DM_CALLS,"widgetcanvas::realize");
	valid = TRUE;
	init_gl();
	canvas_master::globs.initialise();
	dbg_end(DM_CALLS,"widgetcanvas::realize");
}

// Invalidate
void widgetcanvas::postredisplay()
{
	dbg_begin(DM_CALLS,"widgetcanvas::postredisplay");
	if (gui.exists()) context_widget->paintGL();
	dbg_end(DM_CALLS,"widgetcanvas::postredisplay");
}

// Widget Expose
void widgetcanvas::expose()
{
	if ((!gui.exists()) || gui.no_rendering() ) return;
	// Render from the current rendering source
	render_scene(master.get_currentmodel()->get_render_source());
	#ifdef SPEEDTEST
		speedtest_numrenders ++;
		speedtest_totalrenders ++;
	#endif
}

// Widget configure
void widgetcanvas::configure()
{
	// Store the new width and height of the widget and re-do projection
	w = (float)context_widget->width();
	h = (float)context_widget->height();
	do_projection();
	// Flag that render source needs to be reprojected
	if (displaymodel != NULL) displaymodel->log_change(LOG_VISUAL);
}
