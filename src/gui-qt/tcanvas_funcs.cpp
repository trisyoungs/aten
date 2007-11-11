/*
	*** Qt canvas functions
	*** src/gui-qt/tcanvas_funcs.cpp
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

#include "base/master.h"
#include "gui-qt/canvas-qt.h"
#include "gui-qt/tcanvas.uih"
#include "gui/gui.h"
#include <QtGui/QMouseEvent>

// Local variables
bool DONTDRAW = FALSE;

TCanvas::TCanvas(QWidget *parent)
{
	canvas = NULL;
}

void TCanvas::initializeGL()
{
	// Call the realize method of the associated widgetcanvas.
	if (canvas != NULL) canvas->realize();
	else printf("NO CANVAS SET INIT\n");
}

void TCanvas::paintGL()
{
	if (canvas != NULL) canvas->render_scene(master.get_currentmodel()->get_render_source());
	else printf("NO CANVAS SET PAINT\n");
	swapBuffers();
}

void TCanvas::resizeGL(int width, int height)
{
	if (canvas != NULL) canvas->configure();
	else printf("NO CANVAS SET RESIZE\n");
}

void TCanvas::mousePressEvent(QMouseEvent *event)
{
	// Handle button presses (button down) from the mouse
	dbg_begin(DM_CALLS,"TCanvas::mousePressEvent");
	mouse_button button;
	if (event->button() == Qt::LeftButton) button = MB_LEFT;
	else if (event->button() == Qt::MidButton) button = MB_MIDDLE;
	else if (event->button() == Qt::RightButton) button = MB_RIGHT;
	else
	{
		dbg_end(DM_CALLS,"TCanvas::mousePressEvent");
		return;
	}
	// Do the requested action as defined in the edit panel, but only if another action
	// isn't currently in progress. Set the user_action based on the mouse button that sent
	// the signal, current selection / draw modes and key modifier states.
	// Preliminary check to see if RMB was pressed over an atom - if so , show the popup menu and exit.
	if (button == MB_RIGHT)
	{
		atom *tempi = master.get_currentmodel()->atom_on_screen(event->x(), event->y());
		if (tempi != NULL)
		{
			gui.call_atompopup(tempi, event->globalX(), event->globalY());
			gui.mainview.postredisplay();
			dbg_end(DM_CALLS,"TCanvas::mousePressEvent");
			return;
		}
	}
	// If the left mouse button is double-clicked over an atom, show the atomlist window
	if ((button == MB_LEFT) && (event->type() == QEvent::MouseButtonDblClick))
	{
		atom *tempi = canvas->get_atom_hover();
		if (tempi != NULL)
		{
			gui.show(GW_ATOMLIST);
			printf("gui-qt::dblclick show atom list not done.\n");
			//gui.atomwin_list_refresh();
			dbg_end(DM_CALLS,"TCanvas::mousePressEvent");
			return;
		}
	}
	// Inform the main canvas that a button action has occurred
	gui.mainview.inform_mousedown(button,event->x(),event->y());
	dbg_end(DM_CALLS,"TCanvas::mousePressEvent");
}

void TCanvas::mouseReleaseEvent(QMouseEvent *event)
{
	// Handle button releases (button up) from the mouse
	dbg_begin(DM_CALLS,"TCanvas::mouseReleaseEvent");
	mouse_button button;
	if (event->button() == Qt::LeftButton) button = MB_LEFT;
	else if (event->button() == Qt::MidButton) button = MB_MIDDLE;
	else if (event->button() == Qt::RightButton) button = MB_RIGHT;
	else
	{
		dbg_end(DM_CALLS,"TCanvas::mouseReleaseEvent");
		return;
	}
	// Finalize the requested action
	gui.mainview.inform_mouseup(button,event->x(),event->y());
	gui.refresh();
	dbg_end(DM_CALLS,"TCanvas::mouseReleaseEvent");
}

void TCanvas::mouseMoveEvent(QMouseEvent *event)
{
	// Mouse motion handler.
	// Tell the main canvas that the mouse has moved
	gui.mainview.inform_mousemove(event->x(),event->y());
}

void TCanvas::wheelEvent(QWheelEvent *event)
{
	// Handle mouse-wheel scroll events.
	if (event->delta() > 0) gui.mainview.inform_scroll(TRUE);
	else gui.mainview.inform_scroll(FALSE);
}

void TCanvas::timerEvent(QTimerEvent *event)
{
	// Move on to the next frame in the trajectory
	// Check that we're not still drawing the last frame from the last timerEvent
	if (DONTDRAW) printf("Still drawing previous frame.\n");
	else
	{
		DONTDRAW = TRUE;
		model *m = master.get_currentmodel();
		m->seek_next_frame();
		if (m->get_frameposition() == m->get_totalframes()) gui.stop_trajectory_playback();
		gui.update_labels();
		gui.mainview.postredisplay();
		DONTDRAW = FALSE;
	}
}
