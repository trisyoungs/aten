/*
	*** Qt canvas functions
	*** src/gui/tcanvas_funcs.cpp
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

#include "base/master.h"
#include "gui/canvas.h"
#include "gui/tcanvas.uih"
#include "gui/gui.h"
#include "model/model.h"
#include <QtGui/QMouseEvent>

// Local variables
bool DONTDRAW = FALSE;

// Constructor
TCanvas::TCanvas(QWidget *parent)
{
	widgetCanvas_ = NULL;
}

// Set the widgetcanvas for the display
void TCanvas::setCanvas(Canvas *wc)
{
	widgetCanvas_ = wc;
}

void TCanvas::initializeGL()
{
	// Call the realize method of the associated widgetCanvas_.
	if (widgetCanvas_ != NULL) widgetCanvas_->realize();
	else printf("NO CANVAS SET INIT\n");
}

void TCanvas::paintGL()
{
	if (widgetCanvas_ != NULL) widgetCanvas_->renderScene(master.currentModel()->renderSource());
	else printf("NO CANVAS SET PAINT\n");
	swapBuffers();
}

void TCanvas::resizeGL(int width, int height)
{
	if (widgetCanvas_ != NULL)
	{
		widgetCanvas_->configure();
		if (widgetCanvas_->displayModel() != NULL) widgetCanvas_->displayModel()->logChange(LOG_CAMERA);
	}
	else printf("NO CANVAS SET RESIZE\n");
}

void TCanvas::mousePressEvent(QMouseEvent *event)
{
	// Handle button presses (button down) from the mouse
	dbgBegin(Debug::Calls,"TCanvas::mousePressEvent");
	Prefs::MouseButton button;
	if (event->button() == Qt::LeftButton) button = Prefs::LeftButton;
	else if (event->button() == Qt::MidButton) button = Prefs::MiddleButton;
	else if (event->button() == Qt::RightButton) button = Prefs::RightButton;
	else
	{
		dbgEnd(Debug::Calls,"TCanvas::mousePressEvent");
		return;
	}
	// Do the requested action as defined in the edit panel, but only if another action
	// isn't currently in progress. Set the UserAction based on the mouse button that sent
	// the signal, current selection / draw modes and key modifier states.
	// Preliminary check to see if RMB was pressed over an atom - if so , show the popup menu and exit.
	if (button == Prefs::RightButton)
	{
		Atom *tempi = master.currentModel()->atomOnScreen(event->x(), event->y());
		if (tempi != NULL)
		{
			gui.callAtomPopup(tempi, event->globalX(), event->globalY());
			gui.mainView.postRedisplay();
			dbgEnd(Debug::Calls,"TCanvas::mousePressEvent");
			return;
		}
	}
	// If the left mouse button is double-clicked over an atom, show the atomlist window
	if ((button == Prefs::LeftButton) && (event->type() == QEvent::MouseButtonDblClick))
	{
		Atom *tempi = widgetCanvas_->atomHover();
		if (tempi != NULL)
		{
			printf("gui::dblclick show atom list not done.\n");
			//gui.atomwin_list_refresh();
			dbgEnd(Debug::Calls,"TCanvas::mousePressEvent");
			return;
		}
	}
	// Inform the main canvas that a button action has occurred
	gui.mainView.informMouseDown(button,event->x(),event->y());
	dbgEnd(Debug::Calls,"TCanvas::mousePressEvent");
}

void TCanvas::mouseReleaseEvent(QMouseEvent *event)
{
	// Handle button releases (button up) from the mouse
	dbgBegin(Debug::Calls,"TCanvas::mouseReleaseEvent");
	Prefs::MouseButton button;
	if (event->button() == Qt::LeftButton) button = Prefs::LeftButton;
	else if (event->button() == Qt::MidButton) button = Prefs::MiddleButton;
	else if (event->button() == Qt::RightButton) button = Prefs::RightButton;
	else
	{
		dbgEnd(Debug::Calls,"TCanvas::mouseReleaseEvent");
		return;
	}
	// Finalize the requested action
	gui.mainView.informMouseUp(button,event->x(),event->y());
	gui.refresh();
	dbgEnd(Debug::Calls,"TCanvas::mouseReleaseEvent");
}

void TCanvas::mouseMoveEvent(QMouseEvent *event)
{
	// Mouse motion handler.
	// Tell the main canvas that the mouse has moved
	gui.mainView.informMouseMove(event->x(),event->y());
}

void TCanvas::wheelEvent(QWheelEvent *event)
{
	// Handle mouse-wheel scroll events.
	if (event->delta() > 0) gui.mainView.informScroll(TRUE);
	else gui.mainView.informScroll(FALSE);
}

void TCanvas::timerEvent(QTimerEvent *event)
{
	// Move on to the next frame in the trajectory
	// Check that we're not still drawing the last frame from the last timerEvent
	if (DONTDRAW) printf("Still drawing previous frame.\n");
	else
	{
		DONTDRAW = TRUE;
		Model *m = master.currentModel();
		m->seekNextFrame();
		if (m->framePosition() == m->totalFrames()) gui.stopTrajectoryPlayback();
		gui.updateLabels();
		gui.mainView.postRedisplay();
		DONTDRAW = FALSE;
	}
}
