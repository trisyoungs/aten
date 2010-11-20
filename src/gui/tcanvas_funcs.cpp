/*
	*** Qt canvas functions
	*** src/gui/tcanvas_funcs.cpp
	Copyright T. Youngs 2007-2010

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
#include "gui/tcanvas.uih"
#include "gui/gui.h"
#include "model/model.h"

// Local variables
bool DONTDRAW = FALSE;

// Constructor
TCanvas::TCanvas(QGLContext *context, QWidget *parent) : QGLWidget(context, parent)
{
	// Private variables
	canvas_ = NULL;
	useCurrentModel_ = TRUE;
	renderSource_ = NULL;

	setAutoFillBackground(FALSE);
}

// Set the widgetcanvas for the display
void TCanvas::setCanvas(Canvas *wc)
{
	canvas_ = wc;
}

// Set the rendering source to the supplied model (uses useCurrentModel_ ifa NULL pointer is supplied)
void TCanvas::setRenderSource(Model *source)
{
	if (source == NULL)
	{
		useCurrentModel_ = TRUE;
		renderSource_ = NULL ;
	}
	else
	{
		useCurrentModel_ = FALSE;
		renderSource_ = source;
	}
}

// Probe features
void TCanvas::probeFeatures()
{
	if (msg.isOutputActive(Messenger::GL))
	{
		QGLFormat fmt = context()->format();
		// Probe this format!
		printf(" QGLFormat: Alpha buffer is %s.\n", fmt.alpha() ? "enabled" : "disabled");
		printf(" QGLFormat: Accumulation buffer is %s.\n", fmt.accum() ? "enabled" : "disabled");
		printf(" QGLFormat: Depth buffer is %s.\n", fmt.depth() ? "enabled" : "disabled");
		printf(" QGLFormat: Double-buffering is %s.\n", fmt.doubleBuffer() ? "enabled" : "disabled");
		printf(" QGLFormat: Direct rendering is %s.\n", fmt.directRendering() ? "enabled" : "disabled");
		printf(" QGLFormat: RGBA colour mode is %s.\n", fmt.rgba() ? "enabled" : "disabled");
		printf(" QGLFormat: Multisample buffer is %s.\n", fmt.sampleBuffers() ? "enabled" : "disabled");
		printf(" QGLFormat: Stencil buffer is %s.\n", fmt.stencil() ? "enabled" : "disabled");
		printf(" QGLWidget: Autoswap buffers is %s.\n", autoBufferSwap() ? "enabled" : "disabled");
	}
}

void TCanvas::initializeGL()
{
	// Initialize GL
	if (canvas_ != NULL)
	{
		canvas_->setValid(TRUE);
		canvas_->initGl();
	}
	else printf("NO CANVAS SET IN TCANVAS::INITIALIZEGL!!!\n");
}

void TCanvas::paintGL()
{
	static QFont font;
	if (canvas_ != NULL)
	{
		// Note: An internet source suggests that the QPainter documentation is incomplete, and that
		// all OpenGL calls should be made after the QPainter is constructed, and befor the QPainter
		// is destroyed. However, this results in mangled graphics on the Linux (and other?) versions,
		// so here it is done in the 'wrong' order.

		Model *source;
		if (useCurrentModel_)
		{
			source = aten.currentModelOrFrame();
			if (source != NULL) source = (source->renderFromVibration() ? source->vibrationCurrentFrame() : source->renderSourceModel());
		}
		else
		{
			source = renderSource_;
			if (source != NULL) source = (source->renderFromVibration() ? source->vibrationCurrentFrame() : source->renderSourceModel());
		}

		if (source != NULL)
		{
			// Initialise QPainter
			QPainter painter(this);
			
			// Draw on text objects (with QPainter)
			font.setPointSize(prefs.labelSize());
			painter.setFont(font);
			painter.setBrush( QBrush(QColor(0,0,0), Qt::SolidPattern) );
			painter.setRenderHint(QPainter::Antialiasing);
			// 			canvas_->renderText(painter);
			
			// Render model
			canvas_->renderModel(source);
	
			// Draw on text objects (with QPainter)
			font.setPointSize(prefs.labelSize());
			painter.setFont(font);
			painter.setBrush( QBrush(QColor(0,0,0), Qt::SolidPattern) );
			painter.setRenderHint(QPainter::Antialiasing);
// 			canvas_->renderText(painter);
		}
	}
	else printf("NO CANVAS SET PAINT\n");
}

void TCanvas::resizeGL(int width, int height)
{
	if (canvas_ != NULL)
	{
		canvas_->configure(width, height);
		if (canvas_->displayModel() != NULL) canvas_->displayModel()->changeLog.add(Log::Camera);
	}
	else printf("NO CANVAS SET RESIZE\n");
}

/*
// Input
*/

void TCanvas::keyPressEvent(QKeyEvent *event)
{
	Canvas::KeyCode kc = gui.convertToKeyCode(event->key());
	Qt::KeyboardModifiers km = event->modifiers();
	if (kc != Canvas::OtherKey) gui.mainView.informKeyDown(kc, km&Qt::ShiftModifier, km&Qt::ControlModifier, km&Qt::AltModifier);
	else event->ignore();
}

void TCanvas::keyReleaseEvent(QKeyEvent *event)
{
	Canvas::KeyCode kc = gui.convertToKeyCode(event->key());
	Qt::KeyboardModifiers km = event->modifiers();
	if (kc != Canvas::OtherKey) gui.mainView.informKeyUp(kc, km&Qt::ShiftModifier, km&Qt::ControlModifier, km&Qt::AltModifier);
	else event->ignore();
}

void TCanvas::mousePressEvent(QMouseEvent *event)
{
	// Handle button presses (button down) from the mouse
	msg.enter("TCanvas::mousePressEvent");
	Prefs::MouseButton button;
	if (event->button() == Qt::LeftButton) button = Prefs::LeftButton;
	else if (event->button() == Qt::MidButton) button = Prefs::MiddleButton;
	else if (event->button() == Qt::RightButton) button = Prefs::RightButton;
	else
	{
		msg.exit("TCanvas::mousePressEvent");
		return;
	}
	// Preliminary check to see if RMB was pressed over an atom - if so , show the popup menu and exit.
	if ((button == Prefs::RightButton) && gui.mainView.editable())
	{
		Atom *tempi = gui.mainView.displayModel()->atomOnScreen(event->x(), event->y());
		if (tempi != NULL)
		{
			gui.callContextMenu(tempi, event->globalX(), event->globalY());
			gui.mainView.postRedisplay();
			msg.exit("TCanvas::mousePressEvent");
			return;
		}
	}
	// If the left mouse button is double-clicked over an atom, show the atomlist window
	if ((button == Prefs::LeftButton) && (event->type() == QEvent::MouseButtonDblClick))
	{
		Atom *tempi = canvas_->atomClicked();
		if (tempi != NULL)
		{
			printf("gui::dblclick show atom list not done.\n");
			//gui.atomwin_list_refresh();
			msg.exit("TCanvas::mousePressEvent");
			return;
		}
	}
	// Store the state of the modifier keys here
	Qt::KeyboardModifiers km = event->modifiers();
	// Inform the main canvas that a button action has occurred
	gui.mainView.informMouseDown(button, event->x(), event->y(), km&Qt::ShiftModifier, km&Qt::ControlModifier, km&Qt::AltModifier);
	msg.exit("TCanvas::mousePressEvent");
}

void TCanvas::mouseReleaseEvent(QMouseEvent *event)
{
	// Handle button releases (button up) from the mouse
	msg.enter("TCanvas::mouseReleaseEvent");
	Prefs::MouseButton button;
	if (event->button() == Qt::LeftButton) button = Prefs::LeftButton;
	else if (event->button() == Qt::MidButton) button = Prefs::MiddleButton;
	else if (event->button() == Qt::RightButton) button = Prefs::RightButton;
	else
	{
		msg.exit("TCanvas::mouseReleaseEvent");
		return;
	}
	// Finalize the requested action
	gui.mainView.informMouseUp(button, event->x(), event->y());
	msg.exit("TCanvas::mouseReleaseEvent");
}

void TCanvas::mouseMoveEvent(QMouseEvent *event)
{
	// Mouse motion handler.
	// Tell the main canvas that the mouse has moved
	gui.mainView.informMouseMove(event->x(),event->y());
	setFocus();
	gui.updateStatusBar();
}

void TCanvas::wheelEvent(QWheelEvent *event)
{
	// Handle mouse-wheel scroll events.
	if (event->delta() > 0) gui.mainView.informScroll(TRUE);
	else gui.mainView.informScroll(FALSE);
}

void TCanvas::focusOutEvent(QFocusEvent *event)
{
	gui.updateStatusBar(TRUE);
}

void TCanvas::timerEvent(QTimerEvent *event)
{
	// Move on to the next frame in the trajectory
	// Check that we're not still drawing the last frame from the last timerEvent
	if (DONTDRAW) printf("Still drawing previous frame.\n");
	else
	{
		DONTDRAW = TRUE;
		Model *m = aten.currentModel();
		m->seekNextTrajectoryFrame();
		if (m->trajectoryFrameIndex() == m->nTrajectoryFrames()-1) gui.stopTrajectoryPlayback();
		gui.update(FALSE,FALSE,FALSE);
		DONTDRAW = FALSE;
	}
}
