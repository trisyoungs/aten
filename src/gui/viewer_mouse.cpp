/*
	*** Viewer - Mouse input
	*** src/gui/viewer_mouse.cpp
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

#include <QtGui/QMouseEvent>
#include "gui/mainwindow.h"
#include "gui/fragments.h"
#include "main/aten.h"

// Qt Signal (mouse press event)
void Viewer::mousePressEvent(QMouseEvent* event)
{
	// Handle button presses (button down) from the mouse
	Messenger::enter("Viewer::mousePressEvent");
	static Prefs::MouseButton button = Prefs::nMouseButtons;

	// End old mode if one is active (i.e. prevent mode overlap on different mouse buttons)
	if (activeMode_ != UserAction::NoAction) endMode(button);
	
	// Which mouse button was pressed?
	if (event->button() == Qt::LeftButton) button = Prefs::LeftButton;
	else if (event->button() == Qt::MidButton) button = Prefs::MiddleButton;
	else if (event->button() == Qt::RightButton) button = Prefs::RightButton;
	else
	{
		Messenger::exit("Viewer::mousePressEvent");
		return;
	}
	
	// Store event information
	rMouseDown_.set(event->x(), event->y(), 0.0);
	rMouseUp_.set(event->x(), event->y(), 0.0);
	Qt::KeyboardModifiers km = event->modifiers();
	keyModifier_[Prefs::ShiftKey] = km&Qt::ShiftModifier;
	keyModifier_[Prefs::CtrlKey] = km&Qt::ControlModifier;
	keyModifier_[Prefs::AltKey] = km&Qt::AltModifier;

	// Determine whether we need to change Aten's currentmodel based on click position on the canvas
	Model* m = modelAt(rMouseDown_.x, rMouseDown_.y);
	if (m != aten_->currentModel())
	{
		aten_->setCurrentModel(m);
		atenWindow_->updateWidgets(AtenWindow::AllTarget-AtenWindow::ModelsTarget-AtenWindow::CanvasTarget);
	}
	
	// Get current active model
	Model* source = aten_->currentModelOrFrame();
	if (source == NULL)
	{
		printf("Pointless Viewer::mousePressEvent - no source model.\n");
		Messenger::exit("Viewer::mousePressEvent");
		return;
	}

	// Preliminary check to see if RMB was pressed over an atom - if so , show the popup menu and exit.
	if ((button == Prefs::RightButton) && editable_)
	{
		Atom* tempi = source->atomOnScreen(event->x(), contextHeight_-event->y());
		if (tempi != NULL)
		{
			atenWindow_->callContextMenu(tempi, event->globalX(), event->globalY());
			update();
			Messenger::exit("Viewer::mousePressEvent");
			return;
		}
	}

	// Determine if there is an atom under the mouse
	atomClicked_ = source->atomOnScreen(event->x(), contextHeight_-event->y());
	
	// Perform atom picking before entering mode (if required)
	if (pickEnabled_ && (atomClicked_ != NULL))
	{
		// Don't add the same atom more than once
		if (pickedAtoms_.contains(atomClicked_) == NULL)
		{
			pickedAtoms_.add(atomClicked_);
			Messenger::print(Messenger::Verbose, "Adding atom %i to canvas subselection.",atomClicked_);
		}
		else Messenger::print(Messenger::Verbose, "Atom %i is already in canvas subselection.",atomClicked_);
	}
	
	// Activate mode...
	beginMode(button);
	Messenger::exit("Viewer::mousePressEvent");
}

// Qt Signal (mouse release event)
void Viewer::mouseReleaseEvent(QMouseEvent *event)
{
	Messenger::enter("Viewer::mouseReleaseEvent");

	Prefs::MouseButton button;
	if (event->button() == Qt::LeftButton) button = Prefs::LeftButton;
	else if (event->button() == Qt::MidButton) button = Prefs::MiddleButton;
	else if (event->button() == Qt::RightButton) button = Prefs::RightButton;
	else
	{
		Messenger::exit("Viewer::mouseReleaseEvent");
		return;
	}
	
	// Only finalise the mode if the button is the same as the one that caused the mousepress event.
	if (mouseButton_[button])
	{
		rMouseUp_.set(event->x(), event->y(), 0.0);
		// Deactivate mode...
		endMode(button);
	}
	atomClicked_ = NULL;
	
	update();
	
	Messenger::exit("Viewer::mouseReleaseEvent");
}

// Qt Signal (mouse move event)
void Viewer::mouseMoveEvent(QMouseEvent *event)
{
	static Vec3<double> delta;

	Messenger::enter("Viewer::mouseMoveEvent");

	// Get current active model
	Model* source = aten_->currentModelOrFrame();
	if (source == NULL)
	{
		printf("Pointless Viewer::mouseMoveEvent - no source model.\n");
		Messenger::exit("Viewer::mouseMoveEvent");
		return;
	}

	// Perform action associated with mode (if any)
	if ((activeMode_ != UserAction::NoAction) || (selectedMode_ == UserAction::DrawFragmentAction))
	{
		// Calculate new delta.
		delta.set(event->x(), event->y(), 0.0);
		delta = delta - rMouseLast_;
		// Use activeMode_ to determine what needs to be performed
		switch (activeMode_)
		{
			case (UserAction::NoAction):
				break;
			case (UserAction::RotateXYAction):
				source->rotateView(delta.x/2.0,delta.y/2.0);
				break;
			case (UserAction::RotateZAction):
				source->zRotateView(delta.x/2.0);
				break;
			case (UserAction::TranslateAction):
				source->adjustCamera(delta.x/15.0, delta.y/15.0, 0.0);
				break;
			case (UserAction::ZoomAction):
				source->adjustZoom(delta.y < 0.0);
				break;
			case (UserAction::DrawFragmentAction):
				if (atenWindow_->fragmentsWidget->currentFragment() != NULL)
				{
					if (atomClicked_ == NULL) atenWindow_->fragmentsWidget->currentFragment()->rotateOrientedModel(delta.x/2.0,delta.y/2.0);
					else atenWindow_->fragmentsWidget->currentFragment()->rotateAnchoredModel(delta.x, delta.y);
					update();
				}
				break;
			case (UserAction::TransformRotateXYAction):
				source->rotateSelectionWorld(delta.x/2.0,delta.y/2.0);
				source->updateMeasurements();
				hasMoved_ = TRUE;
				break;
			case (UserAction::TransformRotateZAction):
				source->rotateSelectionZaxis(delta.x/2.0);
				source->updateMeasurements();
				hasMoved_ = TRUE;
				break;
			case (UserAction::TransformTranslateAction):
				delta.y = -delta.y;
				delta /= source->translateScale() * 2.0;
				source->translateSelectionWorld(delta);
				source->updateMeasurements();
				hasMoved_ = TRUE;
				break;
			default:
				break;
		}
	}

	rMouseLast_.set(event->x(), event->y(), 0.0);
	setFocus();

	update();

	Messenger::exit("Viewer::mouseMoveEvent");
}

// Qt Signal (mouse wheel event)
void Viewer::wheelEvent(QWheelEvent *event)
{
	Messenger::enter("Viewer::wheelEvent");
	
	// Get current active model
	Model* source = aten_->currentModelOrFrame();
	if (source == NULL)
	{
		printf("Pointless Viewer::wheelEvent - no source model.\n");
		Messenger::exit("Viewer::wheelEvent");
		return;
	}

	// Do the requested wheel action as defined in the control panel
	bool scrollup = event->delta() > 0;
	switch (prefs.mouseAction(Prefs::WheelButton))
	{
		case (Prefs::NoAction):
			break;
		case (Prefs::InteractAction):
			// Only act if the editable_ flag is set
			if (!editable_) break;
			useSelectedMode();
			break;
		case (Prefs::RotateAction):
			scrollup ? source->rotateView(1.0,0.0) : source->rotateView(-1.0,0.0);
			break;
		case (Prefs::TranslateAction):
			// Only act if the editable_ flag is set
			if (!editable_) break;
			break;
		case (Prefs::ZoomAction):
			source->adjustZoom(scrollup);
			break;
		default:
			break;
	}

	update();

	Messenger::exit("Viewer::wheelEvent");
}

// Return mouse coordinates at last mousedown event
Vec3<double> Viewer::rMouseDown()
{
	return rMouseDown_;
}

// Return mouse coordinates at last mouseup event
Vec3<double> Viewer::rMouseUp()
{
	return rMouseUp_;
}

// Return mouse coordinates at last mousemove event
Vec3<double> Viewer::rMouseLast()
{
	return rMouseLast_;
}
