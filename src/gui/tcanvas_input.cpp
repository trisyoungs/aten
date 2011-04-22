/*
	*** TCanvas input functions
	*** src/gui/tcanvas_input.cpp
	Copyright T. Youngs 2007-2011

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

#include "gui/tcanvas.uih"
#include "gui/fragments.h"
#include "gui/build.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "model/model.h"
#include "model/fragment.h"
#include "main/aten.h"

/*
// Mouse Input
*/

// Qt Signal (mouse press event)
void TCanvas::mousePressEvent(QMouseEvent *event)
{
	// Handle button presses (button down) from the mouse
	msg.enter("TCanvas::mousePressEvent");
	static Prefs::MouseButton button = Prefs::nMouseButtons;

	// End old mode if one is active (i.e. prevent mode overlap on different mouse buttons)
	if (activeMode_ != UserAction::NoAction) endMode(button);
	
	// Which mouse button was pressed?
	if (event->button() == Qt::LeftButton) button = Prefs::LeftButton;
	else if (event->button() == Qt::MidButton) button = Prefs::MiddleButton;
	else if (event->button() == Qt::RightButton) button = Prefs::RightButton;
	else
	{
		msg.exit("TCanvas::mousePressEvent");
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
	Model *m = modelAt(rMouseDown_.x, rMouseDown_.y);
	if (m != aten.currentModel())
	{
		aten.setCurrentModel(m);
		gui.update(GuiQt::AllTarget-GuiQt::ModelsTarget-GuiQt::CanvasTarget);
	}
	
	// Get current active model
	Model *source = displayModel();
	if (source == NULL)
	{
		printf("Pointless TCanvas::mousePressEvent - no source model.\n");
		msg.exit("TCanvas::mousePressEvent");
		return;
	}

	// Preliminary check to see if RMB was pressed over an atom - if so , show the popup menu and exit.
	if ((button == Prefs::RightButton) && editable_)
	{
		Atom *tempi = source->atomOnScreen(event->x(), event->y());
		if (tempi != NULL)
		{
			gui.callContextMenu(tempi, event->globalX(), event->globalY());
			postRedisplay();
			msg.exit("TCanvas::mousePressEvent");
			return;
		}
	}

	// Determine if there is an atom under the mouse
	atomClicked_ = source->atomOnScreen(event->x(), event->y());
	
	// Perform atom picking before entering mode (if required)
	if (pickEnabled_ && (atomClicked_ != NULL))
	{
		// Don't add the same atom more than once
		if (pickedAtoms_.contains(atomClicked_) == NULL)
		{
			pickedAtoms_.add(atomClicked_);
			msg.print(Messenger::Verbose,"Adding atom %i to canvas subselection.\n",atomClicked_);
		}
		else msg.print(Messenger::Verbose,"Atom %i is already in canvas subselection.\n",atomClicked_);
	}
	
	// Activate mode...
	beginMode(button);
	msg.exit("TCanvas::mousePressEvent");
}

// Qt Signal (mouse release event)
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
	
	// Only finalise the mode if the button is the same as the one that caused the mousepress event.
	if (mouseButton_[button])
	{
		rMouseUp_.set(event->x(), event->y(), 0.0);
		// Deactivate mode...
		endMode(button);
	}
	atomClicked_ = NULL;
	
	postRedisplay(FALSE,TRUE);
	
	msg.exit("TCanvas::mouseReleaseEvent");
}

// Qt Signal (mouse move event)
void TCanvas::mouseMoveEvent(QMouseEvent *event)
{
	static Vec3<double> delta;
	
	// Get current active model
	Model *source = displayModel();
	if (source == NULL)
	{
		printf("Pointless TCanvas::mouseMoveEvent - no source model.\n");
		msg.exit("TCanvas::mouseMoveEvent");
		return;
	}
	
	// Perform action associated with mode (if any)
	if ((activeMode_ != UserAction::NoAction) || (selectedMode_ == UserAction::DrawFragmentAction))
	{
		// Calculate new delta.
		delta.set(event->x(), event->y(),0.0);
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
				if (gui.fragmentsWidget->currentFragment() != NULL)
				{
					if (atomClicked_ == NULL) gui.fragmentsWidget->currentFragment()->rotateOrientedModel(delta.x/2.0,delta.y/2.0);
					else gui.fragmentsWidget->currentFragment()->rotateAnchoredModel(delta.x, delta.y);
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
		
		// Update display (only if mouse move filtering permits)
		if (mouseMoveCounter_.elapsed() > prefs.mouseMoveFilter())
		{
			mouseMoveCounter_.start();
			postRedisplay(FALSE,TRUE);
		}
	}
	rMouseLast_.set(event->x(), event->y(), 0.0);
	setFocus();
}

// Qt Signal (mouse wheel event)
void TCanvas::wheelEvent(QWheelEvent *event)
{
	msg.enter("TCanvas::modeScroll");
	
	// Get current active model
	Model *source = displayModel();
	if (source == NULL)
	{
		printf("Pointless TCanvas::modeScroll - no source model.\n");
		msg.exit("TCanvas::modeScroll");
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
	postRedisplay();
	msg.exit("TCanvas::modeScroll");
}

// Return mouse coordinates at last mousedown event
Vec3<double> TCanvas::rMouseDown()
{
	return rMouseDown_;
}

// Return mouse coordinates at last mouseup event
Vec3<double> TCanvas::rMouseUp()
{
	return rMouseUp_;
}

// Return mouse coordinates at last mousemove event
Vec3<double> TCanvas::rMouseLast()
{
	return rMouseLast_;
}

/*
// Key Input
*/

// Return state of specified keymodifier
bool TCanvas::keyModifier(Prefs::ModifierKey mk)
{
	return keyModifier_[mk];
}

// Qt Slot (key press event)
void TCanvas::keyPressEvent(QKeyEvent *event)
{
	// Check datamodel...
	bool refresh = FALSE, ignore = TRUE;
	Qt::KeyboardModifiers km = event->modifiers();
	keyModifier_[Prefs::ShiftKey] = km&Qt::ShiftModifier;
	keyModifier_[Prefs::CtrlKey] = km&Qt::ControlModifier;
	keyModifier_[Prefs::AltKey] = km&Qt::AltModifier;
	
	// Get current active model
	Model *source = displayModel();
	if (source == NULL)
	{
		printf("Pointless TCanvas::keyPressEvent - no source model.\n");
		msg.exit("TCanvas::keyPressEvent");
		return;
	}

	// Set some useful flags...
	bool manipulate = FALSE;
	bool nofold = gui.buildWidget->ui.PreventFoldCheck->isChecked();
	for (int n=0; n<3; n++)
	{
		if (keyModifier_[n])
		{
			switch (prefs.keyAction(Prefs::ModifierKey(n)))
			{
				case (Prefs::ManipulateKeyAction):
					manipulate = TRUE;
					break;
				default:
					break;
			}
		}
	}
	
	int n;
	
	switch (event->key())
	{
		case (Qt::Key_Left):
			if (keyModifier_[Prefs::CtrlKey])
			{
				printf("Why doesn't this ever get printed?\n");
				source->prepareTransform();
				source->beginUndoState("Rotate selection about world Y axis");
				source->rotateSelectionWorld(2.0,0.0);
				source->endUndoState();
				source->updateMeasurements();
				source->finalizeTransform(oldPositions_, "Transform Selection", nofold);
				gui.update(GuiQt::CanvasTarget);
			}
			else source->rotateView( keyModifier_[Prefs::ShiftKey] ? -1.0 : -10.0, 0.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Right):
			source->rotateView( keyModifier_[Prefs::ShiftKey] ? 1.0 : 10.0, 0.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Up):
			source->rotateView(0.0, keyModifier_[Prefs::ShiftKey] ? -1.0 : -10.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Down):
			source->rotateView(0.0, keyModifier_[Prefs::ShiftKey] ? 1.0 : 10.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Escape):
			gui.mainWindow()->cancelCurrentMode();
			refresh = TRUE;
			ignore = FALSE;
			break;
		// Cycle render styles
		case (Qt::Key_F8):
			n = prefs.renderStyle() + 1;
			if (n == Atom::nDrawStyles) n = 0;
			gui.mainWindow()->setActiveStyleAction( (Atom::DrawStyle) n);
			ignore = FALSE;
			break;
		// Cycle colouring styles
		case (Qt::Key_F9):
			n = prefs.colourScheme() + 1;
			if (n == Prefs::nColouringSchemes) n = 0;
			gui.mainWindow()->setActiveSchemeAction( (Prefs::ColouringScheme) n);
			ignore = FALSE;
			break;
		default:
			break;
	}
	
	// Mode-specific
	switch (selectedMode_)
	{
		case (UserAction::DrawFragmentAction):
			// Cycle link atom....
			if (keyModifier_[Prefs::AltKey])
			{
				Fragment *frag = gui.fragmentsWidget->currentFragment();
				if (frag == NULL) break;
				frag->cycleLinkAtom();
				refresh = TRUE;
			}
			// Refresh if Shift status has changed
			if (keyModifier_[Prefs::ShiftKey]) refresh = TRUE;
			if (keyModifier_[Prefs::CtrlKey])
			{
				refresh = TRUE;
				gui.fragmentsWidget->increaseBondId();
			}
			break;
		default:
			break;
	}
	// Update display if necessary
	if (refresh) postRedisplay();
	if (ignore) event->ignore();
}

// Qt Slot (key release event)
void TCanvas::keyReleaseEvent(QKeyEvent *event)
{
	// Set keystates
	bool oldshift = keyModifier_[Prefs::ShiftKey];
	bool oldctrl = keyModifier_[Prefs::CtrlKey];
	bool oldalt = keyModifier_[Prefs::AltKey];
	Qt::KeyboardModifiers km = event->modifiers();
	keyModifier_[Prefs::ShiftKey] = km&Qt::ShiftModifier;
	keyModifier_[Prefs::CtrlKey] = km&Qt::ControlModifier;
	keyModifier_[Prefs::AltKey] = km&Qt::AltModifier;
	
	// Get current active model
	Model *source = displayModel();
	if (source == NULL)
	{
		printf("Pointless TCanvas::keyReleaseEvent - no source model.\n");
		msg.exit("TCanvas::keyReleaseEvent");
		return;
	}

	// Set some useful flags...
	bool manipulate = FALSE;
	for (int n=0; n<3; n++)
	{
		if (keyModifier_[n])
		{
			switch (prefs.keyAction(Prefs::ModifierKey(n)))
			{
				case (Prefs::ManipulateKeyAction):
					manipulate = TRUE;
					break;
				default:
					break;
			}
		}
	}
	
	// Mode-specific
	switch (selectedMode_)
	{
		case (UserAction::DrawFragmentAction):
			// Refresh if Shift status has changed
			if (keyModifier_[Prefs::ShiftKey] != oldshift) postRedisplay();
			break;
		default:
			break;
	}

	event->ignore();
}

/*
// User Actions
*/

// Set selected mode
void TCanvas::setSelectedMode(UserAction::Action ua, int atomsToPick, void (*callback)(Reflist<Atom,int>*))
{
	msg.enter("TCanvas::setSelectedMode");

	// Get current active model
	Model *source = displayModel();
	if (source == NULL)
	{
		printf("Pointless TCanvas::setSelectedMode - no source model.\n");
		msg.exit("TCanvas::setSelectedMode");
		return;
	}
	
	// If previous action was a Pick action then finalise it first
	if (selectedMode_ >= UserAction::ShiftPickVectorAction)
	{
		// If a previous callback was defined then call it before we move on
		if (pickAtomsCallback_ != NULL) (*pickAtomsCallback_)(&pickedAtoms_);
		pickAtomsCallback_ = NULL;
		pickedAtoms_.clear();
		nAtomsToPick_ = -1;
	}
	
	// Store picking information in case that's what we're about to do
	actionBeforePick_ = selectedMode_;
	pickAtomsCallback_ = callback;
	nAtomsToPick_ = atomsToPick;

	printf("SETSELECTEDMODE %i, natomstopick = %i\n", selectedMode_, nAtomsToPick_);
	
	// Clear any old selection (from e.g. bonding, measurements....)
	clearPicked();
	
	// Prepare canvas for the selected action
	switch (ua)
	{
		case (UserAction::ShiftPickVectorAction):
		case (UserAction::RotatePickAxisAction):
		case (UserAction::TransformPickAAction):
		case (UserAction::TransformPickBAction):
		case (UserAction::TransformPickCAction):
		case (UserAction::ConvertSourcePickAAction):
		case (UserAction::ConvertSourcePickBAction):
		case (UserAction::ConvertSourcePickCAction):
		case (UserAction::ConvertTargetPickAAction):
		case (UserAction::ConvertTargetPickBAction):
		case (UserAction::ConvertTargetPickCAction):
		case (UserAction::MeasureDistanceAction):
		case (UserAction::MeasureAngleAction):
		case (UserAction::MeasureTorsionAction):
		case (UserAction::DrawBondSingleAction):
		case (UserAction::DrawBondDoubleAction):
		case (UserAction::DrawBondTripleAction):
		case (UserAction::DrawDeleteBondAction):
			pickEnabled_ = TRUE;
			pickedAtoms_.clear();
			break;
		default:
			pickEnabled_ = FALSE;
			break;
	}

	// Finally, set the mode and refresh
	selectedMode_ = ua;
	gui.mainWindow()->setActiveUserAction(ua);
	
	// Change mouse cursor depending on mode
	if (selectedMode_ == UserAction::SelectAction) setCursor(Qt::ArrowCursor);
	else setCursor(Qt::CrossCursor);

	gui.update(GuiQt::CanvasTarget+GuiQt::StatusBarTarget);
	msg.exit("TCanvas::setSelectedMode");
}

// Set the active mode to the current user mode
void TCanvas::useSelectedMode()
{
	activeMode_ = selectedMode_;
}

// Return the currently selected mode
UserAction::Action TCanvas::selectedMode() const
{
	return selectedMode_;
}

// Return the currently active mode
UserAction::Action TCanvas::activeMode() const
{
	return activeMode_;
}

// Set current drawing element
void TCanvas::setSketchElement(short int el)
{
	sketchElement_ = el;
}

// Return current drawing element
short int TCanvas::sketchElement() const
{
	return sketchElement_;
}

// Current drawing depth for certain tools
double TCanvas::currentDrawDepth()
{
	return currentDrawDepth_;
}

// Set whether to accept editing actions (i.e. anything other than view manipulation)
void TCanvas::setEditable(bool b)
{
	editable_ = b;
}

// Return whether to accept editing actions (i.e. anything other than view manipulation)
bool TCanvas::editable()
{
	return editable_;
}

// Begin Mode
void TCanvas::beginMode(Prefs::MouseButton button)
{
	msg.enter("TCanvas::beginMode");
	static bool manipulate, zrotate;
	static int n;
	static Atom *i;
	// Do the requested action as defined in the control panel, but only if another action
	// isn't currently in progress. Set the UserAction based on the mouse button that sent
	// the signal, current selection / draw modes and key modifier states.

	// Get current active model
	Model *source = displayModel();
	if (source == NULL)
	{
		printf("Pointless TCanvas::beginMode - no source model.\n");
		msg.exit("TCanvas::beginMode");
		return;
	}
	
	// Note the mouse button pressed
	mouseButton_[button] = TRUE;
	// Check for modifier keys
	zrotate = FALSE;
	manipulate = FALSE;
	hasMoved_ = FALSE;

	for (n=0; n<3; n++)
	{
		if (keyModifier_[n])
		{
			switch (prefs.keyAction(Prefs::ModifierKey(n)))
			{
				case (Prefs::ManipulateKeyAction):
					manipulate = TRUE;
					break;
				case (Prefs::ZrotateKeyAction):
					zrotate = TRUE;
					break;
				default:
					break;
			}
		}
	}
	// Now prepare for the action
	if (activeMode_ == UserAction::NoAction)
	{
		switch (prefs.mouseAction(button))
		{
			// Main interactor - selection, sketching, measuring
			case (Prefs::InteractAction):
				// Only act if the editable_ flag is set
				if (!editable_) break;
				useSelectedMode();
				// Some modes require actions to be done when the button is first depressed
				switch (activeMode_)
				{
					case (UserAction::DrawChainAction):
						// If there is currently no atom under the mouse, draw one...
						if (atomClicked_ == NULL)
						{
							source->beginUndoState("Draw Chain");
							currentDrawDepth_ = prefs.drawDepth();
							i = source->addAtom(sketchElement_, source->screenToModel(rMouseDown_.x, rMouseDown_.y, currentDrawDepth_));
							source->endUndoState();
							atomClicked_ = i;
						}
						else currentDrawDepth_ = source->modelToWorld(atomClicked_->r()).z;
						break;
					default:
						break;
				}
				break;
			case (Prefs::RotateAction):
				// Check for multiple key modifiers first.
				if (manipulate && zrotate && editable_) activeMode_ = UserAction::TransformRotateZAction;
				else if (manipulate && editable_) activeMode_ = UserAction::TransformRotateXYAction;
				else if (zrotate) activeMode_ = UserAction::RotateZAction;
				else activeMode_ = UserAction::RotateXYAction;
				break;
			case (Prefs::ZoomAction):
				activeMode_ = UserAction::ZoomAction;
				break;
			case (Prefs::TranslateAction):
				if (manipulate && editable_) activeMode_ = UserAction::TransformTranslateAction;
				else activeMode_ = UserAction::TranslateAction;
				break;
				
			default:
				break;
		}
		// If we're manipulating, prepare the transform
		if (manipulate)
		{
			/* We don't begin an undostate here - this will be done in endMode().
			Instead, store pointers to all selected atoms in a Reflist, along
			with their current positions.
			*/
			oldPositions_.clear();
			for (Refitem<Atom,int> *ri = source->selection(); ri != NULL; ri = ri->next) oldPositions_.add(ri->item, ri->item->r());
			source->prepareTransform();
		}
	}
	postRedisplay();
	msg.exit("TCanvas::beginMode");
}

// End Mode
void TCanvas::endMode(Prefs::MouseButton button)
{
	// Finalize the current action on the model
	msg.enter("TCanvas::endMode");
	double area, radius;
	Vec4<double> screenr;
	Atom *atoms[4], *i;
	Bond *b;
	Bond::BondType bt;
	Fragment *frag;
	
	// Get current active model
	Model *source = displayModel();
	if (source == NULL)
	{
		printf("Pointless TCanvas::endMode - no source model.\n");
		msg.exit("TCanvas::endMode");
		return;
	}
	
	// Store modifier states for convenience
	bool shifted = keyModifier_[Prefs::ShiftKey];
	bool ctrled = keyModifier_[Prefs::CtrlKey];
	bool modded = (shifted || ctrled);
	bool nofold = gui.buildWidget->ui.PreventFoldCheck->isChecked();
	
	// Reset mouse button flag
	mouseButton_[button] = FALSE;
	// Copy the current mode and reset it so we redraw properly
	UserAction::Action endingMode = activeMode_;
	activeMode_ = UserAction::NoAction;
	// Finalize the action
	switch (endingMode)
	{
		// No action
		case (UserAction::NoAction):
			break;
		// Plain atom / box select
		case (UserAction::SelectAction):
			area = fabs(rMouseUp_.x - rMouseDown_.x) * fabs(rMouseUp_.y - rMouseDown_.y);
			source->beginUndoState("Change Selection");
			// If neither shift nor ctrl are not held down, deselect the current selection
			if (!modded) source->selectNone();
			// Do either point select or box select based on the size of the selected area
			if (area > 50.0) source->selectBox(rMouseDown_.x, rMouseDown_.y, rMouseUp_.x, rMouseUp_.y, ctrled);
			else if (atomClicked_ != NULL)
			{
				if (shifted) source->selectionToggle(atomClicked_);
				else if (ctrled) source->deselectAtom(atomClicked_);
				else source->selectAtom(atomClicked_);
			}
			source->endUndoState();
			gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget+GuiQt::GeometryTarget);
			break;
		// Other selection operations
		case (UserAction::SelectMoleculeAction):
			source->beginUndoState("Select Molecule");
			if (!modded) source->selectNone();
			if (atomClicked_ != NULL)	source->selectTree(atomClicked_, FALSE, ctrled);
			source->endUndoState();
			gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget+GuiQt::GeometryTarget);
			break;
		case (UserAction::SelectElementAction):
			source->beginUndoState("Select Element");
			if (!modded) source->selectNone();
			if (atomClicked_ != NULL) source->selectElement(atomClicked_, FALSE, ctrled);
			source->endUndoState();
			gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget+GuiQt::GeometryTarget);
			break;
		case (UserAction::SelectRadialAction):
			source->beginUndoState("Select Radial");
			if (!modded) source->selectNone();
			if (atomClicked_ != NULL)
			{
				radius = (rMouseDown_-rMouseUp_).magnitude();
				source->modelToWorld(atomClicked_->r(), &screenr, prefs.styleRadius(atomClicked_));
				radius /= screenr.w * prefs.styleRadius(atomClicked_);
				source->selectRadial(atomClicked_,radius);
			}
			source->endUndoState();
			gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget+GuiQt::SelectTarget+GuiQt::GeometryTarget);
			break;
		// Measurements
		case (UserAction::MeasureDistanceAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 2) break;
			source->beginUndoState("Measure Distance");
			pickedAtoms_.fillArray(2,atoms);
			source->addDistanceMeasurement(atoms[0],atoms[1]);
			source->endUndoState();
			pickedAtoms_.clear();
			gui.update(GuiQt::CanvasTarget);
			break;
		case (UserAction::MeasureAngleAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 3) break;
			source->beginUndoState("Measure Angle");
			pickedAtoms_.fillArray(3,atoms);
			source->addAngleMeasurement(atoms[0],atoms[1],atoms[2]);
			source->endUndoState();
			pickedAtoms_.clear();
			gui.update(GuiQt::CanvasTarget);
			break;
		case (UserAction::MeasureTorsionAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 4) break;
			source->beginUndoState("Measure Torsion");
			pickedAtoms_.fillArray(4,atoms);
			source->addTorsionMeasurement(atoms[0],atoms[1],atoms[2],atoms[3]);
			source->endUndoState();
			pickedAtoms_.clear();
			gui.update(GuiQt::CanvasTarget);
			break;
		// Draw single atom
		case (UserAction::DrawAtomAction):
			// Make sure we don't draw on top of an existing atom
			if (atomClicked_ == NULL)
			{
				source->beginUndoState("Draw Atom");
				currentDrawDepth_ = prefs.drawDepth();
				source->addAtom(sketchElement_, source->screenToModel(rMouseDown_.x, rMouseDown_.y, currentDrawDepth_));
				source->endUndoState();
			}
			gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
			break;
		// Draw chains of atoms
		case (UserAction::DrawChainAction):
			// If there is no atom under the mouse we draw one
			i = source->atomOnScreen(rMouseUp_.x,rMouseUp_.y);
			if ((atomClicked_ == i) && (i != NULL)) break;
			source->beginUndoState("Draw Chain");
			if (i == NULL)
			{
				// No atom under the mouse, so draw an atom at previous draw depth
				i = source->addAtom(sketchElement_, source->screenToModel(rMouseUp_.x, rMouseUp_.y, currentDrawDepth_));
			}
			// Now bond the atoms, unless atomClicked_ and i are the same (i.e. the button was clicked and not moved)
			if (atomClicked_ != i)
			{
				// Search for existing bond between atoms
				b = i->findBond(atomClicked_);
				if (b == NULL) bt = Bond::Single;
				else
				{
					bt = Bond::increase(b->type());
					source->unbondAtoms(i,atomClicked_);
				}
				source->bondAtoms(i,atomClicked_,bt);
			}
			source->endUndoState();
			gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
			break;
		// Draw fragments
		case (UserAction::DrawFragmentAction):
			frag = gui.fragmentsWidget->currentFragment();
			if (frag == NULL) break;
			if (atomClicked_ != NULL)
			{
				source->beginUndoState("Draw Attached Fragment");
				frag->pasteAnchoredModel(atomClicked_, keyModifier_[Prefs::ShiftKey], gui.fragmentsWidget->bondId(), source, gui.fragmentsWidget->ui.AdjustBondLengthCheck->isChecked());
			}
			else
			{
				// No atom under the moust pointer, so draw on at the prefs drawing depth in its current orientation
				source->beginUndoState("Draw Fragment");
				frag->pasteOrientedModel(source->screenToModel(rMouseDown_.x, rMouseDown_.y, prefs.drawDepth()), source);
			}
			source->endUndoState();
			gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
			break;
		case (UserAction::DrawTransmuteAction):
			if (atomClicked_ == NULL) break;
			source->beginUndoState("Transmute");
			// If SHIFT was held, transmute all atoms of the same element...
			if (shifted)
			{
				int element = atomClicked_->element();
				for (Atom *i = source->atoms(); i != NULL; i = i->next) if (i->element() == element) source->transmuteAtom(i,sketchElement_);
			}
			else source->transmuteAtom(atomClicked_, sketchElement_);
			source->endUndoState();
			gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
			break;
		case (UserAction::DrawDeleteAction):
			if (shifted)
			{
				source->beginUndoState("Delete Bonds to Atom");
				while (atomClicked_->bonds() != NULL)
				{
					source->unbondAtoms(atomClicked_, atomClicked_->bonds()->item->partner(atomClicked_));
				}
				source->endUndoState();
			}
			else
			{
				source->beginUndoState("Delete Atom");
				source->deleteAtom(atomClicked_);
				source->endUndoState();
			}
			gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
			break;
		case (UserAction::DrawProbeAction):
			if (atomClicked_ != NULL) atomClicked_->print();
			break;
		// Bonding
		case (UserAction::DrawBondSingleAction):
		case (UserAction::DrawBondDoubleAction):
		case (UserAction::DrawBondTripleAction):
			if (pickedAtoms_.nItems() == 0) break;
			// Must be two atoms in subselection to continue, or we must be hovering over a different atom (which we'll add to the list)
			if (pickedAtoms_.nItems() == 1)
			{
				i = source->atomOnScreen(rMouseUp_.x,rMouseUp_.y);
				if (pickedAtoms_.last()->item != i) pickedAtoms_.add(i);
			}
			if (pickedAtoms_.nItems() != 2) break;
			pickedAtoms_.fillArray(2,atoms);
			b = atoms[0]->findBond(atoms[1]);
			if (b == NULL)
			{
				source->beginUndoState("Bond Atoms");
				source->bondAtoms(atoms[0],atoms[1],Bond::BondType(endingMode-UserAction::DrawBondSingleAction+1));
				source->endUndoState();
			}
			else
			{
				source->beginUndoState("Change Bond");
				source->changeBond(b,Bond::BondType(endingMode-UserAction::DrawBondSingleAction+1));
				source->endUndoState();
			}
			pickedAtoms_.clear();
			gui.update(GuiQt::CanvasTarget);
			break;
		// Delete bond
		case (UserAction::DrawDeleteBondAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 2) break;
			pickedAtoms_.fillArray(2,atoms);
			if (atoms[0]->findBond(atoms[1]) != NULL)
			{
				source->beginUndoState("Delete Bond");
				source->unbondAtoms(atoms[0],atoms[1]);
				source->endUndoState();
			}
			pickedAtoms_.clear();
			gui.update(GuiQt::CanvasTarget);
			break;
		// Misc
		case (UserAction::DrawAddHydrogenAction):
			if (atomClicked_ != NULL)
			{
				source->beginUndoState("Add Hydrogen to Atom");
				source->hydrogenSatisfy(atomClicked_);
				source->endUndoState();
				gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
			}
			break;
		// Model transformations
		case (UserAction::TransformRotateXYAction):
		case (UserAction::TransformRotateZAction):
		case (UserAction::TransformTranslateAction):
			// Clear list of oldPositions_ if nothing was moved
			if (!hasMoved_) oldPositions_.clear();
			source->finalizeTransform(oldPositions_, "Transform Selection", nofold);
			gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
			break;
		// View changes (no action)
		case (UserAction::RotateXYAction):
		case (UserAction::RotateZAction):
		case (UserAction::TranslateAction):
		case (UserAction::ZoomAction):
			break;
		// Manual picking modes (for axis definitions etc.)
		case (UserAction::ShiftPickVectorAction):
		case (UserAction::RotatePickAxisAction):
		case (UserAction::TransformPickAAction):
		case (UserAction::TransformPickBAction):
		case (UserAction::TransformPickCAction):
		case (UserAction::ConvertSourcePickAAction):
		case (UserAction::ConvertSourcePickBAction):
		case (UserAction::ConvertSourcePickCAction):
		case (UserAction::ConvertTargetPickAAction):
		case (UserAction::ConvertTargetPickBAction):
		case (UserAction::ConvertTargetPickCAction):
			printf("Current number of atoms picked = %i, wanted = %i\n", pickedAtoms_.nItems(), nAtomsToPick_);
			// Have we picked the right number of atoms?
			if (pickedAtoms_.nItems() != nAtomsToPick_) break;
			// If a previous callback was defined then call it before we move on
			if (pickAtomsCallback_ != NULL) (*pickAtomsCallback_)(&pickedAtoms_);
			pickAtomsCallback_ = NULL;
			gui.mainWindow()->setActiveUserAction(actionBeforePick_);
			pickedAtoms_.clear();
			nAtomsToPick_ = -1;
			break;
		default:
			printf("No endMode handler defined for UserAction %i.\n", endingMode);
			break;
	}
	msg.exit("UserAction::endMode");
}

// Returns the atom currently under the mouse
Atom *TCanvas::atomClicked()
{
	return atomClicked_;
}

// Clears the list of picked atoms
void TCanvas::clearPicked()
{
	pickedAtoms_.clear();
}

// Return start of picked atom list
Refitem<Atom,int> *TCanvas::pickedAtoms()
{
	return pickedAtoms_.first();
}
