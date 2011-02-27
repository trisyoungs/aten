/*
	*** Qt canvas input functions
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
	
	// Preliminary check to see if RMB was pressed over an atom - if so , show the popup menu and exit.
	if ((button == Prefs::RightButton) && editable_)
	{
		Atom *tempi = displayModel_->atomOnScreen(event->x(), event->y());
		if (tempi != NULL)
		{
			gui.callContextMenu(tempi, event->globalX(), event->globalY());
			postRedisplay();
			msg.exit("TCanvas::mousePressEvent");
			return;
		}
	}

	// Determine if there is an atom under the mouse
	atomClicked_ = displayModel_->atomOnScreen(event->x(), event->y());
	
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
	
	msg.exit("TCanvas::mouseReleaseEvent");
}

// Qt Signal (mouse move event)
void TCanvas::mouseMoveEvent(QMouseEvent *event)
{
	static Vec3<double> delta;
	
	// Perform action associated with mode (if any)
	if ((activeMode_ != UserAction::NoAction) || (selectedMode_ == UserAction::DrawFragmentAction))
	{
		if (displayModel_ == NULL)
		{
			printf("Pointless TCanvas::modeMotion - datamodel == NULL.\n");
			msg.exit("TCanvas::modeMotion");
			return;
		}
		
		// Calculate new delta.
		delta.set(event->x(), event->y(),0.0);
		delta = delta - rMouseLast_;
		// Use activeMode_ to determine what needs to be performed
		switch (activeMode_)
		{
			case (UserAction::NoAction):
				break;
			case (UserAction::RotateXYAction):
				displayModel_->rotateView(delta.x/2.0,delta.y/2.0);
				break;
			case (UserAction::RotateZAction):
				displayModel_->zRotateView(delta.x/2.0);
				break;
			case (UserAction::TranslateAction):
				displayModel_->adjustCamera(delta.x/15.0, delta.y/15.0, 0.0);
				break;
			case (UserAction::ZoomAction):
				displayModel_->adjustZoom(delta.y < 0.0);
				break;
			case (UserAction::DrawFragmentAction):
				if (gui.fragmentsWidget->currentFragment() != NULL)
				{
					if (atomClicked_ == NULL) gui.fragmentsWidget->currentFragment()->rotateOrientedModel(delta.x/2.0,delta.y/2.0);
					else gui.fragmentsWidget->currentFragment()->rotateAnchoredModel(delta.x, delta.y);
				}
				break;
			case (UserAction::TransformRotateXYAction):
				displayModel_->rotateSelectionWorld(delta.x/2.0,delta.y/2.0);
				displayModel_->updateMeasurements();
				hasMoved_ = TRUE;
				break;
			case (UserAction::TransformRotateZAction):
				displayModel_->rotateSelectionZaxis(delta.x/2.0);
				displayModel_->updateMeasurements();
				hasMoved_ = TRUE;
				break;
			case (UserAction::TransformTranslateAction):
				delta.y = -delta.y;
				delta /= displayModel_->translateScale() * 2.0;
				displayModel_->translateSelectionWorld(delta);
				displayModel_->updateMeasurements();
				hasMoved_ = TRUE;
				break;
			default:
				break;
		}
		
		// Update display (only if mouse move filtering permits)
		if (mouseMoveCounter_.elapsed() > prefs.mouseMoveFilter())
		{
			mouseMoveCounter_.start();
			postRedisplay();
		}
	}
	rMouseLast_.set(event->x(), event->y(), 0.0);
	setFocus();
}

// Qt Signal (mouse wheel event)
void TCanvas::wheelEvent(QWheelEvent *event)
{
	msg.enter("TCanvas::modeScroll");
	if (displayModel_ == NULL)
	{
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
			scrollup ? displayModel_->rotateView(1.0,0.0) : displayModel_->rotateView(-1.0,0.0);
			break;
		case (Prefs::TranslateAction):
			// Only act if the editable_ flag is set
			if (!editable_) break;
			break;
		case (Prefs::ZoomAction):
			displayModel_->adjustZoom(scrollup);
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
	if (displayModel_ == NULL) return;
	bool refresh = FALSE, ignore = TRUE;
	Qt::KeyboardModifiers km = event->modifiers();
	keyModifier_[Prefs::ShiftKey] = km&Qt::ShiftModifier;
	keyModifier_[Prefs::CtrlKey] = km&Qt::ControlModifier;
	keyModifier_[Prefs::AltKey] = km&Qt::AltModifier;
	
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
				displayModel_->prepareTransform();
				displayModel_->beginUndoState("Rotate selection about world Y axis");
				displayModel_->rotateSelectionWorld(2.0,0.0);
				displayModel_->endUndoState();
				displayModel_->updateMeasurements();
				displayModel_->finalizeTransform(oldPositions_, "Transform Selection", nofold);
				gui.update();
			}
			else displayModel_->rotateView( keyModifier_[Prefs::ShiftKey] ? -1.0 : -10.0, 0.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Right):
			displayModel_->rotateView( keyModifier_[Prefs::ShiftKey] ? 1.0 : 10.0, 0.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Up):
			displayModel_->rotateView(0.0, keyModifier_[Prefs::ShiftKey] ? -1.0 : -10.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Down):
			displayModel_->rotateView(0.0, keyModifier_[Prefs::ShiftKey] ? 1.0 : 10.0);
			refresh = TRUE;
			ignore = FALSE;
			break;
		case (Qt::Key_Escape):
			gui.mainWindow->cancelCurrentMode();
			refresh = TRUE;
			ignore = FALSE;
			break;
		// Cycle render styles
		case (Qt::Key_F8):
			n = prefs.renderStyle() + 1;
			if (n == Atom::nDrawStyles) n = 0;
			gui.mainWindow->setActiveStyleAction( (Atom::DrawStyle) n);
			ignore = FALSE;
			break;
		// Cycle colouring styles
		case (Qt::Key_F9):
			n = prefs.colourScheme() + 1;
			if (n == Prefs::nColouringSchemes) n = 0;
			gui.mainWindow->setActiveSchemeAction( (Prefs::ColouringScheme) n);
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
	if (displayModel_ == NULL)
	{
		msg.exit("Canvas::setSelectedMode");
		return;
	}
	
	// If previous action was a Pick action then finalise it first
	if (selectedMode_ >= UserAction::PickPositionVectorShiftAction)
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

	// Clear any old selection (from e.g. bonding, measurements....)
	clearPicked();
	
	// Prepare canvas for the selected action
	switch (ua)
	{
		case (UserAction::PickPositionVectorShiftAction):
		case (UserAction::PickTransformRotateAxisAction):
		case (UserAction::PickTransformDefineAAction):
		case (UserAction::PickTransformDefineBAction):
		case (UserAction::PickTransformDefineCAction):
		case (UserAction::PickConvertDefineSourceAAction):
		case (UserAction::PickConvertDefineSourceBAction):
		case (UserAction::PickConvertDefineSourceCAction):
		case (UserAction::PickConvertDefineTargetAAction):
		case (UserAction::PickConvertDefineTargetBAction):
		case (UserAction::PickConvertDefineTargetCAction):
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
	// Change mouse cursor depending on mode
	if (selectedMode_ == UserAction::SelectAction) setCursor(Qt::ArrowCursor);
	else setCursor(Qt::CrossCursor);
	postRedisplay();
	gui.updateStatusBar();
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
	// Set mouse flag and get state of modifier keys
	if (displayModel_ == NULL)
	{
		printf("Pointless TCanvas::beginMode - datamodel == NULL.\n");
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
							displayModel_->beginUndoState("Draw Chain");
							currentDrawDepth_ = prefs.drawDepth();
							i = displayModel_->addAtom(sketchElement_, screenToModel(rMouseDown_.x, rMouseDown_.y, currentDrawDepth_));
							displayModel_->endUndoState();
							atomClicked_ = i;
						}
						else currentDrawDepth_ = modelToWorld(atomClicked_->r()).z;
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
			for (Refitem<Atom,int> *ri = displayModel_->selection(); ri != NULL; ri = ri->next) oldPositions_.add(ri->item, ri->item->r());
			displayModel_->prepareTransform();
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
	if (displayModel_ == NULL)
	{
		printf("Pointless TCanvas::endMode - datamodel == NULL.\n");
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
			displayModel_->beginUndoState("Change Selection");
			// If neither shift nor ctrl are not held down, deselect the current selection
			if (!modded) displayModel_->selectNone();
			// Do either point select or box select based on the size of the selected area
			if (area > 50.0) displayModel_->selectBox(rMouseDown_.x, rMouseDown_.y, rMouseUp_.x, rMouseUp_.y, ctrled);
			else if (atomClicked_ != NULL)
			{
				if (shifted) displayModel_->selectionToggle(atomClicked_);
				else if (ctrled) displayModel_->deselectAtom(atomClicked_);
				else displayModel_->selectAtom(atomClicked_);
			}
			displayModel_->endUndoState();
			gui.update(GuiQt::AtomsTarget);
			break;
		// Other selection operations
		case (UserAction::SelectMoleculeAction):
			displayModel_->beginUndoState("Select Molecule");
			if (!modded) displayModel_->selectNone();
			if (atomClicked_ != NULL)	displayModel_->selectTree(atomClicked_, FALSE, ctrled);
			displayModel_->endUndoState();
			gui.update(GuiQt::AtomsTarget);
			break;
		case (UserAction::SelectElementAction):
			displayModel_->beginUndoState("Select Element");
			if (!modded) displayModel_->selectNone();
			if (atomClicked_ != NULL) displayModel_->selectElement(atomClicked_, FALSE, ctrled);
			displayModel_->endUndoState();
			gui.update(GuiQt::AtomsTarget);
			break;
		case (UserAction::SelectRadialAction):
			displayModel_->beginUndoState("Select Radial");
			if (!modded) displayModel_->selectNone();
			if (atomClicked_ != NULL)
			{
				radius = (rMouseDown_-rMouseUp_).magnitude();
				modelToWorld(atomClicked_->r(), &screenr, prefs.styleRadius(atomClicked_));
				radius /= screenr.w * prefs.styleRadius(atomClicked_);
				displayModel_->selectRadial(atomClicked_,radius);
			}
			displayModel_->endUndoState();
			gui.update(GuiQt::AtomsTarget);
			break;
		// Measurements
		case (UserAction::MeasureDistanceAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 2) break;
			displayModel_->beginUndoState("Measure Distance");
			pickedAtoms_.fillArray(2,atoms);
			displayModel_->addDistanceMeasurement(atoms[0],atoms[1]);
			displayModel_->endUndoState();
			pickedAtoms_.clear();
			gui.update();
			break;
		case (UserAction::MeasureAngleAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 3) break;
			displayModel_->beginUndoState("Measure Angle");
			pickedAtoms_.fillArray(3,atoms);
			displayModel_->addAngleMeasurement(atoms[0],atoms[1],atoms[2]);
			displayModel_->endUndoState();
			pickedAtoms_.clear();
			gui.update();
			break;
		case (UserAction::MeasureTorsionAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 4) break;
			displayModel_->beginUndoState("Measure Torsion");
			pickedAtoms_.fillArray(4,atoms);
			displayModel_->addTorsionMeasurement(atoms[0],atoms[1],atoms[2],atoms[3]);
			displayModel_->endUndoState();
			pickedAtoms_.clear();
			gui.update();
			break;
		// Draw single atom
		case (UserAction::DrawAtomAction):
			// Make sure we don't draw on top of an existing atom
			if (atomClicked_ == NULL)
			{
				displayModel_->beginUndoState("Draw Atom");
				currentDrawDepth_ = prefs.drawDepth();
				displayModel_->addAtom(sketchElement_, screenToModel(rMouseDown_.x, rMouseDown_.y, currentDrawDepth_));
				displayModel_->endUndoState();
			}
			gui.update(GuiQt::AtomsTarget);
			break;
		// Draw chains of atoms
		case (UserAction::DrawChainAction):
			// If there is no atom under the mouse we draw one
			i = displayModel_->atomOnScreen(rMouseUp_.x,rMouseUp_.y);
			if ((atomClicked_ == i) && (i != NULL)) break;
			displayModel_->beginUndoState("Draw Chain");
			if (i == NULL)
			{
				// No atom under the mouse, so draw an atom at previous draw depth
				i = displayModel_->addAtom(sketchElement_, screenToModel(rMouseUp_.x, rMouseUp_.y, currentDrawDepth_));
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
					displayModel_->unbondAtoms(i,atomClicked_);
				}
				displayModel_->bondAtoms(i,atomClicked_,bt);
			}
			displayModel_->endUndoState();
			gui.update(GuiQt::AtomsTarget);
			break;
		// Draw fragments
		case (UserAction::DrawFragmentAction):
			frag = gui.fragmentsWidget->currentFragment();
			if (frag == NULL) break;
			if (atomClicked_ != NULL)
			{
				displayModel_->beginUndoState("Draw Attached Fragment");
				frag->pasteAnchoredModel(atomClicked_, keyModifier_[Prefs::ShiftKey], gui.fragmentsWidget->bondId(), displayModel_, gui.fragmentsWidget->ui.AdjustBondLengthCheck->isChecked());
			}
			else
			{
				// No atom under the moust pointer, so draw on at the prefs drawing depth in its current orientation
				displayModel_->beginUndoState("Draw Fragment");
				frag->pasteOrientedModel(screenToModel(rMouseDown_.x, rMouseDown_.y, prefs.drawDepth()), displayModel_);
			}
			displayModel_->endUndoState();
			gui.update(GuiQt::AtomsTarget);
			break;
		case (UserAction::DrawTransmuteAction):
			if (atomClicked_ == NULL) break;
			displayModel_->beginUndoState("Transmute");
			// If SHIFT was held, transmute all atoms of the same element...
			if (shifted)
			{
				int element = atomClicked_->element();
				for (Atom *i = displayModel_->atoms(); i != NULL; i = i->next) if (i->element() == element) displayModel_->transmuteAtom(i,sketchElement_);
			}
			else displayModel_->transmuteAtom(atomClicked_, sketchElement_);
			displayModel_->endUndoState();
			gui.update(GuiQt::AtomsTarget);
			break;
		case (UserAction::DrawDeleteAction):
			if (shifted)
			{
				displayModel_->beginUndoState("Delete Bonds to Atom");
				while (atomClicked_->bonds() != NULL)
				{
					displayModel_->unbondAtoms(atomClicked_, atomClicked_->bonds()->item->partner(atomClicked_));
				}
				displayModel_->endUndoState();
			}
			else
			{
				displayModel_->beginUndoState("Delete Atom");
				displayModel_->deleteAtom(atomClicked_);
				displayModel_->endUndoState();
			}
			gui.update(GuiQt::AtomsTarget);
			break;
		case (UserAction::DrawProbeAction):
			if (atomClicked_ != NULL) atomClicked_->print();
			break;
		// Bonding
		case (UserAction::DrawBondSingleAction):
		case (UserAction::DrawBondDoubleAction):
		case (UserAction::DrawBondTripleAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 2) break;
			pickedAtoms_.fillArray(2,atoms);
			b = atoms[0]->findBond(atoms[1]);
			if (b == NULL)
			{
				displayModel_->beginUndoState("Bond Atoms");
				displayModel_->bondAtoms(atoms[0],atoms[1],Bond::BondType(endingMode-UserAction::DrawBondSingleAction+1));
				displayModel_->endUndoState();
			}
			else
			{
				displayModel_->beginUndoState("Change Bond");
				displayModel_->changeBond(b,Bond::BondType(endingMode-UserAction::DrawBondSingleAction+1));
				displayModel_->endUndoState();
			}
			pickedAtoms_.clear();
			gui.update();
			break;
		// Delete bond
		case (UserAction::DrawDeleteBondAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 2) break;
			pickedAtoms_.fillArray(2,atoms);
			if (atoms[0]->findBond(atoms[1]) != NULL)
			{
				displayModel_->beginUndoState("Delete Bond");
				displayModel_->unbondAtoms(atoms[0],atoms[1]);
				displayModel_->endUndoState();
			}
			pickedAtoms_.clear();
			gui.update();
			break;
		// Misc
		case (UserAction::DrawAddHydrogenAction):
			if (atomClicked_ != NULL)
			{
				displayModel_->beginUndoState("Add Hydrogen to Atom");
				displayModel_->hydrogenSatisfy(atomClicked_);
				displayModel_->endUndoState();
				gui.update(GuiQt::AtomsTarget);
			}
			break;
		// Model transformations
		case (UserAction::TransformRotateXYAction):
		case (UserAction::TransformRotateZAction):
		case (UserAction::TransformTranslateAction):
			// Clear list of oldPositions_ if nothing was moved
			if (!hasMoved_) oldPositions_.clear();
			displayModel_->finalizeTransform(oldPositions_, "Transform Selection", nofold);
			gui.update(GuiQt::AtomsTarget);
			break;
		// View changes (no action)
		case (UserAction::RotateXYAction):
		case (UserAction::RotateZAction):
		case (UserAction::TranslateAction):
		case (UserAction::ZoomAction):
			break;
		// Manual picking modes (for axis definitions etc.)
		case (UserAction::PickPositionVectorShiftAction):
		case (UserAction::PickTransformRotateAxisAction):
		case (UserAction::PickTransformDefineAAction):
		case (UserAction::PickTransformDefineBAction):
		case (UserAction::PickTransformDefineCAction):
		case (UserAction::PickConvertDefineSourceAAction):
		case (UserAction::PickConvertDefineSourceBAction):
		case (UserAction::PickConvertDefineSourceCAction):
		case (UserAction::PickConvertDefineTargetAAction):
		case (UserAction::PickConvertDefineTargetBAction):
		case (UserAction::PickConvertDefineTargetCAction):
			// Have we picked the right number of atoms?
			if (pickedAtoms_.nItems() != nAtomsToPick_) break;
			// If a previous callback was defined then call it before we move on
			if (pickAtomsCallback_ != NULL) (*pickAtomsCallback_)(&pickedAtoms_);
			pickAtomsCallback_ = NULL;
			gui.mainWindow->setActiveUserAction(actionBeforePick_);
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
