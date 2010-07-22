/*
	*** GUI input routines
	*** src/gui/input.cpp
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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/fragment.h"
#include "render/canvas.h"
#include "model/model.h"

// Inform mouse down
void Canvas::informMouseDown(Prefs::MouseButton button, double x, double y, bool shiftkey, bool ctrlkey, bool altkey)
{
	// Store mouse position and key modifier status
	rMouseDown_.set(x,y,0.0);
	rMouseUp_.set(x,y,0.0);
	keyModifier_[Prefs::ShiftKey] = shiftkey;
	keyModifier_[Prefs::CtrlKey] = ctrlkey;
	keyModifier_[Prefs::AltKey] = altkey;
	// Determine if there is an atom under the mouse
	atomClicked_ = displayModel_->atomOnScreen(x,y);
	// Perform atom picking before entering mode (if required)
	if (pickEnabled_ && (atomClicked_ != NULL))
	{
		// Don't add the same atom more than once
		if (pickedAtoms_.search(atomClicked_) == NULL)
		{
			pickedAtoms_.add(atomClicked_);
			msg.print(Messenger::Verbose,"Adding atom %i to canvas subselection.\n",atomClicked_);
		}
		else msg.print(Messenger::Verbose,"Atom %i is already in canvas subselection.\n",atomClicked_);
	}
	// Activate mode...
	beginMode(button);
}

// Inform mouse up
void Canvas::informMouseUp(Prefs::MouseButton button, double x, double y)
{
	// Only finalise the mode if the button is the same as the one that caused the mousedown event.
	if (mouseButton_[button])
	{
		rMouseUp_.set(x,y,0.0);
		// Deactivate mode...
		endMode(button);
	}
	atomClicked_ = NULL;
}

// Inform mouse move
void Canvas::informMouseMove(double x, double y)
{
	// Perform action associated with mode (if any)
	if ((activeMode_ != Canvas::NoAction) || (selectedMode_ == Canvas::DrawFragmentAction)) modeMotion(x,y);
	rMouseLast_.set(x,y,0.0);
}

// Inform mouse wheel scroll
void Canvas::informScroll(bool dir)
{
	modeScroll(dir);
}

// Inform key down
void Canvas::informKeyDown(Canvas::KeyCode key, bool shiftkey, bool ctrlkey, bool altkey)
{
	// Check datamodel...
	if (displayModel_ == NULL) return;
	// Set keystates
	keyModifier_[Prefs::ShiftKey] = shiftkey;
	keyModifier_[Prefs::CtrlKey] = ctrlkey;
	keyModifier_[Prefs::AltKey] = altkey;

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

	switch (key)
	{
		case (Canvas::LeftKey):
// 			if (keyModifier_[Prefs::CtrlKey])
// 			{
// 				displayModel_->prepareTransform();
// 				displayModel_->beginUndoState("Rotate selection about world Y axis");
// 				displayModel_->rotateSelectionWorld(2.0,0.0);
// 				displayModel_->endUndoState();
// 				displayModel_->updateMeasurements();
// 				displayModel_->finalizeTransform(oldPositions_, "Transform Selection");
// 				gui.update(TRUE,FALSE,FALSE);
// 			}
// 			else
			displayModel_->rotateView( shiftkey ? -1.0 : -10.0, 0.0);
			postRedisplay();
			break;
		case (Canvas::RightKey):
			displayModel_->rotateView( shiftkey ? 1.0 : 10.0, 0.0);
			postRedisplay();
			break;
		case (Canvas::UpKey):
			displayModel_->rotateView(0.0, shiftkey ? -1.0 : -10.0);
			postRedisplay();
			break;
		case (Canvas::DownKey):
			displayModel_->rotateView(0.0, shiftkey ? 1.0 : 10.0);
			postRedisplay();
			break;
		case (Canvas::EscapeKey):
			gui.mainWindow->cancelCurrentMode();
			postRedisplay();
			break;
		default:
			break;
	}
	
	// Mode-specific
	switch (selectedMode_)
	{
		case (Canvas::DrawFragmentAction):
			// Cycle link atom....
			if (keyModifier_[Prefs::AltKey])
			{
				Fragment *frag = gui.fragmentWindow->currentFragment();
				if (frag == NULL) break;
				frag->cycleLinkAtom();
				gui.mainView.postRedisplay();
			}
			// Refresh if Shift status has changed
			if (keyModifier_[Prefs::ShiftKey]) gui.mainView.postRedisplay();
			break;
		default:
			break;
	}
}

// Inform key up
void Canvas::informKeyUp(Canvas::KeyCode key, bool shiftkey, bool ctrlkey, bool altkey)
{
	// Set keystates
	bool oldshift = keyModifier_[Prefs::ShiftKey];
	//bool oldctrl = keyModifier_[Prefs::CtrlKey];
	//bool oldalt = keyModifier_[Prefs::AltKey];
	keyModifier_[Prefs::ShiftKey] = shiftkey;
	keyModifier_[Prefs::CtrlKey] = ctrlkey;
	keyModifier_[Prefs::AltKey] = altkey;

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
		case (Canvas::DrawFragmentAction):
			// Refresh if Shift status has changed
			if (keyModifier_[Prefs::ShiftKey] != oldshift) gui.mainView.postRedisplay();
			break;
		default:
			break;
	}
}

// Return modifier status
bool Canvas::modifierOn(Prefs::ModifierKey mk) const
{
	return keyModifier_[mk];
}

/*
// Canvas Modes
*/

// Set selected mode
void Canvas::setSelectedMode(UserAction ua)
{
	msg.enter("Canvas::setSelectedMode");
	if (displayModel_ == NULL)
	{
		printf("Pointless Canvas::setSelectedMode - datamodel == NULL.\n");
		msg.exit("Canvas::setSelectedMode");
		return;
	}
	// If previous action was Canvas::ManualPickAction then finalise it first
	if (selectedMode_ == Canvas::ManualPickAction) endManualPick(FALSE);
	// Prepare canvas for the selected action
	switch (ua)
	{
		case (Canvas::ManualPickAction):
		case (Canvas::MeasureDistanceAction):
		case (Canvas::MeasureAngleAction):
		case (Canvas::MeasureTorsionAction):
		case (Canvas::DrawBondSingleAction):
		case (Canvas::DrawBondDoubleAction):
		case (Canvas::DrawBondTripleAction):
		case (Canvas::DrawDeleteBondAction):
			pickEnabled_ = TRUE;
			pickedAtoms_.clear();
			break;
		default:
			pickEnabled_ = FALSE;
			break;
	}
	// Finally, set the mode and refresh
	selectedMode_ = ua;
	gui.mainView.postRedisplay();
	gui.updateStatusBar();
	msg.exit("Canvas::setSelectedMode");
}

// Begin Mode
void Canvas::beginMode(Prefs::MouseButton button)
{
	msg.enter("widgetCanvas::beginMode");
	static bool manipulate, zrotate;
	static int n;
	static Atom *i;
	// Do the requested action as defined in the control panel, but only if another action
	// isn't currently in progress. Set the UserAction based on the mouse button that sent
	// the signal, current selection / draw modes and key modifier states.
	// Set mouse flag and get state of modifier keys
	if (displayModel_ == NULL)
	{
		printf("Pointless Canvas::beginMode - datamodel == NULL.\n");
		msg.exit("Canvas::beginMode");
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
	if (activeMode_ == Canvas::NoAction)
	{
		switch (prefs.mouseAction(button))
		{
			// Main interactor - selection, sketching, measuring
			case (Prefs::InteractAction):
				useSelectedMode();
				// Some modes require actions to be done when the button is first depressed
				switch (activeMode_)
				{
					case (Canvas::DrawChainAction):
						// If there is currently no atom under the mouse, draw one...
						if (atomClicked_ == NULL)
						{
							displayModel_->beginUndoState("Draw Chain");
							currentDrawDepth_ = prefs.drawDepth();
							i = displayModel_->addAtom(aten.sketchElement(), displayModel_->guideToModel(rMouseDown_, currentDrawDepth_));
							displayModel_->endUndoState();
							displayModel_->projectAtom(i);
							atomClicked_ = i;
						}
						else currentDrawDepth_ = atomClicked_->rWorld().z;
						break;
					default:
						break;
				}
				break;
			case (Prefs::RotateAction):
				// Check for multiple key modifiers first.
				if (manipulate && zrotate) activeMode_ = Canvas::TransformRotateZAction;
				else if (manipulate) activeMode_ = Canvas::TransformRotateXYAction;
				else if (zrotate) activeMode_ = Canvas::RotateZAction;
				else activeMode_ = Canvas::RotateXYAction;
				break;
			case (Prefs::ZoomAction):
				activeMode_ = Canvas::ZoomAction;
				break;
			case (Prefs::TranslateAction):
				activeMode_ = (manipulate ? Canvas::TransformTranslateAction : Canvas::TranslateAction);
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
	gui.mainView.postRedisplay();
	msg.exit("Canvas::beginMode");
}

void Canvas::modeMotion(double x, double y)
{
	// Actively update variables when moving the mouse (possibly while performing a given action)
	msg.enter("Canvas::modeMotion");
	static Vec3<double> delta;
	//static Model *viewtarget;
	if (displayModel_ == NULL)
	{
		printf("Pointless Canvas::modeMotion - datamodel == NULL.\n");
		msg.exit("Canvas::modeMotion");
		return;
	}
	// Calculate new delta.
	delta.set(x,y,0.0);
	delta = delta - rMouseLast_;
	// Use activeMode_ to determine what needs to be performed
	switch (activeMode_)
	{
		case (Canvas::NoAction):
			break;
		case (Canvas::RotateXYAction):
			displayModel_->rotateView(delta.x/2.0,delta.y/2.0);
			break;
		case (Canvas::RotateZAction):
			displayModel_->zRotateView(delta.x/2.0);
			break;
		case (Canvas::TranslateAction):
			delta.y = -delta.y;
			displayModel_->adjustCamera(delta/15.0,0.0);
			break;
		case (Canvas::ZoomAction):
			displayModel_->adjustZoom(delta.y < 0.0);
			break;
		case (Canvas::DrawFragmentAction):
			if (gui.fragmentWindow->currentFragment() != NULL)
			{
				if (atomClicked_ == NULL) gui.fragmentWindow->currentFragment()->rotateOrientedModel(delta.x/2.0,delta.y/2.0);
				else gui.fragmentWindow->currentFragment()->rotateAnchoredModel(delta.x, delta.y);
			}
			break;
		case (Canvas::TransformRotateXYAction):
			displayModel_->rotateSelectionWorld(delta.x/2.0,delta.y/2.0);
			displayModel_->updateMeasurements();
			hasMoved_ = TRUE;
			break;
		case (Canvas::TransformRotateZAction):
			displayModel_->rotateSelectionZaxis(delta.x/2.0);
			displayModel_->updateMeasurements();
			hasMoved_ = TRUE;
			break;
		case (Canvas::TransformTranslateAction):
			delta.y = -delta.y;
			delta /= displayModel_->translateScale() * 2.0;
			displayModel_->translateSelectionWorld(delta);
			displayModel_->updateMeasurements();
			hasMoved_ = TRUE;
			break;
		default:
			break;
	}
	postRedisplay();
	msg.exit("Canvas::modeMotion");
}

// End Mode
void Canvas::endMode(Prefs::MouseButton button)
{
	// Finalize the current action on the model
	msg.enter("Canvas::endMode");
	double area, radius;
	Atom *atoms[4], *i;
	Bond *b;
	Bond::BondType bt;
	Fragment *frag;
	if (displayModel_ == NULL)
	{
		printf("Pointless Canvas::endMode - datamodel == NULL.\n");
		msg.exit("Canvas::endMode");
		return;
	}
	// Store modifier states for convenience
	bool shifted = keyModifier_[Prefs::ShiftKey];
	bool ctrled = keyModifier_[Prefs::CtrlKey];
	bool modded = (shifted || ctrled);
	// Reset mouse button flag
	mouseButton_[button] = FALSE;
	// Copy the current mode and reset it so we redraw properly
	Canvas::UserAction endingMode = activeMode_;
	activeMode_ = Canvas::NoAction;
	// Finalize the action
	switch (endingMode)
	{
		// Plain atom / box select
		case (Canvas::SelectAction):
			area = fabs(rMouseUp_.x - rMouseDown_.x) * fabs(rMouseUp_.y - rMouseDown_.y);
			displayModel_->beginUndoState("Change Selection");
			displayModel_->projectAll();
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
			gui.update(TRUE,FALSE,FALSE);
			break;
		// Now do the rest
		case (Canvas::SelectMoleculeAction):
			displayModel_->beginUndoState("Select Molecule");
			if (!modded) displayModel_->selectNone();
			if (atomClicked_ != NULL)	displayModel_->selectTree(atomClicked_, FALSE, ctrled);
			displayModel_->endUndoState();
			gui.update(TRUE,FALSE,FALSE);
			break;
		case (Canvas::SelectElementAction):
			displayModel_->beginUndoState("Select Element");
			if (!modded) displayModel_->selectNone();
			if (atomClicked_ != NULL) displayModel_->selectElement(atomClicked_, FALSE, ctrled);
			displayModel_->endUndoState();
			gui.update(TRUE,FALSE,FALSE);
			break;
		case (Canvas::SelectRadialAction):
			displayModel_->beginUndoState("Select Radial");
			if (!modded) displayModel_->selectNone();
			if (atomClicked_ != NULL)
			{
				radius = (rMouseDown_-rMouseUp_).magnitude();
				radius /= atomClicked_->screenRadius() * prefs.screenRadius(atomClicked_);
				displayModel_->selectRadial(atomClicked_,radius);
			}
			displayModel_->endUndoState();
			gui.update(TRUE,FALSE,FALSE);
			break;
		// Measurements
		case (Canvas::MeasureDistanceAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 2) break;
			displayModel_->beginUndoState("Measure Distance");
			pickedAtoms_.fillArray(2,atoms);
			displayModel_->addDistanceMeasurement(atoms[0],atoms[1]);
			displayModel_->endUndoState();
			pickedAtoms_.clear();
			gui.update(FALSE,FALSE,FALSE);
			break;
		case (Canvas::MeasureAngleAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 3) break;
			displayModel_->beginUndoState("Measure Angle");
			pickedAtoms_.fillArray(3,atoms);
			displayModel_->addAngleMeasurement(atoms[0],atoms[1],atoms[2]);
			displayModel_->endUndoState();
			pickedAtoms_.clear();
			gui.update(FALSE,FALSE,FALSE);
			break;
		case (Canvas::MeasureTorsionAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 4) break;
			displayModel_->beginUndoState("Measure Torsion");
			pickedAtoms_.fillArray(4,atoms);
			displayModel_->addTorsionMeasurement(atoms[0],atoms[1],atoms[2],atoms[3]);
			displayModel_->endUndoState();
			pickedAtoms_.clear();
			gui.update(FALSE,FALSE,FALSE);
			break;
		// Draw single atom
		case (Canvas::DrawAtomAction):
			// Make sure we don't draw on top of an existing atom
			if (atomClicked_ == NULL)
			{
				displayModel_->beginUndoState("Draw Atom");
				currentDrawDepth_ = prefs.drawDepth();
				Atom *i = displayModel_->addAtom(aten.sketchElement(), displayModel_->guideToModel(rMouseDown_, currentDrawDepth_));
				displayModel_->endUndoState();
				displayModel_->projectAtom(i);
			}
			gui.update(TRUE,FALSE,TRUE);
			break;
		// Draw chains of atoms
		case (Canvas::DrawChainAction):
			// If there is no atom under the mouse we draw one
			i = displayModel_->atomOnScreen(rMouseUp_.x,rMouseUp_.y);
			if ((atomClicked_ == i) && (i != NULL)) break;
			displayModel_->beginUndoState("Draw Chain");
			if (i == NULL)
			{
				// No atom under the mouse, so draw an atom at previous draw depth
				i = displayModel_->addAtom(aten.sketchElement(), displayModel_->guideToModel(rMouseUp_, currentDrawDepth_));
				displayModel_->projectAtom(i);
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
			gui.update(TRUE,FALSE,TRUE);
			break;
		// Draw framents
		case (Canvas::DrawFragmentAction):
			frag = gui.fragmentWindow->currentFragment();
			if (frag == NULL) break;
			if (atomClicked_ != NULL)
			{
				displayModel_->beginUndoState("Draw Attached Fragment");
				frag->pasteAnchoredModel(atomClicked_, keyModifier_[Prefs::ShiftKey], displayModel_);
			}
			else
			{
				// No atom under the moust pointer, so draw on at the prefs drawing depth in its current orientation
				displayModel_->beginUndoState("Draw Fragment");
				frag->pasteOrientedModel(displayModel_->guideToModel(rMouseDown_, prefs.drawDepth()), displayModel_);
			}
			displayModel_->endUndoState();
			gui.update(TRUE,FALSE,TRUE);
			break;
		case (Canvas::DrawTransmuteAction):
			displayModel_->beginUndoState("Transmute");
			displayModel_->transmuteAtom(atomClicked_, aten.sketchElement());
			displayModel_->endUndoState();
			gui.update(TRUE,FALSE,TRUE);
			break;
		case (Canvas::DrawDeleteAction):
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
			gui.update(TRUE,FALSE,TRUE);
			break;
		case (Canvas::DrawProbeAction):
			if (atomClicked_ != NULL) atomClicked_->print();
			break;
		// Bonding
		case (Canvas::DrawBondSingleAction):
		case (Canvas::DrawBondDoubleAction):
		case (Canvas::DrawBondTripleAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 2) break;
			pickedAtoms_.fillArray(2,atoms);
			b = atoms[0]->findBond(atoms[1]);
			if (b == NULL)
			{
				displayModel_->beginUndoState("Bond Atoms");
				displayModel_->bondAtoms(atoms[0],atoms[1],Bond::BondType(endingMode-Canvas::DrawBondSingleAction+1));
				displayModel_->endUndoState();
			}
			else
			{
				displayModel_->beginUndoState("Change Bond");
				displayModel_->changeBond(b,Bond::BondType(endingMode-Canvas::DrawBondSingleAction+1));
				displayModel_->endUndoState();
			}
			pickedAtoms_.clear();
			gui.update(FALSE,FALSE,FALSE);
			break;
		// Delete bond
		case (Canvas::DrawDeleteBondAction):
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
			gui.update(FALSE,FALSE,FALSE);
			break;
		// Misc
		case (Canvas::DrawAddHydrogenAction):
			if (atomClicked_ != NULL)
			{
				displayModel_->beginUndoState("Add Hydrogen to Atom");
				displayModel_->hydrogenSatisfy(atomClicked_);
				displayModel_->endUndoState();
				gui.update(TRUE,FALSE,TRUE);
			}
			break;
		// Model transformations
		case (Canvas::TransformRotateXYAction):
		case (Canvas::TransformRotateZAction):
		case (Canvas::TransformTranslateAction):
			// Clear list of oldPositions_ if nothing was moved
			if (!hasMoved_) oldPositions_.clear();
			displayModel_->finalizeTransform(oldPositions_, "Transform Selection");
			gui.update(TRUE,FALSE,FALSE);
			break;
		// View changes (no action)
		case (Canvas::RotateXYAction):
		case (Canvas::RotateZAction):
		case (Canvas::TranslateAction):
		case (Canvas::ZoomAction):
			break;
		// Manual picking mode (for toolwindow axis definitions etc.)
		case (Canvas::ManualPickAction):
			// Have we picked the right number of atoms?
			if (pickedAtoms_.nItems() != nAtomsToPick_) break;
			// Call callback and re-set used mode (if callback was defined)
			endManualPick(TRUE);
			break;
		default:
			printf("No endMode handler defined for UserAction %i.\n", endingMode);
			break;
	}
	msg.exit("Canvas::endMode");
}

void Canvas::modeScroll(bool scrollup)
{
	// Handle mouse-wheel scroll events.
	// Do the requested wheel action as defined in the control panel
	msg.enter("Canvas::modeScroll");
	if (displayModel_ == NULL)
	{
		printf("Pointless Canvas::modeScroll - datamodel == NULL.\n");
		msg.exit("Canvas::modeScroll");
		return;
	}
	switch (prefs.mouseAction(Prefs::WheelButton))
	{
		case (Prefs::NoAction):
			break;
		case (Prefs::InteractAction):
			useSelectedMode();
			break;
		case (Prefs::RotateAction):
			scrollup ? displayModel_->rotateView(1.0,0.0) : displayModel_->rotateView(-1.0,0.0);
			break;
		case (Prefs::TranslateAction):
			break;
		case (Prefs::ZoomAction):
			displayModel_->adjustZoom(scrollup);
			break;
		default:
			break;
	}
	postRedisplay();
	msg.exit("Canvas::modeScroll");
}
