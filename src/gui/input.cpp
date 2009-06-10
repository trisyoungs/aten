/*
	*** GUI input routines
	*** src/gui/input.cpp
	Copyright T. Youngs 2007-2009

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
	atomHover_ = displayModel_->atomOnScreen(x,y);
	// Perform atom picking before entering mode (if required)
	if (pickEnabled_ && (atomHover_ != NULL))
	{
		// Don't add the same atom more than once
		if (pickedAtoms_.search(atomHover_) == NULL)
		{
			pickedAtoms_.add(atomHover_);
			msg.print(Messenger::Verbose,"Adding atom %i to canvas subselection.\n",atomHover_);
		}
		else msg.print(Messenger::Verbose,"Atom %i is already in canvas subselection.\n",atomHover_);
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
	atomHover_ = NULL;
}

// Inform mouse move
void Canvas::informMouseMove(double x, double y)
{
	// Perform action associated with mode (if any)
	if (activeMode_ != Canvas::NoAction) modeMotion(x,y);
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
	keyModifier_[Prefs::ShiftKey] = shiftkey;
	keyModifier_[Prefs::CtrlKey] = ctrlkey;
	keyModifier_[Prefs::AltKey] = altkey;
	switch (key)
	{
		case (Canvas::LeftKey):
			displayModel_->rotate( shiftkey ? -1.0 : -10.0, 0.0);
			postRedisplay();
			break;
		case (Canvas::RightKey):
			displayModel_->rotate( shiftkey ? 1.0 : 10.0, 0.0);
			postRedisplay();
			break;
		case (Canvas::UpKey):
			displayModel_->rotate(0.0, shiftkey ? -1.0 : -10.0);
			postRedisplay();
			break;
		case (Canvas::DownKey):
			displayModel_->rotate(0.0, shiftkey ? 1.0 : 10.0);
			postRedisplay();
			break;
	}
}

// Inform key up
void Canvas::informKeyUp(Canvas::KeyCode key, bool shiftkey, bool ctrlkey, bool altkey)
{
	switch (key)
	{
	}
}

// Return modifier status
bool Canvas::modifierOn(Prefs::ModifierKey mk)
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
		case (Canvas::EditBondSingleAction):
		case (Canvas::EditBondDoubleAction):
		case (Canvas::EditBondTripleAction):
		case (Canvas::EditDeleteBondAction):
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
					case (Canvas::EditChainAction):
						// If there is currently no atom under the mouse, draw one...
						if (atomHover_ == NULL)
						{
							displayModel_->beginUndoState("Draw Chain");
							i = displayModel_->addAtom(aten.sketchElement(), displayModel_->guideToModel(rMouseDown_));
							displayModel_->endUndoState();
							displayModel_->projectAtom(i);
							atomHover_ = i;
						}
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
		}
		// If we're manipulating, prepare the transform
		if (manipulate)
		{
			/* We don't begin an undostate here - this will be done in endMode().
			   Instead, store pointers to all selected atoms in a Reflist, along
			   with their current positions.
			*/
			oldPositions_.clear();
			for (Atom *i = displayModel_->firstSelected(); i != NULL; i = i->nextSelected()) oldPositions_.add(i, i->r());
			displayModel_->prepareTransform();
		}
	}
	gui.mainView.postRedisplay();
	msg.exit("Canvas::beginMode");
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
			else if (atomHover_ != NULL)
			{
				if (shifted) displayModel_->selectionToggle(atomHover_);
				else if (ctrled) displayModel_->deselectAtom(atomHover_);
				else displayModel_->selectAtom(atomHover_);
			}
			displayModel_->endUndoState();
			gui.modelChanged(TRUE,FALSE,FALSE);
			break;
		// Now do the rest
		case (Canvas::SelectMoleculeAction):
			displayModel_->beginUndoState("Select Molecule");
			if (!modded) displayModel_->selectNone();
			if (atomHover_ != NULL)	displayModel_->selectTree(atomHover_, FALSE, ctrled);
			displayModel_->endUndoState();
			gui.modelChanged(TRUE,FALSE,FALSE);
			break;
		case (Canvas::SelectElementAction):
			displayModel_->beginUndoState("Select Element");
			if (!modded) displayModel_->selectNone();
			if (atomHover_ != NULL) displayModel_->selectElement(atomHover_, FALSE, ctrled);
			displayModel_->endUndoState();
			gui.modelChanged(TRUE,FALSE,FALSE);
			break;
		case (Canvas::SelectRadialAction):
			displayModel_->beginUndoState("Select Radial");
			if (!modded) displayModel_->selectNone();
			if (atomHover_ != NULL)
			{
				radius = (rMouseDown_-rMouseUp_).magnitude();
				radius /= atomHover_->screenRadius() * prefs.screenRadius(atomHover_);
				displayModel_->selectRadial(atomHover_,radius);
			}
			displayModel_->endUndoState();
			gui.modelChanged(TRUE,FALSE,FALSE);
			break;
		// Measurements
		case (Canvas::MeasureDistanceAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 2) break;
			displayModel_->beginUndoState("Measure Distance");
			pickedAtoms_.fillArray(2,atoms);
			displayModel_->measureDistance(atoms[0],atoms[1]);
			displayModel_->endUndoState();
			pickedAtoms_.clear();
			gui.modelChanged(FALSE,FALSE,FALSE);
			break;
		case (Canvas::MeasureAngleAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 3) break;
			displayModel_->beginUndoState("Measure Angle");
			pickedAtoms_.fillArray(3,atoms);
			displayModel_->measureAngle(atoms[0],atoms[1],atoms[2]);
			displayModel_->endUndoState();
			pickedAtoms_.clear();
			gui.modelChanged(FALSE,FALSE,FALSE);
			break;
		case (Canvas::MeasureTorsionAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 4) break;
			displayModel_->beginUndoState("Measure Torsion");
			pickedAtoms_.fillArray(4,atoms);
			displayModel_->measureTorsion(atoms[0],atoms[1],atoms[2],atoms[3]);
			displayModel_->endUndoState();
			pickedAtoms_.clear();
			gui.modelChanged(FALSE,FALSE,FALSE);
			break;
		// Draw single atom
		case (Canvas::EditDrawAction):
			// Make sure we don't draw on top of an existing atom
			if (atomHover_ == NULL)
			{
				displayModel_->beginUndoState("Draw Atom");
				Atom *i = displayModel_->addAtom(aten.sketchElement(), displayModel_->guideToModel(rMouseDown_));
				displayModel_->endUndoState();
				displayModel_->projectAtom(i);
			}
			gui.modelChanged(TRUE,FALSE,TRUE);
			break;
		// Draw chains of atoms
		case (Canvas::EditChainAction):
			// If there is no atom under the mouse we draw one
			i = displayModel_->atomOnScreen(rMouseUp_.x,rMouseUp_.y);
			if ((atomHover_ == i) && (i != NULL)) break;
			displayModel_->beginUndoState("Draw Chain");
			if (i == NULL)
			{
				// No atom under the mouse, so draw an atom
				i = displayModel_->addAtom(aten.sketchElement(), displayModel_->guideToModel(rMouseUp_));
				displayModel_->projectAtom(i);
			}
			// Now bond the atoms, unless atomHover_ and i are the same (i.e. the button was clicked and not moved)
			if (atomHover_ != i)
			{
				// Search for existing bond between atoms
				b = i->findBond(atomHover_);
				if (b == NULL) bt = Bond::Single;
				else
				{
					bt = Bond::increase(b->type());
					displayModel_->unbondAtoms(i,atomHover_);
				}
				displayModel_->bondAtoms(i,atomHover_,bt);
			}
			displayModel_->endUndoState();
			gui.modelChanged(TRUE,FALSE,TRUE);
			break;
		case (Canvas::EditTransmuteAction):
			displayModel_->beginUndoState("Transmute");
			displayModel_->transmuteAtom(atomHover_, aten.sketchElement());
			displayModel_->endUndoState();
			gui.modelChanged(TRUE,FALSE,TRUE);
			break;
		case (Canvas::EditDeleteAction):
			displayModel_->beginUndoState("Delete Atom");
			displayModel_->deleteAtom(atomHover_);
			displayModel_->endUndoState();
			gui.modelChanged(TRUE,FALSE,TRUE);
			break;
		case (Canvas::EditProbeAction):
			if (atomHover_ != NULL) atomHover_->print();
			break;
		// Bonding
		case (Canvas::EditBondSingleAction):
		case (Canvas::EditBondDoubleAction):
		case (Canvas::EditBondTripleAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 2) break;
			pickedAtoms_.fillArray(2,atoms);
			b = atoms[0]->findBond(atoms[1]);
			if (b == NULL)
			{
				displayModel_->beginUndoState("Bond Atoms");
				displayModel_->bondAtoms(atoms[0],atoms[1],Bond::BondType(endingMode-Canvas::EditBondSingleAction+1));
				displayModel_->endUndoState();
			}
			else
			{
				displayModel_->beginUndoState("Change Bond");
				displayModel_->changeBond(b,Bond::BondType(endingMode-Canvas::EditBondSingleAction+1));
				displayModel_->endUndoState();
			}
			pickedAtoms_.clear();
			gui.modelChanged(FALSE,FALSE,FALSE);
			break;
		// Delete bond
		case (Canvas::EditDeleteBondAction):
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
			gui.modelChanged(FALSE,FALSE,FALSE);
			break;
		// Misc
		case (Canvas::EditAddHydrogenAction):
			if (atomHover_ != NULL)
			{
				displayModel_->beginUndoState("Add Hydrogen to Atom");
				displayModel_->hydrogenSatisfy(atomHover_);
				displayModel_->endUndoState();
				gui.modelChanged(TRUE,FALSE,TRUE);
			}
			break;
		// Model transformations
		case (Canvas::TransformRotateXYAction):
		case (Canvas::TransformRotateZAction):
		case (Canvas::TransformTranslateAction):
			// Clear list of oldPositions_ if nothing was moved
			if (!hasMoved_) oldPositions_.clear();
			displayModel_->finalizeTransform(oldPositions_, "Transform Selection");
			gui.modelChanged(TRUE,FALSE,FALSE);
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
	// For view operations when we have a trajectory, apply all movement to the parent model
	//viewtarget = displayModel_->trajectoryParent();
	//if (viewtarget == NULL) viewtarget = displayModel_;
	// Calculate new delta.
	delta.set(x,y,0.0);
	delta = delta - rMouseLast_;
	// Use activeMode_ to determine what needs to be performed
	switch (activeMode_)
	{
		case (Canvas::NoAction):
			break;
		case (Canvas::RotateXYAction):
			displayModel_->rotate(delta.x/2.0,delta.y/2.0);
			break;
		case (Canvas::RotateZAction):
			displayModel_->zRotate(delta.x/2.0);
			break;
		case (Canvas::TranslateAction):
			delta.y = -delta.y;
			displayModel_->adjustCamera(delta/15.0,0.0);
			break;
		case (Canvas::ZoomAction):
			displayModel_->adjustZoom(delta.y < 0.0);
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
			scrollup ? displayModel_->rotate(1.0,0.0) : displayModel_->rotate(-1.0,0.0);
			break;
		case (Prefs::TranslateAction):
			break;
		case (Prefs::ZoomAction):
			displayModel_->adjustZoom(scrollup);
			break;
	}
	postRedisplay();
	msg.exit("Canvas::modeScroll");
}
