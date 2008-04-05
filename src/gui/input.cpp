/*
	*** GUI input routines
	*** src/gui/input.cpp
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
#include "base/prefs.h"
#include "gui/gui.h"
#include "gui/canvas.h"
#include "model/model.h"

// Inform mouse down
void Canvas::informMouseDown(Prefs::MouseButton button, double x, double y)
{
	rMouseDown_.set(x,y,0.0);
	rMouseUp_.set(x,y,0.0);
	// Determine if there is an atom under the mouse
	atomHover_ = displayModel_->atomOnScreen(x,y);
	// If a model is being rendered, perform atom selection (if enabled)
	if (subselectEnabled_ && (atomHover_ != NULL))
	{
		// Don't add the same atom more than once
		if (subselection_.search(atomHover_) == NULL)
		{
			subselection_.add(atomHover_);
			msg(Debug::Verbose,"Adding atom %i to canvas subselection.\n",atomHover_);
		}
		else msg(Debug::Verbose,"Atom %i is already in canvas subselection.\n",atomHover_);
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
	if (activeMode_ != UA_NONE) modeMotion(x,y);
	rMouseLast_.set(x,y,0.0);
}

// Inform mouse wheel scroll
void Canvas::informScroll(bool dir)
{
	modeScroll(dir);
}

// Inform key down
void Canvas::informKeyDown(key_code key)
{
	// Check datamodel...
	if (displayModel_ == NULL) return;
	static Model *viewtarget;
	// For view operations when we have a trajectory, apply all movement to the parent model
	//viewtarget = (displayModel_->trajectoryParent() == NULL ? displayModel_ : displayModel_->trajectoryParent());
	viewtarget = displayModel_;
	switch (key)
	{
		case (KC_SHIFT_L):
			keyModifier_[Prefs::ShiftKey] = TRUE;
			break;
		case (KC_SHIFT_R):
			keyModifier_[Prefs::ShiftKey] = TRUE;
			break;
		case (KC_CONTROL_L):
			keyModifier_[Prefs::CtrlKey] = TRUE;
			break;
		case (KC_CONTROL_R):
			keyModifier_[Prefs::CtrlKey] = TRUE;
			break;
		case (KC_ALT_L):
			keyModifier_[Prefs::AltKey] = TRUE;
			break;
		case (KC_ALT_R):
			keyModifier_[Prefs::AltKey] = TRUE;
			break;
		//case (GDK_Escape): master.check_before_close(); break;
		case (KC_LEFT):
			viewtarget->rotate(-10.0,0.0);
			postRedisplay();
			break;
		case (KC_RIGHT):
			viewtarget->rotate(10.0,0.0);
			postRedisplay();
			break;
		case (KC_UP):
			viewtarget->rotate(0.0,-10.0);
			postRedisplay();
			break;
		case (KC_DOWN):
			viewtarget->rotate(0.0,10.0);
			postRedisplay();
			break;
	}
}

// Inform key up
void Canvas::informKeyUp(key_code key)
{
	switch (key)
	{
		case (KC_SHIFT_L):
			keyModifier_[Prefs::ShiftKey] = FALSE;
			break;
		case (KC_SHIFT_R):
			keyModifier_[Prefs::ShiftKey] = FALSE;
			break;
		case (KC_CONTROL_L):
			keyModifier_[Prefs::CtrlKey] = FALSE;
			break;
		case (KC_CONTROL_R):
			keyModifier_[Prefs::CtrlKey] = FALSE;
			break;
		case (KC_ALT_L):
			keyModifier_[Prefs::AltKey] = FALSE;
			break;
		case (KC_ALT_R):
			keyModifier_[Prefs::AltKey] = FALSE;
			break;
	}
}

/*
// Canvas Modes
*/

// Set selected mode
void Canvas::setSelectedMode(UserAction ua)
{
	dbgBegin(Debug::Calls,"Canvas::setSelectedMode");
	selectedMode_ = ua;
	if (displayModel_ == NULL)
	{
		printf("Pointless Canvas::setSelectedMode - datamodel == NULL.\n");
		dbgEnd(Debug::Calls,"Canvas::setSelectedMode");
		return;
	}
	// Prepare canvas / model depending on the mode
	switch (ua)
	{
		case (UA_GEOMDIST):
		case (UA_GEOMANGLE):
		case (UA_GEOMTORSION):
		case (UA_BONDSINGLE):
		case (UA_BONDDOUBLE):
		case (UA_BONDTRIPLE):
		case (UA_DELBOND):
			subselectEnabled_ = TRUE;
			subselection_.clear();
			break;
		default:
			subselectEnabled_ = FALSE;
			break;
	}
	gui.mainView.postRedisplay();
	dbgEnd(Debug::Calls,"Canvas::setSelectedMode");
}

// Begin Mode
void Canvas::beginMode(Prefs::MouseButton button)
{
	dbgBegin(Debug::Calls,"widgetCanvas::beginMode");
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
		dbgEnd(Debug::Calls,"Canvas::beginMode");
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
	if (activeMode_ == UA_NONE)
	{
		switch (prefs.mouseAction(button))
		{
			// Main interactor - selection, sketching, measuring
			case (Prefs::InteractAction):
				useSelectedMode();
				// Some modes require actions to be done when the button is first depressed
				switch (activeMode_)
				{
					case (UA_DRAWCHAIN):
						// If there is currently no atom under the mouse, draw one...
						if (atomHover_ == NULL)
						{
							displayModel_->beginUndostate("Draw Chain");
							i = displayModel_->addAtom(master.sketchElement(), displayModel_->guideToModel(rMouseDown_));
							displayModel_->endUndostate();
							displayModel_->projectAtom(i);
							atomHover_ = i;
						}
						break;
				}
				break;
			case (Prefs::RotateAction):
				// Check for multiple key modifiers first.
				if (manipulate && zrotate) activeMode_ = UA_MANIPROTZ;
				else if (manipulate) activeMode_ = UA_MANIPROTXY;
				else if (zrotate) activeMode_ = UA_ROTATEZ;
				else activeMode_ = UA_ROTATEXY;
				break;
			case (Prefs::ZoomAction):
				activeMode_ = UA_ZOOMCAM;
				break;
			case (Prefs::TranslateAction):
				activeMode_ = UA_MOVECAM;
				manipulate ? activeMode_ = UA_MANIPTRANS : activeMode_ = UA_MOVECAM;
				break;
		}
		// If we're manipulating, prepare the transform
		if (manipulate)
		{
			/* We don't begin an undostate here - this will be done in end_mode().
			   Instead, store pointers to all selected atoms in a Reflist, along
			   with their current positions.
			*/
			rSelection_.clear();
			for (Atom *i = displayModel_->firstSelected(); i != NULL; i = i->nextSelected()) rSelection_.add(i, i->r());
			displayModel_->prepareTransform();
		}
	}
	dbgEnd(Debug::Calls,"Canvas::begin_mode");
}

// End Mode
void Canvas::endMode(Prefs::MouseButton button)
{
	// Finalize the current action on the model
	dbgBegin(Debug::Calls,"Canvas::endMode");
	bool manipulate;
	double area, radius;
	Atom **atoms, *i;
	Bond *b;
	Bond::BondType bt;
	if (displayModel_ == NULL)
	{
		printf("Pointless Canvas::endMode - datamodel == NULL.\n");
		dbgEnd(Debug::Calls,"Canvas::endMode");
		return;
	}
	// Create atoms pointer array
	atoms = new Atom*[4];
	// Reset mouse button flag
	mouseButton_[button] = FALSE;
	// Finalize the action
	switch (activeMode_)
	{
		// Group all the plain selection modes together (one for each toolbar in a diff. window)
		case (UA_PICKSELECT):
		case (UA_GEOMSELECT):
		case (UA_POSSELECT):
			area = fabs(rMouseUp_.x - rMouseDown_.x) * fabs(rMouseUp_.y - rMouseDown_.y);
			displayModel_->beginUndostate("Change Selection");
			displayModel_->projectAll();
			// If SHIFT is not held down, deselect the current selection
			if (!keyModifier_[Prefs::ShiftKey]) displayModel_->selectNone();
			// Do either point select or box select based on the size of the selected area
			if (area < 100.0)
			{
				if (keyModifier_[Prefs::ShiftKey])
				{
					if (atomHover_ != NULL) displayModel_->selectionToggle(atomHover_);
				}
				else if (atomHover_ != NULL) displayModel_->selectAtom(atomHover_);
			}
			else displayModel_->selectBox(rMouseDown_.x, rMouseDown_.y, rMouseUp_.x, rMouseUp_.y);
			displayModel_->endUndostate();
			// Set activeMode_ early to prevent presistence of selection box
			activeMode_ = UA_NONE;
			gui.modelChanged(TRUE,FALSE,FALSE);
			break;
		// Now do the rest
		case (UA_PICKFRAG):
			displayModel_->beginUndostate("Select Molecule");
			if (!keyModifier_[Prefs::ShiftKey]) displayModel_->selectNone();
			if (atomHover_ != NULL) displayModel_->selectTree(atomHover_);
			displayModel_->endUndostate();
			gui.modelChanged(TRUE,FALSE,FALSE);
			break;
		case (UA_PICKELEMENT):
			displayModel_->beginUndostate("Select Element");
			if (!keyModifier_[Prefs::ShiftKey]) displayModel_->selectNone();
			if (atomHover_ != NULL) displayModel_->selectElement(atomHover_);
			displayModel_->endUndostate();
			gui.modelChanged(TRUE,FALSE,FALSE);
			break;
		case (UA_PICKRADIAL):
			displayModel_->beginUndostate("Select Radial");
			if (!keyModifier_[Prefs::ShiftKey]) displayModel_->selectNone();
			if (atomHover_ != NULL)
			{
				radius = (rMouseDown_-rMouseUp_).magnitude();
				radius /= atomHover_->screenRadius() * prefs.screenRadius(atomHover_);
				displayModel_->selectRadial(atomHover_,radius);
			}
			displayModel_->endUndostate();
			gui.modelChanged(TRUE,FALSE,FALSE);
			break;
		// Measurements
		case (UA_GEOMDIST):
			// Must be two atoms in subselection to continue
			if (subselection_.nItems() != 2) break;
			displayModel_->beginUndostate("Measure Distance");
			subselection_.fillArray(2,atoms);
			displayModel_->measureDistance(atoms[0],atoms[1]);
			displayModel_->endUndostate();
			subselection_.clear();
			gui.modelChanged(FALSE,FALSE,FALSE);
			break;
		case (UA_GEOMANGLE):
			// Must be two atoms in subselection to continue
			if (subselection_.nItems() != 3) break;
			displayModel_->beginUndostate("Measure Angle");
			subselection_.fillArray(3,atoms);
			displayModel_->measureAngle(atoms[0],atoms[1],atoms[2]);
			displayModel_->endUndostate();
			subselection_.clear();
			gui.modelChanged(FALSE,FALSE,FALSE);
			break;
		case (UA_GEOMTORSION):
			// Must be two atoms in subselection to continue
			if (subselection_.nItems() != 4) break;
			displayModel_->beginUndostate("Measure Torsion");
			subselection_.fillArray(4,atoms);
			displayModel_->measureTorsion(atoms[0],atoms[1],atoms[2],atoms[3]);
			displayModel_->endUndostate();
			subselection_.clear();
			gui.modelChanged(FALSE,FALSE,FALSE);
			break;
		// Draw single atom
		case (UA_DRAWATOM):
			// Make sure we don't draw on top of an existing atom
			if (atomHover_ == NULL)
			{
				displayModel_->beginUndostate("Draw Atom");
				Atom *i = displayModel_->addAtom(master.sketchElement(), displayModel_->guideToModel(rMouseDown_));
				displayModel_->endUndostate();
				displayModel_->projectAtom(i);
			}
			gui.modelChanged(TRUE,FALSE,TRUE);
			break;
		// Draw chains of atoms
		case (UA_DRAWCHAIN):
			// If there is no atom under the mouse we draw one
			i = displayModel_->atomOnScreen(rMouseUp_.x,rMouseUp_.y);
			if ((atomHover_ == i) && (i != NULL)) break;
			displayModel_->beginUndostate("Draw Chain");
			if (i == NULL)
			{
				// No atom under the mouse, so draw an atom
				i = displayModel_->addAtom(master.sketchElement(), displayModel_->guideToModel(rMouseUp_));
				displayModel_->projectAtom(i);
			}
			// Now bond the atoms, unless atomHover_ and i are the same (i.e. the button was clicked and not moved)
			if (atomHover_ != i)
			{
				// Search for existing bond between atoms
				b = i->findBond(atomHover_);
				if (b == NULL) bt = Bond::Single;
				else bt = Bond::increaseBondType(b->order());
				displayModel_->bondAtoms(i,atomHover_,bt);
			}
			displayModel_->endUndostate();
			gui.modelChanged(TRUE,FALSE,TRUE);
			break;
		case (UA_TRANSATOM):
			displayModel_->beginUndostate("Transmute");
			displayModel_->transmuteAtom(atomHover_, master.sketchElement());
			displayModel_->endUndostate();
			gui.modelChanged(TRUE,FALSE,TRUE);
			break;
		case (UA_DELATOM):
			displayModel_->beginUndostate("Delete Atom");
			displayModel_->deleteAtom(atomHover_);
			displayModel_->endUndostate();
			gui.modelChanged(TRUE,FALSE,TRUE);
			break;
		case (UA_PROBEATOM):
			if (atomHover_ != NULL) atomHover_->print();
			break;
		// Bonding
		case (UA_BONDSINGLE):
		case (UA_BONDDOUBLE):
		case (UA_BONDTRIPLE):
			// Must be two atoms in subselection to continue
			if (subselection_.nItems() != 2) break;
			subselection_.fillArray(2,atoms);
			b = atoms[0]->findBond(atoms[1]);
			if (b == NULL)
			{
				displayModel_->beginUndostate("Bond Atoms");
				displayModel_->bondAtoms(atoms[0],atoms[1],Bond::BondType(activeMode_-UA_BONDSINGLE+1));
				displayModel_->endUndostate();
			}
			else
			{
				displayModel_->beginUndostate("Change Bond");
				displayModel_->changeBond(b,Bond::BondType(activeMode_-UA_BONDSINGLE+1));
				displayModel_->endUndostate();
			}
			subselection_.clear();
			gui.modelChanged(FALSE,FALSE,FALSE);
			break;
		case (UA_DELBOND):
			// Must be two atoms in subselection to continue
			if (subselection_.nItems() != 2) break;
			subselection_.fillArray(2,atoms);
			if (atoms[0]->findBond(atoms[1]) != NULL)
			{
				displayModel_->beginUndostate("Delete Bond");
				displayModel_->unbondAtoms(atoms[0],atoms[1]);
				displayModel_->endUndostate();
			}
			subselection_.clear();
			gui.modelChanged(FALSE,FALSE,FALSE);
			break;
		// Misc
		case (UA_ATOMADDHYDROGEN):
			if (atomHover_ != NULL)
			{
				displayModel_->beginUndostate("Add Hydrogen to Atom");
				displayModel_->hydrogenSatisfy(atomHover_);
				displayModel_->endUndostate();
				gui.modelChanged(TRUE,FALSE,TRUE);
			}
			break;
		// Model transformations
		case (UA_MANIPROTXY):
		case (UA_MANIPROTZ):
		case (UA_MANIPTRANS):
			// Clear list of rSelection_ if nothing was moved
			if (!hasMoved_) rSelection_.clear();
			displayModel_->finalizeTransform(rSelection_);
			gui.modelChanged(TRUE,FALSE,FALSE);
			break;
		// View changes (no action)
		case (UA_ROTATEXY):
		case (UA_ROTATEZ):
		case (UA_MOVECAM):
		case (UA_ZOOMCAM):
			break;
		default:
			printf("No button_up handler defined for UserAction %i.\n", activeMode_);
			break;
	}
	activeMode_ = UA_NONE;
	dbgEnd(Debug::Calls,"Canvas::endMode");
}

void Canvas::modeMotion(double x, double y)
{
	// Actively update variables when moving the mouse (possibly while performing a given action)
	dbgBegin(Debug::Calls,"Canvas::modeMotion");
	static Vec3<double> delta;
	//static Model *viewtarget;
	if (displayModel_ == NULL)
	{
		printf("Pointless Canvas::modeMotion - datamodel == NULL.\n");
		dbgEnd(Debug::Calls,"Canvas::modeMotion");
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
		case (UA_NONE):
			break;
		case (UA_ROTATEXY):
			displayModel_->rotate(delta.x/2.0,delta.y/2.0);
			break;
		case (UA_ROTATEZ):
			displayModel_->zRotate(delta.x/2.0);
			break;
		case (UA_MOVECAM):
			displayModel_->adjustCamera(delta/15.0,0.0);
			break;
		case (UA_MANIPROTXY):
			displayModel_->rotateSelectionWorld(delta.x/2.0,delta.y/2.0);
			hasMoved_ = TRUE;
			break;
		case (UA_MANIPROTZ):
			displayModel_->rotateSelectionZaxis(delta.x/2.0);
			hasMoved_ = TRUE;
			break;
		case (UA_MANIPTRANS):
			delta.y = -delta.y;
			delta /= displayModel_->translateScale() * 2.0;
			displayModel_->translateSelectionWorld(delta);
			hasMoved_ = TRUE;
			break;
		case (UA_ZOOMCAM):
			if (prefs.hasPerspective()) displayModel_->adjustCamera(0.0,0.0,delta.y,0.0);
			else displayModel_->adjustOrthoSize(delta.y);
			calculateDrawPixelWidth();
			break;
		default:
			break;
	}
	postRedisplay();
	dbgEnd(Debug::Calls,"Canvas::modeMotion");
}

void Canvas::modeScroll(bool scrollup)
{
	// Handle mouse-wheel scroll events.
	// Do the requested wheel action as defined in the control panel
	dbgBegin(Debug::Calls,"Canvas::modeScroll");
	if (displayModel_ == NULL)
	{
		printf("Pointless Canvas::modeScroll - datamodel == NULL.\n");
		dbgEnd(Debug::Calls,"Canvas::modeScroll");
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
			if (prefs.hasPerspective())
				scrollup ? displayModel_->adjustCamera(0.0,0.0,-5.0,0.0) : displayModel_->adjustCamera(0.0,0.0,5.0,0.0);
			else scrollup ? displayModel_->adjustOrthoSize(1.0) : displayModel_->adjustOrthoSize(-1.0);
			calculateDrawPixelWidth();
			break;
	}
	postRedisplay();
	dbgEnd(Debug::Calls,"Canvas::modeScroll");
}
