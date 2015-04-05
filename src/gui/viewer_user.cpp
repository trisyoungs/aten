/*
	*** Viewer - User mode actions
	*** src/gui/viewer_user.cpp
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

#include <QMouseEvent>
#include "gui/mainwindow.h"
#include "gui/fragments.h"
#include "main/aten.h"

// Set selected mode
void Viewer::setSelectedMode(UserAction::Action ua, int atomsToPick, void (*callback)(Reflist<Atom,int>*))
{
	Messenger::enter("Viewer::setSelectedMode");

	// Get current active model
	Model* source = aten_->currentModelOrFrame();
	if (source == NULL)
	{
		printf("Pointless Viewer::setSelectedMode - no source model.\n");
		Messenger::exit("Viewer::setSelectedMode");
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
			pickEnabled_ = true;
			pickedAtoms_.clear();
			break;
		default:
			pickEnabled_ = false;
			break;
	}

	// Finally, set the mode and refresh
	selectedMode_ = ua;
	atenWindow_->setActiveUserAction(ua);
	
	// Change mouse cursor depending on mode
	if (selectedMode_ == UserAction::SelectAction) setCursor(Qt::ArrowCursor);
	else setCursor(Qt::CrossCursor);

	atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::StatusBarTarget);
	Messenger::exit("Viewer::setSelectedMode");
}

// Set the active mode to the current user mode
void Viewer::useSelectedMode()
{
	activeMode_ = selectedMode_;
}

// Return the currently selected mode
UserAction::Action Viewer::selectedMode() const
{
	return selectedMode_;
}

// Return the currently active mode
UserAction::Action Viewer::activeMode() const
{
	return activeMode_;
}

// Set current drawing element
void Viewer::setBuildElement(short int el)
{
	buildElement_ = el;
}

// Return current drawing element
short int Viewer::buildElement() const
{
	return buildElement_;
}

// Set current build geometry
void Viewer::setBuildGeometry(Atom::AtomGeometry ag)
{
	buildGeometry_ = ag;
}

// Return current build geometry
Atom::AtomGeometry Viewer::buildGeometry() const
{
	return buildGeometry_;
}

// Current drawing depth for certain tools
double Viewer::currentDrawDepth()
{
	return currentDrawDepth_;
}

// Set whether to accept editing actions (i.e. anything other than view manipulation)
void Viewer::setEditable(bool b)
{
	editable_ = b;
}

// Return whether to accept editing actions (i.e. anything other than view manipulation)
bool Viewer::editable()
{
	return editable_;
}

// Begin Mode
void Viewer::beginMode(Prefs::MouseButton button)
{
	Messenger::enter("Viewer::beginMode");
	static bool manipulate, zrotate;
	static int n;
	static Atom* i;
	// Do the requested action as defined in the control panel, but only if another action
	// isn't currently in progress. Set the UserAction based on the mouse button that sent
	// the signal, current selection / draw modes and key modifier states.

	// Get current active model
	Model* source = aten_->currentModelOrFrame();
	if (source == NULL)
	{
		printf("Pointless Viewer::beginMode - no source model.\n");
		Messenger::exit("Viewer::beginMode");
		return;
	}
	
	// Note the mouse button pressed
	mouseButton_[button] = true;
	// Check for modifier keys
	zrotate = false;
	manipulate = false;
	hasMoved_ = false;

	for (n=0; n<3; n++)
	{
		if (keyModifier_[n])
		{
			switch (prefs.keyAction(Prefs::ModifierKey(n)))
			{
				case (Prefs::ManipulateKeyAction):
					manipulate = true;
					break;
				case (Prefs::ZrotateKeyAction):
					zrotate = true;
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
					case (UserAction::DrawAtomsAction):
						// If there is currently no atom under the mouse, draw one...
						if (atomClicked_ == NULL)
						{
							source->beginUndoState("Draw Atoms");
							currentDrawDepth_ = prefs.drawDepth();
							i = source->addAtom(buildElement_, source->screenToModel(rMouseDown_.x, rMouseDown_.y, currentDrawDepth_));
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
			for (Refitem<Atom,int>* ri = source->selection(); ri != NULL; ri = ri->next) oldPositions_.add(ri->item, ri->item->r());
			source->prepareTransform();
		}
	}

	update();
	Messenger::exit("Viewer::beginMode");
}

// End Mode
void Viewer::endMode(Prefs::MouseButton button)
{
	// Finalize the current action on the model
	Messenger::enter("Viewer::endMode");
	double area, radius;
	Vec4<double> screenr;
	Atom* atoms[4], *i;
	Bond *b;
	Bond::BondType bt;
	Fragment* frag;
	
	// Get current active model
	Model* source = aten_->currentModelOrFrame();
	if (source == NULL)
	{
		printf("Pointless Viewer::endMode - no source model.\n");
		Messenger::exit("Viewer::endMode");
		return;
	}
	
	// Store modifier states for convenience
	bool shifted = keyModifier_[Prefs::ShiftKey];
	bool ctrled = keyModifier_[Prefs::CtrlKey];
	bool modded = (shifted || ctrled);
	bool noFold = atenWindow_->ui.BuildOptionsPreventFoldCheck->isChecked();
	
	// Reset mouse button flag
	mouseButton_[button] = false;

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
			if (area > 50.0) source->selectBox(rMouseDown_.x, contextHeight_-rMouseDown_.y, rMouseUp_.x, contextHeight_-rMouseUp_.y, ctrled);
			else if (atomClicked_ != NULL)
			{
				if (shifted) source->selectionToggle(atomClicked_);
				else if (ctrled) source->deselectAtom(atomClicked_);
				else source->selectAtom(atomClicked_);
			}
			source->endUndoState();
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget+AtenWindow::GeometryTarget);
			break;
		// Other selection operations
		case (UserAction::SelectBoundAction):
			source->beginUndoState("Select Bound");
			if (!modded) source->selectNone();
			if (atomClicked_ != NULL)	source->selectTree(atomClicked_, false, ctrled);
			source->endUndoState();
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget+AtenWindow::GeometryTarget);
			break;
		case (UserAction::SelectElementAction):
			source->beginUndoState("Select Element");
			if (!modded) source->selectNone();
			if (atomClicked_ != NULL) source->selectElement(atomClicked_, false, ctrled);
			source->endUndoState();
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget+AtenWindow::GeometryTarget);
			break;
		case (UserAction::SelectRadialAction):
			source->beginUndoState("Select Radial");
			if (!modded) source->selectNone();
			if (atomClicked_ != NULL)
			{
				radius = (rMouseDown_-rMouseUp_).magnitude();
				source->modelToWorld(atomClicked_->r(), &screenr, prefs.styleRadius(atomClicked_->style(), atomClicked_->element()));
				radius /= screenr.w * prefs.styleRadius(atomClicked_->style(), atomClicked_->element());
				source->selectRadial(atomClicked_, radius);
			}
			source->endUndoState();
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget+AtenWindow::GeometryTarget);
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
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget);
			break;
		case (UserAction::MeasureAngleAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 3) break;
			source->beginUndoState("Measure Angle");
			pickedAtoms_.fillArray(3,atoms);
			source->addAngleMeasurement(atoms[0],atoms[1],atoms[2]);
			source->endUndoState();
			pickedAtoms_.clear();
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget);
			break;
		case (UserAction::MeasureTorsionAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 4) break;
			source->beginUndoState("Measure Torsion");
			pickedAtoms_.fillArray(4,atoms);
			source->addTorsionMeasurement(atoms[0],atoms[1],atoms[2],atoms[3]);
			source->endUndoState();
			pickedAtoms_.clear();
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget);
			break;
		// Draw atoms
		case (UserAction::DrawAtomsAction):
			// If there is no atom under the mouse we draw one
			i = source->atomOnScreen(rMouseUp_.x, contextHeight_-rMouseUp_.y);
			if ((atomClicked_ == i) && (i != NULL)) break;
			source->beginUndoState("Draw Chain");
			if (i == NULL)
			{
				// No atom under the mouse, so draw an atom at previous draw depth
				i = source->addAtom(buildElement_, source->screenToModel(rMouseUp_.x, rMouseUp_.y, currentDrawDepth_));
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
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
			break;
		// Draw fragments
		case (UserAction::DrawFragmentsAction):
			frag = atenWindow_->fragmentsWidget->currentFragment();
			if (frag == NULL) break;
			if (atomClicked_ != NULL)
			{
				source->beginUndoState("Draw Attached Fragment");
				frag->pasteAnchoredModel(atomClicked_, keyModifier_[Prefs::ShiftKey], atenWindow_->fragmentsWidget->bondId(), source, atenWindow_->fragmentsWidget->ui.AdjustBondLengthCheck->isChecked());
			}
			else
			{
				// No atom under the moust pointer, so draw on at the prefs drawing depth in its current orientation
				source->beginUndoState("Draw Fragment");
				frag->pasteOrientedModel(source->screenToModel(rMouseDown_.x, rMouseDown_.y, prefs.drawDepth()), source);
			}
			source->endUndoState();
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
			break;
		case (UserAction::DrawTransmuteAction):
			if (atomClicked_ == NULL) break;
			source->beginUndoState("Transmute");
			// If SHIFT was held, transmute all atoms of the same element...
			if (shifted)
			{
				int element = atomClicked_->element();
				for (Atom* i = source->atoms(); i != NULL; i = i->next) if (i->element() == element) source->transmuteAtom(i,buildElement_);
			}
			else source->transmuteAtom(atomClicked_, buildElement_);
			source->endUndoState();
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
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
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
			break;
		case (UserAction::DrawProbeAction):
			if (atomClicked_ != NULL) atomClicked_->print();
			break;
		// Bonding
// 		case (UserAction::DrawBondSingleAction):
// 		case (UserAction::DrawBondDoubleAction):
// 		case (UserAction::DrawBondTripleAction):
// 			if (pickedAtoms_.nItems() == 0) break;
// 			// Must be two atoms in subselection to continue, or we must be hovering over a different atom (which we'll add to the list)
// 			if (pickedAtoms_.nItems() == 1)
// 			{
// 				i = source->atomOnScreen(rMouseUp_.x, contextHeight_-rMouseUp_.y);
// 				if (i == NULL) break;
// 				if (pickedAtoms_.last()->item != i) pickedAtoms_.add(i);
// 			}
// 			if (pickedAtoms_.nItems() != 2) break;
// 			pickedAtoms_.fillArray(2,atoms);
// 			b = atoms[0]->findBond(atoms[1]);
// 			if (b == NULL)
// 			{
// 				source->beginUndoState("Bond Atoms");
// 				source->bondAtoms(atoms[0],atoms[1],Bond::BondType(endingMode-UserAction::DrawBondSingleAction+1));
// 				source->endUndoState();
// 			}
// 			else
// 			{
// 				source->beginUndoState("Change Bond");
// 				source->changeBond(b,Bond::BondType(endingMode-UserAction::DrawBondSingleAction+1));
// 				source->endUndoState();
// 			}
// 			pickedAtoms_.clear();
// 			atenWindow_->updateWidgets(AtenWindow::CanvasTarget);
// 			break;
		// Delete bond
// 		case (UserAction::DrawDeleteBondAction):
// 			// Must be two atoms in subselection to continue
// 			if (pickedAtoms_.nItems() != 2) break;
// 			pickedAtoms_.fillArray(2,atoms);
// 			if (atoms[0]->findBond(atoms[1]) != NULL)
// 			{
// 				source->beginUndoState("Delete Bond");
// 				source->unbondAtoms(atoms[0],atoms[1]);
// 				source->endUndoState();
// 			}
// 			pickedAtoms_.clear();
// 			atenWindow_->updateWidgets(AtenWindow::CanvasTarget);
// 			break;
		// Misc Building
		case (UserAction::DrawAddHydrogenAction):
			if (atomClicked_ != NULL)
			{
				source->beginUndoState("Add Hydrogen to Atom");
				source->hydrogenSatisfy(atomClicked_);
				source->endUndoState();
				atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
			}
			break;
		case (UserAction::DrawGrowAtomsAction):
			if (atomClicked_ != NULL)
			{
				if (shifted)
				{
					source->beginUndoState("Grow Atom (unbound)");
					source->growAtom(atomClicked_, buildElement_, -1.0, buildGeometry_, false);
				}
				else
				{
					source->beginUndoState("Grow Atom");
					source->growAtom(atomClicked_, buildElement_, -1.0, buildGeometry_, true);
				}
				source->endUndoState();
				atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
			}
			break;
		// Model transformations
		case (UserAction::TransformRotateXYAction):
		case (UserAction::TransformRotateZAction):
		case (UserAction::TransformTranslateAction):
			// Clear list of oldPositions_ if nothing was moved
			if (!hasMoved_) oldPositions_.clear();
			source->finalizeTransform(oldPositions_, "Transform Selection", noFold);
			atenWindow_->updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
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
			atenWindow_->setActiveUserAction(actionBeforePick_);
			pickedAtoms_.clear();
			nAtomsToPick_ = -1;
			break;
		default:
			printf("No endMode handler defined for UserAction %i.\n", endingMode);
			break;
	}
	Messenger::exit("UserAction::endMode");
}

// Returns the atom currently under the mouse
Atom* Viewer::atomClicked()
{
	return atomClicked_;
}

// Clears the list of picked atoms
void Viewer::clearPicked()
{
	pickedAtoms_.clear();
}

// Return start of picked atom list
Refitem<Atom,int>* Viewer::pickedAtoms()
{
	return pickedAtoms_.first();
}
