/*
	*** Aten Window - Interaction Modes
	*** src/gui/mainwindow_interaction.cpp
	Copyright T. Youngs 2007-2016

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
#include "main/aten.h"

// Set selected mode
void AtenWindow::setSelectedMode(UserAction::Action ua)
{
	Messenger::enter("AtenWindow::setSelectedMode");

	// Set (check) relevant action or button based on supplied UserAction
	if (!TMenuButton::setGroupButtonChecked("UserActions", ua)) Messenger::print(Messenger::Verbose, "AtenWindow::setSelectedMode() - No button associated to user action %i.\n", ua);

	// Get current active model
	Model* source = aten_.currentModelOrFrame();
	if (source == NULL)
	{
		printf("AtenWindow::setSelectedMode - Warning - no current model or frame.\n");
		Messenger::exit("AtenWindow::setSelectedMode");
		return;
	}

	// Enable picking mode if relevant
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
			actionBeforePick_ = selectedMode_;
			break;
		default:
			pickEnabled_ = false;
			pickedAtoms_.clear();
			actionBeforePick_ = UserAction::NoAction;
			break;
	}

	// Finally, set the mode and refresh
	selectedMode_ = ua;
	
	// Change mouse cursor depending on mode
	if (selectedMode_ == UserAction::SelectAction) setCursor(Qt::ArrowCursor);
	else setCursor(Qt::CrossCursor);

	// Update the necessary targets
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::StatusBarTarget);

	Messenger::exit("AtenWindow::setSelectedMode");
}

// Set the active mode to the current user mode
void AtenWindow::useSelectedMode()
{
	// Only activate the mode if the editable_ flag is set
	if (!editable_) return;

	activeMode_ = selectedMode_;
}

// Return the currently selected mode
UserAction::Action AtenWindow::selectedMode() const
{
	return selectedMode_;
}

// Return the currently active mode
UserAction::Action AtenWindow::activeMode() const
{
	return activeMode_;
}

// Set current build geometry
void AtenWindow::setBuildGeometry(Atom::AtomGeometry ag)
{
	buildGeometry_ = ag;
}

// Return current build geometry
Atom::AtomGeometry AtenWindow::buildGeometry() const
{
	return buildGeometry_;
}

// Current drawing depth for certain tools
double AtenWindow::currentDrawDepth()
{
	return currentDrawDepth_;
}

// Begin Mode
void AtenWindow::beginMode(Prefs::MouseButton button, bool* keyModifiers)
{
	Messenger::enter("AtenWindow::beginMode");
	static bool manipulate, zrotate;
	static int n;
	static Atom* i;

	// Do the requested action as defined in the control panel, but only if another action
	// isn't currently in progress. Set the UserAction based on the mouse button that sent
	// the signal, current selection / draw modes and key modifier states.

	// Get current active model
	Model* targetModel = aten_.currentModelOrFrame();
	if (targetModel == NULL)
	{
		printf("Pointless AtenWindow::beginMode - no target model.\n");
		Messenger::exit("AtenWindow::beginMode");
		return;
	}

	// Check for modifier keys
	zrotate = false;
	manipulate = false;

	for (n=0; n<3; n++)
	{
		if (keyModifiers[n])
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
						if (clickedAtom_ == NULL)
						{
							targetModel->beginUndoState("Draw Atoms");
							currentDrawDepth_ = prefs.drawDepth();
							Vec3<double> rMouseDown = ui.MainView->rMouseDown();
							i = targetModel->addAtom(currentBuildElement(), targetModel->screenToModel(rMouseDown.x, rMouseDown.y, currentDrawDepth_));
							targetModel->endUndoState();
							clickedAtom_ = i;
						}
						else currentDrawDepth_ = targetModel->modelToWorld(clickedAtom_->r()).z;
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
			Instead, store pointers to all selected atoms in a RefList, along
			with their current positions.
			*/
			oldPositions_.clear();
			for (RefListItem<Atom,int>* ri = targetModel->selection(); ri != NULL; ri = ri->next) oldPositions_.add(ri->item, ri->item->r());
			targetModel->prepareTransform();
		}
	}

	update();
	Messenger::exit("AtenWindow::beginMode");
}

// End Mode
void AtenWindow::endMode(Prefs::MouseButton button, bool* keyModifiers)
{
	// Finalize the current action on the model
	Messenger::enter("AtenWindow::endMode");
	double area, radius;
	Vec4<double> screenr;
	Vec3<double> v, rMouseUp, rMouseDown;
	QString methodName;
	ReturnValue rv;
	Atom* atoms[4], *i;
	Bond* b;
	Bond::BondType bt;
	
	// Get current active model
	Model* targetModel = aten_.currentModelOrFrame();
	if (targetModel == NULL)
	{
		printf("Pointless AtenWindow::endMode - no source model.\n");
		Messenger::exit("AtenWindow::endMode");
		return;
	}
	
	// Store modifier states for convenience
	bool shifted = keyModifiers[Prefs::ShiftKey];
	bool ctrled = keyModifiers[Prefs::CtrlKey];
	bool modded = (shifted || ctrled);
	bool noFold = ui.BuildDrawPreventFoldButton->isChecked();

	// Get coordinates of mouse press and release events
	rMouseDown = ui.MainView->rMouseDown();
	rMouseUp = ui.MainView->rMouseUp();

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
			area = fabs(rMouseUp.x - rMouseDown.x) * fabs(rMouseUp.y - rMouseDown.y);
			targetModel->beginUndoState("Change Selection");
			// If neither shift nor ctrl are not held down, deselect the current selection
			if (!modded) targetModel->selectNone();
			// Do either point select or box select based on the size of the selected area
			if (area > 50.0) targetModel->selectBox(rMouseDown.x, ui.MainView->contextHeight()-rMouseDown.y, rMouseUp.x, ui.MainView->contextHeight()-rMouseUp.y, ctrled);
			else if (clickedAtom_ != NULL)
			{
				if (shifted) targetModel->selectionToggle(clickedAtom_);
				else if (ctrled) targetModel->deselectAtom(clickedAtom_);
				else targetModel->selectAtom(clickedAtom_);
			}
			targetModel->endUndoState();
			updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
			break;
		// Other selection operations
		case (UserAction::SelectBoundAction):
			targetModel->beginUndoState("Select Bound");
			if (!modded) targetModel->selectNone();
			if (clickedAtom_ != NULL) targetModel->selectTree(clickedAtom_, false, ctrled);
			targetModel->endUndoState();
			updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
			break;
		case (UserAction::SelectElementAction):
			targetModel->beginUndoState("Select Element");
			if (!modded) targetModel->selectNone();
			if (clickedAtom_ != NULL) targetModel->selectElement(clickedAtom_, false, ctrled);
			targetModel->endUndoState();
			updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
			break;
		case (UserAction::SelectRadialAction):
			targetModel->beginUndoState("Select Radial");
			if (!modded) targetModel->selectNone();
			if (clickedAtom_ != NULL)
			{
				radius = (rMouseDown-rMouseUp).magnitude();
				targetModel->modelToWorld(clickedAtom_->r(), &screenr, prefs.styleRadius(clickedAtom_->style(), clickedAtom_->element()));
				radius /= screenr.w * prefs.styleRadius(clickedAtom_->style(), clickedAtom_->element());
				targetModel->selectRadial(clickedAtom_, radius);
			}
			targetModel->endUndoState();
			updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
			break;
		// Measurements
		case (UserAction::MeasureDistanceAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 2) break;
			targetModel->beginUndoState("Measure Distance");
			pickedAtoms_.fillArray(2, atoms);
			targetModel->addDistanceMeasurement(atoms[0], atoms[1]);
			targetModel->endUndoState();
			pickedAtoms_.clear();
			updateWidgets(AtenWindow::MainViewTarget);
			break;
		case (UserAction::MeasureAngleAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 3) break;
			targetModel->beginUndoState("Measure Angle");
			pickedAtoms_.fillArray(3, atoms);
			targetModel->addAngleMeasurement(atoms[0], atoms[1], atoms[2]);
			targetModel->endUndoState();
			pickedAtoms_.clear();
			updateWidgets(AtenWindow::MainViewTarget);
			break;
		case (UserAction::MeasureTorsionAction):
			// Must be two atoms in subselection to continue
			if (pickedAtoms_.nItems() != 4) break;
			targetModel->beginUndoState("Measure Torsion");
			pickedAtoms_.fillArray(4, atoms);
			targetModel->addTorsionMeasurement(atoms[0], atoms[1], atoms[2], atoms[3]);
			targetModel->endUndoState();
			pickedAtoms_.clear();
			updateWidgets(AtenWindow::MainViewTarget);
			break;
		// Draw atoms
		case (UserAction::DrawAtomsAction):
			// If there is no atom under the mouse we draw one
			i = targetModel->atomOnScreen(rMouseUp.x, ui.MainView->contextHeight()-rMouseUp.y);
			if ((clickedAtom_ == i) && (i != NULL)) break;
			targetModel->beginUndoState("Draw Chain");
			if (i == NULL)
			{
				// No atom under the mouse, so draw an atom at previous draw depth
				i = targetModel->addAtom(currentBuildElement(), targetModel->screenToModel(rMouseUp.x, rMouseUp.y, currentDrawDepth_));
			}
			// Now bond the atoms, unless clickedAtom_ and i are the same (i.e. the button was clicked and not moved)
			if (clickedAtom_ != i)
			{
				// Search for existing bond between atoms
				b = i->findBond(clickedAtom_);
				if (b == NULL) bt = Bond::Single;
				else
				{
					bt = Bond::increase(b->type());
					targetModel->unbondAtoms(i,clickedAtom_);
				}
				targetModel->bondAtoms(i,clickedAtom_,bt);
			}

			// Centre model if the relevant option is checked
			if (ui.BuildDrawKeepCenteredButton->isChecked())
			{
				targetModel->selectAll(true);
				targetModel->centre(0.0, 0.0, 0.0, false, false, false, true);
			}

			targetModel->endUndoState();
			updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
			break;
		// Draw fragments
		case (UserAction::DrawFragmentsAction):
			if (!aten_.currentFragment()) break;
			if (clickedAtom_ != NULL)
			{
				targetModel->beginUndoState("Draw Anchored Fragment");
				aten_.currentFragment()->pasteAnchoredModel(clickedAtom_, keyModifiers[Prefs::ShiftKey], aten_.fragmentBondId(), targetModel, true);
			}
			else
			{
				// No atom under the moust pointer, so draw on at the prefs drawing depth in its current orientation
				targetModel->beginUndoState("Draw Fragment");
				aten_.currentFragment()->pasteOrientedModel(targetModel->screenToModel(rMouseDown.x, rMouseDown.y, prefs.drawDepth()), targetModel);
			}
			targetModel->endUndoState();

			// Centre model if the relevant option is checked
			if (ui.BuildDrawKeepCenteredButton->isChecked())
			{
				targetModel->selectAll(true);
				targetModel->centre(0.0, 0.0, 0.0, false, false, false, true);
			}

			updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
			break;
		case (UserAction::DrawTransmuteAction):
			if (clickedAtom_ == NULL) break;
			targetModel->beginUndoState("Transmute");
			// If SHIFT was held, transmute all atoms of the same element...
			if (shifted)
			{
				int element = clickedAtom_->element();
				for (Atom* i = targetModel->atoms(); i != NULL; i = i->next) if (i->element() == element) targetModel->transmuteAtom(i, currentBuildElement());
			}
			else targetModel->transmuteAtom(clickedAtom_, currentBuildElement());
			targetModel->endUndoState();
			updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
			break;
		case (UserAction::DrawDeleteAction):
			if (clickedAtom_ == NULL) break;
			i = targetModel->atomOnScreen(rMouseUp.x, ui.MainView->contextHeight()-rMouseUp.y);
			if (i != clickedAtom_)
			{
				// Delete specific bond between atoms....
				targetModel->beginUndoState("Delete Bond");
				targetModel->unbondAtoms(clickedAtom_, i);
				targetModel->endUndoState();
			}
			else if (shifted)
			{
				targetModel->beginUndoState("Delete Bonds to Atom");
				while (clickedAtom_->bonds() != NULL)
				{
					targetModel->unbondAtoms(clickedAtom_, clickedAtom_->bonds()->item->partner(clickedAtom_));
				}
				targetModel->endUndoState();
			}
			else
			{
				targetModel->beginUndoState("Delete Atom");
				targetModel->deleteAtom(clickedAtom_);
				targetModel->endUndoState();
			}
			updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
			break;
		case (UserAction::DrawProbeAction):
			if (clickedAtom_ != NULL) clickedAtom_->print();
			break;
		// Misc Building
		case (UserAction::DrawAddHydrogenAction):
			if (clickedAtom_ != NULL)
			{
				targetModel->beginUndoState("Add Hydrogen to Atom");
				targetModel->hydrogenSatisfy(clickedAtom_);
				targetModel->endUndoState();
				updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
			}
			break;
		case (UserAction::DrawGrowAtomsAction):
			if (clickedAtom_ != NULL)
			{
				ReturnValue distance;
				ui.BuildDrawGrowButton->callPopupMethod("distance", distance);
				if (shifted)
				{
					targetModel->beginUndoState("Grow Atom (unbound)");
					targetModel->growAtom(clickedAtom_, currentBuildElement(), distance.asDouble(), buildGeometry_, false);
				}
				else
				{
					targetModel->beginUndoState("Grow Atom");
					targetModel->growAtom(clickedAtom_, currentBuildElement(), distance.asDouble(), buildGeometry_, true);
				}
				targetModel->endUndoState();
				updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
			}
			break;
		// Model transformations
		case (UserAction::TransformRotateXYAction):
		case (UserAction::TransformRotateZAction):
		case (UserAction::TransformTranslateAction):
			// Clear list of oldPositions_ if nothing was moved
			if (!ui.MainView->mouseHasMoved()) oldPositions_.clear();
			targetModel->finalizeTransform(oldPositions_, "Transform Selection", noFold);
			updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
			break;
		// View changes (no action)
		case (UserAction::RotateXYAction):
		case (UserAction::RotateZAction):
		case (UserAction::TranslateAction):
		case (UserAction::ZoomAction):
			break;
		// Manual picking modes (for axis definitions etc.)
		case (UserAction::ShiftPickVectorAction):
			if (pickedAtoms_.nItems() != UserActions[selectedMode_].nAtomsToPick) break;
			rv = pickedAtoms_.last()->item->r() - pickedAtoms_.first()->item->r();
			setSelectedMode(actionBeforePick_);
			ui.TransformPositionShiftButton->callPopupMethod("setShiftVector", rv);
			ui.TransformPositionShiftButton->popup();
			break;
		case (UserAction::RotatePickAxisAction):
			if (pickedAtoms_.nItems() != UserActions[selectedMode_].nAtomsToPick) break;
			rv = pickedAtoms_.last()->item->r() - pickedAtoms_.first()->item->r();
			setSelectedMode(actionBeforePick_);
			ui.TransformTransformRotateButton->callPopupMethod("setRotationVector", rv);
			ui.TransformTransformRotateButton->popup();
			break;
		case (UserAction::TransformPickAAction):
		case (UserAction::TransformPickBAction):
		case (UserAction::TransformPickCAction):
			if (pickedAtoms_.nItems() != UserActions[selectedMode_].nAtomsToPick) break;
			rv = pickedAtoms_.last()->item->r() - pickedAtoms_.first()->item->r();
			setSelectedMode(actionBeforePick_);
			methodName.sprintf("set%cVector", 88+(endingMode-UserAction::TransformPickAAction));
			ui.TransformTransformMultiplyButton->callPopupMethod(methodName, rv);
			ui.TransformTransformMultiplyButton->popup();
			break;
		case (UserAction::ConvertSourcePickAAction):
		case (UserAction::ConvertSourcePickBAction):
		case (UserAction::ConvertSourcePickCAction):
			if (pickedAtoms_.nItems() != UserActions[selectedMode_].nAtomsToPick) break;
			rv = pickedAtoms_.last()->item->r() - pickedAtoms_.first()->item->r();
			setSelectedMode(actionBeforePick_);
			methodName.sprintf("setSource%cVector", 88+(endingMode-UserAction::ConvertSourcePickAAction));
			ui.TransformTransformConvertButton->callPopupMethod(methodName, rv);
			ui.TransformTransformConvertButton->popup();
			break;
		case (UserAction::ConvertTargetPickAAction):
		case (UserAction::ConvertTargetPickBAction):
		case (UserAction::ConvertTargetPickCAction):
			if (pickedAtoms_.nItems() != UserActions[selectedMode_].nAtomsToPick) break;
			rv = pickedAtoms_.last()->item->r() - pickedAtoms_.first()->item->r();
			setSelectedMode(actionBeforePick_);
			methodName.sprintf("setTarget%cVector", 88+(endingMode-UserAction::ConvertTargetPickAAction));
			ui.TransformTransformConvertButton->callPopupMethod(methodName, rv);
			ui.TransformTransformConvertButton->popup();
			break;
		default:
			printf("No endMode handler defined for UserAction %i.\n", endingMode);
			break;
	}

	Messenger::exit("UserAction::endMode");
}

// Notify that an Atom was clicked
void AtenWindow::atomClicked(Atom* atom)
{
	// Set the clicked atom
	clickedAtom_ = atom;

	// Perform atom picking before entering mode (if required)
	if (pickEnabled_ && (atom != NULL))
	{
		// Don't add the same atom more than once
		if (pickedAtoms_.contains(atom) == NULL)
		{
			pickedAtoms_.add(atom);
			Messenger::print(Messenger::Verbose, "Adding atom %i to canvas subselection.", atom->id());
		}
		else Messenger::print(Messenger::Verbose, "Atom %i is already in canvas subselection.", atom->id());
	}
}

// Return the last clicked atom
Atom* AtenWindow::clickedAtom()
{
	return clickedAtom_;
}

// Return start of picked atom list
RefListItem<Atom,int>* AtenWindow::pickedAtoms()
{
	return pickedAtoms_.first();
}

// Return RefList of selected atoms and their positions so manipulations may be un-done
RefList< Atom,Vec3<double> >& AtenWindow::oldPositions()
{
	return oldPositions_;
}
