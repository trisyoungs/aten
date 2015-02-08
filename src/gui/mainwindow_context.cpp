/*
	*** Qt context menu functions
	*** src/gui/contextmenu_funcs.cpp
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

#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/geometry.h"
#include "gui/fragments.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Update context menu
void AtenWindow::updateContextMenu()
{
	msg.enter("AtenWindow::updateContextMenu");
	Model *viewTarget = aten_.currentModel();

	// Enable bond, angle, and torsion editing
	int nselected = (viewTarget == NULL ? 0 : viewTarget->nSelected());
	ui.actionSetBondLength->setEnabled(FALSE);
	ui.actionSetBondAngle->setEnabled(FALSE);
	ui.actionSetTorsionAngle->setEnabled(FALSE);
	if (nselected == 2) ui.actionSetBondLength->setEnabled(TRUE);
	else if (nselected == 3) ui.actionSetBondAngle->setEnabled(TRUE);
	else if (nselected == 4) ui.actionSetTorsionAngle->setEnabled(TRUE);
	
	// (De)Activate glyph menu items based on number of atoms selected
	activateGlyphActions(nselected);
	msg.exit("AtenWindow::updateContextMenu");
}

// Activate glyph actions 
void AtenWindow::activateGlyphActions(int n)
{
	for (int gt=0; gt<Glyph::nGlyphTypes; ++gt) createGlyphActions[gt]->setEnabled( Glyph::nGlyphData( (Glyph::GlyphType) gt) == n);
}

// Show the modelview context menu
void AtenWindow::callContextMenu(Atom *undermouse, int x, int y)
{
	// If there is no atom under the mouse, then exit
	contextAtom_ = undermouse;
	if (contextAtom_ == NULL) return;

	// If the atom under the mouse is selected, just run the popup. If it is not selected, deselect everything else and select it
	QPoint pos(x,y);
//	printf("AtomPopup: model %li, undermouse = %li, nselected = %i\n", viewTarget, target, viewTarget->nSelected());
	Model *viewTarget = aten_.currentModelOrFrame();
	if (!contextAtom_->isSelected())
	{
		viewTarget->beginUndoState("Select atom (Context Menu)");
		viewTarget->selectNone();
		viewTarget->selectAtom(contextAtom_);
		viewTarget->endUndoState();
		postRedisplay();
		// Make sure context menu items are enabled, since nothing may have been selected beforehand
		ui.AtomContextMenu->setEnabled(TRUE);
	}
	
	// Atom selection may have just changed, so update context menu
	updateContextMenu();
	// Run the popup
	ui.AtomContextMenu->exec(pos);
}

// Set atom style
void AtenWindow::setAtomStyle(Atom::DrawStyle ds)
{
	if ((contextAtom_ == NULL) || (aten_.currentModelOrFrame()->nSelected() > 1)) CommandNode::run(Command::AtomStyle, "c", Atom::drawStyle(ds));
	else CommandNode::run(Command::AtomStyle, "ci", Atom::drawStyle(ds), contextAtom_->id()+1);
	contextAtom_ = NULL;
}

void AtenWindow::on_actionAtomStyleStick_triggered(bool checked)
{
	setAtomStyle(Atom::StickStyle);
	postRedisplay();
}

void AtenWindow::on_actionAtomStyleTube_triggered(bool checked)
{
	setAtomStyle(Atom::TubeStyle);
	postRedisplay();
}

void AtenWindow::on_actionAtomStyleSphere_triggered(bool checked)
{
	setAtomStyle(Atom::SphereStyle);
	postRedisplay();
}

void AtenWindow::on_actionAtomStyleScaled_triggered(bool checked)
{
	setAtomStyle(Atom::ScaledStyle);
	postRedisplay();
}

// Set atom labels
void AtenWindow::setAtomLabel(Atom::AtomLabel al)
{
	if ((contextAtom_ == NULL) || (aten_.currentModelOrFrame()->nSelected() > 1)) CommandNode::run(Command::Label, "c", Atom::atomLabel(al));
	else CommandNode::run(Command::Label, "ci", Atom::atomLabel(al), contextAtom_->id()+1);
	contextAtom_ = NULL;
	postRedisplay();
}

// Clear atom labels
void AtenWindow::removeAtomLabels(bool all)
{
	if (all) CommandNode::run(Command::ClearLabels, "");
	else if ((contextAtom_ == NULL) || (aten_.currentModelOrFrame()->nSelected() > 1)) CommandNode::run(Command::RemoveLabels, "");
	else CommandNode::run(Command::RemoveLabels, "i", contextAtom_->id()+1);
	contextAtom_ = NULL;
	postRedisplay();
}

void AtenWindow::on_actionAtomLabelID_triggered(bool checked)
{
	setAtomLabel(Atom::IdLabel);
}

void AtenWindow::on_actionAtomLabelCharge_triggered(bool checked)
{
	setAtomLabel(Atom::ChargeLabel);
}

void AtenWindow::on_actionAtomLabelFFType_triggered(bool checked)
{
	setAtomLabel(Atom::TypeLabel);
}

void AtenWindow::on_actionAtomLabelElement_triggered(bool checked)
{
	setAtomLabel(Atom::ElementLabel);
}

void AtenWindow::on_actionAtomLabelFFEquiv_triggered(bool checked)
{
	setAtomLabel(Atom::EquivLabel);
}

void AtenWindow::on_actionAtomLabelClear_triggered(bool checked)
{
	removeAtomLabels(FALSE);
}

void AtenWindow::on_actionAtomLabelClearAll_triggered(bool checked)
{
	removeAtomLabels(TRUE);
}

// Reset atom custom colour to element colour
void AtenWindow::on_actionAtomColourReset_triggered(bool checked)
{
	CommandNode::run(Command::RecolourAtoms, "");
	contextAtom_ = NULL;
	postRedisplay();
}

// Reset atom custom colour to element colour
void AtenWindow::on_actionAtomColourSet_triggered(bool checked)
{
	QColor oldcol, newcol;
	// Get colour of clicked atom and convert into a QColor
	if (contextAtom_ != NULL) oldcol.setRgbF( contextAtom_->colour()[0], contextAtom_->colour()[1], contextAtom_->colour()[2], contextAtom_->colour()[3] );
	else oldcol.setRgbF(1.0, 1.0, 1.0, 1.0);
		
	// Request a colour dialog
	bool ok = FALSE;
	newcol.setRgba(QColorDialog::getRgba(oldcol.rgba(), &ok, this));
	if (!ok) return;
	// Store new colour
	CommandNode::run(Command::ColourAtoms, "dddd", newcol.redF(), newcol.greenF(), newcol.blueF(), newcol.alphaF());
	contextAtom_ = NULL;
	// Set colour scheme menu option automatically if necessary
	if (prefs.colourScheme() != Prefs::CustomScheme) ui.actionSchemeCustom->trigger();
	msg.print("Colouring scheme changed to 'custom'.\n");
	postRedisplay();
}

// Shift atom order up
void AtenWindow::on_actionOrderShiftUp_triggered(bool checked)
{
	CommandNode::run(Command::ShiftUp, "i", 1);
	updateWidgets(AtenWindow::CanvasTarget);
}

// Shift atom order down
void AtenWindow::on_actionOrderShiftDown_triggered(bool checked)
{
	CommandNode::run(Command::ShiftDown, "i", 1);
	updateWidgets(AtenWindow::CanvasTarget);
}

// Shift atoms to beginning of list
void AtenWindow::on_actionOrderMoveToStart_triggered(bool checked)
{
	CommandNode::run(Command::MoveToStart, "");
	updateWidgets(AtenWindow::CanvasTarget);
}

// Shift atoms to end of list
void AtenWindow::on_actionOrderMoveToEnd_triggered(bool checked)
{
	CommandNode::run(Command::MoveToEnd, "");
	updateWidgets(AtenWindow::CanvasTarget);
}

// Reorder atoms in current selection
void AtenWindow::on_actionOrderReorder_triggered(bool checked)
{
	CommandNode::run(Command::ReOrder, "");
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Set atom hidden
void AtenWindow::setAtomHidden(bool hidden)
{
	if ((contextAtom_ == NULL) || (aten_.currentModelOrFrame()->nSelected() > 1)) 
	{
		if (hidden) CommandNode::run(Command::Hide, "");
		else CommandNode::run(Command::Show, "");
	}
	else
	{
		if (hidden) CommandNode::run(Command::Hide, "i", contextAtom_->id()+1);
		else CommandNode::run(Command::Show, "i", contextAtom_->id()+1);
	}
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

void AtenWindow::on_actionAtomHide_triggered(bool checked)
{
	setAtomHidden(TRUE);
}

void AtenWindow::on_actionAtomProbe_triggered(bool checked)
{
	if (contextAtom_ != NULL) contextAtom_->print();
}

void AtenWindow::on_actionAtomFixPosition_triggered(bool checked)
{
	if ((contextAtom_ == NULL) || (aten_.currentModelOrFrame()->nSelected() > 1)) CommandNode::run(Command::Fix, "");
	else CommandNode::run(Command::Fix, "i", contextAtom_->id()+1);
	postRedisplay();
}

void AtenWindow::on_actionAtomFreePosition_triggered(bool checked)
{
	if ((contextAtom_ == NULL) || (aten_.currentModelOrFrame()->nSelected() > 1)) CommandNode::run(Command::Free, "");
	else CommandNode::run(Command::Free, "i", contextAtom_->id()+1);
	postRedisplay();
}

void AtenWindow::on_actionSetBondLength_triggered(bool checked)
{
// 	gui.geometryWidget->showWidget(); ATEN2 TODO
}

void AtenWindow::on_actionSetBondAngle_triggered(bool checked)
{
// 	gui.geometryWidget->showWidget(); ATEN2 TODO
}

void AtenWindow::on_actionSetTorsionAngle_triggered(bool checked)
{
// 	gui.geometryWidget->showWidget(); ATEN2 TODO
}

void AtenWindow::on_actionCentreAtOrigin_triggered(bool checked)
{
	CommandNode::run(Command::Centre, "ddd", 0.0, 0.0, 0.0);
	postRedisplay();
}

void AtenWindow::on_actionCreateFragment_triggered(bool checked)
{
	Model *viewTarget = aten_.currentModelOrFrame();
	aten_.addFragmentFromSelection(viewTarget, "Selections");
	fragmentsWidget->refresh();
}

void AtenWindow::createGlyph()
{
	// Cast sending QAction and grab filename
	QAction *action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("AtenWindow::loadRecent - Sender was not a QAction.\n");
		return;
	}
	// Which action was it?
	int n;
	for (n=0; n<Glyph::nGlyphTypes; ++n) if (createGlyphActions[n] == action) break;
	if (n == Glyph::nGlyphTypes)
	{
		printf("Internal Error - Failed to 'cast' action into glyph type.\n");
		return;
	}
	Glyph::GlyphType gt = (Glyph::GlyphType) n;
	// Create glyph in model
	CommandNode::run(Command::NewGlyph, "c", Glyph::glyphType(gt));
	// Set data to atom selection
	Model *viewTarget = aten_.currentModelOrFrame();
	n = 1;
	for (Refitem<Atom,int> *ri = viewTarget->selection(); ri != NULL; ri = ri->next)
	{
		CommandNode::run(Command::GlyphAtomR, "ii", n, ri->item->id()+1);
		n++;
	}
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::GlyphsTarget);
}

