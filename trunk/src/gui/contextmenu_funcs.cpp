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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/geometry.h"
#include "gui/toolbox.h"
#include "gui/fragments.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Local variables
Atom *target = NULL;

// Update context menu
void GuiQt::updateContextMenu()
{
	msg.enter("GuiQt::updateContextMenu");
	Model *viewTarget = aten.currentModel();
	// Enable bond, angle, and torsion editing
	int nselected = (viewTarget == NULL ? 0 : viewTarget->nSelected());
	mainWindow_->ui.actionSetBondLength->setEnabled(FALSE);
	mainWindow_->ui.actionSetBondAngle->setEnabled(FALSE);
	mainWindow_->ui.actionSetTorsionAngle->setEnabled(FALSE);
	if (nselected == 2) mainWindow_->ui.actionSetBondLength->setEnabled(TRUE);
	else if (nselected == 3) mainWindow_->ui.actionSetBondAngle->setEnabled(TRUE);
	else if (nselected == 4) mainWindow_->ui.actionSetTorsionAngle->setEnabled(TRUE);
	// (De)Activate glyph menu items based on number of atoms selected
	mainWindow_->activateGlyphActions(nselected);
	msg.exit("GuiQt::updateContextMenu");
}

// Activate glyph actions 
void AtenForm::activateGlyphActions(int n)
{
	for (int gt=0; gt<Glyph::nGlyphTypes; ++gt) createGlyphActions[gt]->setEnabled( Glyph::nGlyphData( (Glyph::GlyphType) gt) == n);
}

// Show the modelview context menu
void GuiQt::callContextMenu(Atom *undermouse, int x, int y)
{
	// If there is no atom under the mouse, then exit
	target = undermouse;
	if (target == NULL) return;

	// If the atom under the mouse is selected, just run the popup. If it is not selected, deselect everything else and select it
	QPoint pos(x,y);
//	printf("AtomPopup: model %li, undermouse = %li, nselected = %i\n", viewTarget, target, viewTarget->nSelected());
	Model *viewTarget = aten.currentModelOrFrame();
	if (!target->isSelected())
	{
		viewTarget->beginUndoState("Select atom (Context Menu)");
		viewTarget->selectNone();
		viewTarget->selectAtom(target);
		viewTarget->endUndoState();
		gui.mainCanvas()->postRedisplay();
		// Make sure context menu items are enabled, since nothing may have been selected beforehand
		mainWindow_->ui.AtomContextMenu->setEnabled(TRUE);
	}
	
	// Atom selection may have just changed, so update context menu
	updateContextMenu();
	// Run the popup
	mainWindow_->ui.AtomContextMenu->exec(pos);
}

// Set atom style
void AtenForm::setAtomStyle(Atom::DrawStyle ds)
{
	if ((target == NULL) || (aten.currentModelOrFrame()->nSelected() > 1)) CommandNode::run(Command::AtomStyle, "c", Atom::drawStyle(ds));
	else CommandNode::run(Command::AtomStyle, "ci", Atom::drawStyle(ds), target->id()+1);
	target = NULL;
}

void AtenForm::on_actionAtomStyleStick_triggered(bool checked)
{
	setAtomStyle(Atom::StickStyle);
	gui.mainCanvas()->postRedisplay();
}

void AtenForm::on_actionAtomStyleTube_triggered(bool checked)
{
	setAtomStyle(Atom::TubeStyle);
	gui.mainCanvas()->postRedisplay();
}

void AtenForm::on_actionAtomStyleSphere_triggered(bool checked)
{
	setAtomStyle(Atom::SphereStyle);
	gui.mainCanvas()->postRedisplay();
}

void AtenForm::on_actionAtomStyleScaled_triggered(bool checked)
{
	setAtomStyle(Atom::ScaledStyle);
	gui.mainCanvas()->postRedisplay();
}

// Set atom labels
void AtenForm::setAtomLabel(Atom::AtomLabel al)
{
	if ((target == NULL) || (aten.currentModelOrFrame()->nSelected() > 1)) CommandNode::run(Command::Label, "c", Atom::atomLabel(al));
	else CommandNode::run(Command::Label, "ci", Atom::atomLabel(al), target->id()+1);
	target = NULL;
	gui.mainCanvas()->postRedisplay();
}

// Clear atom labels
void AtenForm::removeAtomLabels(bool all)
{
	if (all) CommandNode::run(Command::ClearLabels, "");
	else if ((target == NULL) || (aten.currentModelOrFrame()->nSelected() > 1)) CommandNode::run(Command::RemoveLabels, "");
	else CommandNode::run(Command::RemoveLabels, "i", target->id()+1);
	target = NULL;
	gui.mainCanvas()->postRedisplay();
}

void AtenForm::on_actionAtomLabelID_triggered(bool checked)
{
	setAtomLabel(Atom::IdLabel);
}

void AtenForm::on_actionAtomLabelCharge_triggered(bool checked)
{
	setAtomLabel(Atom::ChargeLabel);
}

void AtenForm::on_actionAtomLabelFFType_triggered(bool checked)
{
	setAtomLabel(Atom::TypeLabel);
}

void AtenForm::on_actionAtomLabelElement_triggered(bool checked)
{
	setAtomLabel(Atom::ElementLabel);
}

void AtenForm::on_actionAtomLabelFFEquiv_triggered(bool checked)
{
	setAtomLabel(Atom::EquivLabel);
}

void AtenForm::on_actionAtomLabelClear_triggered(bool checked)
{
	removeAtomLabels(FALSE);
}

void AtenForm::on_actionAtomLabelClearAll_triggered(bool checked)
{
	removeAtomLabels(TRUE);
}

// Reset atom custom colour to element colour
void AtenForm::on_actionAtomColourReset_triggered(bool checked)
{
	CommandNode::run(Command::RecolourAtoms, "");
	target = NULL;
	gui.mainCanvas()->postRedisplay();
}

// Reset atom custom colour to element colour
void AtenForm::on_actionAtomColourSet_triggered(bool checked)
{
	QColor oldcol, newcol;
	// Get colour of clicked atom and convert into a QColor
	if (target != NULL) oldcol.setRgbF( target->colour()[0], target->colour()[1], target->colour()[2], target->colour()[3] );
	else oldcol.setRgbF(1.0, 1.0, 1.0, 1.0);
		
	// Request a colour dialog
	bool ok = FALSE;
	newcol.setRgba(QColorDialog::getRgba(oldcol.rgba(), &ok, this));
	if (!ok) return;
	// Store new colour
	CommandNode::run(Command::ColourAtoms, "dddd", newcol.redF(), newcol.greenF(), newcol.blueF(), newcol.alphaF());
	target = NULL;
	// Set colour scheme menu option automatically if necessary
	if (prefs.colourScheme() != Prefs::CustomScheme) ui.actionSchemeCustom->trigger();
	msg.print("Colouring scheme changed to 'custom'.\n");
	gui.mainCanvas()->postRedisplay();
}

// Shift atom order up
void AtenForm::on_actionOrderShiftUp_triggered(bool checked)
{
	CommandNode::run(Command::ShiftUp, "i", 1);
	gui.update(GuiQt::CanvasTarget);
}

// Shift atom order down
void AtenForm::on_actionOrderShiftDown_triggered(bool checked)
{
	CommandNode::run(Command::ShiftDown, "i", 1);
	gui.update(GuiQt::CanvasTarget);
}

// Shift atoms to beginning of list
void AtenForm::on_actionOrderMoveToStart_triggered(bool checked)
{
	CommandNode::run(Command::MoveToStart, "");
	gui.update(GuiQt::CanvasTarget);
}

// Shift atoms to end of list
void AtenForm::on_actionOrderMoveToEnd_triggered(bool checked)
{
	CommandNode::run(Command::MoveToEnd, "");
	gui.update(GuiQt::CanvasTarget);
}

// Reorder atoms in current selection
void AtenForm::on_actionOrderReorder_triggered(bool checked)
{
	CommandNode::run(Command::ReOrder, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// Set atom hidden
void AtenForm::setAtomHidden(bool hidden)
{
	if ((target == NULL) || (aten.currentModelOrFrame()->nSelected() > 1)) 
	{
		if (hidden) CommandNode::run(Command::Hide, "");
		else CommandNode::run(Command::Show, "");
	}
	else
	{
		if (hidden) CommandNode::run(Command::Hide, "i", target->id()+1);
		else CommandNode::run(Command::Show, "i", target->id()+1);
	}
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void AtenForm::on_actionAtomHide_triggered(bool checked)
{
	setAtomHidden(TRUE);
}

void AtenForm::on_actionAtomProbe_triggered(bool checked)
{
	if (target != NULL) target->print();
}

void AtenForm::on_actionAtomFixPosition_triggered(bool checked)
{
	if ((target == NULL) || (aten.currentModelOrFrame()->nSelected() > 1)) CommandNode::run(Command::Fix, "");
	else CommandNode::run(Command::Fix, "i", target->id()+1);
	gui.mainCanvas()->postRedisplay();
}

void AtenForm::on_actionAtomFreePosition_triggered(bool checked)
{
	if ((target == NULL) || (aten.currentModelOrFrame()->nSelected() > 1)) CommandNode::run(Command::Free, "");
	else CommandNode::run(Command::Free, "i", target->id()+1);
	gui.mainCanvas()->postRedisplay();
}

void AtenForm::on_actionSetBondLength_triggered(bool checked)
{
	gui.geometryWidget->showWidget();
}

void AtenForm::on_actionSetBondAngle_triggered(bool checked)
{
	gui.geometryWidget->showWidget();
}

void AtenForm::on_actionSetTorsionAngle_triggered(bool checked)
{
	gui.geometryWidget->showWidget();
}

void AtenForm::on_actionCentreAtOrigin_triggered(bool checked)
{
	CommandNode::run(Command::Centre, "ddd", 0.0, 0.0, 0.0);
	gui.mainCanvas()->postRedisplay();
}

void AtenForm::on_actionCreateFragment_triggered(bool checked)
{
	Model *viewTarget = aten.currentModelOrFrame();
	aten.addFragmentFromSelection(viewTarget, "Selections");
	gui.fragmentsWidget->refresh();
}

void AtenForm::createGlyph()
{
	// Cast sending QAction and grab filename
	QAction *action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("AtenForm::loadRecent - Sender was not a QAction.\n");
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
	Model *viewTarget = aten.currentModelOrFrame();
	n = 1;
	for (Refitem<Atom,int> *ri = viewTarget->selection(); ri != NULL; ri = ri->next)
	{
		CommandNode::run(Command::GlyphAtomR, "ii", n, ri->item->id()+1);
		n++;
	}
	gui.update(GuiQt::CanvasTarget+GuiQt::GlyphsTarget);
}

