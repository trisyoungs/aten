/*
	*** Qt context menu functions
	*** src/gui/contextmenu_funcs.cpp
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
#include "gui/mainwindow.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Local variables
Atom *target = NULL;

// Show the modelview context menu
void GuiQt::callAtomPopup(Atom *undermouse, int x, int y)
{
	// If there is no atom under the mouse, then exit
	target = undermouse;
	if (target == NULL) return;
	// If the atom under the mouse is selected, just run the popup. If it is not selected, deselect everything else and select it
	QPoint pos(x,y);
// 	printf("AtomPopup: model %li, undermouse = %li, nselected = %i\n", viewTarget, target, viewTarget->nSelected());
	if (!target->isSelected())
	{
		Model *viewTarget = gui.mainView.displayModel();
		viewTarget->beginUndoState("Context menu atom focus");
		viewTarget->selectNone();
		viewTarget->selectAtom(target);
		viewTarget->endUndoState();
		gui.mainView.postRedisplay();
		// Make sure context menu items are enabled, since nothing may have been selected beforehand
		mainWindow->ui.AtomContextMenu->setEnabled(TRUE);
	}
	// Run the popup
	mainWindow->ui.AtomContextMenu->exec(pos);
}

// Set atom style
void AtenForm::setAtomStyle(Atom::DrawStyle ds)
{
	if ((target == NULL) || (gui.mainView.displayModel()->nSelected() > 1)) CommandNode::run(Command::AtomStyle, "c", Atom::drawStyle(ds));
	else CommandNode::run(Command::AtomStyle, "ci", Atom::drawStyle(ds), target->id());
	target = NULL;
}

void AtenForm::on_actionAtomStyleStick_triggered(bool checked)
{
	setAtomStyle(Atom::StickStyle);
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionAtomStyleTube_triggered(bool checked)
{
	setAtomStyle(Atom::TubeStyle);
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionAtomStyleSphere_triggered(bool checked)
{
	setAtomStyle(Atom::SphereStyle);
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionAtomStyleScaled_triggered(bool checked)
{
	setAtomStyle(Atom::ScaledStyle);
	gui.mainView.postRedisplay();
}

// Set atom labels
void AtenForm::setAtomLabel(Atom::AtomLabel al)
{
	if ((target == NULL) || (gui.mainView.displayModel()->nSelected() > 1)) CommandNode::run(Command::Label, "c", Atom::atomLabel(al));
	else CommandNode::run(Command::Label, "ci", Atom::atomLabel(al), target->id());
	target = NULL;
	gui.update(FALSE,FALSE,FALSE);
}

// Clear atom labels
void AtenForm::removeAtomLabels(bool all)
{
	if (all) CommandNode::run(Command::ClearLabels, "");
	else if ((target == NULL) || (gui.mainView.displayModel()->nSelected() > 1)) CommandNode::run(Command::RemoveLabels, "");
	else CommandNode::run(Command::RemoveLabels, "i", target->id());
	target = NULL;
	gui.update(FALSE,FALSE,FALSE);
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

// Set atom hidden
void AtenForm::setAtomHidden(bool hidden)
{
	if ((target == NULL) || (gui.mainView.displayModel()->nSelected() > 1)) 
	{
		if (hidden) CommandNode::run(Command::Hide, "");
		else CommandNode::run(Command::Show, "");
	}
	else
	{
		if (hidden) CommandNode::run(Command::Hide, "i", target->id());
		else CommandNode::run(Command::Show, "i", target->id());
	}
	gui.update(TRUE, FALSE, FALSE);
}

void AtenForm::on_actionAtomHide_triggered(bool checked)
{
	setAtomHidden(TRUE);
}

void AtenForm::on_actionAtomProbe_triggered(bool checked)
{
	if (target != NULL) target->print();
}
