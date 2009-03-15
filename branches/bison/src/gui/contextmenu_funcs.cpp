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
	target = undermouse;
	if (target == NULL) return;
	QPoint pos(x,y);
// 	printf("AtomPopup: model %li, undermouse = %li, nselected = %i\n", viewTarget, target, viewTarget->nSelected());
	if (!target->isSelected())
	{
		Model *viewTarget = gui.mainView.displayModel();
		viewTarget->beginUndoState("Context menu atom focus");
		viewTarget->selectNone();
		viewTarget->selectAtom(target);
		viewTarget->endUndoState();
		mainWindow->ui.AtomContextMenu->setEnabled(TRUE);
		mainWindow->ui.AtomContextMenu->exec(pos);
		mainWindow->ui.AtomContextMenu->setEnabled(FALSE);
	}
	else mainWindow->ui.AtomContextMenu->exec(pos);
}

// Set atom style
void AtenForm::setAtomStyle(Atom::DrawStyle ds)
{
	if (gui.mainView.displayModel()->nSelected() > 1) NuCommandNode::run(NuCommand::AtomStyle, "c", Atom::drawStyle(ds));
	else NuCommandNode::run(NuCommand::AtomStyle, "ci", Atom::drawStyle(ds), target->id());
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
	if (gui.mainView.displayModel()->nSelected() > 1) NuCommandNode::run(NuCommand::Label, "c", Atom::atomLabel(al));
	else NuCommandNode::run(NuCommand::Label, "ci", Atom::atomLabel(al), target->id());
	target = NULL;
	gui.modelChanged(FALSE,FALSE,FALSE);
}

// Clear atom labels
void AtenForm::removeAtomLabels(bool all)
{
	if (all) NuCommandNode::run(NuCommand::ClearLabels, "");
	else if (gui.mainView.displayModel()->nSelected() > 1) NuCommandNode::run(NuCommand::RemoveLabels, "");
	else NuCommandNode::run(NuCommand::RemoveLabels, "i", target->id());
	target = NULL;
	gui.modelChanged(FALSE,FALSE,FALSE);
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
	if (gui.mainView.displayModel()->nSelected() > 1)
	{
		if (hidden) NuCommandNode::run(NuCommand::Hide, "");
		else NuCommandNode::run(NuCommand::Show, "");
	}
	else
	{
		if (hidden) NuCommandNode::run(NuCommand::Hide, "i", target->id());
		else NuCommandNode::run(NuCommand::Show, "i", target->id());
	}
	gui.modelChanged(TRUE, FALSE, FALSE);
}

void AtenForm::on_actionAtomHide_triggered(bool checked)
{
	setAtomHidden(TRUE);
}

void AtenForm::on_actionAtomProbe_triggered(bool checked)
{
	if (target != NULL) target->print();
}
