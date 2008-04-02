/*
	*** Qt context menu functions
	*** src/gui/contextmenu_funcs.cpp
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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "model/model.h"

// Local variables
Atom *target = NULL;

// Show the modelview context menu
void GuiQt::callAtomPopup(Atom *undermouse, int x, int y)
{
	Model *viewTarget = master.currentModel()->renderSource();
	target = undermouse;
	if ((viewTarget->nSelected() != 0) && (undermouse->isSelected())) target = NULL;
	QPoint pos(x,y);
	// If there are no atoms selected we must enable the menu first...
	if (viewTarget->nSelected() == 0) mainWindow->ui.AtomMenu->setEnabled(TRUE);
	mainWindow->ui.AtomMenu->exec(pos);
	if (viewTarget->nSelected() == 0) mainWindow->ui.AtomMenu->setEnabled(FALSE);
}

// Set atom style
void AtenForm::setAtomStyle(Atom::DrawStyle ds)
{
	if (target == NULL) master.currentModel()->renderSource()->selectionSetStyle(ds);
	else target->setStyle(ds);
	target = NULL;
}

void AtenForm::on_actionAtomStyleStick_triggered(bool checked)
{
	setAtomStyle(Atom::StickStyle);
}

void AtenForm::on_actionAtomStyleTube_triggered(bool checked)
{
	setAtomStyle(Atom::TubeStyle);
}

void AtenForm::on_actionAtomStyleSphere_triggered(bool checked)
{
	setAtomStyle(Atom::SphereStyle);
}

void AtenForm::on_actionAtomStyleScaled_triggered(bool checked)
{
	setAtomStyle(Atom::ScaledStyle);
}

// Set atom labels
void AtenForm::setAtomLabel(Atom::AtomLabel al)
{
	Model *m = master.currentModel()->renderSource();
	m->beginUndostate("Add Labels");
	if (target == NULL) m->selectionAddLabels(al);
	else target->addLabel(al);
	target = NULL;
	m->endUndostate();
}

// Clear atom labels
void AtenForm::removeAtomLabels(bool all)
{
	Model *m = master.currentModel()->renderSource();
	if (all)
	{
		m->beginUndostate("Clear All Labels");
		master.currentModel()->clearAllLabels();
	}
	else
	{
		m->beginUndostate("Clear All Labels");
		master.currentModel()->selectionClearLabels();
	}
	m->endUndostate();
	gui.mainView.postRedisplay();
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
	Model *m = master.currentModel()->renderSource();
	if (target == NULL) m->selectionSetHidden(hidden);
	else m->setHidden(target, hidden);
	target = NULL;
}

void AtenForm::on_actionAtomHide_triggered(bool checked)
{
	setAtomHidden(TRUE);
}
