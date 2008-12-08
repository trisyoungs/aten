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

#include "main/aten.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "model/model.h"
#include "command/staticcommand.h"

// Local variables
Atom *target = NULL;

// Show the modelview context menu
void GuiQt::callAtomPopup(Atom *undermouse, int x, int y)
{
	//Model *viewTarget = aten.currentModel()->renderSource();
	Model *viewTarget = gui.mainView.displayModel();
	target = undermouse;
	//printf("AtomPopup: model %li, undermouse = %li, nselected = %i\n", viewTarget, target, viewTarget->nSelected());
	if ((viewTarget->nSelected() != 0) && (undermouse->isSelected())) target = NULL;
	QPoint pos(x,y);
	// If there are no atoms selected we must enable the menu first...
	if (viewTarget->nSelected() == 0)
	{
		// Select atom under the mouse...
		viewTarget->selectAtom(target);
		mainWindow->ui.AtomContextMenu->setEnabled(TRUE);
		mainWindow->ui.AtomContextMenu->exec(pos);
		mainWindow->ui.AtomContextMenu->setEnabled(FALSE);
	}
	else mainWindow->ui.AtomContextMenu->exec(pos);
}

// Set atom style
void AtenForm::setAtomStyle(Atom::DrawStyle ds)
{
	static StaticCommandNode cmd(Command::CA_ATOMSTYLE, "c", "none");
	static StaticCommandNode cmdatom(Command::CA_ATOMSTYLE, "ci", "none", 0);
	if (target == NULL)
	{
		cmd.pokeArguments("c", Atom::drawStyle(ds));
		cmd.execute();
	}
	else
	{
		cmdatom.pokeArguments("ci", Atom::drawStyle(ds), target->id());
		cmdatom.execute();
	}
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
	static StaticCommandNode cmd(Command::CA_LABEL, "c", "none");
	static StaticCommandNode cmdatom(Command::CA_LABEL, "ci", "none", 0);
	if (target == NULL)
	{
		cmd.pokeArguments("c", Atom::atomLabel(al));
		cmd.execute();
	}
	else
	{
		cmdatom.pokeArguments("ci", Atom::atomLabel(al), target->id());
		cmdatom.execute();
	}
	target = NULL;
	gui.modelChanged(FALSE,FALSE,FALSE);
}

// Clear atom labels
void AtenForm::removeAtomLabels(bool all)
{
	static StaticCommandNode cmd(Command::CA_REMOVELABELS, "");
	static StaticCommandNode cmdatom(Command::CA_REMOVELABELS, "i", 0);
	static StaticCommandNode cmdall(Command::CA_CLEARLABELS, "");
	if (all) cmdall.execute();
	else if (target == NULL) cmd.execute();
	else
	{
		cmdatom.pokeArguments("i", target->id());
		cmdatom.execute();
	}
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
	static StaticCommandNode cmdh(Command::CA_HIDE, "");
	static StaticCommandNode cmdhatom(Command::CA_HIDE, "i", 0);
	static StaticCommandNode cmds(Command::CA_SHOW, "");
	static StaticCommandNode cmdsatom(Command::CA_SHOW, "i", 0);
	if (target == NULL) hidden ? cmdh.execute() : cmds.execute();
	else
	{
		if (hidden)
		{
			cmdhatom.pokeArguments("i", target->id());
			cmdhatom.execute();
		}
		else
		{
			cmdsatom.pokeArguments("i", target->id());
			cmdsatom.execute();
		}
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
