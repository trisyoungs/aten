/*
	*** Qt GUI: Draw toolbar actions
	*** src/gui/drawactions.cpp
	Copyright T. Youngs 2007-2010

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
#include "gui/selectelement.h"
#include "gui/gui.h"
#include "model/model.h"
#include "parser/commandnode.h"

void AtenForm::on_actionDrawAtom_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(UserAction::DrawAtomAction);
}

void AtenForm::on_actionDrawChain_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(UserAction::DrawChainAction);
}

void AtenForm::on_actionDrawFragment_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(UserAction::DrawFragmentAction);
}

void AtenForm::on_actionDeleteAtom_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(UserAction::DrawDeleteAction);
}

void AtenForm::on_actionTransmuteAtom_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(UserAction::DrawTransmuteAction);
}

void AtenForm::on_actionTransmuteSelection_triggered(bool on)
{
	CommandNode::run(Command::Transmute, "i", aten.sketchElement());
	gui.update();
}

void AtenForm::on_actionBondSingle_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(UserAction::DrawBondSingleAction);
}

void AtenForm::on_actionBondDouble_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(UserAction::DrawBondDoubleAction);
}

void AtenForm::on_actionBondTriple_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(UserAction::DrawBondTripleAction);
}

void AtenForm::on_actionDeleteBond_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(UserAction::DrawDeleteBondAction);
}

void AtenForm::on_actionElementH_triggered(bool on)
{
	if (on) aten.setSketchElement(1);
}

void AtenForm::on_actionElementC_triggered(bool on)
{
	if (on) aten.setSketchElement(6);
}

void AtenForm::on_actionElementN_triggered(bool on)
{
	if (on) aten.setSketchElement(7);
}

void AtenForm::on_actionElementCustom_triggered(bool on)
{
	if (on) aten.setSketchElement(customElement_);
}

void AtenForm::on_actionSelectCustomElement_triggered(bool on)
{
	// Call the select element dialog...
	int newel = gui.selectElementDialog->selectElement();
	if (newel != -1)
	{
		// Set text of custom element button
		ui.actionElementCustom->setText( elements().symbol(newel) );
		customElement_ = newel;
		// Activate custom element button
		ui.actionElementCustom->setChecked(TRUE);
		aten.setSketchElement(customElement_);
	}
}

void AtenForm::on_actionAddHydrogenAtom_triggered(bool on)
{
	if (on) gui.mainView.setSelectedMode(UserAction::DrawAddHydrogenAction);
}

//void AtenForm::on_actionProbeAtom_triggered(bool on)
//{
//	if (on) gui.mainView.setSelectedMode(UserAction::DrawProbeAction);
//}

void AtenForm::on_actionAddHydrogen_triggered(bool on)
{
	CommandNode::run(Command::AddHydrogen, "");
	gui.update();
}

