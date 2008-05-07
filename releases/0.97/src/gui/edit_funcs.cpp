/*
	*** Qt edit functions interface
	*** src/gui/edit_funcs.cpp
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
#include "base/elements.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "model/model.h"

void AtenForm::setSketchElement(int el)
{
	master.setSketchElement(el);
}

void AtenForm::on_DrawAtomButton_clicked(bool on)
{
	if (on) setUserAction(on, Canvas::EditDrawAction);
}

void AtenForm::on_DrawChainButton_clicked(bool on)
{
	if (on) setUserAction(on, Canvas::EditChainAction);
}

void AtenForm::on_DrawDeleteButton_clicked(bool on)
{
	if (on) setUserAction(on, Canvas::EditDeleteAction);
}

void AtenForm::on_DrawTransmuteButton_clicked(bool on)
{
	if (on) setUserAction(on, Canvas::EditTransmuteAction);
}

void AtenForm::on_BondToleranceSpin_valueChanged(double d)
{
	prefs.setBondTolerance(d);
}

void AtenForm::on_BondSingleButton_clicked(bool on)
{
	if (on) setUserAction(on, Canvas::EditBondSingleAction);
}

void AtenForm::on_BondDoubleButton_clicked(bool on)
{
	if (on) setUserAction(on, Canvas::EditBondDoubleAction);
}

void AtenForm::on_BondTripleButton_clicked(bool on)
{
	if (on) setUserAction(on, Canvas::EditBondTripleAction);
}

void AtenForm::on_BondDeleteButton_clicked(bool on)
{
	if (on) setUserAction(on, Canvas::EditDeleteBondAction);
}

void AtenForm::on_ElementHButton_clicked(bool on)
{
	if (on) master.setSketchElement(1);
}

void AtenForm::on_ElementCButton_clicked(bool on)
{
	if (on) master.setSketchElement(6);
}

void AtenForm::on_ElementNButton_clicked(bool on)
{
	if (on) master.setSketchElement(7);
}

void AtenForm::on_AtomAddHydrogenButton_clicked(bool on)
{
	if (on) setUserAction(on, Canvas::EditAddHydrogenAction);
}

void AtenForm::on_ProbeAtomButton_clicked(bool on)
{
	if (on) setUserAction(on, Canvas::EditProbeAction);
}

void AtenForm::on_ElementUserButton_clicked(bool on)
{
	master.setSketchElement(elements.find(qPrintable(ui.ElementUserButton->text())));
}

void AtenForm::on_BondCalcButton_clicked(bool on)
{
	Model *m = master.currentModel()->renderSource();
	m->beginUndostate("Calculate Bonding");
	m->clearBonding();
	m->calculateBonding();
	m->endUndostate();
	gui.mainView.postRedisplay();
}

void AtenForm::on_BondClearButton_clicked(bool on)
{
	Model *m = master.currentModel()->renderSource();
	m->beginUndostate("Clear Bonding");
	m->clearBonding();
	m->endUndostate();
	gui.mainView.postRedisplay();
}

void AtenForm::on_BondCalcSelButton_clicked(bool on)
{
	Model *m = master.currentModel()->renderSource();
	m->beginUndostate("Calculate Bonding (Selection)");
	m->selectionCalculateBonding();
	m->endUndostate();
	gui.mainView.postRedisplay();
}

void AtenForm::on_BondClearSelButton_clicked(bool on)
{
	Model *m = master.currentModel()->renderSource();
	m->beginUndostate("Clear Bonding (Selection)");
	m->selectionClearBonding();
	m->endUndostate();
	gui.mainView.postRedisplay();
}

void AtenForm::on_BondAugmentButton_clicked(bool on)
{
	Model *m = master.currentModel()->renderSource();
	m->beginUndostate("Augment Bonding");
	m->augmentBonding();
	m->endUndostate();
	gui.mainView.postRedisplay();
}

void AtenForm::on_ElementEdit_returnPressed()
{
	// Get the contents of the line edit and check that it is an element symbol
	int el = elements.find(qPrintable(ui.ElementEdit->text()));
	if (el == -1)
	{
		msg(Debug::None,"Unknown element '%s'\n",qPrintable(ui.ElementEdit->text()));
		ui.ElementEdit->setText(ui.ElementUserButton->text());
	}
	else
	{
		// Set the text of the user element button and select it
		ui.ElementUserButton->setText(elements.symbol(el));
		master.setSketchElement(el);
		ui.ElementUserButton->setChecked(TRUE);
	}
}

void AtenForm::on_AddHydrogenButton_clicked(bool on)
{
	Model *m = master.currentModel()->renderSource();
	m->beginUndostate("Hydrogen Satisfy Model");
	m->hydrogenSatisfy();
	m->endUndostate();
	gui.modelChanged();
}

void AtenForm::on_AddAtomButton_clicked(bool on)
{
	static char s[256];
	int el;
	Vec3<double> newpos;
	newpos.set(ui.AtomXCoordSpin->value(), ui.AtomYCoordSpin->value(), ui.AtomZCoordSpin->value());
	Model *m = master.currentModel()->renderSource();
	if (ui.AddAtomFractionalCheck->isChecked())
	{
		sprintf(s,"Add Atom (%s at {%f, %f, %f}, frac)", elements.symbol(master.sketchElement()), newpos.x, newpos.y, newpos.z);
		if (m->cell()->type() == Cell::NoCell) msg(Debug::None,"Warning: No unit cell present - atom added with supplied coordinates.\n");
		else newpos = m->cell()->fracToReal(newpos);
	}
	else sprintf(s,"Add Atom (%s at {%f, %f, %f})", elements.symbol(master.sketchElement()), newpos.x, newpos.y, newpos.z);
	m->beginUndostate(s);
	m->addAtom(master.sketchElement(), newpos);
	m->endUndostate();
	gui.modelChanged();
}
