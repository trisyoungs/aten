/*
	*** Qt GUI: Bond toolbar actions
	*** src/gui/bondactions.cpp
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

#include "base/aten.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "model/model.h"

void AtenForm::on_actionCalculateBonding_triggered(bool on)
{
	Model *m = aten.currentModel()->renderSource();
	m->beginUndostate("Calculate Bonding");
	m->clearBonding();
	m->calculateBonding();
	m->endUndostate();
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionClearBonding_triggered(bool on)
{
	Model *m = aten.currentModel()->renderSource();
	m->beginUndostate("Clear Bonding");
	m->clearBonding();
	m->endUndostate();
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionCalculateBondingSelection_triggered(bool on)
{
	Model *m = aten.currentModel()->renderSource();
	m->beginUndostate("Calculate Bonding (Selection)");
	m->selectionCalculateBonding();
	m->endUndostate();
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionClearBondingSelection_triggered(bool on)
{
	Model *m = aten.currentModel()->renderSource();
	m->beginUndostate("Clear Bonding (Selection)");
	m->selectionClearBonding();
	m->endUndostate();
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionAugmentBonding_triggered(bool on)
{
	Model *m = aten.currentModel()->renderSource();
	m->beginUndostate("Augment Bonding");
	m->augmentBonding();
	m->endUndostate();
	gui.mainView.postRedisplay();
}

void AtenForm::bondTolerance_valueChanged(double value)
{
	prefs.setBondTolerance(value);
}

