/*
	*** Qt geometry dialog functions
	*** src/gui/geometry_funcs.cpp
	Copyright T. Youngs 2007-2011

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
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
AtenGeometry::AtenGeometry(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);
}

// Destructor
AtenGeometry::~AtenGeometry()
{
}

void AtenGeometry::showWindow()
{
	refresh();
	show();
}

void AtenGeometry::refresh()
{
	// Check current atom selection
	ui.DistanceTab->setEnabled(FALSE);
	ui.AngleTab->setEnabled(FALSE);
	ui.TorsionTab->setEnabled(FALSE);
	Model *m = aten.currentModel()->renderSourceModel();
	if (m->nSelected() < 2) return;
	double value;
	Atom *i, *j, *k, *l;
	Dnchar text;
	Refitem<Atom,int> *ri = m->selection();
	i = ri->item;
	j = ri->next->item;
	ri = ri->next->next;
	switch (m->nSelected())
	{
		case (2):
			ui.DistanceTab->setEnabled(TRUE);
			ui.Tabs->setCurrentIndex(0);
			value = m->distance(i,j);
			text.sprintf("%f (atoms %i-%i)", value, i->id()+1, j->id()+1);
			ui.DistanceLabel->setText(text.get());
			break;
		case (3):
			ui.AngleTab->setEnabled(TRUE);
			ui.Tabs->setCurrentIndex(1);
			k = ri->item;
			value = m->angle(i,j,k);
			text.sprintf("%f (atoms %i-%i-%i)", value, i->id()+1, j->id()+1, k->id()+1);
			ui.AngleLabel->setText(text.get());
			break;
		case (4):
			ui.TorsionTab->setEnabled(TRUE);
			ui.Tabs->setCurrentIndex(2);
			k = ri->item;
			l = ri->next->item;
			value = m->torsion(i,j,k,l);
			text.sprintf("%f (atoms %i-%i-%i-%i)", value, i->id()+1, j->id()+1, k->id()+1, l->id()+1);
			ui.TorsionLabel->setText(text.get());
			break;
	}
}

void AtenGeometry::dialogFinished(int result)
{
	gui.mainWindow->ui.actionGeometryWindow->setChecked(FALSE);
}

/*
// Bond Length Tab
*/

void AtenGeometry::on_SetNewDistanceButton_clicked(bool checked)
{
	Model *m = aten.currentModel()->renderSourceModel();
	if (m->nSelected() != 2)
	{
		msg.print("Can't set distance - %i atoms are selected but 2 are required.\n", m->nSelected());
		return;
	}
	Atom *i, *j;
	Refitem<Atom,int> *ri = m->selection();
	i = ri->item;
	j = ri->next->item;
	CommandNode::run(Command::SetDistance, "iid", i->id()+1, j->id()+1, ui.NewDistanceSpin->value());
	gui.update(TRUE, FALSE, FALSE);
}

void AtenGeometry::on_NudgeDistancePlusButton_clicked(bool checked)
{
	Model *m = aten.currentModel()->renderSourceModel();
	if (m->nSelected() != 2)
	{
		msg.print("Can't nudge distance - %i atoms are selected but 2 are required.\n", m->nSelected());
		return;
	}
	Atom *i, *j;
	Refitem<Atom,int> *ri = m->selection();
	i = ri->item;
	j = ri->next->item;
	double value = m->distance(i,j) + ui.NudgeDistanceSpin->value();
	CommandNode::run(Command::SetDistance, "iid", i->id()+1, j->id()+1, value);
	gui.update(TRUE, FALSE, FALSE);
}

void AtenGeometry::on_NudgeDistanceMinusButton_clicked(bool checked)
{
	Model *m = aten.currentModel()->renderSourceModel();
	if (m->nSelected() != 2)
	{
		msg.print("Can't nudge distance - %i atoms are selected but 2 are required.\n", m->nSelected());
		return;
	}
	Atom *i, *j;
	Refitem<Atom,int> *ri = m->selection();
	i = ri->item;
	j = ri->next->item;
	double value = m->distance(i,j) - ui.NudgeDistanceSpin->value();
	CommandNode::run(Command::SetDistance, "iid", i->id()+1, j->id()+1, value);
	gui.update(TRUE, FALSE, FALSE);
}

/*
// Bond Angle Tab
*/

void AtenGeometry::on_SetNewAngleButton_clicked(bool checked)
{
	Model *m = aten.currentModel()->renderSourceModel();
	if (m->nSelected() != 3)
	{
		msg.print("Can't set angle - %i atoms are selected but 3 are required.\n", m->nSelected());
		return;
	}
	Atom *i, *j, *k;
	Refitem<Atom,int> *ri = m->selection();
	i = ri->item;
	j = ri->next->item;
	k = ri->next->next->item;
	CommandNode::run(Command::SetAngle, "iiid", i->id()+1, j->id()+1, k->id()+1, ui.NewAngleSpin->value());
	gui.update(TRUE, FALSE, FALSE);
}

void AtenGeometry::on_NudgeAnglePlusButton_clicked(bool checked)
{
	Model *m = aten.currentModel()->renderSourceModel();
	if (m->nSelected() != 3)
	{
		msg.print("Can't nudge angle - %i atoms are selected but 3 are required.\n", m->nSelected());
		return;
	}
	Atom *i, *j, *k;
	Refitem<Atom,int> *ri = m->selection();
	i = ri->item;
	j = ri->next->item;
	k = ri->next->next->item;
	double value = m->angle(i,j,k) + ui.NudgeAngleSpin->value();
	CommandNode::run(Command::SetAngle, "iiid", i->id()+1, j->id()+1, k->id()+1, value);
	gui.update(TRUE, FALSE, FALSE);
}

void AtenGeometry::on_NudgeAngleMinusButton_clicked(bool checked)
{
	Model *m = aten.currentModel()->renderSourceModel();
	if (m->nSelected() != 3)
	{
		msg.print("Can't nudge angle - %i atoms are selected but 3 are required.\n", m->nSelected());
		return;
	}
	Atom *i, *j, *k;
	Refitem<Atom,int> *ri = m->selection();
	i = ri->item;
	j = ri->next->item;
	k = ri->next->next->item;
	double value = m->angle(i,j,k) - ui.NudgeAngleSpin->value();
	CommandNode::run(Command::SetAngle, "iiid", i->id()+1, j->id()+1, k->id()+1, value);
	gui.update(TRUE, FALSE, FALSE);
}

/*
// Angle Torsion Tab
*/

void AtenGeometry::on_SetNewTorsionButton_clicked(bool checked)
{
	Model *m = aten.currentModel()->renderSourceModel();
	if (m->nSelected() != 4)
	{
		msg.print("Can't set torsion angle - %i atoms are selected but 4 are required.\n", m->nSelected());
		return;
	}
	Atom *i, *j, *k, *l;
	Refitem<Atom,int> *ri = m->selection();
	i = ri->item;
	j = ri->next->item;
	k = ri->next->next->item;
	l = ri->next->next->next->item;
	CommandNode::run(Command::SetTorsion, "iiiid", i->id()+1, j->id()+1, k->id()+1, l->id()+1, ui.NewTorsionSpin->value());
	gui.update(TRUE, FALSE, FALSE);
}

void AtenGeometry::on_NudgeTorsionPlusButton_clicked(bool checked)
{
	Model *m = aten.currentModel()->renderSourceModel();
	if (m->nSelected() != 4)
	{
		msg.print("Can't nudge torsion angle - %i atoms are selected but 4 are required.\n", m->nSelected());
		return;
	}
	Atom *i, *j, *k, *l;
	Refitem<Atom,int> *ri = m->selection();
	i = ri->item;
	j = ri->next->item;
	k = ri->next->next->item;
	l = ri->next->next->next->item;
	double value = m->torsion(i,j,k,l) + ui.NudgeTorsionSpin->value();
	CommandNode::run(Command::SetTorsion, "iiiid", i->id()+1, j->id()+1, k->id()+1, l->id()+1, value);
	gui.update(TRUE, FALSE, FALSE);
}

void AtenGeometry::on_NudgeTorsionMinusButton_clicked(bool checked)
{
	Model *m = aten.currentModel()->renderSourceModel();
	if (m->nSelected() != 4)
	{
		msg.print("Can't nudge torsion angle - %i atoms are selected but 4 are required.\n", m->nSelected());
		return;
	}
	Atom *i, *j, *k, *l;
	Refitem<Atom,int> *ri = m->selection();
	i = ri->item;
	j = ri->next->item;
	k = ri->next->next->item;
	l = ri->next->next->next->item;
	double value = m->torsion(i,j,k,l) - ui.NudgeTorsionSpin->value();
	CommandNode::run(Command::SetTorsion, "iiiid", i->id()+1, j->id()+1, k->id()+1, l->id()+1, value);
	gui.update(TRUE, FALSE, FALSE);
}
