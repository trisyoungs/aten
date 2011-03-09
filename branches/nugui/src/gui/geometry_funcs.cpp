/*
	*** Geometry Dock Widget
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
#include "gui/toolbox.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
GeometryWidget::GeometryWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
}

// Destructor
GeometryWidget::~GeometryWidget()
{
}

void GeometryWidget::showWidget()
{
	refresh();
	show();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.AtomListButton->setChecked(TRUE);
}

void GeometryWidget::refresh()
{
	// Check current atom selection
	ui.DistanceTab->setEnabled(FALSE);
	ui.AngleTab->setEnabled(FALSE);
	ui.TorsionTab->setEnabled(FALSE);
	Model *m = aten.currentModelOrFrame();
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
			ui.Tabs->setCurrentIndex(1);
			value = m->distance(i,j);
			text.sprintf("%f (atoms %i-%i)", value, i->id()+1, j->id()+1);
			ui.DistanceLabel->setText(text.get());
			break;
		case (3):
			ui.AngleTab->setEnabled(TRUE);
			ui.Tabs->setCurrentIndex(2);
			k = ri->item;
			value = m->angle(i,j,k);
			text.sprintf("%f (atoms %i-%i-%i)", value, i->id()+1, j->id()+1, k->id()+1);
			ui.AngleLabel->setText(text.get());
			break;
		case (4):
			ui.TorsionTab->setEnabled(TRUE);
			ui.Tabs->setCurrentIndex(3);
			k = ri->item;
			l = ri->next->item;
			value = m->torsion(i,j,k,l);
			text.sprintf("%f (atoms %i-%i-%i-%i)", value, i->id()+1, j->id()+1, k->id()+1, l->id()+1);
			ui.TorsionLabel->setText(text.get());
			break;
		default:
			ui.Tabs->setCurrentIndex(0);
			break;
	}
}

/*
// Measure Tab
*/

// Measure all bond distances in current selection
void GeometryWidget::on_MeasureDistanceSelectionButton_clicked(bool checked)
{
	CommandNode::run(Command::MeasureSelected, "i", 2);
	gui.update();
}

// Measure all bond angles in current selection
void GeometryWidget::on_MeasureAngleSelectionButton_clicked(bool checked)
{
	CommandNode::run(Command::MeasureSelected, "i", 3);
	gui.update();
}

// Measure all bond torsions in current selection
void GeometryWidget::on_MeasureTorsionSelectionButton_clicked(bool checked)
{
	CommandNode::run(Command::MeasureSelected, "i", 4);
	gui.update();
}

// Clear all selections from model
void GeometryWidget::on_MeasureClearAllButton_clicked(bool checked)
{
	CommandNode::run(Command::ClearMeasurements, "");
	gui.update();
}

/*
// Bond Length Tab
*/

void GeometryWidget::on_SetNewDistanceButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
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
	gui.update(GuiQt::AtomsTarget);
}

void GeometryWidget::on_NudgeDistancePlusButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
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
	gui.update(GuiQt::AtomsTarget);
}

void GeometryWidget::on_NudgeDistanceMinusButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
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
	gui.update(GuiQt::AtomsTarget);
}

/*
// Bond Angle Tab
*/

void GeometryWidget::on_SetNewAngleButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
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
	gui.update(GuiQt::AtomsTarget);
}

void GeometryWidget::on_NudgeAnglePlusButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
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
	gui.update(GuiQt::AtomsTarget);
}

void GeometryWidget::on_NudgeAngleMinusButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
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
	gui.update(GuiQt::AtomsTarget);
}

/*
// Angle Torsion Tab
*/

void GeometryWidget::on_SetNewTorsionButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
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
	gui.update(GuiQt::AtomsTarget);
}

void GeometryWidget::on_NudgeTorsionPlusButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
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
	gui.update(GuiQt::AtomsTarget);
}

void GeometryWidget::on_NudgeTorsionMinusButton_clicked(bool checked)
{
	Model *m = aten.currentModelOrFrame();
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
	gui.update(GuiQt::AtomsTarget);
}

void GeometryWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.GeometryButton->setChecked(FALSE);
	event->accept();
}
