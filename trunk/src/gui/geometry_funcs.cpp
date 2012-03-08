/*
	*** Geometry Dock Widget
	*** src/gui/geometry_funcs.cpp
	Copyright T. Youngs 2007-2012

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

void GeometryWidget::showWidget()
{
	refresh();
	show();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.GeometryButton->setChecked(TRUE);
}

// Update active tab and labels
void GeometryWidget::refresh()
{
	updateTabs();
	updateLabels();
}

// Update active tab control
void GeometryWidget::updateTabs()
{
	// Check current atom selection
	Model *m = aten.currentModelOrFrame();
	if (m->nSelected() < 2) return;
	switch (m->nSelected())
	{
		case (2):
			ui.DistanceTab->setEnabled(TRUE);
			ui.AngleTab->setEnabled(FALSE);
			ui.TorsionTab->setEnabled(FALSE);
			ui.Tabs->setCurrentIndex(1);
			break;
		case (3):
			ui.DistanceTab->setEnabled(FALSE);
			ui.AngleTab->setEnabled(TRUE);
			ui.TorsionTab->setEnabled(FALSE);
			ui.Tabs->setCurrentIndex(2);
			break;
		case (4):
			ui.DistanceTab->setEnabled(FALSE);
			ui.AngleTab->setEnabled(FALSE);
			ui.TorsionTab->setEnabled(TRUE);
			ui.Tabs->setCurrentIndex(3);
			break;
		default:
			ui.Tabs->setCurrentIndex(0);
			break;
	}
}

// Update value labels
void GeometryWidget::updateLabels()
{
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
			value = m->distance(i,j);
			text.sprintf("%f (atoms %i-%i)", value, i->id()+1, j->id()+1);
			ui.DistanceLabel->setText(text.get());
			break;
		case (3):
			k = ri->item;
			value = m->angle(i,j,k);
			text.sprintf("%f (atoms %i-%i-%i)", value, i->id()+1, j->id()+1, k->id()+1);
			ui.AngleLabel->setText(text.get());
			break;
		case (4):
			k = ri->item;
			l = ri->next->item;
			value = m->torsion(i,j,k,l);
			text.sprintf("%f (atoms %i-%i-%i-%i)", value, i->id()+1, j->id()+1, k->id()+1, l->id()+1);
			ui.TorsionLabel->setText(text.get());
			break;
		default:
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
	gui.update(GuiQt::CanvasTarget);
}

// Measure all bond angles in current selection
void GeometryWidget::on_MeasureAngleSelectionButton_clicked(bool checked)
{
	CommandNode::run(Command::MeasureSelected, "i", 3);
	gui.update(GuiQt::CanvasTarget);
}

// Measure all bond torsions in current selection
void GeometryWidget::on_MeasureTorsionSelectionButton_clicked(bool checked)
{
	CommandNode::run(Command::MeasureSelected, "i", 4);
	gui.update(GuiQt::CanvasTarget);
}

// Clear all selections from model
void GeometryWidget::on_MeasureClearAllButton_clicked(bool checked)
{
	CommandNode::run(Command::ClearMeasurements, "");
	gui.update(GuiQt::CanvasTarget);
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
	updateLabels();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
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
	updateLabels();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
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
	updateLabels();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
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
	updateLabels();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
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
	updateLabels();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
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
	updateLabels();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
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
	updateLabels();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
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
	updateLabels();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
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
	updateLabels();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void GeometryWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.GeometryButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainCanvas()->postRedisplay();

	// Return to select mode if one of the modes in this window is still selected
	if (UserAction::isGeometryWidgetAction(gui.mainCanvas()->selectedMode())) gui.mainWindow()->cancelCurrentMode();

	event->accept();
}
