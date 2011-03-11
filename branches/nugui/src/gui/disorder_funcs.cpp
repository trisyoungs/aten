/*
	*** Qt GUI: Disordered builder interface
	*** src/gui/disorder_funcs.cpp
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
#include "gui/mainwindow.h"
#include "gui/disorder.h"
#include "gui/gui.h"
#include "gui/ttablewidgetitem.h"
#include "gui/toolbox.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include "parser/commandnode.h"

// Constructor
DisorderWidget::DisorderWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);

	// Private variables
	refreshing_ = FALSE;
}

// Show widget
void DisorderWidget::showWidget()
{
	refresh();
	show();
}

// Refresh widget
void DisorderWidget::refresh()
{
	msg.enter("DisorderWidget::refresh");
	// If the window is not visible, don't do anything
	if (!gui.disorderWidget->isVisible())
	{
		msg.exit("DisorderWidget::refresh");
		return;
	}

	if (aten.currentModelOrFrame() == NULL) return;

	refreshing_ = TRUE;
	// (De)sensitize controls
	ui.DisorderStartButton->setDisabled(aten.currentModelOrFrame()->cell()->type() == Cell::NoCell);
	// Update model (component) list
	TTableWidgetItem *item, *firstitem = NULL;
	//ui.ComponentTable->setCurrentRow(-1);
	ui.ComponentTable->clear();
	componentList.clear();
	ui.ComponentTable->setHorizontalHeaderLabels(QStringList() << "N" << "R" << "T" << "Model");
	int count = 0;
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		if (m->cell()->type() != Cell::NoCell) continue;
		// Add model to reflist
		componentList.add(m,0);
		// Number requested
		item = new TTableWidgetItem();
		if (count == 0) firstitem = item;
		item->setText(itoa(m->nRequested()));
		item->data.set(VTypes::ModelData, m);
		ui.ComponentTable->setItem(count, 0, item);
		// Rotation allowed
		item = new TTableWidgetItem();
		item->setCheckState(m->isMoveAllowed(MonteCarlo::Rotate) ? Qt::Checked : Qt::Unchecked);
		item->data.set(VTypes::ModelData, m);
		ui.ComponentTable->setItem(count, 1, item);
		// Translate allowed
		item = new TTableWidgetItem();
		item->setCheckState(m->isMoveAllowed(MonteCarlo::Translate) ? Qt::Checked : Qt::Unchecked);
		item->data.set(VTypes::ModelData, m);
		ui.ComponentTable->setItem(count, 2, item);
		// Model name
		item = new TTableWidgetItem();
		item->setText(m->name());
		item->data.set(VTypes::ModelData, m);
		ui.ComponentTable->setItem(count, 3, item);
		count ++;
	}
	ui.ComponentTable->setRowCount(count);
	for (int i=0; i<4; i++) ui.ComponentTable->resizeColumnToContents(i);
	// Select the last component in the list
	ui.ComponentTable->setCurrentItem(firstitem);
	refreshing_ = FALSE;
	if (count > 0) refreshComponentData(0);
	msg.exit("DisorderWidget::refresh");
}

void DisorderWidget::refreshComponentData(int comp)
{
	if (comp == -1) return;
	Model *m = componentList[comp]->item;
	ComponentRegion *r = m->region();
	refreshing_ = TRUE;
	// Set controls
	Vec3<double> v;
	v = r->geometry();
	ui.GeometryXSpin->setValue(v.x);
	ui.GeometryYSpin->setValue(v.y);
	ui.GeometryZSpin->setValue(v.z);
	ui.GeometryFracCheck->setChecked(r->isGeometryFrac());
	v = r->centre();
	ui.CentreXSpin->setValue(v.x);
	ui.CentreYSpin->setValue(v.y);
	ui.CentreZSpin->setValue(v.z);
	ui.CentreFracCheck->setChecked(r->isCentreFrac());
	v = r->rotations();
	ui.RotationXSpin->setValue(v.x);
	ui.RotationYSpin->setValue(v.y);
	ui.RotationCheck->setChecked(r->rotateRegion());
	ui.AllowOverlapCheck->setChecked(r->allowOverlap());
	ui.ComponentRegionCombo->setCurrentIndex(r->shape());
	refreshing_ = FALSE;
}

void DisorderWidget::setComponentCentre()
{
	if (refreshing_) return;
	Vec3<double> v;
	v.set(ui.CentreXSpin->value(), ui.CentreYSpin->value(), ui.CentreZSpin->value());
	// Get current component
	int comp = ui.ComponentTable->currentRow();
	if (comp == -1) return;
	Model *m = componentList[comp]->item;
	ComponentRegion *r = m->region();
	ui.CentreFracCheck->isChecked() ? r->setCentreFrac(v) : r->setCentre(v);
	gui.mainWidget->postRedisplay();
}

void DisorderWidget::setComponentGeometry()
{
	if (refreshing_) return;
	Vec3<double> v;
	v.set(ui.GeometryXSpin->value(), ui.GeometryYSpin->value(), ui.GeometryZSpin->value());
	// Get current component
	int comp = ui.ComponentTable->currentRow();
	if (comp == -1) return;
	Model *m = componentList[comp]->item;
	ComponentRegion *r = m->region();
	ui.GeometryFracCheck->isChecked() ? r->setGeometryFrac(v) : r->setGeometry(v);
	gui.mainWidget->postRedisplay();
}

void DisorderWidget::setComponentRotation()
{
	if (refreshing_) return;
	Vec3<double> v(ui.RotationXSpin->value(), ui.RotationYSpin->value(), 0.0);
	// Get current component
	int comp = ui.ComponentTable->currentRow();
	if (comp == -1) return;
	Model *m = componentList[comp]->item;
	ComponentRegion *r = m->region();
	r->setRotateRegion(ui.RotationCheck->isChecked());
	r->setRotations(v);
	gui.mainWidget->postRedisplay();
}

void DisorderWidget::on_CentreXSpin_valueChanged(double d)
{
	setComponentCentre();
}

void DisorderWidget::on_CentreYSpin_valueChanged(double d)
{
	setComponentCentre();
}

void DisorderWidget::on_CentreZSpin_valueChanged(double d)
{
	setComponentCentre();
}

void DisorderWidget::on_CentreFracCheck_clicked(bool checked)
{
	setComponentCentre();
}

void DisorderWidget::on_GeometryXSpin_valueChanged(double d)
{
	setComponentGeometry();
}

void DisorderWidget::on_GeometryYSpin_valueChanged(double d)
{
	setComponentGeometry();
}

void DisorderWidget::on_GeometryZSpin_valueChanged(double d)
{
	setComponentGeometry();
}

void DisorderWidget::on_GeometryFracCheck_clicked(bool checked)
{
	setComponentGeometry();
}

void DisorderWidget::on_RotationXSpin_valueChanged(double d)
{
	setComponentRotation();
}

void DisorderWidget::on_RotationYSpin_valueChanged(double d)
{
	setComponentRotation();
}

void DisorderWidget::on_RotationCheck_clicked(bool checked)
{
	setComponentRotation();
}

void DisorderWidget::on_AllowOverlapCheck_clicked(bool checked)
{
	
}

void DisorderWidget::on_ComponentTable_itemClicked(QTableWidgetItem *item)
{
	refreshComponentData(item->row());
}

void DisorderWidget::on_ComponentTable_itemChanged(QTableWidgetItem *item)
{
	if (!gui.exists() || refreshing_) return;
	int column = ui.ComponentTable->column(item);
	TTableWidgetItem *twi = (TTableWidgetItem*) item;
	Model *m = (Model*) twi->data.asPointer(VTypes::ModelData);
	switch (column)
	{
		// NRequested
		case (0):
			m->setNRequested(atoi(qPrintable(item->text())));
			ui.ComponentTable->resizeColumnToContents(0);
			break;
		// Allow rotate
		case (1):
			m->setMoveAllowed(MonteCarlo::Rotate, (item->checkState() == Qt::Checked ? TRUE : FALSE));
			break;
		// Allow translate
		case (2):
			m->setMoveAllowed(MonteCarlo::Translate, (item->checkState() == Qt::Checked ? TRUE : FALSE));
			break;
	}
}

void DisorderWidget::on_ComponentRegionCombo_currentIndexChanged(int index)
{
	if (refreshing_) return;
	int comp = ui.ComponentTable->currentRow();
	if (comp == -1) return;
	Model *m = componentList[comp]->item;
	ComponentRegion *r = m->region();
// 	printf("Setting model %s region to be %i\n",m->name(), index);
	r->setShape( (ComponentRegion::RegionShape) index);
	gui.mainWidget->postRedisplay();
}

void DisorderWidget::on_ShowRegionsCheck_clicked(bool checked)
{
	prefs.setVisibleOnScreen(Prefs::ViewRegions, checked);
	gui.mainWidget->postRedisplay();
}

void DisorderWidget::on_DisorderStartButton_clicked(bool checked)
{
	CommandNode::run(Command::Disorder, "i", ui.DisorderCyclesSpin->value());
}

void DisorderWidget::on_VDWScaleSpin_valueChanged(double d)
{
	CommandNode::run(Command::VdwScale, "d", d);
}

void DisorderWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.DisorderButton->setChecked(FALSE);
	event->accept();
}

