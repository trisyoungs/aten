/*
	*** Qt GUI: Disordered builder interface
	*** src/gui/disorder_funcs.cpp
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

#include "methods/mc.h"
#include "base/aten.h"
#include "gui/mainwindow.h"
#include "gui/disorder.h"
#include "gui/gui.h"
#include "gui/ttablewidgetitem.h"
#include "model/model.h"

// Constructor
AtenDisorder::AtenDisorder(QWidget *parent)
{
	ui.setupUi(this);

	// Private variables
	refreshing_ = FALSE;
}

// Destructor
AtenDisorder::~AtenDisorder()
{
}

// Show window
void AtenDisorder::showWindow()
{
	//if (shouldRefresh_) refresh();
	show();
}

void AtenDisorder::on_ComponentCentreXSpin_valueChanged(double d)
{
	setComponentCoords(0,0,d);
}

void AtenDisorder::on_ComponentCentreYSpin_valueChanged(double d)
{
	setComponentCoords(0,1,d);
}

void AtenDisorder::on_ComponentCentreZSpin_valueChanged(double d)
{
	setComponentCoords(0,2,d);
}

void AtenDisorder::on_ComponentSizeXSpin_valueChanged(double d)
{
	setComponentCoords(1,0,d);
}

void AtenDisorder::on_ComponentSizeYSpin_valueChanged(double d)
{
	setComponentCoords(2,1,d);
}

void AtenDisorder::on_ComponentSizeZSpin_valueChanged(double d)
{
	setComponentCoords(3,2,d);
}

void AtenDisorder::refresh()
{
	if (!gui.exists()) return;
	refreshing_ = TRUE;
	// (De)sensitize controls
	ui.DisorderStartButton->setDisabled(aten.currentModel()->cell()->type() == Cell::NoCell);
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
		item->setModel(m);
		ui.ComponentTable->setItem(count, 0, item);
		// Rotation allowed
		item = new TTableWidgetItem();
		item->setCheckState(m->isMoveAllowed(MonteCarlo::Rotate) ? Qt::Checked : Qt::Unchecked);
		item->setModel(m);
		ui.ComponentTable->setItem(count, 1, item);
		// Translate allowed
		item = new TTableWidgetItem();
		item->setCheckState(m->isMoveAllowed(MonteCarlo::Translate) ? Qt::Checked : Qt::Unchecked);
		item->setModel(m);
		ui.ComponentTable->setItem(count, 2, item);
		// Model name
		item = new TTableWidgetItem();
		item->setText(m->name());
		item->setModel(m);
		ui.ComponentTable->setItem(count, 3, item);
		count ++;
	}
	ui.ComponentTable->setRowCount(count);
	for (int i=0; i<4; i++) ui.ComponentTable->resizeColumnToContents(i);
	// Select the last component in the list
	ui.ComponentTable->setCurrentItem(firstitem);
	refreshing_ = FALSE;
	refreshComponentData();
}

void AtenDisorder::refreshComponentData()
{
	// Get current component
	int comp = ui.ComponentTable->currentRow();
	if (comp == -1) return;
	Model *m = componentList[comp]->item;
	// Set controls
	Vec3<double> v;
	v = m->area.size();
	ui.ComponentSizeXSpin->setValue(v.x);
	ui.ComponentSizeYSpin->setValue(v.y);
	ui.ComponentSizeZSpin->setValue(v.z);
	v = m->area.centre();
	ui.ComponentCentreXSpin->setValue(v.x);
	ui.ComponentCentreYSpin->setValue(v.y);
	ui.ComponentCentreZSpin->setValue(v.z);
}

void AtenDisorder::setComponentCoords(int centsize, int element, double value)
{
	// Get current component
	static Vec3<double> v;
	int comp = ui.ComponentTable->currentRow();
	if (comp == -1) return;
	Model *m = componentList[comp]->item;
	if (centsize == 0)
	{
		v = m->area.centre();
		v.set(element, value);
		m->area.setCentre(v);
	}
	else
	{
		v = m->area.size();
		v.set(element, value);
		m->area.setSize(v);
	}
	gui.mainView.postRedisplay();
}

void AtenDisorder::on_ComponentTable_itemSelectionChanged()
{
	refreshComponentData();
}

void AtenDisorder::on_ComponentTable_itemChanged(QTableWidgetItem *item)
{
	if (!gui.exists() || refreshing_) return;
	int column = ui.ComponentTable->column(item);
	Model *m;
	m = ((TTableWidgetItem*) item)->model();
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

void AtenDisorder::on_ComponentRegionCombo_currentIndexChanged(int index)
{
	int comp = ui.ComponentTable->currentRow();
	if (comp == -1) return;
	Model *m = aten.model(comp);
	m->area.setShape( (ComponentRegion::RegionShape) index);
	gui.mainView.postRedisplay();
}

void AtenDisorder::on_ShowRegionsCheck_clicked(bool checked)
{
	prefs.setVisibleOnScreen(Prefs::ViewRegions, checked);
	gui.mainView.postRedisplay();
}

void AtenDisorder::on_DisorderStartButton_clicked(bool checked)
{
	mc.setNCycles(ui.DisorderCyclesSpin->value());
	mc.disorder(aten.currentModel());
}

void AtenDisorder::on_VDWScaleSpin_valueChanged(double d)
{
	mc.setVdwScale(d);
}

void AtenDisorder::dialogFinished(int result)
{
	gui.mainWindow->ui.actionDisorderWindow->setChecked(FALSE);
}
