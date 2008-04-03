/*
	*** Qt disorder functions interface
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
#include "base/master.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "model/model.h"

void AtenForm::on_ComponentCentreXSpin_valueChanged(double d)
{
	setComponentCoords(0,0,d);
}

void AtenForm::on_ComponentCentreYSpin_valueChanged(double d)
{
	setComponentCoords(0,1,d);
}

void AtenForm::on_ComponentCentreZSpin_valueChanged(double d)
{
	setComponentCoords(0,2,d);
}

void AtenForm::on_ComponentSizeXSpin_valueChanged(double d)
{
	setComponentCoords(1,0,d);
}

void AtenForm::on_ComponentSizeYSpin_valueChanged(double d)
{
	setComponentCoords(2,1,d);
}

void AtenForm::on_ComponentSizeZSpin_valueChanged(double d)
{
	setComponentCoords(3,2,d);
}

void AtenForm::refreshDisorderPage()
{
	if (!gui.exists()) return;
	// (De)sensitize controls
	ui.DisorderStartButton->setDisabled(master.currentModel()->cell()->type() == CT_NONE);
	// Update model (component) list
	QListWidgetItem *item;
	ui.ComponentList->setCurrentRow(-1);
	ui.ComponentList->clear();
	for (Model *m = master.models(); m != NULL; m = m->next)
	{
		if (m->cell()->type() != CT_NONE) continue;
		item = new QListWidgetItem(ui.ComponentList);
		item->setText(m->name());
	}
	// Select the last component in the list
	ui.ComponentList->setCurrentRow(master.nModels()-1);
	refreshComponentData();
}

void AtenForm::refreshComponentData()
{
	// Get current component
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Model *m = master.model(comp);
	// Set controls
	ui.PopulationSpin->setValue(m->nRequested());
	ui.ComponentRegionCombo->setCurrentIndex(m->area.shape());
	Vec3<double> v;
	v = m->area.size();
	ui.ComponentSizeXSpin->setValue(v.x);
	ui.ComponentSizeYSpin->setValue(v.y);
	ui.ComponentSizeZSpin->setValue(v.z);
	v = m->area.centre();
	ui.ComponentCentreXSpin->setValue(v.x);
	ui.ComponentCentreYSpin->setValue(v.y);
	ui.ComponentCentreZSpin->setValue(v.z);
	ui.ComponentTranslateCheck->setChecked(m->isMoveAllowed(MonteCarlo::Translate));
	ui.ComponentRotateCheck->setChecked(m->isMoveAllowed(MonteCarlo::Rotate));
}

void AtenForm::setComponentCoords(int centsize, int element, double value)
{
	// Get current component
	static Vec3<double> v;
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Model *m = master.model(comp);
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

void AtenForm::on_ComponentList_itemSelectionChanged()
{
	refreshComponentData();
}

void AtenForm::on_PopulationSpin_valueChanged(int value)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Model *m = master.model(comp);
	m->setNRequested(value);
}

void AtenForm::on_ComponentTranslateCheck_clicked(bool checked)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Model *m = master.model(comp);
	m->setMoveAllowed(MonteCarlo::Translate, checked);
}

void AtenForm::on_ComponentRotateCheck_clicked(bool checked)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Model *m = master.model(comp);
	m->setMoveAllowed(MonteCarlo::Translate, checked);
}

void AtenForm::on_ComponentRegionCombo_currentIndexChanged(int index)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Model *m = master.model(comp);
	m->area.setShape( (ComponentRegionShape) index);
	gui.mainView.postRedisplay();
}

void AtenForm::on_ShowRegionsCheck_clicked(bool checked)
{
	prefs.setVisible(Prefs::ViewRegions, checked);
	gui.mainView.postRedisplay();
}

void AtenForm::on_DisorderStartButton_clicked(bool checked)
{
	mc.setNCycles(ui.DisorderCyclesSpin->value());
	mc.disorder(master.currentModel());
}

void AtenForm::on_VDWScaleSpin_valueChanged(double d)
{
	mc.setVdwScale(d);
}

