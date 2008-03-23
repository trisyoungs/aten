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
	Model *m = master.currentModel();
	// (De)sensitize controls
	if (m->cell()->type() == CT_NONE)
	{
		ui.DisorderStartButton->setDisabled(TRUE);
		ui.AddComponentButton->setDisabled(FALSE);
	}
	else
	{
		ui.DisorderStartButton->setDisabled(FALSE);
		ui.AddComponentButton->setDisabled(TRUE);
	}
}

void AtenForm::refreshComponents()
{
	ui.ComponentList->setCurrentRow(-1);
	ui.ComponentList->clear();
	QListWidgetItem *item;
	for (Component *c = mc.components.first(); c != NULL; c = c->next)
	{
		item = new QListWidgetItem(ui.ComponentList);
		item->setText(c->model()->name());
	}
	// Select the last component in the list
	ui.ComponentList->setCurrentRow(mc.components.nItems()-1);
}

void AtenForm::refreshComponentData()
{
	// Get current component
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Component *c = mc.components[comp];
	// Set controls
	ui.PopulationSpin->setValue(c->nRequested());
	ui.ComponentRegionCombo->setCurrentIndex(c->area.shape());
	Vec3<double> v;
	v = c->area.size();
	ui.ComponentSizeXSpin->setValue(v.x);
	ui.ComponentSizeYSpin->setValue(v.y);
	ui.ComponentSizeZSpin->setValue(v.z);
	v = c->area.centre();
	ui.ComponentCentreXSpin->setValue(v.x);
	ui.ComponentCentreYSpin->setValue(v.y);
	ui.ComponentCentreZSpin->setValue(v.z);
	ui.ComponentTranslateCheck->setChecked(c->isMoveAllowed(MT_TRANSLATE));
	ui.ComponentRotateCheck->setChecked(c->isMoveAllowed(MT_ROTATE));
}

void AtenForm::setComponentCoords(int centsize, int element, double value)
{
	// Get current component
	static Vec3<double> v;
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Component *c = mc.components[comp];
	if (centsize == 0)
	{
		v = c->area.centre();
		v.set(element, value);
		c->area.setCentre(v);
	}
	else
	{
		v = c->area.size();
		v.set(element, value);
		c->area.setSize(v);
	}
	gui.mainView.postRedisplay();
}

void AtenForm::on_ComponentList_itemSelectionChanged()
{
	refreshComponentData();
}

// Add the current model to the component list
void AtenForm::on_AddComponentButton_clicked(bool checked)
{
	// If the current model is periodic, refuse to add it
	Model *m = master.currentModel();
	if (m->cell()->type() != CT_NONE)
	{
		msg(DM_NONE,"Model is periodic - can't add to component list.\n");
		return;
	}
	// Add it to mc.s component list and refresh the list
	Component *comp = mc.components.add();
	comp->setModel(m);
	refreshComponents();
}

void AtenForm::on_DeleteComponentButton_clicked(bool checked)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Component *c = mc.components[comp];
	mc.components.remove(c);
	// Refresh the components list and GUI (for regions)
	refreshComponents();
	gui.mainView.postRedisplay();
}

void AtenForm::on_PopulationSpin_valueChanged(int value)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Component *c = mc.components[comp];
	c->setNRequested(value);
}

void AtenForm::on_ComponentTranslateCheck_clicked(bool checked)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Component *c = mc.components[comp];
	c->setMoveAllowed(MT_TRANSLATE, checked);
}

void AtenForm::on_ComponentRotateCheck_clicked(bool checked)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Component *c = mc.components[comp];
	c->setMoveAllowed(MT_ROTATE, checked);
}

void AtenForm::on_ComponentRegionCombo_currentIndexChanged(int index)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	Component *c = mc.components[comp];
	c->area.setShape( (ComponentRegionShape) index);
	gui.mainView.postRedisplay();
}

void AtenForm::on_ShowRegionsCheck_clicked(bool checked)
{
	prefs.setVisible(VO_REGIONS, checked);
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

