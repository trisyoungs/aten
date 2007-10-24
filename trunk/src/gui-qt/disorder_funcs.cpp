/*
	*** Qt disorder functions interface
	*** src/gui-qt/disorder_funcs.cpp
	Copyright T. Youngs 2007

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
#include "gui/gui.h"
#include "gui-qt/mainwindow.h"

void AtenForm::refresh_components()
{
	ui.ComponentList->clear();
	QListWidgetItem *item;
	for (component *c = mc.get_components(); c != NULL; c = c->next)
	{
		item = new QListWidgetItem(ui.ComponentList);
		item->setText(c->get_model()->get_name());
	}
	// Select the last component in the list
	ui.ComponentList->setCurrentRow(mc.get_ncomponents()-1);
}

void AtenForm::refresh_component_data()
{
	// Get current component
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = mc.get_component_by_index(comp);
	// Set controls
	ui.PopulationSpin->setValue(c->get_nrequested());
	ui.ComponentRegionCombo->setCurrentIndex(c->area.get_shape());
	vec3<double> v;
	v = c->area.get_size();
	ui.ComponentSizeXSpin->setValue(v.x);
	ui.ComponentSizeYSpin->setValue(v.y);
	ui.ComponentSizeZSpin->setValue(v.z);
	v = c->area.get_centre();
	ui.ComponentPosXSpin->setValue(v.x);
	ui.ComponentPosYSpin->setValue(v.y);
	ui.ComponentPosZSpin->setValue(v.z);
	ui.ComponentTranslateCheck->setChecked(c->get_allowed(MT_TRANSLATE));
	ui.ComponentRotateCheck->setChecked(c->get_allowed(MT_ROTATE));
}

void AtenForm::on_ComponentList_itemSelectionChanged()
{
	refresh_component_data();
}

// Add the current model to the component list
void AtenForm::on_AddComponentButton_clicked(bool checked)
{
	// If the current model is periodic, refuse to add it
	model *m = master.get_currentmodel();
	if (m->cell.get_type() != CT_NONE)
	{
		msg(DM_NONE,"Model is periodic - can't add to component list.\n");
		return;
	}
	// Add it to mc's component list and refresh the list
	component *comp = mc.add_component();
	comp->set_model(m);
	refresh_components();
}

void AtenForm::on_DeleteComponentButton_clicked(bool checked)
{
}

void AtenForm::on_PopulationSpin_valueChanged(int value)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = mc.get_component_by_index(comp);
	c->set_nrequested(value);
}

void AtenForm::on_ComponentTranslateCheck_clicked(bool checked)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = mc.get_component_by_index(comp);
	c->set_allowed(MT_TRANSLATE, checked);
}

void AtenForm::on_ComponentRotateCheck_clicked(bool checked)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = mc.get_component_by_index(comp);
	c->set_allowed(MT_ROTATE, checked);
}

void AtenForm::on_ComponentRegionCombo_currentIndexChanged(int index)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = mc.get_component_by_index(comp);
	c->area.set_shape( (region_shape) index);
}
