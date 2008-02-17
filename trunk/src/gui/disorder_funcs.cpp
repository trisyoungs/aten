/*
	*** Qt disorder functions interface
	*** src/gui/disorder_funcs.cpp
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
#include "gui/mainwindow.h"

void AtenForm::refresh_disorderpage()
{
	model *m = master.get_currentmodel();
	// (De)sensitize controls
	if (m->get_celltype() == CT_NONE)
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

void AtenForm::refresh_components()
{
	ui.ComponentList->setCurrentRow(-1);
	ui.ComponentList->clear();
	QListWidgetItem *item;
	for (component *c = master.mc.components.first(); c != NULL; c = c->next)
	{
		item = new QListWidgetItem(ui.ComponentList);
		item->setText(c->get_model()->get_name());
	}
	// Select the last component in the list
	ui.ComponentList->setCurrentRow(master.mc.components.size()-1);
}

void AtenForm::refresh_component_data()
{
	// Get current component
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = master.mc.components[comp];
	// Set controls
	ui.PopulationSpin->setValue(c->get_nrequested());
	ui.ComponentRegionCombo->setCurrentIndex(c->area.get_shape());
	vec3<double> v;
	v = c->area.get_size();
	ui.ComponentSizeXSpin->setValue(v.x);
	ui.ComponentSizeYSpin->setValue(v.y);
	ui.ComponentSizeZSpin->setValue(v.z);
	v = c->area.get_centre();
	ui.ComponentCentreXSpin->setValue(v.x);
	ui.ComponentCentreYSpin->setValue(v.y);
	ui.ComponentCentreZSpin->setValue(v.z);
	ui.ComponentTranslateCheck->setChecked(c->get_allowed(MT_TRANSLATE));
	ui.ComponentRotateCheck->setChecked(c->get_allowed(MT_ROTATE));
}

void AtenForm::set_component_coords(int centsize, int element, double value)
{
	// Get current component
	static vec3<double> v;
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = master.mc.components[comp];
	if (centsize == 0)
	{
		v = c->area.get_centre();
		v.set(element, value);
		c->area.set_centre(v);
	}
	else
	{
		v = c->area.get_size();
		v.set(element, value);
		c->area.set_size(v);
	}
	gui.mainview.postredisplay();
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
	if (m->get_celltype() != CT_NONE)
	{
		msg(DM_NONE,"Model is periodic - can't add to component list.\n");
		return;
	}
	// Add it to master.mc.s component list and refresh the list
	component *comp = master.mc.components.add();
	comp->set_model(m);
	refresh_components();
}

void AtenForm::on_DeleteComponentButton_clicked(bool checked)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = master.mc.components[comp];
	master.mc.components.remove(c);
	// Refresh the components list and GUI (for regions)
	refresh_components();
	gui.mainview.postredisplay();
}

void AtenForm::on_PopulationSpin_valueChanged(int value)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = master.mc.components[comp];
	c->set_nrequested(value);
}

void AtenForm::on_ComponentTranslateCheck_clicked(bool checked)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = master.mc.components[comp];
	c->set_allowed(MT_TRANSLATE, checked);
}

void AtenForm::on_ComponentRotateCheck_clicked(bool checked)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = master.mc.components[comp];
	c->set_allowed(MT_ROTATE, checked);
}

void AtenForm::on_ComponentRegionCombo_currentIndexChanged(int index)
{
	int comp = ui.ComponentList->currentRow();
	if (comp == -1) return;
	component *c = master.mc.components[comp];
	c->area.set_shape( (region_shape) index);
	gui.mainview.postredisplay();
}

void AtenForm::on_ShowRegionsCheck_clicked(bool checked)
{
	prefs.set_visible(VO_REGIONS, checked);
	gui.mainview.postredisplay();
}

void AtenForm::on_DisorderStartButton_clicked(bool checked)
{
	master.mc.set_ncycles(ui.DisorderCyclesSpin->value());
	master.mc.disorder(master.get_currentmodel());
}

void AtenForm::on_VDWScaleSpin_valueChanged(double d)
{
	master.mc.set_vdw_radius_scale(d);
}

