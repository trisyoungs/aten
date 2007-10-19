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

#include "base/master.h"
#include "classes/component.h"
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
}

void AtenForm::on_ComponentList_itemSelectionChanged()
{
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
}

void AtenForm::on_ComponentTranslateCheck_clicked(bool checked)
{
}

void AtenForm::on_ComponentRotateCheck_clicked(bool checked)
{
}

void AtenForm::on_ComponentRegionCombo_currentIndexChanged(int index)
{
}
