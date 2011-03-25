/*
	*** Disordered Builder Wizard
	*** src/gui/disorderwizard_funcs.cpp
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
#include "gui/disorderwizard.h"
#include "gui/ttreewidgetitem.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include "parser/commandnode.h"

// Constructor
DisorderWizard::DisorderWizard(QWidget *parent) : QWizard(parent)
{
	ui.setupUi(this);

	// Private variables
	DisorderWizard::TargetModelType targetType_;
}

// Run dialog, initialising any values first
int DisorderWizard::run()
{
	return exec();
}

// Update component data shown for current component
void DisorderWizard::updateComponentData()
{
}

// DIfferent page selected in the wizard...
void DisorderWizard::pageChanged(int id)
{
	printf("Current wizard page id is now %i\n", id);
	TTreeWidgetItem *item;
	switch (id)
	{
		// Step 1 / 5 - Select target model type
		case (0):
			break;
		// Step 2 / 5 - Select model or define unit cell
		case (1):
			// Clear the current list
			ui.ExistingModelTree->clear();
			ui.ExistingModelTree->setColumnCount(2);
			for (Model *m = aten.models(); m != NULL; m = m->next)
			{
				if (m->cell()->type() == UnitCell::NoCell) continue;
				item = new TTreeWidgetItem(ui.ExistingModelTree);
				item->data.set(VTypes::ModelData, m);
				item->setIcon(0,m->icon());
				item->setText(1,m->name());
				item->setTextAlignment(1, Qt::AlignLeft | Qt::AlignTop);
			}
			ui.ExistingModelTree->resizeColumnToContents(0);
			ui.ExistingModelTree->resizeColumnToContents(1);
			ui.ExistingModelTree->setCurrentItem(0);
		// Step 3 / 5 - Select partitioning scheme for cell
		case (2):
			break;
		// Step 4 / 5 - Select component models
		case (3):
			break;
		// Step 5 / 5 - Select component populations and partition assignments
		case (4):
			break;
	}
}

// Step 1 / 5 - Select target model type
void DisorderWizard::on_TargetExistingRadio_clicked(bool checked)
{
}

void DisorderWizard::on_TargetNewRadio_clicked(bool checked)
{
}

void DisorderWizard::on_TargetGenerateRadio_clicked(bool checked)
{
}

// Step 2 / 5 - Select model or define unit cell

// Step 3 / 5 - Select partitioning scheme for cell
void DisorderWizard::on_PartitionSchemeOptionsButton_clicked(bool checked)
{
}

// Step 4 / 5 - Select component models

// Step 5 / 5 - Select component populations and partition assignments
void DisorderWizard::on_ComponentPopulationSpin_valueChanged(int value)
{
}

void DisorderWizard::on_ComponentBulkCheck_clicked(bool checked)
{
}

void DisorderWizard::on_ComponentDensitySpin_valueChanged(double value)
{
}

void DisorderWizard::on_ComponentFreeDensityCheck_clicked(bool checked)
{
}

void DisorderWizard::on_ComponentAllowRotationsCheck_clicked(bool checked)
{
}

void DisorderWizard::on_ComponentAllowTranslationsCheck_clicked(bool checked)
{
}

void DisorderWizard::on_ComponentTargetPartitionCombo_currentIndexChanged(int index)
{
}
