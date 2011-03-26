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
#include "gui/gui.h"
#include "gui/disorderwizard.h"
#include "gui/ttreewidgetitem.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include "parser/commandnode.h"

// Constructor
DisorderWizard::DisorderWizard(QWidget *parent) : QWizard(parent)
{
	ui.setupUi(this);
	QObject::connect(this, SIGNAL(currentIdChanged(int)), SLOT(pageChanged(int)));
	QObject::connect(this, SIGNAL(rejected()), SLOT(rejected()));
	QObject::connect(this, SIGNAL(accepted()), SLOT(accepted()));
	QObject::connect(ui.CellAngleASpin, SIGNAL(valueChanged(double)), SLOT(setCellAbsolute(double)));
	QObject::connect(ui.CellAngleBSpin, SIGNAL(valueChanged(double)), SLOT(setCellAbsolute(double)));
	QObject::connect(ui.CellAngleCSpin, SIGNAL(valueChanged(double)), SLOT(setCellAbsolute(double)));
	QObject::connect(ui.CellLengthASpin, SIGNAL(valueChanged(double)), SLOT(setCellAbsolute(double)));
	QObject::connect(ui.CellLengthBSpin, SIGNAL(valueChanged(double)), SLOT(setCellAbsolute(double)));
	QObject::connect(ui.CellLengthCSpin, SIGNAL(valueChanged(double)), SLOT(setCellAbsolute(double)));
	QObject::connect(ui.CellRelativeAngleASpin, SIGNAL(valueChanged(double)), SLOT(setCellRelative(double)));
	QObject::connect(ui.CellRelativeAngleBSpin, SIGNAL(valueChanged(double)), SLOT(setCellRelative(double)));
	QObject::connect(ui.CellRelativeAngleCSpin, SIGNAL(valueChanged(double)), SLOT(setCellRelative(double)));
	QObject::connect(ui.CellRelativeASpin, SIGNAL(valueChanged(double)), SLOT(setCellRelative(double)));
	QObject::connect(ui.CellRelativeBSpin, SIGNAL(valueChanged(double)), SLOT(setCellRelative(double)));
	QObject::connect(ui.CellRelativeCSpin, SIGNAL(valueChanged(double)), SLOT(setCellRelative(double)));

	// Private variables
	DisorderWizard::TargetModelType targetType_;
}

// Run dialog, initialising any values first
int DisorderWizard::run()
{
	// If there are no loaded models with periodicity, disable this option on page 1
	int nperiodic = 0;
	for (Model *m = aten.models(); m != NULL; m = m->next) if (m->cell()->type() != UnitCell::NoCell) ++nperiodic;
	ui.TargetExistingRadio->setEnabled(nperiodic != 0);
	ui.TargetNewRadio->setChecked(nperiodic == 0);
	ui.TargetExistingRadio->setChecked(nperiodic != 0);
	targetType_ = nperiodic == 0 ? DisorderWizard::NewTarget : DisorderWizard::ExistingTarget;
	targetModel_ = NULL;
	newModel_ = NULL;
	partitioningScheme_ = NULL;
	return exec();
}

// Update component data shown for current component
void DisorderWizard::updateComponentData()
{
}

// Different page selected in the wizard...
void DisorderWizard::pageChanged(int id)
{
	TTreeWidgetItem *item, *selectitem;
	int count;
	switch (id)
	{
		// Step 1 / 5 - Select target model type
		case (1):
			// CLEANUP - Do we need to remove a previously-created model since we moved back a page?
			if (newModel_ != NULL)
			{
				aten.removeModel(newModel_);
				gui.update(GuiQt::AllTarget);
				newModel_ = NULL;
			}
			break;
		// Step 2 / 5 - Select model or define unit cell
		case (2):
			// INITIALISE - Create new model if necessary
			if (targetType_ != DisorderWizard::ExistingTarget)
			{
				newModel_ = aten.addModel();
				gui.update(GuiQt::AllTarget);
			}
			
			// Set correct stack page to show...
			ui.ModelSelectionStack->setCurrentIndex(targetType_);
			
			// Clear the current list
			if (targetType_ == DisorderWizard::ExistingTarget)
			{
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
					if (m == aten.models()) selectitem = item;
				}
				ui.ExistingModelTree->resizeColumnToContents(0);
				ui.ExistingModelTree->resizeColumnToContents(1);
				ui.ExistingModelTree->setCurrentItem(selectitem);
			}
			break;
		// Step 3 / 5 - Select partitioning scheme for cell
		case (3):
			ui.PartitionTree->clear();
			ui.PartitionTree->setColumnCount(2);
			count = 0;
			for (PartitioningScheme *ps = aten.partitioningSchemes(); ps != NULL; ps = ps->next)
			{
				// Update grid and icon for PartitioningScheme
				ps->updatePartitions(TRUE);
				item = new TTreeWidgetItem(ui.PartitionTree);
				item->data.set(VTypes::IntegerData, count);
				item->setIcon(0,ps->icon());
				Dnchar text(-1,"%s\n%s\nNumber of partitions = %i\n", ps->name(), ps->description(), ps->nPartitions());
				item->setText(1,text.get());
				item->setTextAlignment(1, Qt::AlignLeft | Qt::AlignTop);
				++count;
			}
			ui.PartitionTree->resizeColumnToContents(0);
			ui.PartitionTree->resizeColumnToContents(1);
			ui.PartitionTree->setCurrentItem(0);
			break;
		// Step 4 / 5 - Select component models
		case (4):
			break;
		// Step 5 / 5 - Select component populations and partition assignments
		case (5):
			break;
	}
}

void DisorderWizard::rejected()
{
	printf("REJECTED\n");
}

void DisorderWizard::accepted()
{
	printf("ACCEPTED\n");
}

/*
// Step 1 / 5 - Select target model type
*/

void DisorderWizard::on_TargetExistingRadio_clicked(bool checked)
{
	if (checked) targetType_ = DisorderWizard::ExistingTarget;
}

void DisorderWizard::on_TargetNewRadio_clicked(bool checked)
{
	if (checked) targetType_ = DisorderWizard::NewTarget;
}

void DisorderWizard::on_TargetGenerateRadio_clicked(bool checked)
{
	if (checked) targetType_ = DisorderWizard::GenerateTarget;
}

/*
// Step 2 / 5 - Select model or define unit cell
*/

void DisorderWizard::on_ExistingModelTree_currentItemChanged(QTreeWidgetItem *current, QTreeWidgetItem *previous)
{
	if (current == NULL) return;
	TTreeWidgetItem *twi = (TTreeWidgetItem*) current;
	targetModel_ = (Model*) twi->data.asPointer(VTypes::ModelData);
	if (targetModel_ == NULL) return;
	aten.setCurrentModel(targetModel_);
	gui.update(GuiQt::AllTarget);
}

// Grab values from defined cell controls
void DisorderWizard::setCellAbsolute(double value)
{
	if (newModel_ != NULL)
	{
		newModel_->cell()->setLengths(Vec3<double>(ui.CellLengthASpin->value(), ui.CellLengthBSpin->value(), ui.CellLengthCSpin->value()));
		newModel_->cell()->setAngles(Vec3<double>(ui.CellAngleASpin->value(), ui.CellAngleBSpin->value(), ui.CellAngleCSpin->value()));
		gui.update(GuiQt::CanvasTarget+GuiQt::CellTarget);
	}
	else printf("Internal Error: No newModel_ pointer defined to set UnitCell in.\n");
}

// Grab values from relative cell controls
void DisorderWizard::setCellRelative(double value)
{
	if (newModel_ != NULL)
	{
		newModel_->cell()->setLengths(Vec3<double>(ui.CellRelativeASpin->value(), ui.CellRelativeBSpin->value(), ui.CellRelativeCSpin->value()));
		newModel_->cell()->setAngles(Vec3<double>(ui.CellRelativeAngleASpin->value(), ui.CellRelativeAngleBSpin->value(), ui.CellRelativeAngleCSpin->value()));
		gui.update(GuiQt::CanvasTarget+GuiQt::CellTarget);
	}
	else printf("Internal Error: No newModel_ pointer defined to set UnitCell in.\n");

}

/*
// Step 3 / 5 - Select partitioning scheme for cell
*/

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

/*
// Local Variable Functions
*/

// Return currently-selected partitioning scheme
PartitioningScheme *DisorderWizard::partitioningScheme()
{
	return partitioningScheme_;
}