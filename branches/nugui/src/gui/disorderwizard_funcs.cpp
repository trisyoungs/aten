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
#include "methods/mc.h"
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
	existingModel_ = NULL;
	newModel_ = NULL;
	partitioningScheme_ = NULL;
	componentTarget_ = NULL;
	refreshing_ = FALSE;
	return exec();
}

// Update component data shown for current component
void DisorderWizard::setComponentData(Model *m)
{
	// Find the corresponding QTreeWidgetItem
	Refitem<QTreeWidgetItem, Model*> *ri = componentModelItems_.containsData(m);
	if (ri == NULL) return;
	ri->item->setIcon(0, m->icon());
	Dnchar text(-1,"%s\n", m->name());	//TGAY
	ri->item->setText(1, text.get());
	ri->item->setTextAlignment(1, Qt::AlignLeft | Qt::AlignTop);
}

void DisorderWizard::updateComponentControls()
{
	if ((componentTarget_ == NULL) || refreshing_) return;
	refreshing_ = TRUE;
	switch (componentTarget_->componentInsertionPolicy())
	{
		case (Model::NoPolicy):
			componentTarget_->setComponentInsertionPolicy(Model::NumberPolicy);
		case (Model::NumberPolicy):
			ui.NumberPolicyRadio->setChecked(TRUE);
			break;
		case (Model::DensityPolicy):
			ui.DensityPolicyRadio->setChecked(TRUE);
			break;
		case (Model::NumberAndDensityPolicy):
			ui.NumberAndDensityPolicyRadio->setChecked(TRUE);
			break;
		case (Model::RelativePolicy):
			ui.RelativePolicyRadio->setChecked(TRUE);
			break;
	}
	ui.ComponentPopulationSpin->setValue(componentTarget_->componentPopulation());
	ui.ComponentPopulationSpin->setDisabled(componentTarget_->componentInsertionPolicy() == Model::DensityPolicy);
	ui.ComponentDensitySpin->setValue(componentTarget_->componentDensity());
	ui.ComponentDensitySpin->setDisabled(componentTarget_->componentInsertionPolicy() == Model::NumberPolicy);
	ui.ComponentAllowRotationsCheck->setChecked(componentTarget_->componentRotatable());
	refreshing_ = FALSE;
}

// Update data in specified TTreeWidgetItem (from supplied data)
void DisorderWizard::setPartitionData(QTreeWidgetItem *target, PartitioningScheme *ps)
{
	target->setIcon(0,ps->icon());
	Dnchar text(-1,"%s\n%s\nNumber of partitions = %i\n", ps->name(), ps->description(), ps->nPartitions());
	target->setText(1,text.get());
	target->setTextAlignment(1, Qt::AlignLeft | Qt::AlignTop);
}

// Different page selected in the wizard...
void DisorderWizard::pageChanged(int id)
{
	TTreeWidgetItem *item;
	QTreeWidgetItem *qitem, *selectitem;
	Model *m;
	Dnchar text;
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
			existingModel_ = NULL;
			break;
		// Step 2 / 5 - Select model or define unit cell
		case (2):
			// INITIALISE - Create new model if necessary
			if (targetType_ != DisorderWizard::ExistingTarget)
			{
				newModel_ = aten.addModel();
				aten.setCurrentModel(newModel_, TRUE);
				newModel_->setName("Disorder Model");
				gui.update(GuiQt::AllTarget);
			}
			
			// Set correct stack page to show...
			ui.ModelSelectionStack->setCurrentIndex(targetType_);
			
			// Clear the current list
			if (targetType_ == DisorderWizard::ExistingTarget)
			{
				ui.ExistingModelTree->clear();
				ui.ExistingModelTree->setColumnCount(2);
				selectitem = NULL;
				for (m = aten.models(); m != NULL; m = m->next)
				{
					if (m->cell()->type() == UnitCell::NoCell) continue;
					item = new TTreeWidgetItem(ui.ExistingModelTree);
					item->data.set(VTypes::ModelData, m);
					item->setIcon(0,m->icon());
					item->setText(1,m->name());
					item->setTextAlignment(1, Qt::AlignLeft | Qt::AlignTop);
					if (selectitem == NULL) selectitem = item;
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
			partitioningSchemeItems_.clear();
			selectitem = NULL;
			for (PartitioningScheme *ps = aten.partitioningSchemes(); ps != NULL; ps = ps->next)
			{
				// Update grid and icon for PartitioningScheme
				ps->updatePartitions(TRUE);
				qitem = new QTreeWidgetItem(ui.PartitionTree);
				partitioningSchemeItems_.add(qitem, ps);
				setPartitionData(qitem,ps);
				if (selectitem == NULL) selectitem = qitem;
				// If the selected mode is DisorderWizard::GenerateTarget then we only allow the simple unit cell partitioning
				// Since it is always the first in the list, we cna just exit early.
				if (targetType_ == DisorderWizard::GenerateTarget) break;
			}
			ui.PartitionTree->resizeColumnToContents(0);
			ui.PartitionTree->resizeColumnToContents(1);
			ui.PartitionTree->setCurrentItem(selectitem);
			break;
		// Step 4 / 5 - Select component models
		case (4):
			// Enable/disable relative populations checkbox
			ui.NumberPolicyRadio->setDisabled(targetType_ == DisorderWizard::GenerateTarget);
			ui.DensityPolicyRadio->setDisabled(targetType_ == DisorderWizard::GenerateTarget);
			ui.RelativePolicyRadio->setDisabled(targetType_ == DisorderWizard::GenerateTarget);
			ui.NumberAndDensityPolicyRadio->setChecked(targetType_ == DisorderWizard::GenerateTarget);
			ui.ChooseComponentsTree->clear();
			ui.ChooseComponentsTree->setColumnCount(2);
			for (m = aten.models(); m != NULL; m = m->next)
			{
				if (m->cell()->type() != UnitCell::NoCell) continue;
				item = new TTreeWidgetItem(ui.ChooseComponentsTree);
				item->data.set(VTypes::ModelData, m);
				item->setIcon(0,m->icon());
				item->setText(1,m->name());
				item->setTextAlignment(1, Qt::AlignLeft | Qt::AlignTop);
			}
			ui.ChooseComponentsTree->resizeColumnToContents(0);
			ui.ChooseComponentsTree->resizeColumnToContents(1);
			// No selection by default, so disable Next button
			button(QWizard::NextButton)->setEnabled(FALSE);
			break;
		// Step 5 / 5 - Select component populations and partition assignments
		case (5):
			// Flag all components as not required except those selected in the ChooseComponentsTree
			for (m = aten.models(); m != NULL; m = m->next) m->setComponentInsertionPolicy(Model::NoPolicy);
			ui.EditComponentsTree->clear();
			ui.EditComponentsTree->setColumnCount(2);
			componentModelItems_.clear();
			selectitem = NULL;
			foreach (QTreeWidgetItem *qtwi, ui.ChooseComponentsTree->selectedItems())
			{
				TTreeWidgetItem *twi = (TTreeWidgetItem*) qtwi;
				m = (Model*) twi->data.asPointer(VTypes::ModelData);
				if (m == NULL)
				{
					printf("Error: Found a NULL model reference when populating EditComponentsTree.\n");
					continue;
				}
				m->setComponentInsertionPolicy(Model::NumberPolicy);
				item = new TTreeWidgetItem(ui.EditComponentsTree);
				item->data.set(VTypes::ModelData, m);
				componentModelItems_.add(item, m);
				setComponentData(m);
				if (selectitem == NULL) selectitem = item;
			}
			ui.EditComponentsTree->resizeColumnToContents(0);
			ui.EditComponentsTree->resizeColumnToContents(1);
			ui.EditComponentsTree->setCurrentItem(selectitem);
			// Refresh items in partition combo box
			ui.ComponentTargetPartitionCombo->clear();
			for (int n = 0; n < partitioningScheme_->nPartitions(); ++n)
			{
				text.sprintf("%i %s", n, partitioningScheme_->partitionName(n));
				ui.ComponentTargetPartitionCombo->addItem(text.get());
			}
			updateComponentControls();
			break;
	}
}

void DisorderWizard::rejected()
{
	// If a new model was created, remove it here
	if (newModel_ != NULL)
	{
		aten.removeModel(newModel_);
		newModel_ = NULL;
	}
	existingModel_ = NULL;
	gui.update(GuiQt::AllTarget);
}

void DisorderWizard::accepted()
{
	// Ready to run disordered builder!
	bool success;
	if (targetType_ == DisorderWizard::ExistingTarget) success = mc.disorder(existingModel_, partitioningScheme_, TRUE);
	else if (targetType_ == DisorderWizard::NewTarget) success = mc.disorder(newModel_, partitioningScheme_, TRUE);
	else success = mc.disorder(newModel_, partitioningScheme_, FALSE);
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
	existingModel_ = (Model*) twi->data.asPointer(VTypes::ModelData);
	if (existingModel_ == NULL) return;
	aten.setCurrentModel(existingModel_, TRUE);
	if (existingModel_ != NULL) existingModel_->changeLog.add(Log::Visual);
	gui.update(GuiQt::AllTarget);
}

void DisorderWizard::on_ExistingModelTree_itemSelectionChanged()
{
	// Get number of selected items in tree
	int nselected = ui.ExistingModelTree->selectedItems().size();
	// Get button pointer
	QAbstractButton *nextButton = button(QWizard::NextButton);
	nextButton->setEnabled(nselected != 0);
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

void DisorderWizard::on_PartitionTree_itemSelectionChanged()
{
	// Get number of selected items in tree
	int nselected = ui.PartitionTree->selectedItems().size();
	// Get button pointer
	QAbstractButton *nextButton = button(QWizard::NextButton);
	nextButton->setEnabled(nselected != 0);
}

void DisorderWizard::on_PartitionTree_currentItemChanged(QTreeWidgetItem *current, QTreeWidgetItem *previous)
{
	if (current == NULL) return;
	Refitem<QTreeWidgetItem, PartitioningScheme*> *ri = partitioningSchemeItems_.contains(current);
	if (ri == NULL) return;
	partitioningScheme_ = ri->data;
	if (existingModel_ != NULL) existingModel_->changeLog.add(Log::Visual);
	else if (newModel_ != NULL) newModel_->changeLog.add(Log::Visual);
	gui.update(GuiQt::CanvasTarget);
	// Enable/disable options button based on presence of custom dialog widgets...
	ui.PartitionSchemeOptionsButton->setEnabled(partitioningScheme_->hasOptions());
}

void DisorderWizard::on_PartitionSchemeOptionsButton_clicked(bool checked)
{
	// Run custom dialog for scheme
	if (partitioningScheme_ == NULL) return;
	partitioningScheme_->runOptions();
	partitioningScheme_->updatePartitions(TRUE);
	Refitem<QTreeWidgetItem, PartitioningScheme*> *ri = partitioningSchemeItems_.containsData(partitioningScheme_);
	if (ri == NULL) return;
	setPartitionData(ri->item, ri->data);
	partitioningScheme_ = ri->data;
	if (existingModel_ != NULL) existingModel_->changeLog.add(Log::Visual);
	else if (newModel_ != NULL) newModel_->changeLog.add(Log::Visual);
	gui.update(GuiQt::CanvasTarget);
}

// Step 4 / 5 - Select component models

void DisorderWizard::on_ChooseComponentsTree_itemSelectionChanged()
{
	// Get number of selected items in tree
	int nselected = ui.ChooseComponentsTree->selectedItems().size();
	// Get button pointer
	QAbstractButton *nextButton = button(QWizard::NextButton);
	nextButton->setEnabled(nselected != 0);
}

// Step 5 / 5 - Select component populations and partition assignments

void DisorderWizard::on_EditComponentsTree_currentItemChanged(QTreeWidgetItem *current, QTreeWidgetItem *previous)
{
	Refitem<QTreeWidgetItem, Model*> *ri = componentModelItems_.contains(current);
	if (ri == NULL) return;
	componentTarget_ = ri->data;
	updateComponentControls();
}

void DisorderWizard::on_NumberPolicyRadio_clicked(bool checked)
{
	if ((componentTarget_ == NULL) || refreshing_) return;
	if (checked) componentTarget_->setComponentInsertionPolicy(Model::NumberPolicy);
	setComponentData(componentTarget_);
	updateComponentControls();
}

void DisorderWizard::on_DensityPolicyRadio_clicked(bool checked)
{
	if ((componentTarget_ == NULL) || refreshing_) return;
	if (checked) componentTarget_->setComponentInsertionPolicy(Model::DensityPolicy);
	setComponentData(componentTarget_);
	updateComponentControls();
}

void DisorderWizard::on_NumberAndDensityPolicyRadio_clicked(bool checked)
{
	if ((componentTarget_ == NULL) || refreshing_) return;
	if (checked) componentTarget_->setComponentInsertionPolicy(Model::NumberAndDensityPolicy);
	setComponentData(componentTarget_);
	updateComponentControls();
}

void DisorderWizard::on_RelativePolicyRadio_clicked(bool checked)
{
	if ((componentTarget_ == NULL) || refreshing_) return;
	if (checked) componentTarget_->setComponentInsertionPolicy(Model::RelativePolicy);
	setComponentData(componentTarget_);
	updateComponentControls();
}

void DisorderWizard::on_ComponentPopulationSpin_valueChanged(int value)
{
	if ((componentTarget_ == NULL) || refreshing_) return;
	componentTarget_->setComponentPopulation(value);
	setComponentData(componentTarget_);
}

void DisorderWizard::on_ComponentDensitySpin_valueChanged(double value)
{
	if ((componentTarget_ == NULL) || refreshing_) return;
	componentTarget_->setComponentDensity(value);
	setComponentData(componentTarget_);
}

void DisorderWizard::on_ComponentAllowRotationsCheck_clicked(bool checked)
{
	if ((componentTarget_ == NULL) || refreshing_) return;
	componentTarget_->setComponentRotatable(checked);
	setComponentData(componentTarget_);
}

void DisorderWizard::on_ComponentTargetPartitionCombo_currentIndexChanged(int index)
{
	if ((componentTarget_ == NULL) || refreshing_) return;
	componentTarget_->setComponentPartition(index);
	setComponentData(componentTarget_);
}

/*
// Local Variable Functions
*/

// Return currently-selected partitioning scheme
PartitioningScheme *DisorderWizard::partitioningScheme()
{
	return partitioningScheme_;
}

// Return relevant unit cell
UnitCell *DisorderWizard::cell()
{
	if (targetType_ == DisorderWizard::ExistingTarget)
	{
		if (existingModel_ == NULL)
		{
			printf("Internal Error: DisorderWizard was asked for a cell when no existing model had been set.\n");
			return NULL;
		}
		else return existingModel_->cell();
	}
	else
	{
		if (newModel_ == NULL)
		{
			printf("Internal Error: DisorderWizard was asked for a cell when no new model had been set.\n");
			return NULL;
		}
		else return newModel_->cell();
	}
}
