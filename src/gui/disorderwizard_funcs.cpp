/*
	*** Disorder Builder Wizard
	*** src/gui/disorderwizard_funcs.cpp
	Copyright T. Youngs 2007-2018

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
#include "gui/disorderoptions.h"
#include "model/model.h"
#include "methods/mc.h"
#include "base/sysfunc.h"
#include "methods/partitiondata.h"
#include "parser/commandnode.h"
#include "templates/variantpointer.h"

// Constructor
DisorderWizard::DisorderWizard(AtenWindow& parent) : QWizard(&parent), parent_(parent)
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
	DisorderWizard::TargetModelType targetType_ = DisorderWizard::ExistingTarget;
	existingModel_ = NULL;
	newModel_ = NULL;
	partitioningScheme_ = NULL;
	componentTarget_ = NULL;
	refreshing_ = false;
}

// Run dialog, initialising any values first
int DisorderWizard::run()
{
	// If there are no loaded models with periodicity, disable this option on page 1
	int nPeriodic = 0;
	for (Model* m = parent_.aten().models(); m != NULL; m = m->next) if (m->renderSourceModel()->cell().type() != UnitCell::NoCell) ++nPeriodic;
	ui.TargetExistingRadio->setEnabled(nPeriodic != 0);
	ui.TargetNewRadio->setChecked(nPeriodic == 0);
	ui.TargetExistingRadio->setChecked(nPeriodic != 0);
	targetType_ = nPeriodic == 0 ? DisorderWizard::NewTarget : DisorderWizard::ExistingTarget;
	existingModel_ = NULL;
	newModel_ = NULL;
	partitioningScheme_ = NULL;
	componentTarget_ = NULL;
	refreshing_ = false;
	
	// Make sure page 1 is the starting page
	restart();
	
	// Update partition grids
	Task* task = Messenger::initialiseTask("Generating Partition Data", parent_.aten().nPartitioningSchemes());
	for (PartitioningScheme* ps = parent_.aten().partitioningSchemes(); ps != NULL; ps = ps->next)
	{
		ps->setGridSize(prefs.partitionGridSize());
		ps->recalculatePartitions();
		if (!Messenger::incrementTaskProgress(task)) return QDialog::Rejected;
	}
	Messenger::terminateTask(task);

	return exec();
}

// Update component data shown for current component
void DisorderWizard::setComponentData(Model* m)
{
	// Find the corresponding QTreeWidgetItem
	RefListItem<QTreeWidgetItem, Model*>* ri = componentModelItems_.containsData(m);
	if (ri == NULL) return;
	ri->item->setIcon(0, m->icon());
	QString text;
	text.sprintf("%s\nPolicy: %s", qPrintable(m->name()), Model::insertionPolicy(m->componentInsertionPolicy()));
	if (m->componentInsertionPolicy() != Model::DensityPolicy) text += QString("\nPopulation: %1").arg(m->componentPopulation());
	if (m->componentInsertionPolicy() != Model::NumberPolicy) text += QString("\nDensity: %1").arg(m->componentDensity());
	text += QString("\nPartition: %1 %2").arg(m->componentPartition()+1).arg(partitioningScheme_->partitionName(m->componentPartition()));
	ri->item->setText(1, text);
	ri->item->setTextAlignment(1, Qt::AlignLeft | Qt::AlignTop);
}

void DisorderWizard::updateComponentControls()
{
	if ((componentTarget_ == NULL) || refreshing_) return;
	refreshing_ = true;
	switch (componentTarget_->componentInsertionPolicy())
	{
		case (Model::NoPolicy):
			componentTarget_->setComponentInsertionPolicy(Model::NumberPolicy);
		case (Model::NumberPolicy):
			ui.NumberPolicyRadio->setChecked(true);
			break;
		case (Model::DensityPolicy):
			ui.DensityPolicyRadio->setChecked(true);
			break;
		case (Model::NumberAndDensityPolicy):
			ui.NumberAndDensityPolicyRadio->setChecked(true);
			break;
		case (Model::RelativePolicy):
			ui.RelativePolicyRadio->setChecked(true);
			break;
		default:
			break;
	}

	ui.ComponentPopulationSpin->setValue(componentTarget_->componentPopulation());
	ui.ComponentPopulationSpin->setDisabled(componentTarget_->componentInsertionPolicy() == Model::DensityPolicy);
	ui.ComponentDensitySpin->setValue(componentTarget_->componentDensity());
	ui.ComponentDensitySpin->setDisabled(componentTarget_->componentInsertionPolicy() == Model::NumberPolicy);
	ui.ComponentAllowRotationsCheck->setChecked(componentTarget_->componentRotatable());
	if (componentTarget_->componentPartition() >= partitioningScheme_->nPartitions()) componentTarget_->setComponentPartition(0);
	ui.ComponentTargetPartitionCombo->setCurrentIndex(componentTarget_->componentPartition());
	refreshing_ = false;
}

// Update data in specified TTreeWidgetItem (from supplied data)
void DisorderWizard::setPartitionData(QTreeWidgetItem* target, PartitioningScheme* ps)
{
	target->setIcon(0,ps->icon());
	QString text;
	text.sprintf("%s\n%s\nNumber of partitions = %i\n", qPrintable(ps->name()), qPrintable(ps->description()), ps->nPartitions());
	target->setText(1,text);
	target->setTextAlignment(1, Qt::AlignLeft | Qt::AlignTop);
}

// Different page selected in the wizard...
void DisorderWizard::pageChanged(int id)
{
	QTreeWidgetItem* item;
	QTreeWidgetItem* qitem, *selectItem;
	Model* m;
	int count;
	switch (id)
	{
		// Step 1 / 5 - Select target model type
		case (1):
			// CLEANUP - Do we need to remove a previously-created model since we moved back a page?
			if (newModel_ != NULL)
			{
				parent_.aten().removeModel(newModel_);
				parent_.updateWidgets(AtenWindow::AllTargets);
				newModel_ = NULL;
			}
			existingModel_ = NULL;
			break;
		// Step 2 / 5 - Select model or define unit cell
		case (2):
			// INITIALISE - Create new model if necessary
			if ((targetType_ != DisorderWizard::ExistingTarget) && (newModel_ == NULL))
			{
				newModel_ = parent_.aten().addModel();
				parent_.aten().setCurrentModel(newModel_);
				newModel_->setName("Disorder Model");
				if (targetType_ == DisorderWizard::NewTarget) setCellAbsolute(0.0);
				else setCellRelative(0.0);
				parent_.updateWidgets(AtenWindow::AllTargets);
			}
			
			// Set correct stack page to show...
			ui.ModelSelectionStack->setCurrentIndex(targetType_);
			
			// Clear the current list
			if (targetType_ == DisorderWizard::ExistingTarget)
			{
				ui.ExistingModelTree->clear();
				ui.ExistingModelTree->setColumnCount(2);
				selectItem = NULL;
				for (m = parent_.aten().models(); m != NULL; m = m->next)
				{
					if (m->renderSourceModel()->cell().type() == UnitCell::NoCell) continue;
					item = new QTreeWidgetItem(ui.ExistingModelTree);
					item->setData(0, Qt::UserRole, VariantPointer<Model>(m));
					item->setIcon(0,m->icon());
					item->setText(1,m->name());
					item->setTextAlignment(1, Qt::AlignLeft | Qt::AlignTop);
					if (selectItem == NULL) selectItem = item;
				}
				ui.ExistingModelTree->resizeColumnToContents(0);
				ui.ExistingModelTree->resizeColumnToContents(1);
				ui.ExistingModelTree->setCurrentItem(selectItem);
			}
			break;
		// Step 3 / 5 - Select partitioning scheme for cell
		case (3):
			ui.PartitionTree->clear();
			ui.PartitionTree->setColumnCount(2);
			partitioningSchemeItems_.clear();
			selectItem = NULL;
			
			for (PartitioningScheme* ps = parent_.aten().partitioningSchemes(); ps != NULL; ps = ps->next)
			{
				qitem = new QTreeWidgetItem(ui.PartitionTree);
				partitioningSchemeItems_.add(qitem, ps);
				setPartitionData(qitem,ps);
				if (selectItem == NULL) selectItem = qitem;
				// If the selected mode is DisorderWizard::GenerateTarget then we only allow the simple unit cell partitioning
				// Since it is always the first in the list, we can just exit early.
				if (targetType_ == DisorderWizard::GenerateTarget) break;
			}
			ui.PartitionTree->resizeColumnToContents(0);
			ui.PartitionTree->resizeColumnToContents(1);
			ui.PartitionTree->setCurrentItem(selectItem);
			break;
		// Step 4 / 5 - Select component models
		case (4):
			// Enable/disable relative populations checkbox
			refreshing_ = true;
			ui.NumberPolicyRadio->setDisabled(targetType_ == DisorderWizard::GenerateTarget);
			ui.DensityPolicyRadio->setDisabled(targetType_ == DisorderWizard::GenerateTarget);
			ui.RelativePolicyRadio->setDisabled(targetType_ == DisorderWizard::GenerateTarget);
			ui.NumberAndDensityPolicyRadio->setChecked(targetType_ == DisorderWizard::GenerateTarget);
			ui.ChooseComponentsTree->clear();
			ui.ChooseComponentsTree->setColumnCount(2);
			for (m = parent_.aten().models(); m != NULL; m = m->next)
			{
				if (m->renderSourceModel()->cell().type() != UnitCell::NoCell) continue;
				item = new QTreeWidgetItem(ui.ChooseComponentsTree);
				item->setData(0, Qt::UserRole, VariantPointer<Model>(m));
				item->setIcon(0,m->icon());
				item->setText(1,m->name());
				item->setTextAlignment(1, Qt::AlignLeft | Qt::AlignTop);
			}
			refreshing_ = false;
			ui.ChooseComponentsTree->resizeColumnToContents(0);
			ui.ChooseComponentsTree->resizeColumnToContents(1);
			updateComponentControls();
			// No selection by default, so disable Next button
			button(QWizard::NextButton)->setEnabled(false);
			break;
		// Step 5 / 5 - Select component populations and partition assignments
		case (5):
			refreshing_ = true;
			ui.EditComponentsTree->clear();
			ui.EditComponentsTree->setColumnCount(2);
			componentModelItems_.clear();
			selectItem = NULL;
			foreach (QTreeWidgetItem* twi, ui.ChooseComponentsTree->selectedItems())
			{
				m = VariantPointer<Model>(twi->data(0, Qt::UserRole));
				if (m == NULL)
				{
					printf("Error: Found a NULL model reference when populating EditComponentsTree.\n");
					continue;
				}
				// Force policy if necessary
				if (targetType_ == DisorderWizard::GenerateTarget) m->setComponentInsertionPolicy(Model::NumberAndDensityPolicy);
				item = new QTreeWidgetItem(ui.EditComponentsTree);
				item->setData(0, Qt::UserRole, VariantPointer<Model>(m));
				componentModelItems_.add(item, m);
				setComponentData(m);
				if (selectItem == NULL) selectItem = item;
			}

			// Flag all components not required as having no insertion policy
			for (m = parent_.aten().models(); m != NULL; m = m->next) if (!componentModelItems_.containsData(m)) m->setComponentInsertionPolicy(Model::NoPolicy);
			ui.EditComponentsTree->setCurrentItem(selectItem);
			ui.EditComponentsTree->resizeColumnToContents(0);
			ui.EditComponentsTree->resizeColumnToContents(1);

			// Refresh items in partition combo box
			ui.ComponentTargetPartitionCombo->clear();
			for (int n = 0; n < partitioningScheme_->nPartitions(); ++n)
			{
				PartitionData* pd = partitioningScheme_->partition(n);
				QString text = QString::number(n+1) + " " + pd->name();
				ui.ComponentTargetPartitionCombo->addItem(text);
			}
			refreshing_ = false;
			updateComponentControls();
			break;
	}
}

void DisorderWizard::rejected()
{
	// If a new model was created, remove it here
	if (newModel_ != NULL)
	{
		parent_.aten().removeModel(newModel_);
		newModel_ = NULL;
	}
	existingModel_ = NULL;
	parent_.updateWidgets(AtenWindow::AllTargets);
}

void DisorderWizard::accepted()
{
	// Ready to run disordered builder!
	bool success;
	if (targetType_ == DisorderWizard::ExistingTarget) success = mc.disorder(parent_.aten().models(), existingModel_, partitioningScheme_, true);
	else if (targetType_ == DisorderWizard::NewTarget) success = mc.disorder(parent_.aten().models(), newModel_, partitioningScheme_, true);
	else success = mc.disorder(parent_.aten().models(), newModel_, partitioningScheme_, false);

	// Clean up
	newModel_ = NULL;
	existingModel_ = NULL;
	parent_.updateWidgets(AtenWindow::AllTargets);
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

void DisorderWizard::on_ExistingModelTree_currentItemChanged(QTreeWidgetItem* current, QTreeWidgetItem* previous)
{
	if (current == NULL) return;
	existingModel_ = VariantPointer<Model>(current->data(0, Qt::UserRole));
	if (existingModel_ == NULL) return;
	existingModel_ = existingModel_->renderSourceModel();
	parent_.aten().setCurrentModel(existingModel_);
	parent_.updateWidgets(AtenWindow::AllTargets);
}

void DisorderWizard::on_ExistingModelTree_itemSelectionChanged()
{
	// Get number of selected items in tree
	int nselected = ui.ExistingModelTree->selectedItems().size();
	// Get button pointer
	QAbstractButton* nextButton = button(QWizard::NextButton);
	nextButton->setEnabled(nselected != 0);
}

// Grab values from defined cell controls
void DisorderWizard::setCellAbsolute(double value)
{
	if (newModel_ != NULL)
	{
		newModel_->cell().setLengths(Vec3<double>(ui.CellLengthASpin->value(), ui.CellLengthBSpin->value(), ui.CellLengthCSpin->value()));
		newModel_->cell().setAngles(Vec3<double>(ui.CellAngleASpin->value(), ui.CellAngleBSpin->value(), ui.CellAngleCSpin->value()));
		parent_.updateWidgets();
	}
	else printf("Internal Error: No newModel_ pointer defined to set UnitCell in.\n");
}

// Grab values from relative cell controls
void DisorderWizard::setCellRelative(double value)
{
	if (newModel_ != NULL)
	{
		newModel_->cell().setLengths(Vec3<double>(ui.CellRelativeASpin->value(), ui.CellRelativeBSpin->value(), ui.CellRelativeCSpin->value()));
		newModel_->cell().setAngles(Vec3<double>(ui.CellRelativeAngleASpin->value(), ui.CellRelativeAngleBSpin->value(), ui.CellRelativeAngleCSpin->value()));
		parent_.updateWidgets();
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
	QAbstractButton* nextButton = button(QWizard::NextButton);
	nextButton->setEnabled(nselected != 0);
}

void DisorderWizard::on_PartitionTree_currentItemChanged(QTreeWidgetItem* current, QTreeWidgetItem* previous)
{
	if (current == NULL) return;
	RefListItem<QTreeWidgetItem, PartitioningScheme*>* ri = partitioningSchemeItems_.contains(current);
	if (ri == NULL) return;
	partitioningScheme_ = ri->data;
	parent_.updateWidgets();
	// Enable/disable options button based on presence of custom dialog widgets...
	ui.PartitionSchemeOptionsButton->setEnabled(partitioningScheme_->hasOptions());
}

void DisorderWizard::on_PartitionSchemeOptionsButton_clicked(bool checked)
{
	// Run custom dialog for scheme
	if (partitioningScheme_ == NULL) return;
	if (!partitioningScheme_->hasOptions()) return;
	if (!partitioningScheme_->showOptions()) return;
	partitioningScheme_->recalculatePartitions();
	RefListItem<QTreeWidgetItem, PartitioningScheme*>* ri = partitioningSchemeItems_.containsData(partitioningScheme_);
	if (ri == NULL) return;
	setPartitionData(ri->item, ri->data);
	partitioningScheme_ = ri->data;
	parent_.updateWidgets();
}

// Step 4 / 5 - Select component models

void DisorderWizard::on_ChooseComponentsTree_itemSelectionChanged()
{
	// Get number of selected items in tree
	int nselected = ui.ChooseComponentsTree->selectedItems().size();
	// Get button pointer
	QAbstractButton* nextButton = button(QWizard::NextButton);
	nextButton->setEnabled(nselected != 0);
}

// Step 5 / 5 - Select component populations and partition assignments

void DisorderWizard::on_EditComponentsTree_currentItemChanged(QTreeWidgetItem* current, QTreeWidgetItem* previous)
{
	RefListItem<QTreeWidgetItem, Model*>* ri = componentModelItems_.contains(current);
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

void DisorderWizard::on_MethodOptionsButton_clicked(bool checked)
{
	DisorderOptions disorderOptions(this);
	disorderOptions.exec();
}

/*
 * Local Variable Functions
 */

// Return current target model
Model* DisorderWizard::targetModel()
{
	if (targetType_ == DisorderWizard::ExistingTarget) return existingModel_;
	else return newModel_;
}

// Return currently-selected partitioning scheme
PartitioningScheme* DisorderWizard::partitioningScheme()
{
	return partitioningScheme_;
}

// Return relevant unit cell
UnitCell& DisorderWizard::cell()
{
	static UnitCell dummyCell;
	if (targetType_ == DisorderWizard::ExistingTarget)
	{
		if (existingModel_ == NULL)
		{
			printf("Internal Error: DisorderWizard was asked for a cell when no existing model had been set.\n");
			return dummyCell;
		}
		else return existingModel_->cell();
	}
	else
	{
		if (newModel_ == NULL)
		{
			printf("Internal Error: DisorderWizard was asked for a cell when no new model had been set.\n");
			return dummyCell;
		}
		else return newModel_->cell();
	}
}
