/*
	*** Disorder Wizard
	*** src/gui/disorderwizard.h
	Copyright T. Youngs 2007-2015

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

#ifndef ATEN_DISORDERWIZARD_H
#define ATEN_DISORDERWIZARD_H

#include "gui/ui_disorderwizard.h"
#include "base/cell.h"
#include "templates/reflist.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class PartitioningScheme;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Disordered builder window
class DisorderWizard : public QWizard
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor
	DisorderWizard(AtenWindow& parent);
	// Main form declaration
	Ui::DisorderWizard ui;
	// Target model type
	enum TargetModelType { ExistingTarget, NewTarget, GenerateTarget };

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	 * Local variables
	 */
	private:
	// Whether the window is currently refreshing controls
	bool refreshing_;
	// Target model type
	DisorderWizard::TargetModelType targetType_;
	// Target model for existing model runs
	Model* existingModel_;
	// Target model for new model runs
	Model* newModel_;
	// Selected partitioning scheme
	PartitioningScheme* partitioningScheme_;
	// RefList of tree items and partitioning schemes
	RefList<QTreeWidgetItem, PartitioningScheme*> partitioningSchemeItems_;
	// Current component (Model*) editing target
	Model* componentTarget_;
	// RefList of tree items and selected model components
	RefList<QTreeWidgetItem, Model*> componentModelItems_;
	
	public:
	// Return current target model
	Model* targetModel();
	// Return currently-selected partitioning scheme
	PartitioningScheme* partitioningScheme();
	// Return relevant unit cell
	UnitCell& cell();


	/*
	 * Window Functions
	 */
	public:
	int run();
	private:
	void setComponentData(Model* data);
	void updateComponentControls();
	void setPartitionData(QTreeWidgetItem* target, PartitioningScheme* data);
	private slots:
	void pageChanged(int id);
	void rejected();
	void accepted();
	// Step 1 / 5 - Select target model type
	void on_TargetExistingRadio_clicked(bool checked);
	void on_TargetNewRadio_clicked(bool checked);
	void on_TargetGenerateRadio_clicked(bool checked);
	// Step 2 / 5 - Select model or define unit cell
	void on_ExistingModelTree_currentItemChanged(QTreeWidgetItem* current, QTreeWidgetItem* previous);
	void on_ExistingModelTree_itemSelectionChanged();
	void setCellAbsolute(double value);
	void setCellRelative(double value);
	// Step 3 / 5 - Select partitioning scheme for cell
	void on_PartitionTree_currentItemChanged(QTreeWidgetItem* current, QTreeWidgetItem* previous);
	void on_PartitionTree_itemSelectionChanged();
	void on_PartitionSchemeOptionsButton_clicked(bool checked);
	// Step 4 / 5 - Select component models
	void on_ChooseComponentsTree_itemSelectionChanged();
	// Step 5 / 5 - Select component populations and partition assignments
	void on_EditComponentsTree_currentItemChanged(QTreeWidgetItem* current, QTreeWidgetItem* previous);
	void on_NumberPolicyRadio_clicked(bool checked);
	void on_DensityPolicyRadio_clicked(bool checked);
	void on_NumberAndDensityPolicyRadio_clicked(bool checked);
	void on_RelativePolicyRadio_clicked(bool checked);
	void on_ComponentPopulationSpin_valueChanged(int value);
	void on_ComponentDensitySpin_valueChanged(double value);
	void on_ComponentAllowRotationsCheck_clicked(bool checked);
	void on_ComponentTargetPartitionCombo_currentIndexChanged(int index);
	void on_MethodOptionsButton_clicked(bool checked);
};

#endif
