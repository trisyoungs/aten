/*
	*** Disorder Wizard
	*** src/gui/disorderwizard.h
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

#ifndef ATEN_DISORDERWIZARD_H
#define ATEN_DISORDERWIZARD_H

#include "gui/ui_disorderwizard.h"
#include "templates/reflist.h"

// Disordered builder window
class DisorderWizard : public QWizard
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Enumerations
	*/
	public:
	enum TargetModelType { ExistingTarget, NewTarget, GenerateTarget };

	/*
	// Window Functions
	*/
	public:
	int run();
	private:
	void updateComponentData();
	private slots:
	void pageChanged(int id);
	private slots:
	// Step 1 / 5 - Select target model type
	void on_TargetExistingRadio_clicked(bool checked);
	void on_TargetNewRadio_clicked(bool checked);
	void on_TargetGenerateRadio_clicked(bool checked);
	// Step 2 / 5 - Select model or define unit cell
	// Step 3 / 5 - Select partitioning scheme for cell
	void on_PartitionSchemeOptionsButton_clicked(bool checked);
	// Step 4 / 5 - Select component models
	// Step 5 / 5 - Select component populations and partition assignments
	void on_ComponentPopulationSpin_valueChanged(int value);
	void on_ComponentBulkCheck_clicked(bool checked);
	void on_ComponentDensitySpin_valueChanged(double value);
	void on_ComponentFreeDensityCheck_clicked(bool checked);
	void on_ComponentAllowRotationsCheck_clicked(bool checked);
	void on_ComponentAllowTranslationsCheck_clicked(bool checked);
	void on_ComponentTargetPartitionCombo_currentIndexChanged(int index);
	
	/*
	// Local variables
	*/
	private:
	// Target model type
	DisorderWizard::TargetModelType targetType_;
		
	/*
	// Dialog
	*/
	public:
	// Constructor
	DisorderWizard(QWidget *parent = 0);
	// Main form declaration
	Ui::DisorderWizard ui;
};

#endif
