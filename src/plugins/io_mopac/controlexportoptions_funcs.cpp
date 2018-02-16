/*
	*** MOPAC Control File Export Options Functions
	*** src/gui/io_mopac/controlexportoptions_funcs.cpp
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

#include "plugins/io_mopac/controlexportoptions.h"

// Constructor
MOPACControlExportOptionsDialog::MOPACControlExportOptionsDialog(KVMap& pluginOptions) : QDialog(NULL), pluginOptions_(pluginOptions)
{
	ui.setupUi(this);
}

/*
 * Widget Functions
 */

void MOPACControlExportOptionsDialog::on_CancelButton_clicked(bool checked)
{
	// Don't modify the stored pluginOptions_, just reject() the dialog
	reject(); 
}

void MOPACControlExportOptionsDialog::on_OKButton_clicked(bool checked)
{
	pluginOptions_.add("jobtype", ui.JobTypeCombo->currentText().section(QChar(' '), 0, 0));
	pluginOptions_.add("hamiltonian", ui.HamiltonianCombo->currentText());
	pluginOptions_.add("state", ui.StateCombo->currentText());
	pluginOptions_.add("scftype", ui.UHFRadio->isChecked() ? "UHF" : "RHF");
	pluginOptions_.add("charge", QString::number(ui.ChargeSpin->value()));
	pluginOptions_.add("precise", ui.PreciseCheck->isChecked() ? "true" : "false");
	pluginOptions_.add("bfgs", ui.BFGSCheck->isChecked() ? "true" : "false");
	pluginOptions_.add("campking", ui.CampKingCheck->isChecked() ? "true" : "false");
	pluginOptions_.add("mozyme", ui.MOZYMECheck->isChecked() ? "true" : "false");
	pluginOptions_.add("extra", ui.ExtraKeywordsEdit->text());
	
	accept();
}

/*
 * Show Function
 */

// Update and show dialog, setting controls from pluginOptions
int MOPACControlExportOptionsDialog::updateAndExecute()
{
	// Set controls to reflect current pluginOptions_
	// Hamiltonian - search through combo box items
	QString jobtype = pluginOptions_.value("jobtype").toUpper();
	for (int n=0; n<ui.JobTypeCombo->count(); ++n) if (jobtype == ui.HamiltonianCombo->itemText(n).section(QChar(' '), 0, 0)) ui.JobTypeCombo->setCurrentIndex(n);
	// Hamiltonian - search through combo box items
	QString hamiltonian = pluginOptions_.value("hamiltonian").toUpper();
	for (int n=0; n<ui.HamiltonianCombo->count(); ++n) if (hamiltonian == ui.HamiltonianCombo->itemText(n)) ui.HamiltonianCombo->setCurrentIndex(n);
	// Hamiltonian - search through combo box items
	QString state = pluginOptions_.value("state").toUpper();
	for (int n=0; n<ui.StateCombo->count(); ++n) if (state == ui.StateCombo->itemText(n)) ui.StateCombo->setCurrentIndex(n);

	if (pluginOptions_.value("scftype") == "UHF") ui.UHFRadio->setChecked(true);
	else ui.RHFRadio->setChecked(true);
	ui.ChargeSpin->setValue(pluginOptions_.value("charge").toInt());
	ui.PreciseCheck->setChecked(pluginOptions_.value("precise") == "true");
	ui.BFGSCheck->setChecked(pluginOptions_.value("bfgs") == "true");
	ui.CampKingCheck->setChecked(pluginOptions_.value("campking") == "true");
	ui.MOZYMECheck->setChecked(pluginOptions_.value("mozyme") == "true");
	ui.ExtraKeywordsEdit->setText(pluginOptions_.value("extra"));

	// Execute the dialog - option setting will be handled in the OK button slot
	return exec();
}
