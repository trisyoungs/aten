/*
	*** DL_POLY_4  Export Options Functions
	*** src/gui/io_dlp4/dlp4exportoptions_funcs.cpp
	Copyright T. Youngs 2007-2016
  Copyright A.M. Elena 2016-2016

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

#include "plugins/io_dlpoly/dlp4exportoptions.h"

// Constructor
DLP4ExportOptionsDialog::DLP4ExportOptionsDialog(KVMap& pluginOptions) : QDialog(NULL), pluginOptions_(pluginOptions)
{
	ui.setupUi(this);
}

/*
 * Widget Functions
 */

void DLP4ExportOptionsDialog::on_CancelButton_clicked(bool checked)
{
	// Don't modify the stored pluginOptions_, just reject() the dialog
	reject(); 
}

void DLP4ExportOptionsDialog::on_OKButton_clicked(bool checked)
{
	// Set options before we accept() the dialog.
	pluginOptions_.add("shiftCell", ui.ShiftCellCentreCheck->isChecked() ? "true" : "false");
	if (ui.LevCfg0Radio->isChecked()) pluginOptions_.add("levcfg", QString::number(0));
	else if (ui.LevCfg1Radio->isChecked()) pluginOptions_.add("levcfg", QString::number(1));
	else if (ui.LevCfg2Radio->isChecked()) pluginOptions_.add("levcfg", QString::number(2));
	pluginOptions_.add("useTypeNames", ui.UseTypeNamesCheck->isChecked() ? "true" : "false");
	
	accept();
}

/*
 * Show Function
 */

// Update and show dialog, setting controls from pluginOptions
int DLP4ExportOptionsDialog::updateAndExecute()
{
	// Set controls to reflect current pluginOptions_
	ui.ShiftCellCentreCheck->setChecked(pluginOptions_.value("shiftCell") == "true");
	int levcfg = pluginOptions_.value("levcfg").toInt();
	if (levcfg == 0) ui.LevCfg0Radio->setChecked(true);
	else if (levcfg == 1) ui.LevCfg1Radio->setChecked(true);
	else if (levcfg == 2) ui.LevCfg2Radio->setChecked(true);
	ui.ShiftCellCentreCheck->setChecked(pluginOptions_.value("useTypeNames") == "true");

	// Execute the dialog - option setting will be handled in the OK button slot
	return exec();
}
