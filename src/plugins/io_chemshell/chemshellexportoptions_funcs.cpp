/*
	*** ChemShell Export Options Functions
	*** src/gui/io_chemshell/chemshellexportoptions_funcs.cpp
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

#include "plugins/io_chemshell/chemshellexportoptions.h"

// Constructor
ChemShellExportOptionsDialog::ChemShellExportOptionsDialog(KVMap& pluginOptions) : QDialog(NULL), pluginOptions_(pluginOptions)
{
	ui.setupUi(this);
}

/*
 * Widget Functions
 */

void ChemShellExportOptionsDialog::on_CancelButton_clicked(bool checked)
{
	// Don't modify the stored pluginOptions_, just reject() the dialog
	reject(); 
}

void ChemShellExportOptionsDialog::on_OKButton_clicked(bool checked)
{
	// Set options before we accept() the dialog.
	pluginOptions_.add("useTypeNames", ui.UseTypeNamesCheck->isChecked() ? "true" : "false");
	
	accept();
}

/*
 * Show Function
 */

// Update and show dialog, setting controls from pluginOptions
int ChemShellExportOptionsDialog::updateAndExecute()
{
	// Set controls to reflect current pluginOptions_
	ui.UseTypeNamesCheck->setChecked(pluginOptions_.value("useTypeNames") == "true");

	// Execute the dialog - option setting will be handled in the OK button slot
	return exec();
}
