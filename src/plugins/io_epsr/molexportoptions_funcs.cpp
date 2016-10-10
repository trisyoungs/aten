/*
	*** EPSR Mol Export Options Functions
	*** src/gui/io_epsr/molexportoptions_funcs.cpp
	Copyright T. Youngs 2016-2016

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

#include "plugins/io_epsr/molexportoptions.h"
#include "plugins/interfaces/fileplugin.h"

// Constructor
EPSRMolExportOptionsDialog::EPSRMolExportOptionsDialog(KVMap& pluginOptions) : QDialog(NULL), pluginOptions_(pluginOptions)
{
	ui.setupUi(this);
}

/*
 * Widget Functions
 */

void EPSRMolExportOptionsDialog::on_CancelButton_clicked(bool checked)
{
	// Don't modify the stored pluginOptions_, just reject() the dialog
	reject(); 
}

void EPSRMolExportOptionsDialog::on_OKButton_clicked(bool checked)
{
	// Set options before we accept() the dialog.
	pluginOptions_.add("eCore", QString::number(ui.ECoreSpin->value()));
	pluginOptions_.add("dCore", QString::number(ui.DCoreSpin->value()));
	pluginOptions_.add("temp", QString::number(ui.TempSpin->value()));
	pluginOptions_.add("vibTemp", QString::number(ui.VibTempSpin->value()));
	pluginOptions_.add("angTemp", QString::number(ui.AngTempSpin->value()));
	pluginOptions_.add("dihTemp", QString::number(ui.DihTempSpin->value()));
	pluginOptions_.add("modelGeometry", ui.UseModelGeometryCheck->isChecked() ? "true" : "false");
	pluginOptions_.add("writeDihedrals", ui.WriteDihedralsCheck->isChecked() ? "true" : "false");

	accept();
}

/*
 * Show Function
 */

// Update and show dialog, setting controls from pluginOptions
int EPSRMolExportOptionsDialog::updateAndExecute()
{
	printf("KJHKJHKJ|N\n");
	pluginOptions_.dump();
	// Set controls to reflect current pluginOptions_
	ui.ECoreSpin->setValue(pluginOptions_.value("eCore").toDouble());
	ui.DCoreSpin->setValue(pluginOptions_.value("dCore").toDouble());
	ui.TempSpin->setValue(pluginOptions_.value("temp").toDouble());
	ui.VibTempSpin->setValue(pluginOptions_.value("vibTemp").toDouble());
	ui.AngTempSpin->setValue(pluginOptions_.value("angTemp").toDouble());
	ui.DihTempSpin->setValue(pluginOptions_.value("dihTemp").toDouble());
	ui.UseModelGeometryCheck->setChecked(FilePluginInterface::toBool(pluginOptions_.value("modelGeometry")));
	ui.WriteDihedralsCheck->setChecked(FilePluginInterface::toBool(pluginOptions_.value("writeDihedrals")));

	// Execute the dialog - option setting will be handled in the OK button slot
	return exec();
}
