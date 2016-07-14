/*
	*** Save Model Dialog
	*** src/gui/savemodel_funcs.cpp
	Copyright T. Youngs 2007-2016

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

#include "gui/savemodel.h"
#include "plugins/plugintypes.h"
#include <QMessageBox>

// Constructor
AtenSaveModel::AtenSaveModel(QWidget* parent, QDir startingDirectory, const RefList<FilePluginInterface,int>& filePlugins) : QDialog(parent), AtenFileDialog(filePlugins)
{
	ui.setupUi(this);

	setFileSelectorWidget(ui.FileSelector, startingDirectory, FileSelectorWidget::SaveSingleMode);

	// Link up some slots
	connect(ui.FileSelector, SIGNAL(selectionMade(bool)), this, SLOT(on_SaveButton_clicked(bool)));
	connect(ui.FileSelector, SIGNAL(selectionValid(bool)), ui.SaveButton, SLOT(setEnabled(bool)));
	connect(ui.FileSelector, SIGNAL(pluginOptionsAvailable(bool)), ui.PluginOptionsButton, SLOT(setEnabled(bool)));
	connect(ui.FileSelector, SIGNAL(pluginSelectionChanged()), this, SLOT(updateStandardOptionsFromPlugin()));
}

/*
 * Widget Functions
 */

void AtenSaveModel::on_PluginOptionsButton_clicked(bool checked)
{
	// Get current interface selected in FileSelector
	FilePluginInterface* plugin = ui.FileSelector->selectedPlugin();
	if (!plugin) return;
	if (plugin->hasExportOptions()) plugin->showExportOptionsDialog();
}

void AtenSaveModel::on_SaveButton_clicked(bool checked)
{
	// Get current filename selection and check that the files exist
	QStringList selectedFiles = ui.FileSelector->selectedFiles();
	if (selectedFiles.count() == 0) return;

	if (selectedFiles.count() != 1)
	{
		printf("Error: FileSelector did not return exactly one filename.\n");
		return;
	}

	if (QFileInfo::exists(selectedFiles.at(0)))
	{
		if (QMessageBox::question(this, "File Exists", "The selected filename already exists. Overwrite it?", QMessageBox::Yes | QMessageBox::No, QMessageBox::No) == QMessageBox::No) return;
	}

	accept();
}

void AtenSaveModel::on_CancelButton_clicked(bool checked)
{
	reject();
}

// Execute dialog
bool AtenSaveModel::execute(int currentPluginsLogPoint, QString currentFileName, FilePluginInterface* plugin)
{
	// Make sure the file selector is up to date
	updateFileSelector(currentPluginsLogPoint, currentFileName, plugin);

	return exec();
}

// Return standard import options from dialog
FilePluginStandardImportOptions AtenSaveModel::standardImportOptions()
{
	FilePluginStandardImportOptions options;

	return options;
}

// Return standard export options from dialog
FilePluginStandardExportOptions AtenSaveModel::standardExportOptions()
{
	FilePluginStandardExportOptions options;

	return options;
}

// Return whether extension by type is selected
bool AtenSaveModel::extensionDeterminesType()
{
	return ui.ExtensionDeterminesTypeCheck->isChecked();
}

/*
 * Signals / Slots
 */

// Update standard options from plugin's local options
void AtenSaveModel::updateStandardOptionsFromPlugin()
{
	// Get current plugin
	FilePluginInterface* plugin = ui.FileSelector->selectedPlugin();
	if (!plugin) return;
}
