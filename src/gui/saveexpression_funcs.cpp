/*
	*** Save Expression Dialog
	*** src/gui/saveexpression_funcs.cpp
	Copyright T. Youngs 2016-2017

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

#include "gui/saveexpression.h"
#include "plugins/plugintypes.h"
#include <QMessageBox>

// Constructor
AtenSaveExpression::AtenSaveExpression(QWidget* parent, QDir startingDirectory, const RefList<FilePluginInterface,KVMap>& filePlugins) : QDialog(parent), AtenFileDialog(filePlugins)
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

void AtenSaveExpression::on_PluginOptionsButton_clicked(bool checked)
{
	// Get current interface selected in FileSelector
	const FilePluginInterface* plugin = ui.FileSelector->selectedPlugin();
	if (!plugin) return;

	KVMap& pluginOptions = ui.FileSelector->selectedPluginOptions();
	if (plugin->hasExportOptions()) plugin->showExportOptionsDialog(pluginOptions);
}

void AtenSaveExpression::on_SaveButton_clicked(bool checked)
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

void AtenSaveExpression::on_CancelButton_clicked(bool checked)
{
	reject();
}

// Execute dialog
bool AtenSaveExpression::execute(int currentPluginsLogPoint, QString currentFileName, const FilePluginInterface* plugin)
{
	// Make sure the file selector is up to date
	updateFileSelector(currentPluginsLogPoint, currentFileName, plugin);

	return exec();
}

// Return standard import options from dialog
FilePluginStandardImportOptions AtenSaveExpression::standardImportOptions()
{
	FilePluginStandardImportOptions options;

	return options;
}

// Return standard export options from dialog
FilePluginStandardExportOptions AtenSaveExpression::standardExportOptions()
{
	FilePluginStandardExportOptions options;

	return options;
}

// Return whether extension by type is selected
bool AtenSaveExpression::extensionDeterminesType()
{
	return ui.ExtensionDeterminesTypeCheck->isChecked();
}

/*
 * Signals / Slots
 */

// Update standard options from plugin's local options
void AtenSaveExpression::updateStandardOptionsFromPlugin()
{
	// Get current plugin
	const FilePluginInterface* plugin = ui.FileSelector->selectedPlugin();
	if (!plugin) return;
}
