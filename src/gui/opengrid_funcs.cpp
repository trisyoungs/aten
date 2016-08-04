/*
	*** Open Grid Dialog
	*** src/gui/opengrid_funcs.cpp
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

#include "gui/opengrid.h"
#include "plugins/plugintypes.h"
#include <QMessageBox>

// Constructor
AtenOpenGrid::AtenOpenGrid(QWidget* parent, QDir startingDirectory, const RefList<FilePluginInterface,int>& filePlugins) : QDialog(parent), AtenFileDialog(filePlugins)
{
	ui.setupUi(this);

	setFileSelectorWidget(ui.FileSelector, startingDirectory, FileSelectorWidget::OpenMultipleMode);

	// Link up some slots
	connect(ui.FileSelector, SIGNAL(selectionMade(bool)), this, SLOT(on_OpenButton_clicked(bool)));
	connect(ui.FileSelector, SIGNAL(selectionValid(bool)), ui.OpenButton, SLOT(setEnabled(bool)));
	connect(ui.FileSelector, SIGNAL(pluginOptionsAvailable(bool)), ui.PluginOptionsButton, SLOT(setEnabled(bool)));
	connect(ui.FileSelector, SIGNAL(pluginSelectionChanged()), this, SLOT(updateStandardOptionsFromPlugin()));
}

/*
 * Widget Functions
 */

void AtenOpenGrid::on_PluginOptionsButton_clicked(bool checked)
{
	// Get current interface selected in FileSelector
	FilePluginInterface* plugin = ui.FileSelector->selectedPlugin();
	if (!plugin) return;
	if (plugin->hasImportOptions()) plugin->showImportOptionsDialog();
}

void AtenOpenGrid::on_OpenButton_clicked(bool checked)
{
	// Get current filename selection and check that the files exist
	QStringList selectedFiles = ui.FileSelector->selectedFiles();
	if (selectedFiles.count() == 0) return;

	QStringList missingFiles;
	for (int n=0; n<selectedFiles.count(); ++n)
	{
		if (! QFileInfo::exists(selectedFiles.at(n))) missingFiles << selectedFiles.at(n);
	}
	if (missingFiles.count() > 0)
	{
		QString message = "The following files do not exist:\n";
		for (int n=0; n<missingFiles.count(); ++n) message += "'" + missingFiles.at(n) + "'\n";
		if (QMessageBox::critical(this, "Open Grid(s)", message, QMessageBox::Retry | QMessageBox::Cancel) == QMessageBox::Cancel) return;
		reject();
	}

	accept();
}

void AtenOpenGrid::on_CancelButton_clicked(bool checked)
{
	reject();
}

// Execute dialog
bool AtenOpenGrid::execute(int currentPluginsLogPoint, QString currentFilename, FilePluginInterface* currentPlugin)
{
	// Make sure the file selector is up to date
	updateFileSelector(currentPluginsLogPoint);

	return exec();
}

// Return standard import options from dialog
FilePluginStandardImportOptions AtenOpenGrid::standardImportOptions()
{
	FilePluginStandardImportOptions options;

	options.setCoordinatesInBohr(ui.BohrCheck->isChecked());
	options.setZMappingType( (ElementMap::ZMapType) ui.ZMappingCombo->currentIndex());
	
	return options;
}

// Return standard export options from dialog
FilePluginStandardExportOptions AtenOpenGrid::standardExportOptions()
{
	FilePluginStandardExportOptions options;

	return options;
}


/*
 * Signals / Slots
 */

// Update standard options from plugin's local options
void AtenOpenGrid::updateStandardOptionsFromPlugin()
{
	// Get current plugin
	FilePluginInterface* plugin = ui.FileSelector->selectedPlugin();
	if (!plugin) return;

	ui.BohrCheck->setChecked(plugin->standardOptions().coordinatesInBohr());
	ui.ZMappingCombo->setCurrentIndex(plugin->standardOptions().zMappingType());
}
