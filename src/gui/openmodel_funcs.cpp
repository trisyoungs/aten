/*
	*** Open Model Dialog
	*** src/gui/openmodel_funcs.cpp
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

#include "gui/openmodel.h"
#include "plugins/plugintypes.h"
#include <QMessageBox>

// Constructor
AtenOpenModel::AtenOpenModel(QWidget* parent, QDir startingDirectory, const RefList<FilePluginInterface,int>& filePlugins) : QDialog(parent), filePlugins_(filePlugins)
{
	ui.setupUi(this);

	pluginsLogPoint_ = -1;

	// Set the mode of the FileSelectorWidget
	ui.FileSelector->setMode(FileSelectorWidget::OpenMultipleMode, startingDirectory);

	// Link up some slots
	connect(ui.FileSelector, SIGNAL(selectionMade(bool)), this, SLOT(on_OpenButton_clicked(bool)));
	connect(ui.FileSelector, SIGNAL(selectionValid(bool)), ui.OpenButton, SLOT(setEnabled(bool)));
}

/*
 * Widget Functions
 */

void AtenOpenModel::on_OpenButton_clicked(bool checked)
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
		if (QMessageBox::critical(this, "Open Model(s)", message, QMessageBox::Retry | QMessageBox::Cancel) == QMessageBox::Cancel) return;
		reject();
	}

	accept();
}

void AtenOpenModel::on_CancelButton_clicked(bool checked)
{
	reject();
}

// Execute dialog
bool AtenOpenModel::execute(int currentPluginsLogPoint)
{
	// Make sure the file selector is up to date
	if (currentPluginsLogPoint != pluginsLogPoint_)
	{
		ui.FileSelector->refreshPlugins(filePlugins_);
		pluginsLogPoint_ = currentPluginsLogPoint;
	}
	ui.FileSelector->clearSelectedFilenames();
	ui.FileSelector->updateWidgets();

	return exec();
}

// Return selected filename(s)
QStringList AtenOpenModel::selectedFilenames()
{
	return ui.FileSelector->selectedFiles();
}

// Return selected file plugin
FilePluginInterface* AtenOpenModel::selectedPlugin()
{
	return ui.FileSelector->selectedPlugin();
}

// Return map of standard options from dialog
KVMap AtenOpenModel::standardOptions()
{
	KVMap options;

	options.add("preventRebonding", ui.PreventRebondingCheck->isChecked() ? "true" : "false");
	options.add("preventFolding", ui.PreventFoldingCheck->isChecked() ? "true" : "false");
	options.add("preventPacking", ui.PreventPackingCheck->isChecked() ? "true" : "false");
	options.add("coordinatesInBohr", ui.BohrCheck->isChecked() ? "true" : "false");
	options.add("keepTypes", ui.KeepTypesCheck->isChecked() ? "true" : "false");
	options.add("keepNames", ui.KeepNamesCheck->isChecked() ? "true" : "false");

	return options;
}
