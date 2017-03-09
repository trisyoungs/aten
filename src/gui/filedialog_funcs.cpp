/*
	*** File Dialog Common Functions
	*** src/gui/filedialog_funcs.cpp
	Copyright T. Youngs 2007-2017

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

#include "gui/filedialog.h"
#include "plugins/plugintypes.h"

// Constructor
AtenFileDialog::AtenFileDialog(const RefList<FilePluginInterface,KVMap>& filePlugins) : filePlugins_(filePlugins)
{
	pluginsLogPoint_ = -1;
	fileSelectorWidget_ = NULL;
}

// Set pointer to associated FileSelector widget
void AtenFileDialog::setFileSelectorWidget(FileSelectorWidget* widget, QDir startingDirectory, FileSelectorWidget::SelectionMode mode)
{
	fileSelectorWidget_ = widget;

	// Set the mode of the FileSelectorWidget
	fileSelectorWidget_->setMode(mode, startingDirectory);
}

// Make sure file selector is up to date
void AtenFileDialog::updateFileSelector(int currentPluginsLogPoint, QString currentFilename, const FilePluginInterface* currentPlugin)
{
	// Make sure the file selector is up to date
	if (currentPluginsLogPoint != pluginsLogPoint_)
	{
		fileSelectorWidget_->refreshPlugins(filePlugins_);
		pluginsLogPoint_ = currentPluginsLogPoint;
	}

	// Set / clear filename
	if (currentFilename.isEmpty()) fileSelectorWidget_->clearSelectedFilenames();
	else fileSelectorWidget_->setSelectedFilename(currentFilename);

	// Set plugin (if specified)
	if (currentPlugin) fileSelectorWidget_->setSelectedPlugin(currentPlugin);

	fileSelectorWidget_->updateWidgets();
}

// Return selected filename(s)
QStringList AtenFileDialog::selectedFilenames()
{
	return fileSelectorWidget_->selectedFiles();
}

// Return selected file plugin
const FilePluginInterface* AtenFileDialog::selectedPlugin()
{
	return fileSelectorWidget_->selectedPlugin();
}

// Return selected file plugin options map
KVMap& AtenFileDialog::selectedPluginOptions()
{
	return fileSelectorWidget_->selectedPluginOptions();
}
