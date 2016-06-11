/*
	*** Open Dialog Common Functions
	*** src/gui/opendialog_funcs.cpp
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

#include "gui/opendialog.h"
#include "plugins/plugintypes.h"

// Constructor
AtenOpenDialog::AtenOpenDialog(const RefList<FilePluginInterface,int>& filePlugins) : filePlugins_(filePlugins)
{
	pluginsLogPoint_ = -1;
}

// Set pointer to associated FileSelector widget
void AtenOpenDialog::setFileSelectorWidget(FileSelectorWidget* widget, QDir startingDirectory, FileSelectorWidget::SelectionMode mode)
{
	fileSelectorWidget_ = widget;

	// Set the mode of the FileSelectorWidget
	fileSelectorWidget_->setMode(mode, startingDirectory);
}

// Make sure file selector is up to date
void AtenOpenDialog::updateFileSelector(int currentPluginsLogPoint)
{
	// Make sure the file selector is up to date
	if (currentPluginsLogPoint != pluginsLogPoint_)
	{
		fileSelectorWidget_->refreshPlugins(filePlugins_);
		pluginsLogPoint_ = currentPluginsLogPoint;
	}
	fileSelectorWidget_->clearSelectedFilenames();
	fileSelectorWidget_->updateWidgets();
}

// Return selected filename(s)
QStringList AtenOpenDialog::selectedFilenames()
{
	return fileSelectorWidget_->selectedFiles();
}

// Return selected file plugin
FilePluginInterface* AtenOpenDialog::selectedPlugin()
{
	return fileSelectorWidget_->selectedPlugin();
}
