/*
	*** Open Dialog Common Functions
	*** src/gui/opendialog.h
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

#ifndef ATEN_OPENDIALOG_H
#define ATEN_OPENDIALOG_H

#include "plugins/interfaces/fileplugin.h"
#include "gui/fileselectorwidget.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
/* none */

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
/* none */

// Open Dialog Common Functions
class AtenOpenDialog
{

	public:
	// Constructor
	AtenOpenDialog(const RefList<FilePluginInterface,int>& filePlugins);


	/*
	 * Data
	 */
	protected:
	// Reference to plugin list to use for this file dialog
	const RefList<FilePluginInterface,int>& filePlugins_;
	// PluginStore logpoint at which plugins were added to the file selector
	int pluginsLogPoint_;
	// Pointer to associated FileSelector widget
	FileSelectorWidget* fileSelectorWidget_;

	public:
	// Set pointer to associated FileSelector widget
	void setFileSelectorWidget(FileSelectorWidget* widget, QDir startingDirectory, FileSelectorWidget::SelectionMode mode);
	// Make sure file selector is up to date
	void updateFileSelector(int currentPluginsLogPoint);
	// Execute dialog
	virtual bool execute(int currentPluginsLogPoint) = 0;
	// Return selected filename(s)
	QStringList selectedFilenames();
	// Return selected file plugin
	FilePluginInterface* selectedPlugin();
	// Return map of standard options from dialog
	virtual FilePluginStandardImportOptions standardOptions() = 0;
};

ATEN_END_NAMESPACE

#endif
