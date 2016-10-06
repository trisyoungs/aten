/*
	*** File Dialog Common Functions
	*** src/gui/filedialog.h
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

#ifndef ATEN_FILEDIALOG_H
#define ATEN_FILEDIALOG_H

#include "plugins/interfaces/fileplugin.h"
#include "gui/fileselectorwidget.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
/* none */

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
/* none */

// File Dialog Common Functions
class AtenFileDialog
{

	public:
	// Constructor
	AtenFileDialog(const RefList<FilePluginInterface,KVMap>& filePlugins);


	/*
	 * Data
	 */
	protected:
	// Reference to plugin list to use for this file dialog
	const RefList<FilePluginInterface,KVMap>& filePlugins_;
	// PluginStore logpoint at which plugins were added to the file selector
	int pluginsLogPoint_;
	// Pointer to associated FileSelector widget
	FileSelectorWidget* fileSelectorWidget_;

	public:
	// Set pointer to associated FileSelector widget
	void setFileSelectorWidget(FileSelectorWidget* widget, QDir startingDirectory, FileSelectorWidget::SelectionMode mode);
	// Make sure file selector is up to date
	void updateFileSelector(int currentPluginsLogPoint, QString currentFilename = QString(), const FilePluginInterface* currentPlugin = NULL);
	// Execute dialog
	virtual bool execute(int currentPluginsLogPoint, QString currentFilename = QString(), const FilePluginInterface* currentPlugin = NULL) = 0;
	// Return selected filename(s)
	QStringList selectedFilenames();
	// Return selected file plugin
	const FilePluginInterface* selectedPlugin();
	// Return map of standard import options from dialog
	virtual FilePluginStandardImportOptions standardImportOptions() = 0;
	// Return map of standard export options from dialog
	virtual FilePluginStandardExportOptions standardExportOptions() = 0;


	/*
	 * Signals / Slots
	 */
	private slots:
	// Update standard options from plugin's local options
	virtual void updateStandardOptionsFromPlugin() = 0;
};

ATEN_END_NAMESPACE

#endif
