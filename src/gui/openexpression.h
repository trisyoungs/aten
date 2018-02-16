/*
	*** Open Expression Dialog
	*** src/gui/openexpression.h
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

#ifndef ATEN_OPENEXPRESSIONDIALOG_H
#define ATEN_OPENEXPRESSIONDIALOG_H

#include "gui/ui_openexpression.h"
#include "gui/filedialog.h"
#include "plugins/interfaces/fileplugin.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
/* none */

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
/* none */

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Open Expression Dialog
class AtenOpenExpression : public QDialog, public AtenFileDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor
	AtenOpenExpression(QWidget* parent, QDir startingDirectory, const RefList<FilePluginInterface,KVMap>& filePlugins);
	// Main form declaration
	Ui::OpenExpressionDialog ui;


	/*
	 * Widget Functions
	 */
	private slots:
	void on_PluginOptionsButton_clicked(bool checked);
	void on_OpenButton_clicked(bool checked);
	void on_CancelButton_clicked(bool checked);

	public:
	// Execute dialog
	bool execute(int currentPluginsLogPoint, QString currentFilename = QString(), const FilePluginInterface* currentPlugin = NULL);
	// Return standard import options from dialog
	FilePluginStandardImportOptions standardImportOptions();
	// Return standard export options from dialog
	FilePluginStandardExportOptions standardExportOptions();


	/*
	 * Signals / Slots
	 */
	private slots:
	// Update standard options from plugin's local options
	void updateStandardOptionsFromPlugin();
};

#endif
