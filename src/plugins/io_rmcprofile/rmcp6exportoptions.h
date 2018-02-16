/*
	*** RMCProfile V6 Export Options Dialog
	*** src/plugins/io_rmcprofile/rmcp6exportoptions.h
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

#ifndef ATEN_RMCPROFILE6EXPORTOPTIONS_H
#define ATEN_RMCPROFILE6EXPORTOPTIONS_H

#include "base/kvmap.h"
#include "plugins/io_rmcprofile/ui_rmcp6exportoptions.h"

ATEN_USING_NAMESPACE

// Forward Declarations (Aten)
/* none */

// XYZ Export Options Dialog
class RMCProfile6ExportOptionsDialog : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor
	RMCProfile6ExportOptionsDialog(KVMap& pluginOptions);

	private:
	// Main form declaration
	Ui::RMCProfile6ExportOptionsDialog ui;
	// Reference to KVMap of plugin options stored in plugin
	KVMap& pluginOptions_;


	/*
	 * Widget Functions
	 */
	private slots:
	// Cancel / OK buttons
	void on_CancelButton_clicked(bool checked);
	void on_OKButton_clicked(bool checked);


	/*
	 * Show Function
	 */
	public:
	// Update and show dialog (setting controls from pluginOptions_ if necessary)
	int updateAndExecute();
};

#endif
