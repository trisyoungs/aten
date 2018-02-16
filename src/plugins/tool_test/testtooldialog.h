/*
	*** Test Tool Dialog
	*** src/plugins/tool_test/testtooldialog.h
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

#ifndef ATEN_TESTTOOLDIALOG_H
#define ATEN_TESTTOOLDIALOG_H

#include "base/kvmap.h"
#include "plugins/interfaces/toolplugin.h"
#include "plugins/tool_test/ui_testtooldialog.h"

ATEN_USING_NAMESPACE

// Forward Declarations (Aten)
/* None */

// Test Tool Dialog
class TestToolDialog : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor
	TestToolDialog(ToolPluginInterface& targetInterface, KVMap& pluginOptions);

	private:
	// Main form declaration
	Ui::TestToolDialog ui;
	// Reference to the source plugin calling the dialog
	ToolPluginInterface& targetInterface_;
	// Reference to KVMap of plugin options stored in plugin
	KVMap& pluginOptions_;


	/*
	 * Widget Functions
	 */
	private slots:
	// Cancel / OK buttons
	void on_RunButton_clicked(bool checked);
	void on_CloseButton_clicked(bool checked);

	private:
	// Set plugin options from UI controls
	void setPluginOptions();

	public:
	// Apply plugin options to UI controls
	void applyPluginOptions();
};

#endif
