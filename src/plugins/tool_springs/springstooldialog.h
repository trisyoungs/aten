/*
	*** SPRings Tool Dialog
	*** src/plugins/tool_springs/springstooldialog.h
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

#ifndef ATEN_RINGSTOOLDIALOG_H
#define ATEN_RINGSTOOLDIALOG_H

#include "base/kvmap.h"
#include "plugins/interfaces/toolplugin.h"
#include "plugins/tool_springs/ui_springstooldialog.h"

ATEN_USING_NAMESPACE

// Forward Declarations (Aten)
/* None */

// SPRings Tool Dialog
class SPRingsToolDialog : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor
	SPRingsToolDialog(ToolPluginInterface& targetInterface, KVMap& pluginOptions);
	// Main form declaration
	Ui::SPRingsToolDialog ui;

	private:
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
