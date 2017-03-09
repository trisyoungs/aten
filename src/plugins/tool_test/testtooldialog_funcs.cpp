/*
	*** Test Tool Dialog Functions
	*** src/gui/tool_test/testtooldialog_funcs.cpp
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

#include "plugins/tool_test/testtooldialog.h"

// Constructor
TestToolDialog::TestToolDialog(ToolPluginInterface& targetInterface, KVMap& pluginOptions) : QDialog(NULL), targetInterface_(targetInterface), pluginOptions_(pluginOptions)
{
	ui.setupUi(this);

	// Initialise the icon resource
	Q_INIT_RESOURCE(testtool_icons);
}

/*
 * Widget Functions
 */

void TestToolDialog::on_CloseButton_clicked(bool checked)
{
	// Close the dialog, storing UI options before we close
	setPluginOptions();
	accept(); 
}

void TestToolDialog::on_RunButton_clicked(bool checked)
{
	// Set options before we call the run method
	setPluginOptions();

	targetInterface_.runTool();
}

// Apply plugin options to UI controls
void TestToolDialog::applyPluginOptions()
{
	ui.RandomiseRadio->setChecked(pluginOptions_.value("randomise") == "true");
	ui.ConvertToRadio->setChecked(pluginOptions_.value("randomise") == "false");
	ui.ElementEdit->setText(pluginOptions_.value("element"));
	ui.ApplyToAllCheck->setChecked(pluginOptions_.value("applyToAll") == "true");
}

// Set plugin options from UI controls
void TestToolDialog::setPluginOptions()
{
	pluginOptions_.add("randomise", ui.RandomiseRadio->isChecked() ? "true" : "false");
	pluginOptions_.add("element", ui.ElementEdit->text());
	pluginOptions_.add("applyToAll", ui.ApplyToAllCheck->isChecked() ? "true" : "false");
}
