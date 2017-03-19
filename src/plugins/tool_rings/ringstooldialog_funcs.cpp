/*
	*** Rings Tool Dialog Functions
	*** src/gui/tool_rings/ringstooldialog_funcs.cpp
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

#include "plugins/tool_rings/ringstooldialog.h"

// Constructor
RingsToolDialog::RingsToolDialog(ToolPluginInterface& targetInterface, KVMap& pluginOptions) : QDialog(NULL), targetInterface_(targetInterface), pluginOptions_(pluginOptions)
{
	ui.setupUi(this);

	// Initialise the icon resource
	Q_INIT_RESOURCE(ringstool_icons);
}

/*
 * Widget Functions
 */

void RingsToolDialog::on_CloseButton_clicked(bool checked)
{
	// Close the dialog, storing UI options before we close
	setPluginOptions();
	accept(); 
}

void RingsToolDialog::on_RunButton_clicked(bool checked)
{
	// Set options before we call the run method
	setPluginOptions();

	targetInterface_.runTool();
}

// Apply plugin options to UI controls
void RingsToolDialog::applyPluginOptions()
{
	ui.LinkElementEdit->setText(pluginOptions_.value("linkElement"));
	ui.ElementEdit->setText(pluginOptions_.value("element"));
	ui.LinkElementCheck->setChecked(pluginOptions_.value("links") == "true");
	ui.CalculateForAllCheck->setChecked(pluginOptions_.value("calculateForAll") == "true");
	ui.MaxRingSizeSpin->setValue(pluginOptions_.value("maxRingSize").toInt());
	ui.MaxDistanceSpin->setValue(pluginOptions_.value("maxDistance").toDouble());
}

// Set plugin options from UI controls
void RingsToolDialog::setPluginOptions()
{
	pluginOptions_.add("element", ui.ElementEdit->text());
	pluginOptions_.add("linkElement", ui.LinkElementEdit->text());
	pluginOptions_.add("links", ui.LinkElementCheck->isChecked() ? "true" : "false");
	pluginOptions_.add("calculateForAll", ui.CalculateForAllCheck->isChecked() ? "true" : "false");
	pluginOptions_.add("maxRingSize", QString::number(ui.MaxRingSizeSpin->value()));
	pluginOptions_.add("maxDistance", QString::number(ui.MaxDistanceSpin->value()));
}
