/*
	*** SPRings Tool Dialog Functions
	*** src/gui/tool_springs/springstooldialog_funcs.cpp
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

#include "plugins/tool_springs/springstooldialog.h"
#include "gui/qcustomplot/qcustomplot.hui"

// Constructor
SPRingsToolDialog::SPRingsToolDialog(ToolPluginInterface& targetInterface, KVMap& pluginOptions) : QDialog(NULL), targetInterface_(targetInterface), pluginOptions_(pluginOptions)
{
	ui.setupUi(this);

	// Initialise the icon resource
	Q_INIT_RESOURCE(springstool_icons);

	// Set up plot
	QCustomPlot* customPlot = ui.PlotWidget->plot();
	customPlot->xAxis->setLabel("Ring Size");
	customPlot->yAxis->setLabel("Frequency");
	customPlot->xAxis->setRange(1, 10);
	customPlot->yAxis->setRange(0, 100);
}

/*
 * Widget Functions
 */

void SPRingsToolDialog::on_CloseButton_clicked(bool checked)
{
	// Close the dialog, storing UI options before we close
	setPluginOptions();
	accept(); 
}

void SPRingsToolDialog::on_RunButton_clicked(bool checked)
{
	// Set options before we call the run method
	setPluginOptions();

	targetInterface_.runTool();
}

// Apply plugin options to UI controls
void SPRingsToolDialog::applyPluginOptions()
{
	ui.LinkElementEdit->setText(pluginOptions_.value("linkElement"));
	ui.ElementEdit->setText(pluginOptions_.value("element"));
	ui.LinkElementCheck->setChecked(pluginOptions_.value("links") == "true");
	ui.CalculateForAllCheck->setChecked(pluginOptions_.value("calculateForAll") == "true");
	ui.MaxRingSizeSpin->setValue(pluginOptions_.value("maxRingSize").toInt());
	ui.MaxDistanceSpin->setValue(pluginOptions_.value("maxDistance").toDouble());
	ui.DebugCheck->setChecked(pluginOptions_.value("debug") == "true");
}

// Set plugin options from UI controls
void SPRingsToolDialog::setPluginOptions()
{
	pluginOptions_.add("element", ui.ElementEdit->text());
	pluginOptions_.add("linkElement", ui.LinkElementEdit->text());
	pluginOptions_.add("links", ui.LinkElementCheck->isChecked() ? "true" : "false");
	pluginOptions_.add("calculateForAll", ui.CalculateForAllCheck->isChecked() ? "true" : "false");
	pluginOptions_.add("maxRingSize", QString::number(ui.MaxRingSizeSpin->value()));
	pluginOptions_.add("maxDistance", QString::number(ui.MaxDistanceSpin->value()));
	pluginOptions_.add("debug", ui.DebugCheck->isChecked() ? "true" : "false");
}
