/*
	*** About Dialog
	*** src/gui/about_funcs.cpp
	Copyright T. Youngs 2013-2018

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

#include "gui/about.h"
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "main/version.h"
#include "plugins/pluginstore.h"
#include "gui/qcustomplot/qcustomplot.hui"

ATEN_USING_NAMESPACE

// Constructor
AtenAbout::AtenAbout(AtenWindow& parent) : QDialog(&parent), atenWindow_(parent), pluginStore_(parent.aten().pluginStore())
{
	ui.setupUi(this);

	// Set version string in label
	ui.AtenVersionLabel->setText(QString("Aten version %1").arg(ATENVERSION));

	// Setup plot
	PlotData data;
	data.setTitle("QCustomPlot");
	QVector<double>& x = data.x(), &y = data.y();
	
	for (int i=0; i<101; ++i)
	{
		x.append(i/50.0 - 1); // x goes from -1 to 1
		y.append( (i/50.0 - 1)*(i/50.0 - 1) ); // let's plot a quadratic function
	}
	
	ui.PlotWidget->addData(data);
	ui.PlotWidget->plot()->xAxis->setLabel("X");
	ui.PlotWidget->plot()->yAxis->setLabel("Y");
	ui.PlotWidget->plot()->xAxis->setRange(-1, 1);
	ui.PlotWidget->plot()->yAxis->setRange(0, 1);
	ui.PlotWidget->plot()->replot();

	// Populate plugin types combo
	for (int n=0; n<PluginTypes::nPluginTypes; ++n) ui.TypeCombo->addItem( PluginTypes::pluginType((PluginTypes::PluginType) n) );
	ui.TypeCombo->addItem("All Plugins");

	connect(ui.TypeCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(updateTree(int)));
	ui.TypeCombo->setCurrentIndex(PluginTypes::nPluginTypes);
}

// Destructor
AtenAbout::~AtenAbout()
{
}

/*
 * Slots
 */

// Update tree with plugins of specified type
void AtenAbout::updateTree(int type)
{
	PluginTypes::PluginType pluginType = (PluginTypes::PluginType) type;

	ui.PluginsTree->clear();
	ui.PluginsTree->setColumnCount(1);

	QTreeWidgetItem* parentItem, *groupItem, *item, *subItem;

	// File plugins
	if ((pluginType == PluginTypes::FilePlugin) || (pluginType == PluginTypes::nPluginTypes))
	{
		parentItem = new QTreeWidgetItem(ui.PluginsTree);
		parentItem->setText(0, PluginTypes::pluginType(PluginTypes::FilePlugin));
		parentItem->setExpanded(true);

		// Loop over file plugin categories
		for (int n=0; n<PluginTypes::nFilePluginCategories; ++n)
		{
			PluginTypes::FilePluginCategory category = (PluginTypes::FilePluginCategory) n;

			groupItem = new QTreeWidgetItem(parentItem);
			groupItem->setText(0, PluginTypes::niceFilePluginCategory(category));
			for (RefListItem<FilePluginInterface,KVMap>* ri = pluginStore_.filePlugins(category).first(); ri != NULL; ri = ri->next)
			{
				FilePluginInterface* plugin = ri->item;

				item = new QTreeWidgetItem(groupItem);
				item->setText(0, plugin->name() + " (" + plugin->pluginFilename() + ")");

				// Add plugin information
				subItem = new QTreeWidgetItem(item);
				subItem->setText(0, "Description: " + plugin->description());
				subItem = new QTreeWidgetItem(item);
				subItem->setText(0, "Matches: " + plugin->filterString());
				subItem = new QTreeWidgetItem(item);
				subItem->setText(0, "Import: " + QString(plugin->canImport() ? "Yes" : "No") + ((plugin->canImport() && plugin->hasImportOptions()) ? " (With Options)" : ""));
				subItem = new QTreeWidgetItem(item);
				subItem->setText(0, "Export: " + QString(plugin->canExport() ? "Yes" : "No") + ((plugin->canExport() && plugin->hasExportOptions()) ? " (With Options)" : ""));
			}
		}
	}

	// Method plugins
	if ((pluginType == PluginTypes::MethodPlugin) || (pluginType == PluginTypes::nPluginTypes))
	{
		parentItem = new QTreeWidgetItem(ui.PluginsTree);
		parentItem->setText(0, PluginTypes::pluginType(PluginTypes::MethodPlugin));
		parentItem->setExpanded(true);

		// Loop over file plugin categories
		for (int n=0; n<PluginTypes::nMethodPluginCategories; ++n)
		{
			PluginTypes::MethodPluginCategory category = (PluginTypes::MethodPluginCategory) n;

			groupItem = new QTreeWidgetItem(parentItem);
			groupItem->setText(0, PluginTypes::niceMethodPluginCategory(category));
			for (RefListItem<MethodPluginInterface,KVMap>* ri = pluginStore_.methodPlugins(category).first(); ri != NULL; ri = ri->next)
			{
				MethodPluginInterface* plugin = ri->item;

				item = new QTreeWidgetItem(groupItem);
				item->setText(0, plugin->name() + " (" + plugin->pluginFilename() + ")");

				// Add plugin information
				subItem = new QTreeWidgetItem(item);
				subItem->setText(0, "Description: " + plugin->description());
			}
		}
	}

	// Tool plugins
	if ((pluginType == PluginTypes::ToolPlugin) || (pluginType == PluginTypes::nPluginTypes))
	{
		parentItem = new QTreeWidgetItem(ui.PluginsTree);
		parentItem->setText(0, PluginTypes::pluginType(PluginTypes::ToolPlugin));
		parentItem->setExpanded(true);

		// Loop over file plugin categories
		for (int n=0; n<PluginTypes::nToolPluginCategories; ++n)
		{
			PluginTypes::ToolPluginCategory category = (PluginTypes::ToolPluginCategory) n;

			groupItem = new QTreeWidgetItem(parentItem);
			groupItem->setText(0, PluginTypes::niceToolPluginCategory(category));
			for (RefListItem<ToolPluginInterface,KVMap>* ri = pluginStore_.toolPlugins(category).first(); ri != NULL; ri = ri->next)
			{
				ToolPluginInterface* plugin = ri->item;

				item = new QTreeWidgetItem(groupItem);
				item->setText(0, plugin->name() + " (" + plugin->pluginFilename() + ")");

				// Add plugin information
				subItem = new QTreeWidgetItem(item);
				subItem->setText(0, "Description: " + plugin->description());
			}
		}
	}

}

void AtenAbout::on_CloseButton_clicked(bool checked)
{
	accept();
}
