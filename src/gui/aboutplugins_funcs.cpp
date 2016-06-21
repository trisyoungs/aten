/*
	*** About Plugins Dialog
	*** src/gui/aboutplugins_funcs.cpp
	Copyright T. Youngs 2013-2016

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

#include "gui/aboutplugins.h"
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "plugins/pluginstore.h"

ATEN_USING_NAMESPACE

// Constructor
AtenAboutPlugins::AtenAboutPlugins(AtenWindow& parent) : QDialog(&parent), atenWindow_(parent), pluginStore_(parent.aten().pluginStore())
{
	ui.setupUi(this);

	// Populate plugin types combo
	for (int n=0; n<PluginTypes::nPluginTypes; ++n) ui.TypeCombo->addItem( PluginTypes::pluginType((PluginTypes::PluginType) n) );
	ui.TypeCombo->addItem("All Plugins");

	connect(ui.TypeCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(updateTree(int)));
	ui.TypeCombo->setCurrentIndex(PluginTypes::nPluginTypes);
}

// Destructor
AtenAboutPlugins::~AtenAboutPlugins()
{
}

/*
 * Slots
 */

// Update tree with plugins of specified type
void AtenAboutPlugins::updateTree(int type)
{
	PluginTypes::PluginType pluginType = (PluginTypes::PluginType) type;

	ui.PluginsTree->clear();
	ui.PluginsTree->setColumnCount(1);

	QTreeWidgetItem* parentItem, *groupItem, *item, *subItem;
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
			for (RefListItem<FilePluginInterface,int>* ri = pluginStore_.filePlugins(category).first(); ri != NULL; ri = ri->next)
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
}

void AtenAboutPlugins::on_CloseButton_clicked(bool checked)
{
	accept();
}
