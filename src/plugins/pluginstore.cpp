/*
        *** Plugin Store
        *** src/plugins/pluginstore.cpp
        Copyright T. Youngs 2016-2016

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

#include "plugins/pluginstore.h"
#include "interfaces/methodplugin.h"
#include "base/messenger.h"

// Constructor
PluginStore::PluginStore()
{
	logPoint_ = 0;
}

// Destructor
PluginStore::~PluginStore()
{
}

/*
 * Internal Data
 */

// return current logpoint of plugin store
int PluginStore::logPoint() const
{
	return logPoint_;
}

// Register file plugin
bool PluginStore::registerFilePlugin(FilePluginInterface* plugin)
{
	if (!plugin) return false;

	// Query the plugin type and category...
	if (plugin->type() != PluginTypes::FilePlugin)
	{
		Messenger::error("Refused to register plugin '" + plugin->name() + "' as a file plugin since it presents itself as a different type.");
		return false;
	}
	
	if (plugin->category() == PluginTypes::nFilePluginCategories)
	{
		Messenger::error("Plugin has unrecognised category - not registered.\n");
		return false;
	}

	// Store the reference to the plugin 
	filePlugins_[plugin->category()].add(plugin);
	Messenger::print(Messenger::Verbose, "Registered new file plugin:");
	Messenger::print(Messenger::Verbose, "       Name : %s", qPrintable(plugin->name()));
	Messenger::print(Messenger::Verbose, "Description : %s", qPrintable(plugin->description()));
	QString targets;
	if (plugin->extensions().count() > 0) targets = "Extensions " + plugin->extensions().join(", ");
	if (plugin->exactNames().count() > 0)
	{
		if (targets.isEmpty()) targets = "Exact names ";
		else targets += ", and exact names ";
		targets += plugin->exactNames().join(", ");
	}
	Messenger::print(Messenger::Verbose, "    Targets : %s", qPrintable(targets));

	++logPoint_;

	return true;
}

// Empty (delete) all file plugins and plugin instances
void PluginStore::clearFilePlugins()
{
	for (int n=0; n<PluginTypes::nFilePluginCategories; ++n)
	{
		// Loop over stored interfaces and clear any instances we have
		for (RefListItem<FilePluginInterface,int>* ri = filePlugins_[n].first(); ri != NULL; ri = ri->next)
		{
			ri->item->deleteInstances();
		}

		filePlugins_[n].clear();
	}

	++logPoint_;
}

// Return reference list of file plugins of specified category
const RefList<FilePluginInterface,int>& PluginStore::filePlugins(PluginTypes::FilePluginCategory category) const
{
	return filePlugins_[category];
}

// Return number of file plugins of specified category and type
int PluginStore::nFilePlugins(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type) const
{
	int count = 0;
	for (RefListItem<FilePluginInterface,int>* ri = filePlugins_[category].first(); ri != NULL; ri = ri->next)
	{
		FilePluginInterface* plugin = ri->item;
		if ((type == PluginTypes::ImportPlugin) && (plugin->canImport())) ++count;
		else if ((type == PluginTypes::ExportPlugin) && (plugin->canExport())) ++count;
	}
	return count;
}

// Return total number of file plugins available
int PluginStore::nFilePlugins() const
{
	int count = 0;
	for (int n=0; n<PluginTypes::nFilePluginCategories; ++n)
	{
		count += nFilePlugins((PluginTypes::FilePluginCategory) n, PluginTypes::ImportPlugin);
		count += nFilePlugins((PluginTypes::FilePluginCategory) n, PluginTypes::ExportPlugin);
	}
	return count;
}

// Show list of valid plugin nicknames
void PluginStore::showFilePluginNicknames(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type) const
{
	Messenger::print("Available plugins for %s %s:", PluginTypes::filePluginCategory(category), PluginTypes::filePluginType(type));

	// Determine longest nickname of all the plugins of the specified category and type, and make a reflist of them while we're at it
	int maxLength = 0;
	RefList<FilePluginInterface,int> plugins;
	for (RefListItem<FilePluginInterface,int>* ri = filePlugins_[category].first(); ri != NULL; ri = ri->next)
	{
		FilePluginInterface* plugin = ri->item;

		// If an import plugin was requested, and this plugin can't import anything, continue
		if ((type == PluginTypes::ImportPlugin) && (!plugin->canImport())) continue;

		// If an export plugin was requested, and this plugin can't export anything, continue
		if ((type == PluginTypes::ExportPlugin) && (!plugin->canExport())) continue;

		plugins.add(plugin);
		if (plugin->nickname().length() > maxLength) maxLength = plugin->nickname().length();
	}

	// Output list (or special case if no plugins of the specified type were found...
	if (plugins.nItems() == 0)
	{
		Messenger::print("  <None Available>");
		return;
	}
	else for (RefListItem<FilePluginInterface,int>* ri = filePlugins_[category].first(); ri != NULL; ri = ri->next)
	{
		FilePluginInterface* plugin = ri->item;

		Messenger::print(QString("\t%1    %2").arg(plugin->nickname(), maxLength).arg(plugin->filterString()));
	}
}

// Show all file plugins, by category, and their nicknames
void PluginStore::showAllFilePluginNicknames() const
{
	for (int n=0; n<PluginTypes::nFilePluginCategories; ++n)
	{
		showFilePluginNicknames((PluginTypes::FilePluginCategory) n, PluginTypes::ImportPlugin);
		showFilePluginNicknames((PluginTypes::FilePluginCategory) n, PluginTypes::ExportPlugin);
	}
}

// Find plugin interface for specified file
FilePluginInterface* PluginStore::findFilePlugin(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type, QString filename) const
{
	// Loop over loaded plugins of the specified category
	for (RefListItem<FilePluginInterface,int>* ri = filePlugins_[category].first(); ri != NULL; ri = ri->next)
	{
		FilePluginInterface* plugin = ri->item;

		// If an import plugin was requested, and this plugin can't import anything, continue
		if ((type == PluginTypes::ImportPlugin) && (!plugin->canImport())) continue;

		// If an export plugin was requested, and this plugin can't export anything, continue
		if ((type == PluginTypes::ExportPlugin) && (!plugin->canExport())) continue;

		// Perform checks to see if the plugin is related to this file
		if (plugin->isRelatedToFile(filename)) return plugin;
	}

	return NULL;
}


// Find plugin interface by nickname provided
FilePluginInterface* PluginStore::findFilePluginByNickname(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type, QString nickname) const
{
	// Loop over loaded plugins of the specified category
	for (RefListItem<FilePluginInterface,int>* ri = filePlugins_[category].first(); ri != NULL; ri = ri->next)
	{
		FilePluginInterface* plugin = ri->item;

		// If an import plugin was requested, and this plugin can't import anything, continue
		if ((type == PluginTypes::ImportPlugin) && (!plugin->canImport())) continue;

		// If an export plugin was requested, and this plugin can't export anything, continue
		if ((type == PluginTypes::ExportPlugin) && (!plugin->canExport())) continue;

		// Perform checks to see if the plugin is related to this file
		if (plugin->nickname() == nickname) return plugin;
	}

	return NULL;
}


/*
 * Method Plugins
 */

// Register method plugin
bool PluginStore::registerMethodPlugin(MethodPluginInterface* plugin)
{
	if (!plugin) return false;

	// Query the plugin type and category...
	if (plugin->type() != PluginTypes::MethodPlugin)
	{
		Messenger::error("Refused to register plugin '" + plugin->name() + "' as a method plugin since it presents itself as a different type.");
		return false;
	}
	
	if (plugin->category() == PluginTypes::nMethodPluginCategories)
	{
		Messenger::error("Plugin has unrecognised category - not registered.\n");
		return false;
	}

	// Store the reference to the plugin 
	methodPlugins_[plugin->category()].add(plugin);
	Messenger::print(Messenger::Verbose, "Registered new method plugin:");
	Messenger::print(Messenger::Verbose, "       Name : %s", qPrintable(plugin->name()));
	Messenger::print(Messenger::Verbose, "Description : %s", qPrintable(plugin->description()));

	plugin->runMethod();

	++logPoint_;

	return true;
}

// Empty (delete) all method plugins
void PluginStore::clearMethodPlugins()
{
	for (int n=0; n<PluginTypes::nMethodPluginCategories; ++n) methodPlugins_[n].clear();

	++logPoint_;
}

// Return reference list of file plugins of specified category
const RefList<MethodPluginInterface,int>& PluginStore::methodPlugins(PluginTypes::MethodPluginCategory category) const
{
	return methodPlugins_[category];
}

// Return number of method plugins of specified category
int PluginStore::nMethodPlugins(PluginTypes::MethodPluginCategory category) const
{
	return methodPlugins_[category].nItems();
}

// Return total number of file plugins available
int PluginStore::nMethodPlugins() const
{
	int count = 0;
	for (int n=0; n<PluginTypes::nMethodPluginCategories; ++n) count += methodPlugins_[n].nItems();
}

// Show list of valid method plugin nicknames
void PluginStore::showMethodPluginNicknames(PluginTypes::MethodPluginCategory category) const
{
	Messenger::print("Available method plugins for %s:", PluginTypes::methodPluginCategory(category));

	// Determine longest nickname of all the plugins of the specified category and type, and make a reflist of them while we're at it
	int maxLength = 0;
	for (RefListItem<MethodPluginInterface,int>* ri = methodPlugins_[category].first(); ri != NULL; ri = ri->next)
	{
		MethodPluginInterface* plugin = ri->item;

		if (plugin->nickname().length() > maxLength) maxLength = plugin->nickname().length();
	}

	// Output list (or special case if no plugins of the specified type were found...
	if (methodPlugins_[category].nItems() == 0)
	{
		Messenger::print("  <None Available>");
		return;
	}
	else for (RefListItem<MethodPluginInterface,int>* ri = methodPlugins_[category].first(); ri != NULL; ri = ri->next)
	{
		MethodPluginInterface* plugin = ri->item;

		Messenger::print(QString("\t%1    %2").arg(plugin->nickname(), maxLength).arg(plugin->name()));
	}
}

// Show all method plugins, by category, and their nicknames
void PluginStore::showAllMethodPluginNicknames() const
{
	for (int n=0; n<PluginTypes::nMethodPluginCategories; ++n) showMethodPluginNicknames((PluginTypes::MethodPluginCategory) n);
}

// Find plugin interface by nickname
MethodPluginInterface* PluginStore::findMethodPluginByNickname(PluginTypes::MethodPluginCategory category, QString nickname) const
{
	// Loop over loaded method plugins of the specified category
	for (RefListItem<MethodPluginInterface,int>* ri = methodPlugins_[category].first(); ri != NULL; ri = ri->next)
	{
		MethodPluginInterface* plugin = ri->item;

		if (plugin->nickname() == nickname) return plugin;
	}

	return NULL;
}
