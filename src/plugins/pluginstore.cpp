/*
        *** Plugin Store
        *** src/plugins/pluginstore.cpp
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

#include "plugins/pluginstore.h"
#include "base/messenger.h"

// Constructor
PluginStore::PluginStore()
{
}

// Destructor
PluginStore::~PluginStore()
{
}

// Register plugin (IOPluginType)
bool PluginStore::registerPlugin(IOPluginInterface* plugin)
{
	if (!plugin) return false;

	// Query the plugin type...
	if (plugin->type() == PluginTypes::nPluginTypes)
	{
		Messenger::error("Plugin has unrecognised type - not registered.\n");
		return false;
	}

	// Store the reference to the plugin 
	ioPlugins_[plugin->type()].add(plugin);
	Messenger::print(Messenger::Verbose, "Registered new plugin:");
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
}
