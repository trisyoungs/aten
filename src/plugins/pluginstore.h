/*
        *** Plugin Store Definition
        *** src/plugins/pluginstore.h
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

#ifndef ATEN_PLUGINSTORE_H
#define ATEN_PLUGINSTORE_H

#include "plugins/plugintypes.h"
#include "plugins/interfaces.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

ATEN_BEGIN_NAMESPACE

// Forward Declarations
/* none */

// Plugin Store
class PluginStore
{
	public:
	// Constructor / Destructor
	PluginStore();
	~PluginStore();

	/*
	 * Internal Data
	 */
	private:
	// Logpoint of plugin store
	int logPoint_;

	public:
	// return current logpoint of plugin store
	int logPoint() const;


	/*
	 * File Import/Export Plugins
	 */
	private:
	// List of IO plugin objects (by category)
	RefList<FilePluginInterface,int> filePlugins_[PluginTypes::nFilePluginCategories];

	public:
	// Register file plugin
	bool registerFilePlugin(FilePluginInterface* plugin);
	// Empty (delete) all plugins and plugin instances
	void clearFilePlugins();
	// Return reference list of file plugins of specified category
	const RefList<FilePluginInterface,int>& filePlugins(PluginTypes::FilePluginCategory category) const;
	// Return number of plugins of specified category and type
	int nFilePlugins(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type) const;
	// Show list of valid plugin nicknames
	void showFilePluginNicknames(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type) const;
	// Show all file plugins, by category, and their nicknames
	void showAllFilePluginNicknames() const;
	// Find plugin interface for specified file
	FilePluginInterface* findFilePlugin(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type, QString filename) const;
	// Find plugin interface by nickname
	FilePluginInterface* findFilePluginByNickname(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type, QString nickname) const;
};

ATEN_END_NAMESPACE

#endif