/*
        *** Plugin Store Definition
        *** src/plugins/pluginstore.h
        Copyright T. Youngs 2016-2017

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
#include "plugins/interfaces/fileplugin.h"
#include "plugins/interfaces/methodplugin.h"
#include "plugins/interfaces/toolplugin.h"
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
	// List of file plugin objects (by category)
	RefList<FilePluginInterface,KVMap> filePlugins_[PluginTypes::nFilePluginCategories];

	public:
	// Register file plugin
	bool registerFilePlugin(FilePluginInterface* plugin);
	// Return reference list of file plugins of specified category
	const RefList<FilePluginInterface,KVMap>& filePlugins(PluginTypes::FilePluginCategory category) const;
	// Return number of plugins of specified category and type
	int nFilePlugins(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type) const;
	// Return total number of file plugins available
	int nFilePlugins() const;
	// Show list of valid plugin nicknames
	void showFilePluginNicknames(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type) const;
	// Show all file plugins, by category, and their nicknames
	void showAllFilePluginNicknames() const;
	// Find plugin interface for specified file
	const FilePluginInterface* findFilePlugin(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type, QString filename) const;
	// Find plugin interface by nickname
	const FilePluginInterface* findFilePluginByNickname(PluginTypes::FilePluginCategory category, PluginTypes::FilePluginType type, QString nickname) const;


	/*
	 * Method Plugins
	 */
	private:
	// List of method plugin objects (by category)
	RefList<MethodPluginInterface,KVMap> methodPlugins_[PluginTypes::nMethodPluginCategories];

	public:
	// Register method plugin
	bool registerMethodPlugin(MethodPluginInterface* plugin);
	// Return reference list of method plugins of specified category
	const RefList<MethodPluginInterface,KVMap>& methodPlugins(PluginTypes::MethodPluginCategory category) const;
	// Return number of method plugins of specified category
	int nMethodPlugins(PluginTypes::MethodPluginCategory category) const;
	// Return total number of method plugins available
	int nMethodPlugins() const;
	// Show list of valid method plugin nicknames
	void showMethodPluginNicknames(PluginTypes::MethodPluginCategory category) const;
	// Show all method plugins, by category, and their nicknames
	void showAllMethodPluginNicknames() const;
	// Find plugin interface by nickname
	MethodPluginInterface* findMethodPluginByNickname(PluginTypes::MethodPluginCategory category, QString nickname) const;


	/*
	 * Tool Plugins
	 */
	private:
	// List of tool plugin objects (by category)
	RefList<ToolPluginInterface,KVMap> toolPlugins_[PluginTypes::nToolPluginCategories];

	public:
	// Register tool plugin
	bool registerToolPlugin(ToolPluginInterface* plugin);
	// Return reference list of tool plugins of specified category
	const RefList<ToolPluginInterface,KVMap>& toolPlugins(PluginTypes::ToolPluginCategory category) const;
	// Return number of tool plugins of specified category
	int nToolPlugins(PluginTypes::ToolPluginCategory category) const;
	// Return total number of tool plugins available
	int nToolPlugins() const;
	// Show list of valid tool plugin nicknames
	void showToolPluginNicknames(PluginTypes::ToolPluginCategory category) const;
	// Show all tool plugins, by category, and their nicknames
	void showAllToolPluginNicknames() const;
	// Find plugin interface by nickname
	ToolPluginInterface* findToolPluginByNickname(PluginTypes::ToolPluginCategory category, QString nickname) const;
};

ATEN_END_NAMESPACE

#endif