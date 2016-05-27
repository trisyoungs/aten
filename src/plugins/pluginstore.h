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
	 * Plugin Data
	 */
	private:
	// List of IO plugin objects (by type)
	RefList<IOPluginInterface,int> ioPlugins_[PluginTypes::nPluginTypes];

	public:
	// Register plugin (IOPluginType)
	bool registerPlugin(IOPluginInterface* plugin);
	// Empty (delete) all plugins and plugin instances
	void clearPlugins();
};

ATEN_END_NAMESPACE

#endif