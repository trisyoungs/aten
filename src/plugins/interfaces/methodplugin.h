/*
        *** Method Plugin Interface
        *** src/plugins/interfaces/methodplugin.h
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

#ifndef ATEN_METHODPLUGININTERFACE_H
#define ATEN_METHODPLUGININTERFACE_H

#include "plugins/interfaces/baseplugin.h"
#include "parser/returnvalue.h"
#include "parser/commandnode.h"
#include "model/model.h"
#include "base/grid.h"
#include "base/kvmap.h"
#include "base/forcefieldatom.h"
#include "base/messenger.h"
#include "base/fileparser.h"
#include "base/namespace.h"
#include "templates/reflist.h"
#include <QStringList>
#include <QtPlugin>
#include <QFileInfo>

ATEN_BEGIN_NAMESPACE

// Forward Declarations
class Model;

// Method Plugin Interface
class MethodPluginInterface : public BasePluginInterface, public ListItem<MethodPluginInterface>
{
	public:
	// Constructor
	MethodPluginInterface() : ListItem<MethodPluginInterface>()
	{
	}
	// Destructor
	virtual ~MethodPluginInterface() {}


	/*
	 * Method
	 */
	public:
	// Run method on the current target model
	virtual bool runMethod() = 0;


	/*
	 * Options
	 */
	protected:
	// Options specific to this plugin
	KVMap pluginOptions_;

	protected:
	// Return conversion of supplied QString to bool
	bool toBool(QString string)
	{
		if ((string.toInt() == 1) || (string.toLower() == "false")) return false;
		return true;
	}

	public:
	// Return whether the plugin has options
	virtual bool hasOptions() = 0;
	// Show options dialog
	virtual bool showOptionsDialog() = 0;
};

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

#define MethodPluginInterface_iid "com.projectaten.Aten.MethodPluginInterface.v1"

Q_DECLARE_INTERFACE(AtenSpace::MethodPluginInterface, MethodPluginInterface_iid)

#endif
