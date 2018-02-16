/*
        *** MOPAC7.1 Optimisation Plugin Functions
        *** src/plugins/method_mopac71/mopac71opt_funcs.cpp
        Copyright T. Youngs 2016-2018

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

#include "plugins/method_mopac71/mopac71opt.hui"
#include "plugins/method_mopac71/mopac71control.hui"
#include "plugins/method_mopac71/mopac71.hui"
#include <plugins/pluginstore.h>
#include "model/model.h"

// Constructor
MOPAC71OptimisationPlugin::MOPAC71OptimisationPlugin()
{
	// Plugin options
// 	pluginOptions_.add("GEO-OK", 
}

// Destructor
MOPAC71OptimisationPlugin::~MOPAC71OptimisationPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* MOPAC71OptimisationPlugin::makeCopy() const
{
	return new MOPAC71OptimisationPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType MOPAC71OptimisationPlugin::type() const
{
	return PluginTypes::MethodPlugin;
}

// Return category of plugin
int MOPAC71OptimisationPlugin::category() const
{
	return PluginTypes::OptimisationMethodPlugin;
}

// Name of plugin
QString MOPAC71OptimisationPlugin::name() const
{
	return QString("MOPAC7.1 Optimiser");
}

// Nickname of plugin
QString MOPAC71OptimisationPlugin::nickname() const
{
	return QString("mopac71opt");
}

// Return whether the plugin is enabled
bool MOPAC71OptimisationPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString MOPAC71OptimisationPlugin::description() const
{
	return QString("MOPAC geometry ooooptimiser (v7.1 public domain version)");
}

/*
 * Method
 */

// Run method on the current target model
bool MOPAC71OptimisationPlugin::runMethod()
{
	// Duplicate MOPAC71MethodPlugin ('mopac71') to perform the calculation...
	// -- First, find the plugin
	const MethodPluginInterface* methodPluginMaster = pluginStore_->findMethodPluginByNickname(PluginTypes::GeneralMethodPlugin, "mopac71");
	if (!methodPluginMaster)
	{
		Messenger::error("Error: MOPAC71OptimisationPlugin also requires the MOPAC71MethodPlugin.");
		return false;
	}
	MethodPluginInterface* mopacMethod = (MethodPluginInterface*) methodPluginMaster->duplicate();

	// Setup options and target model in the method plugin
	mopacMethod->setTargetModel(targetModel());
	//mopacMethod->setOption();

	// Run it
	bool result = mopacMethod->executeMethod(true);
	
	return result;
}

/*
 * Options
 */

// Return whether the plugin has options
bool MOPAC71OptimisationPlugin::hasOptions()
{
	return false;
}

// Show options dialog
bool MOPAC71OptimisationPlugin::showOptionsDialog()
{
	return false;
}
