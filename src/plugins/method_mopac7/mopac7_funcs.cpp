/*
        *** MOPAC7 Method Plugin Functions
        *** src/plugins/method_mopac7/mopac7_funcs.cpp
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

#include "plugins/method_mopac7/mopac7.hui"
#include "model/model.h"

// Constructor
Mopac7MethodPlugin::Mopac7MethodPlugin()
{
}

// Destructor
Mopac7MethodPlugin::~Mopac7MethodPlugin()
{
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType Mopac7MethodPlugin::type() const
{
	return PluginTypes::MethodPlugin;
}

// Return category of plugin
int Mopac7MethodPlugin::category() const
{
	return PluginTypes::OptimisationMethodPlugin;
}

// Name of plugin
QString Mopac7MethodPlugin::name() const
{
	return QString("MOPAC7 Plugin");
}

// Nickname of plugin
QString Mopac7MethodPlugin::nickname() const
{
	return QString("mopac7");
}

// Description (long name) of plugin
QString Mopac7MethodPlugin::description() const
{
	return QString("MOPAC7 functionality (v1.15)");
}

/*
 * Method
 */

// Run method on the current target model
bool Mopac7MethodPlugin::runMethod()
{
	readmo_();
}

/*
 * Options
 */

// Return whether the plugin has options
bool Mopac7MethodPlugin::hasOptions()
{
	return false;
}

// Show options dialog
bool Mopac7MethodPlugin::showOptionsDialog()
{
	return false;
}
