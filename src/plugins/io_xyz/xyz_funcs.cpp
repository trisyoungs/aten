/*
        *** XYZ Plugin Functions
        *** src/plugins/io_xyz/xyz_funcs.cpp
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

#include "plugins/io_xyz/xyz.h"

/*
 * XYZ Model Import / Export Plugin
 */

// Return type of plugin
PluginTypes::PluginType XYZModelPlugin::type() const
{
	return PluginTypes::IOModelPlugin;
}
	
// Name of plugin
QString XYZModelPlugin::name() const
{
	return QString("XYZ Files (XMol Style)");
}

// Nickname of plugin
QString XYZModelPlugin::nickName() const
{
	return QString("xyz");
}

// Description (long name) of plugin
QString XYZModelPlugin::description() const
{
	return QString("Import/export for XMol-style XYZ coordinate files");
}

// Related file extensions
QStringList XYZModelPlugin::extensions() const
{
	return QStringList() << "xyz";
}

// Exact names
QStringList XYZModelPlugin::exactNames() const
{
	return QStringList();
}

// Return whether this plugin can load data
bool XYZModelPlugin::canLoad()
{
	return true;
}

// Load data from the specified file
bool XYZModelPlugin::load(QString filename)
{
	return true;
}

// Return whether this plugin can save data
bool XYZModelPlugin::canSave()
{
	return true;
}

// Save data to the specified file
bool XYZModelPlugin::save(QString filename)
{
	return true;
}
