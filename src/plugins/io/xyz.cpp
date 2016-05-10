/*
        *** XYZ Plugin
        *** src/plugins/io/xyz.cpp
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

#include "plugins/io/xyz.h"

/*
 * XYZ Model Import / Export Plugin
 */

// Name of plugin
QString name()
{
}
// Nickname of plugin
QString nickName();
// Description (long name) of plugin
QString description();
// Related file extensions
QStringList extensions();
// Exact names
QStringList exactNames();

// Return whether this plugin can load the specified filename
bool canLoad(QString filename);
// Load data from the specified file
bool load(QString filename);
// Save data to the specified file
bool save(QString filename);