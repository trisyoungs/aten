/*
        *** XYZ Plugin
        *** src/plugins/io/xyz.h
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

#ifndef ATEN_XYZPLUGIN_H
#define ATEN_XYZPLUGIN_H

#include "plugins/interfaces/io.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations
/* none */

// XYZ Model Import / Export Plugin
class XYZModelPlugin : public QObject, public IOPluginInterface
{
	Q_OBJECT
	Q_PLUGIN_METADATA(IID "com.projectaten.Aten.IOPluginInterface.v1")
	Q_INTERFACES(IOPluginInterface)


	/*
	 * Definition
	 */
	public:
	// Return name of plugin
	QString name();
	// Return nickname of plugin
	QString nickName();
	// Return description (long name) of plugin
	QString description();
	// Return related file extensions
	QStringList extensions();
	// Return exact names
	QStringList exactNames();


	/*
	 * File Handling
	 */
	public:
	// Load data from the specified file
	bool load(QString filename);
	// Save data to the specified file
	bool save(QString filename);
};

ATEN_END_NAMESPACE

#endif
