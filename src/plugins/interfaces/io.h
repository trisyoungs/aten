/*
        *** I/O Plugin Interface Definition
        *** src/plugins/interface/io.h
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

#ifndef ATEN_IOPLUGIN_H
#define ATEN_IOPLUGIN_H

#include "plugins/plugintypes.h"
#include "base/namespace.h"
#include "templates/reflist.h"
#include <QStringList>
#include <QtPlugin>

ATEN_BEGIN_NAMESPACE

// Forward Declarations
class Model;

// IO Plugin Interface
class IOPluginInterface
{
	public:
	// Destructor 
	virtual ~IOPluginInterface() {}


	/*
	 * Definition
	 */
	public:
	// Return type of plugin
	virtual PluginTypes::PluginType type() const = 0;
	// Return name of plugin
	virtual QString name() const = 0;
	// Return nickname of plugin
	virtual QString nickName() const = 0;
	// Return description (long name) of plugin
	virtual QString description() const = 0;
	// Return related file extensions
	virtual QStringList extensions() const = 0;
	// Return exact names list
	virtual QStringList exactNames() const = 0;


	/*
	 * Object Handling
	 */
	private:
	// Model objects created on load
	RefList<Model,int> createdModels_;

	private:
	// Create new model (in Aten)
	Model* createModel();


	/*
	 * File Handling
	 */
	private:
	// Perform secondary checks on whether this plugin can load the specified file
	virtual bool secondaryProbe(QString filename);


	public:
	// Return whether this plugin can load the specified file
	bool probe(QString filename);
	// Return whether this plugin can load data
	virtual bool canLoad() = 0;
	// Load data from the specified file
	virtual bool load(QString filename) = 0;
	// Return whether this plugin can save data
	virtual bool canSave() = 0;
	// Save data to the specified file
	virtual bool save(QString filename) = 0;
};

ATEN_END_NAMESPACE

#define IOPluginInterface_iid "com.projectaten.Aten.IOPluginInterface.v1"

Q_DECLARE_INTERFACE(AtenSpace::IOPluginInterface, IOPluginInterface_iid)

#endif
