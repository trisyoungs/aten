/*
        *** Plugin Interfaces
        *** src/plugins/interfaces.h
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
#include "parser/returnvalue.h"
#include "parser/commandnode.h"
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

// IO Plugin Interface
class IOPluginInterface : public ListItem<IOPluginInterface>
{
	public:
	// Constructor
	IOPluginInterface() : ListItem<IOPluginInterface>()
	{
	}
	// Destructor
	virtual ~IOPluginInterface() {}


	/*
	 * Core
	 */
	private:
	// Object store for plugin instances
	List<IOPluginInterface> instances_;

	private:
	// Return a copy of the plugin object
	virtual IOPluginInterface* duplicate() = 0;

	public:
	// Return instance of plugin
	IOPluginInterface* createInstance()
	{
		// Create a copy with duplicate(), and add it to the instances list
		IOPluginInterface* pluginInstance = duplicate();
		instances_.own(pluginInstance);
		return pluginInstance;
	}
	// Delete all instances of plugin
	void deleteInstances()
	{
		instances_.clear();
	}


	/*
	 * Definition
	 */
	public:
	// Return category of plugin
	virtual PluginTypes::IOPluginCategory category() const = 0;
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
	// Return descriptive filter string
	QString filterString() const
	{
		QString exts, exacts, filter = name();
		if (extensions().count() > 0) exts = "*." + extensions().join(",*.");
		if (exactNames().count() > 0) exacts = exactNames().join(",");
		if ((! exts.isEmpty() ) && (! exacts.isEmpty())) filter += " (" + exts + "," + exacts + ")";
		else if (! exts.isEmpty()) filter += " (" + exts + ")";
		else if (! exacts.isEmpty()) filter += " (" + exacts + ")";
		return filter;
	}


	/*
	 * Object Handling
	 */
	private:
	// Model objects created on import
	RefList<Model,int> createdModels_;

	protected:
	// Create new model (in Aten)
	Model* createModel()
	{
		ReturnValue result = CommandNode::run(Commands::NewModel);
		Model* newModel = (Model*) result.asPointer(VTypes::ModelData);
		createdModels_.add(newModel);
		return newModel;
	}

	public:
	// Return main Model objects created on import
	RefList<Model,int> createdModels()
	{
		return createdModels_;
	}


	/*
	 * Input / Output
	 */
	private:
	// Perform secondary checks on whether this plugin relevant to the specified file(name)
	virtual bool isRelatedToFileSecondary(QString filename)
	{
		return false;
	}

	public:
	// Return whether this plugin is related to the specified file(name)
	bool isRelatedToFile(QString filename)
	{
		// Get file information
		QFileInfo fileInfo(filename);
		if ((!fileInfo.exists()) || (!fileInfo.isReadable())) return false;
	
		// Check filename extensions (if the filename has an extension)
		if (!fileInfo.suffix().isEmpty()) for (int n=0; n<extensions().count(); ++n)
		{
			if (extensions().at(n) == fileInfo.suffix())
			{
				Messenger::print(Messenger::Verbose, "IOPluginInterface : Plugin '%s' matches file extension (%s).", qPrintable(name()), qPrintable(fileInfo.suffix()));
				return true;
			}
		}
	
		// Check for exact name matches
		for (int n=0; n<exactNames().count(); ++n)
		{
			if (exactNames().at(n) == fileInfo.fileName())
			{
				Messenger::print(Messenger::Verbose, "IOPluginInterface : Plugin '%s' matched exact name (%s).", qPrintable(name()), qPrintable(exactNames().at(n)));
				return true;
			}
		}
	
		// Perform secondary checks
		if (isRelatedToFileSecondary(filename)) return true;
	
		return false;
	}
	// Return whether this plugin can import data
	virtual bool canImport() = 0;
	// Import data via the supplied parser
	virtual bool importData(FileParser& parser) = 0;
	// Return whether this plugin can export data
	virtual bool canExport() = 0;
	// Export data via the supplied parser
	virtual bool exportData(FileParser& parser) = 0;
};

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

#define IOPluginInterface_iid "com.projectaten.Aten.IOPluginInterface.v1"

Q_DECLARE_INTERFACE(AtenSpace::IOPluginInterface, IOPluginInterface_iid)

#endif
