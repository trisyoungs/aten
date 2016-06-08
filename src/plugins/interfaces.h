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

#ifndef ATEN_PLUGININTERFACES_H
#define ATEN_PLUGININTERFACES_H

#include "plugins/plugintypes.h"
#include "parser/returnvalue.h"
#include "parser/commandnode.h"
#include "model/model.h"
#include "base/grid.h"
#include "base/kvmap.h"
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

// File Plugin Interface
class FilePluginInterface : public ListItem<FilePluginInterface>
{
	public:
	// Constructor
	FilePluginInterface() : ListItem<FilePluginInterface>()
	{
	}
	// Destructor
	virtual ~FilePluginInterface() {}


	/*
	 * Core
	 */
	private:
	// Original filename for plugin
	QString filename_;
	// Object store for plugin instances
	List<FilePluginInterface> instances_;

	private:
	// Return a copy of the plugin object
	virtual FilePluginInterface* duplicate() = 0;

	public:
	// Set filename for plugin
	void setFilanem(QString filename)
	{
		filename_ = filename;
	}
	// Return filanem for plugin
	QString filename() const
	{
		return filename_;
	}
	// Return instance of plugin
	FilePluginInterface* createInstance()
	{
		// Create a copy with duplicate(), and add it to the instances list
		FilePluginInterface* pluginInstance = duplicate();
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
	virtual PluginTypes::FilePluginCategory category() const = 0;
	// Return name of plugin
	virtual QString name() const = 0;
	// Return nickname of plugin
	virtual QString nickname() const = 0;
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
	// Grid objects created on import
	RefList<Grid,int> createdGrids_;

	protected:
	// Create new model (in Aten)
	Model* createModel(QString name = QString())
	{
		ReturnValue result = CommandNode::run(Commands::NewModel, "c", qPrintable(name));
		Model* newModel = (Model*) result.asPointer(VTypes::ModelData);
		newModel->disableUndoRedo();
		createdModels_.add(newModel);
		return newModel;
	}
	// Create new grid (in target model)
	Grid* createGrid()
	{
		ReturnValue result = CommandNode::run(Commands::NewGrid);
		Grid* newGrid = (Grid*) result.asPointer(VTypes::GridData);
		createdGrids_.add(newGrid);
		return newGrid;
	}

	public:
	// Return main Model objects created on import
	RefList<Model,int> createdModels()
	{
		return createdModels_;
	}
	// Return Grid objects created on import
	RefList<Grid,int> createdGrids()
	{
		return createdGrids_;
	}


	/*
	 * Interface / Standard Options
	 */
	public:
	// Set options for plugin
	virtual bool setOptions(KVMap options)
	{
		return false;
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
	// File offsets for partial datum
	Array<std::streampos> partialDataOffsets_;

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
				Messenger::print(Messenger::Verbose, "FilePluginInterface : Plugin '%s' matches file extension (%s).", qPrintable(name()), qPrintable(fileInfo.suffix()));
				return true;
			}
		}
	
		// Check for exact name matches
		for (int n=0; n<exactNames().count(); ++n)
		{
			if (exactNames().at(n) == fileInfo.fileName())
			{
				Messenger::print(Messenger::Verbose, "FilePluginInterface : Plugin '%s' matched exact name (%s).", qPrintable(name()), qPrintable(exactNames().at(n)));
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
	virtual bool importData(FileParser& parser, const KVMap standardOptions = KVMap()) = 0;
	// Return whether this plugin can export data
	virtual bool canExport() = 0;
	// Export data via the supplied parser
	virtual bool exportData(FileParser& parser, const KVMap standardOptions = KVMap()) = 0;
	// Import next partial data chunk
	virtual bool importNextPart(FileParser& parser, const KVMap standardOptions = KVMap()) = 0;
	// Skip next partial data chunk
	virtual bool skipNextPart(FileParser& parser, const KVMap standardOptions = KVMap()) = 0;
	// Import partial data chunk specified
	bool importPart(int partId, FileParser& parser, const KVMap standardOptions = KVMap())
	{
		// First check (sanity) - are there any file positions stored in the array?
		if (partialDataOffsets_.nItems() == 0)
		{
			// If the requested partId is the first part (0) then store the current file position and read it in
			if (partId == 0)
			{
				partialDataOffsets_.add(parser.tellg());
				bool result = importNextPart(parser, standardOptions);
				if (result) partialDataOffsets_.add(parser.tellg());
				return result;
			}
			else
			{
				Messenger::error("Can't import part - no data positions stored in plugin.");
				return false;
			}
		}

		// So, we have some file positions - is the requested partId within the stored range?
		if ((partId >= 0) && (partId < partialDataOffsets_.nItems()))
		{
			Messenger::print(Messenger::Verbose, "Requested partId is within stored range.");

			// Seek to the stored file position and read the data
			parser.seekg(partialDataOffsets_.value(partId));
			return importNextPart(parser, standardOptions);
		}

		// Requested partId not in file seek table, so go to last known position and try to find it
		int currentId = partialDataOffsets_.nItems() - 1;
		parser.seekg(partialDataOffsets_.last());
		do
		{
			bool result = skipNextPart(parser, standardOptions);
			if (result)
			{
				// Successfully skipped the data, so store the file position
				partialDataOffsets_.add(parser.tellg());
			}
			else
			{
				Messenger::print("Failed to skip to specified part (%i).", partId);
				Messenger::print("Last good data read was id %i.", currentId);
				return false;
			}
			++currentId;
		} while (currentId < partId);

		// Now at correct file position, so read data proper
		bool result = importNextPart(parser, standardOptions);
		if (result)
		{
			// Now at start of next data, so store the file position
			partialDataOffsets_.add(parser.tellg());
		}
		else
		{
			Messenger::print("Failed to read part '%s' from file.", partId);
		}

		return result;
	}
};

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

#define FilePluginInterface_iid "com.projectaten.Aten.FilePluginInterface.v1"

Q_DECLARE_INTERFACE(AtenSpace::FilePluginInterface, FilePluginInterface_iid)

#endif
