/*
        *** File Plugin Interface
        *** src/plugins/interfaces/fileplugin.h
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

#ifndef ATEN_FILEPLUGININTERFACE_H
#define ATEN_FILEPLUGININTERFACE_H

#include "plugins/plugintypes.h"
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

// File Plugin Standard Import Options
class FilePluginStandardImportOptions
{
	public:
	// Constructor
	FilePluginStandardImportOptions()
	{
		cacheAll_ = false;
		coordinatesInBohr_ = false;
		keepNames_ = false;
		keepTypes_ = false;
		keepView_ = false;
		preventFolding_ = false;
		preventPacking_ = false;
		preventRebonding_ = false;
		zMappingType_ = ElementMap::AutoZMap;
	}

	private:
	// Whether all trajectory frames are to be cached
	bool cacheAll_;
	// Whether coordinates in file are in Bohr rather than Angstroms
	bool coordinatesInBohr_;
	// Whether original atom type names in file should be kept in a names forcefield associated to the model
	bool keepNames_;
	// Whether original atom type names should be converted into forcefield types and fixed to atoms
	bool keepTypes_;
	// Whether view should not be reset when GUI starts
	bool keepView_;
	// Whether folding should be prevented
	bool preventFolding_;
	// Whether packing should be prevented
	bool preventPacking_;
	// Whether rebonding should be prevented
	bool preventRebonding_;
	// Z-mapping to use in atom name conversion
	ElementMap::ZMapType zMappingType_;

	public:
	// Set whether all trajectory frames are to be cached
	bool setCacheAll(bool value)
	{
		cacheAll_ = value;
	}
	// Return whether all trajectory frames are to be cached
	bool cacheAll() const
	{
		return cacheAll_;
	}
	// Whether coordinates in file are in Bohr rather than Angstroms
	void setCoordinatesInBohr(bool value)
	{
		coordinatesInBohr_ = value;
	}
	// Whether coordinates in file are in Bohr rather than Angstroms
	bool coordinatesInBohr()
	{
		return coordinatesInBohr_;
	}
	// Set whether original atom type names in file should be kept in a names forcefield associated to the model
	bool setKeepNames(bool value)
	{
		keepNames_ = value;
	}
	// Return whether original atom type names in file should be kept in a names forcefield associated to the model
	bool keepNames() const
	{
		return keepNames_;
	}
	// Set whether original atom type names should be converted into forcefield types and fixed to atoms
	bool setKeepTypes(bool value)
	{
		keepTypes_ = value;
	}
	// Return whether original atom type names should be converted into forcefield types and fixed to atoms
	bool keepTypes() const
	{
		return keepTypes_;
	}
	// Set whether view should not be reset when GUI starts
	bool setKeepView(bool value)
	{
		keepView_ = value;
	}
	// Return whether view should not be reset when GUI starts
	bool keepView() const
	{
		return keepView_;
	}
	// Set whether folding should be prevented
	void setPreventFolding(bool value)
	{
		preventFolding_ = value;
	}
	// Return whether folding should be prevented
	bool preventFolding() const
	{
		return preventFolding_;
	}
	// Set whether packing should be prevented
	void setPreventPacking(bool value)
	{
		preventPacking_ = value;
	}
	// Return whether packing should be prevented
	bool preventPacking() const
	{
		return preventPacking_;
	}
	// Set whether rebonding should be prevented
	void setPreventRebonding(bool value)
	{
		preventRebonding_ = value;
	}
	// Return whether rebonding should be prevented
	bool preventRebonding() const
	{
		return preventRebonding_;
	}
	// Set Z-mapping to use in atom name conversion
	void setZMappingType(ElementMap::ZMapType zMapType)
	{
		zMappingType_ = zMapType;
	}
	// Return Z-mapping to use in atom name conversion
	ElementMap::ZMapType zMappingType() const
	{
		return zMappingType_;
	}
};

// File Plugin Interface
class FilePluginInterface : public ListItem<FilePluginInterface>
{
	public:
	// Constructor
	FilePluginInterface() : ListItem<FilePluginInterface>(), fileParser_(lineParser_)
	{
		// Import / Export
		nDataParts_ = 0;
		nDataPartsEstimated_ = false;
		lastDataPartRead_ = -1;
	}
	// Destructor
	virtual ~FilePluginInterface() {}


	/*
	 * Core
	 */
	private:
	// Original filename for plugin
	QString pluginFilename_;
	// Object store for plugin instances
	List<FilePluginInterface> instances_;
	// Core LineParser object
	LineParser lineParser_;

	private:
	// Return a copy of the plugin object
	virtual FilePluginInterface* duplicate() = 0;

	protected:
	// File parser object, associated to LineParser
	FileParser fileParser_;

	public:
	// Set filename for plugin
	void setPluginFilename(QString filename)
	{
		pluginFilename_ = filename;
	}
	// Return filanem for plugin
	QString pluginFilename() const
	{
		return pluginFilename_;
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
	// Open specified file for input
	bool openInput(QString filename)
	{
		lineParser_.openInput(filename);
		if (!lineParser_.isFileGoodForReading())
		{
			Messenger::error("Couldn't open file '" + filename + "' for reading.");
			return false;
		}
		return true;
	}
	// Open specified file for output
	bool openOutput(QString filename)
	{
		lineParser_.openOutput(filename);
		if (!lineParser_.isFileGoodForWriting())
		{
			Messenger::error("Couldn't open file '" + filename + "' for writing.");
			return false;
		}
		return true;
	}
	// Close file(s)
	void closeFiles()
	{
		lineParser_.closeFiles();
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
	// Parent model objects created on import
	List<Model> createdModels_;
	// Grid objects created on import
	RefList<Grid,int> createdGrids_;
	// Target model for read/write, if any
	Model* targetModel_;
	// Target frame for read/write, if any
	Model* targetFrame_;

	protected:
	// Create new model
	Model* createModel(QString name = QString())
	{
		Model* newModel = createdModels_.add();
		if (!name.isEmpty()) newModel->setName(name);
		return newModel;
	}
	// Create new atom in specified model
	Atom* createAtom(Model* model, QString name, Vec3<double> r = Vec3<double>())
	{
		// Find element in elements map
		int el = Elements().find(name, standardOptions_.zMappingType());

		// Add atom
		Atom* i = model->addAtom(el, r);

		// KeepNames and KeepTypes standard options
		ForcefieldAtom* ffa = NULL;
		if (standardOptions_.keepNames()) ffa = model->addAtomName(el, name);
		else if (standardOptions_.keepTypes()) ffa = Elements().forcefieldAtom(name);
		if (ffa != NULL)
		{
			i->setType(ffa);
			if (ffa != NULL) i->setTypeFixed(true);
		}

		return i;
	}
	// Discard created model
	bool discardModel(Model* model)
	{
		if (createdModels_.contains(model))
		{
			createdModels_.remove(model);
			return true;
		}
		Messenger::error("Can't discard model - not owned by the interface.");
		return false;
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
	// Return parent Model objects created on import
	List<Model>& createdModels()
	{
		return createdModels_;
	}
	// Return Grid objects created on import
	RefList<Grid,int> createdGrids()
	{
		return createdGrids_;
	}
	// Set target model
	void setTargetModel(Model* model)
	{
		targetModel_ = model;
	}
	// Return target model
	Model* targetModel() const
	{
		return targetModel_;
	}
	// Set target frame
	void setTargetFrame(Model* frame)
	{
		targetFrame_ = frame;
	}
	// Return target frame
	Model* targetFrame() const
	{
		return targetFrame_;
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
	Array<std::streampos> dataPartOffsets_;
	// Number of partial data present in file
	int nDataParts_;
	// Whether the number of partial data present in the file is estimated
	bool nDataPartsEstimated_;
	// Index of last partial data read in
	int lastDataPartRead_;

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
	virtual bool importData() = 0;
	// Return whether this plugin can export data
	virtual bool canExport() = 0;
	// Export data via the supplied parser
	virtual bool exportData() = 0;
	// Import next partial data chunk
	virtual bool importNextPart() = 0;
	// Skip next partial data chunk
	virtual bool skipNextPart() = 0;
	// Import partial data chunk specified
	bool importPart(int partId)
	{
		// First check (sanity) - are there any file positions stored in the array?
		if (dataPartOffsets_.nItems() == 0)
		{
			// If the requested partId is the first part (0) then store the current file position and read it in
			if (partId == 0)
			{
				dataPartOffsets_.add(lineParser_.tellg());
				bool result = importNextPart();
				if (result)
				{
					// Add offset for the second datum
					dataPartOffsets_.add(lineParser_.tellg());

					// Estimate total number of parts
					// First, get data size from difference between file positions for zeroth and first parts
					long int partSize = dataPartOffsets_.last() - dataPartOffsets_.first();
					if ((partSize/1024) < 10) Messenger::print("Single data is %i bytes.", partSize);
					else Messenger::print("Single data is (approximately) %i kb.", partSize/1024);

					// Now, skip to end of file to get file size, and estimate number of parts
					lineParser_.seekg(0, std::ios::end);
					std::streampos endOfFilePos = lineParser_.tellg();
					nDataParts_ = (endOfFilePos - dataPartOffsets_.first()) / partSize;
					nDataPartsEstimated_ = true;
					lineParser_.seekg( dataPartOffsets_.last());
				}
				return result;
			}
			else
			{
				Messenger::error("Can't import part - no data positions stored in plugin.");
				return false;
			}
		}

		// So, we have some file positions - is the requested partId within the stored range?
		if ((partId >= 0) && (partId < dataPartOffsets_.nItems()))
		{
			Messenger::print(Messenger::Verbose, "Requested partId is within stored range.");

			// Seek to the stored file position and read the data
			lineParser_.seekg( dataPartOffsets_.value(partId));
			return importNextPart();
		}

		// Requested partId not in file seek table, so go to last known position and try to find it
		int currentId = dataPartOffsets_.nItems() - 1;
		lineParser_.seekg( dataPartOffsets_.last());
		do
		{
			bool result = skipNextPart();
			if (result)
			{
				// Successfully skipped the data, so store the file position
				dataPartOffsets_.add(lineParser_.tellg());
			}
			else
			{
				Messenger::print("Failed to skip to specified part (%i).", partId);
				Messenger::print("Last good data read was id %i.", currentId);

				// Update the number of stored parts
				nDataParts_ = currentId;
				nDataPartsEstimated_ = false;
				return false;
			}
			++currentId;
		} while (currentId < partId);

		// Now at correct file position, so read data proper
		bool result = importNextPart();
		if (result)
		{
			// Now at start of next data, so store the file position
			dataPartOffsets_.add(lineParser_.tellg());
		}
		else
		{
			Messenger::print("Failed to read part '%i' from file.", partId);
		}

		return result;
	}
	// Return number of data parts present in file
	int nDataParts() const
	{
		return nDataParts_;
	}
	// Return whether the number of partial data present in the file is estimated
	bool isNPartialDataEstimated() const
	{
		return nDataPartsEstimated_;
	}
	// Return index of last partial data read in
	int lastPartialDataRead() const
	{
		return lastDataPartRead_;
	}


	/*
	 * Additional Functions / Data
	 */
	protected:
	// Standard options
	FilePluginStandardImportOptions standardOptions_;
	// Plugin Option Keywords
	QStringList pluginOptionKeywords_;

	protected:
	// Return enum'd plugin option from supplied keyword
	virtual int pluginOption(QString optionName) = 0;
	// Return conversion of supplied QString to bool
	bool toBool(QString string)
	{
		if ((string.toInt() == 1) || (string.toLower() == "false")) return false;
		return true;
	}

	public:
	// Set standard options
	void setStandardOptions(const FilePluginStandardImportOptions& standardOptions)
	{
		standardOptions_ = standardOptions;
	}
	// Return standard options for plugin
	const FilePluginStandardImportOptions standardOptions() const
	{
		return standardOptions_;
	}
	// Set option in plugin
	virtual bool setOption(QString optionName, QString optionValue) = 0;
	// Set options for plugin
	bool setOptions(KVMap options)
	{
		bool result = true;
		for (KVPair* pair = options.pairs(); pair != NULL; pair = pair->next) if (!setOption(pair->key(), pair->value())) result = false;

		return result;
	}
};

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

#define FilePluginInterface_iid "com.projectaten.Aten.FilePluginInterface.v1"

Q_DECLARE_INTERFACE(AtenSpace::FilePluginInterface, FilePluginInterface_iid)

#endif
