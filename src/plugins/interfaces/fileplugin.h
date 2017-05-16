/*
        *** File Plugin Interface
        *** src/plugins/interfaces/fileplugin.h
        Copyright T. Youngs 2007-2017

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

#include "plugins/interfaces/baseplugin.h"
#include "parser/returnvalue.h"
#include "parser/commandnode.h"
#include "model/model.h"
#include "base/kvmap.h"
#include "base/forcefieldatom.h"
#include "base/messenger.h"
#include "base/fileparser.h"
#include "base/namespace.h"
#include "base/grid.h"
#include "ff/forcefield.h"
#include "templates/reflist.h"
#include <QStringList>
#include <QtPlugin>
#include <QFileInfo>

ATEN_BEGIN_NAMESPACE

// Forward Declarations
/* none */

// File Plugin Standard Import Options
class FilePluginStandardImportOptions
{
	public:
	// Constructor
	FilePluginStandardImportOptions()
	{
		// All switches default to 'unset'
		for (int n=0; n<FilePluginStandardImportOptions::nImportSwitches; ++n) switches_[n] = -1;
		zMappingType_ = ElementMap::nZMapTypes;
	}
	

	/* 
	 * Switches
	 */
	public:
	// Boolean switches
	enum ImportSwitch {
		CacheAllSwitch,			// Whether all trajectory frames are to be cached
		CoordinatesInBohrSwitch,	// Whether coordinates in file are in Bohr rather than Angstroms
		ForceRhombohedralSwitch,	// Whether rhombohedral (over hexagonal) spacegroup basis is to be forced
		InheritStyleSwitch,		// Whether to inherit style of parent model (for trajectory frames)
		KeepNamesSwitch,		// Whether original atom type names in file should be kept in a names forcefield associated to the model
		KeepTypesSwitch,		// Whether original atom type names should be converted into forcefield types and fixed to atoms
		KeepViewSwitch,			// Whether view should not be reset when GUI starts
		PreventFoldingSwitch,		// Whether folding should be prevented
		PreventPackingSwitch,		// Whether packing should be prevented
		PreventRebondingSwitch,		// Whether rebonding should be prevented
		nImportSwitches
	};

	private:
	// Switch states
	int switches_[FilePluginStandardImportOptions::nImportSwitches];

	public:
	// Set switch status
	void setSwitch(FilePluginStandardImportOptions::ImportSwitch sw, bool status)
	{
		switches_[sw] = (status ? 1 : 0);
	}
	// Return if switch is set and on
	bool isSetAndOn(FilePluginStandardImportOptions::ImportSwitch sw) const
	{
		return (switches_[sw] == 1);
	}
	// Return if switch is off
	bool isSetAndOff(FilePluginStandardImportOptions::ImportSwitch sw) const
	{
		return (switches_[sw] == 0);
	}


	/*
	 * Specific Values
	 */
	private:
	// Z-mapping to use in atom name conversion
	ElementMap::ZMapType zMappingType_;

	public:
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


	/*
	 * Functions
	 */
	public:
	// Apply settings in supplied object to this one
	void apply(const FilePluginStandardImportOptions& other)
	{
		// Switches
		for (int n=0; n<nImportSwitches; ++n) if (other.switches_[n] != -1) switches_[n] = other.switches_[n];

		// Specific values
		if (other.zMappingType_ != ElementMap::nZMapTypes) zMappingType_ = other.zMappingType_;
	}
};

// File Plugin Standard Export Options
class FilePluginStandardExportOptions
{
	public:
	// Constructor
	FilePluginStandardExportOptions()
	{
	}
};

// File Plugin Interface
class FilePluginInterface : public BasePluginInterface
{
	public:
	// Constructor
	FilePluginInterface() : fileParser_(lineParser_)
	{
		// Import / Export
		nDataParts_ = 0;
		nDataPartsEstimated_ = false;
	}
	// Destructor
	virtual ~FilePluginInterface() {}


	/*
	 * Instance Handling
	 */
	private:
	// Return a copy of the plugin object (provided by main plugin)
	virtual BasePluginInterface* makeCopy() const = 0;

	public:
	// Return a duplicate of the plugin object, including options etc.
	BasePluginInterface* duplicate() const
	{
		FilePluginInterface* copy = (FilePluginInterface*) makeCopy();
		copy->setPluginStore(pluginStore_);
		copy->applyStandardOptions(standardOptions_);
		copy->setOptions(pluginOptions_);
		return copy;
	}


	/*
	 * Object Handling
	 */
	private:
	// Parent model objects created on import
	List<Model> createdModels_;
	// Parent model for read/write, if any
	Model* parentModel_;
	// Target model for read/write, if any
	Model* targetModel_;
	// Trajectory frames created on import
	RefList<Model,int> createdFrames_;
	// Grid objects created on import
	List<Grid> createdGrids_;
	// Forcefield objects create on import
	List<Forcefield> createdForcefields_;
	// Target forcefield for read/write, if any
	Forcefield* targetForcefield_;

	public:
	// Create new parent model
	Model* createModel(QString name = QString())
	{
		Model* newModel = createdModels_.add();
		if (!name.isEmpty()) newModel->setName(name);
		setParentModel(newModel);
		return newModel;
	}
	// Discard created model
	bool discardModel(Model* model)
	{
		if (createdModels_.contains(model))
		{
			if ((targetModel_ == model) || (parentModel_ == model)) targetModel_ = NULL;
			if (parentModel_ == model) parentModel_ = NULL;
			createdModels_.remove(model);
			return true;
		}
		Messenger::error("Can't discard model - not owned by the interface.");
		return false;
	}
	// Return parent Model objects created on import
	List<Model>& createdModels()
	{
		return createdModels_;
	}
	// Set parent model
	void setParentModel(Model* model)
	{
		parentModel_ = model;
		targetModel_ = parentModel_;
	}
	// Return parent model
	Model* parentModel() const
	{
		return parentModel_;
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
	// Create frame in parent model
	Model* createFrame()
	{
		Model* frame = parentModel()->addTrajectoryFrame();
		createdFrames_.add(frame);
		targetModel_ = frame;
		return frame;
	}
	// Discard created frame
	bool discardFrame(Model* frame)
	{
		if (createdFrames_.contains(frame))
		{
			if (targetModel_ == frame) targetModel_ = parentModel_;
			parentModel_->removeTrajectoryFrame(frame);
			createdFrames_.remove(frame);
			return true;
		}
		Messenger::error("Can't discard frame - not owned by the interface.");
		return false;
	}
	// Return created frames
	RefList<Model,int>& createdFrames()
	{
		return createdFrames_;
	}
	// Create new grid (in specified model)
	Grid* createGrid(Model* model)
	{
		Grid* newGrid = createdGrids_.add();
		newGrid->setParent(model);
		return newGrid;
	}
	// Return Grid objects created on import
	List<Grid>& createdGrids()
	{
		return createdGrids_;
	}

	// Create new forcefield
	Forcefield* createForcefield(QString name = QString())
	{
		targetForcefield_ = createdForcefields_.add();
		if (!name.isEmpty()) targetForcefield_->setName(name);
		return targetForcefield_;
	}
	// Discard created forcefield
	bool discardForcefield(Forcefield* forcefield)
	{
		if (createdForcefields_.contains(forcefield))
		{
			if (targetForcefield_ == forcefield) targetForcefield_ = NULL;
			createdForcefields_.remove(forcefield);
			return true;
		}
		Messenger::error("Can't discard forcefield - not owned by the interface.");
		return false;
	}
	// Return parent Forcefield objects created on import
	List<Forcefield>& createdForcefields()
	{
		return createdForcefields_;
	}
	// Set target forcefield
	void setTargetForcefield(Forcefield* ff)
	{
		targetForcefield_ = ff;
	}
	// Target forcefield for read/write, if any
	Forcefield* targetForcefield() const
	{
		return targetForcefield_;
	}
	// Create new atom in specified model
	Atom* createAtom(Model* model, QString name = "XX", Vec3<double> r = Vec3<double>(), Vec3<double> v = Vec3<double>(), Vec3<double> f = Vec3<double>())
	{
		// Find element in elements map
		int el = ElementMap::find(name, standardOptions_.zMappingType() != ElementMap::nZMapTypes ? standardOptions_.zMappingType() : ElementMap::AutoZMap);

		// Add atom
		Atom* i = model->addAtom(el, r, v, f);
		i->setData(qPrintable(name));

		// KeepNames and KeepTypes standard options
		ForcefieldAtom* ffa = NULL;
		if (standardOptions_.isSetAndOn(FilePluginStandardImportOptions::KeepNamesSwitch)) ffa = model->addAtomName(el, name);
		else if (standardOptions_.isSetAndOn(FilePluginStandardImportOptions::KeepTypesSwitch)) ffa = ElementMap::forcefieldAtom(name);
		if (ffa != NULL)
		{
			i->setType(ffa);
			i->setTypeFixed(true);
		}

		return i;
	}
	// Clear any created data
	void clearCreatedData()
	{
		createdModels_.clear();
		// Don't need to delete individual frame data, since they should have been removed with their parent model
		createdFrames_.clear();
		createdGrids_.clear();
		createdForcefields_.clear();
	}


	/*
	 * File Handling
	 */
	private:
	// Core LineParser object
	LineParser lineParser_;

	protected:
	// File parser object
	FileParser fileParser_;

	public:
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
	 * Additional Definition
	 */
	public:
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

	public:
	// Return whether this plugin is related to the specified file(name)
	bool isRelatedToFile(QString filename)
	{
		// Get file information
		QFileInfo fileInfo(filename);
	
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
	virtual bool canImport() const = 0;
	// Import data via the supplied parser
	virtual bool importData() = 0;
	// Return whether this plugin can export data
	virtual bool canExport() const = 0;
	// Export data via the supplied parser
	virtual bool exportData() = 0;
	// Import next partial data chunk
	virtual bool importNextPart() = 0;
	// Skip next partial data chunk
	virtual bool skipNextPart() = 0;
	// Set the number of data parts in the file explicitly
	void setNDataParts(int nParts)
	{
		if (nDataParts_ > 0) Messenger::warn("Number of data parts already set / estimated.");
		nDataParts_ = nParts;
		nDataPartsEstimated_ = false;
	}
	// Import partial data chunk specified
	bool importPart(int partId)
	{
		Messenger::print(Messenger::Verbose, "FilePluginInterface::importPart() - trying to import part %i from file.", partId);

		// First check (sanity) - are there any file positions stored in the array?
		if (dataPartOffsets_.nItems() == 0)
		{
			Messenger::print(Messenger::Verbose, "FilePluginInterface::importPart() - no part offset currently stored...");
			// If the requested partId is the first part (0) then store the current file position and read it in
			if (partId == 0)
			{
				dataPartOffsets_.add(lineParser_.tellg());
				bool result = importNextPart();
				if (result)
				{
					// Copy style if requested
					if (standardOptions_.isSetAndOn(FilePluginStandardImportOptions::InheritStyleSwitch))
					{
						Model* frame = (createdFrames_.last() ? createdFrames_.last()->item : NULL);
						if (frame) frame->copyAtomStyle(parentModel_);
					}

					// Add offset for the second datum
					dataPartOffsets_.add(lineParser_.tellg());

					// Estimate total number of parts if we have not already been told the number to expect
					if (nDataParts_ == 0)
					{
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
				}

				Messenger::print(Messenger::Verbose, "FilePluginInterface::importPart() - result of initial part read was %i, nOffsets now %i.", result, dataPartOffsets_.nItems());

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
			Messenger::print(Messenger::Verbose, "FilePluginInterface::importPart() - requested part (%i) is within stored offset range (%i total).", partId, dataPartOffsets_.nItems());

			// Seek to the stored file position and read the data
			lineParser_.seekg(dataPartOffsets_.value(partId));
			bool result = importNextPart();

			// If successful, add the offset for the next part if this was the last offset we had
			if (result && ((partId+1) == dataPartOffsets_.nItems())) dataPartOffsets_.add(lineParser_.tellg());

			// Copy style if requested
			if (result && standardOptions_.isSetAndOn(FilePluginStandardImportOptions::InheritStyleSwitch))
			{
				Model* frame = (createdFrames_.last() ? createdFrames_.last()->item : NULL);
				if (frame) frame->copyAtomStyle(parentModel_);
			}

			return result;
		}

		// Requested partId not in file seek table, so go to last known position and step towards it
		Messenger::print(Messenger::Verbose, "FilePluginInterface::importPart() - requested part (%i) is not within stored offset range (%i total).", partId, dataPartOffsets_.nItems());
		int currentId = dataPartOffsets_.nItems() - 1;
		lineParser_.seekg(dataPartOffsets_.last());
		while (currentId < partId)
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

				// Update the number of stored parts to reflect the read error (early termination of the file perhaps?)
				nDataParts_ = currentId;
				nDataPartsEstimated_ = false;
				return false;
			}
			++currentId;
		}

		Messenger::print(Messenger::Verbose, "Now at position %i.", currentId);
		// Now at correct file position, so read data proper
		bool result = importNextPart();
		if (result)
		{
			// Now at start of next data, so store the file position
			dataPartOffsets_.add(lineParser_.tellg());

			// Copy style if requested
			if (standardOptions_.isSetAndOn(FilePluginStandardImportOptions::InheritStyleSwitch))
			{
				Model* frame = (createdFrames_.last() ? createdFrames_.last()->item : NULL);
				if (frame) frame->copyAtomStyle(parentModel_);
			}
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


	/*
	 * Options
	 */
	protected:
	// Standard options
	FilePluginStandardImportOptions standardOptions_;

	public:
	// Return whether the plugin has import options
	virtual bool hasImportOptions() const = 0;
	// Show import options dialog, setting values in the specified KVMap
	virtual bool showImportOptionsDialog(KVMap& targetOptions) const = 0;
	// Return whether the plugin has export options
	virtual bool hasExportOptions() const = 0;
	// Show export options dialog, setting values in the specified KVMap
	virtual bool showExportOptionsDialog(KVMap& targetOptions) const = 0;
	// Apply standard options
	void applyStandardOptions(const FilePluginStandardImportOptions& standardOptions)
	{
		standardOptions_.apply(standardOptions);
	}
	// Set a standard option
	void setStandardOption(FilePluginStandardImportOptions::ImportSwitch iswitch, bool status)
	{
		standardOptions_.setSwitch(iswitch, status);
	}
	// Return standard options for plugin
	const FilePluginStandardImportOptions standardOptions() const
	{
		return standardOptions_;
	}
};

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

#define FilePluginInterface_iid "com.projectaten.Aten.FilePluginInterface.v1"

Q_DECLARE_INTERFACE(AtenSpace::FilePluginInterface, FilePluginInterface_iid)

#endif
