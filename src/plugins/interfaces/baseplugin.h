/*
        *** Plugin Base Interface
        *** src/plugins/interfaces/baseplugin.h
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

#ifndef ATEN_BASEPLUGININTERFACE_H
#define ATEN_BASEPLUGININTERFACE_H

#include "plugins/plugintypes.h"
#include "model/model.h"
#include "base/kvmap.h"
#include "templates/list.h"
#include <QStringList>

ATEN_BEGIN_NAMESPACE

// Forward Declarations
class PluginStore;

// Base Plugin Interface
class BasePluginInterface : public ListItem<BasePluginInterface>
{
	public:
	// Constructor
	BasePluginInterface() : ListItem<BasePluginInterface>()
	{
		pluginStore_ = NULL;
	}
	// Destructor
	virtual ~BasePluginInterface() {}


	/*
	 * Definition
	 */
	private:
	// Original filename for plugin
	QString pluginFilename_;

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
	// Return type of plugin
	virtual PluginTypes::PluginType type() const = 0;
	// Return category of plugin
	virtual int category() const = 0;
	// Return name of plugin
	virtual QString name() const = 0;
	// Return description (long name) of plugin
	virtual QString description() const = 0;
	// Return nickname of plugin
	virtual QString nickname() const = 0;


	/*
	 * Instance Handling
	 */
	private:
	// Return a copy of the plugin object
	virtual BasePluginInterface* makeCopy() const = 0;

	public:
	// Return a duplicate of the plugin object, including options etc.
	virtual BasePluginInterface* duplicate() const = 0;


	/*
	 * Object Handling
	 */
	private:
	// Parent model objects created on import
	List<Model> createdModels_;
	// Trajectory frames created on import
	RefList<Model,int> createdFrames_;
	// Grid objects created on import
	RefList<Grid,int> createdGrids_;
	// Parent model for read/write, if any
	Model* parentModel_;
	// Target model for read/write, if any
	Model* targetModel_;

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
		Grid* newGrid = model->addGrid();
		createdGrids_.add(newGrid);
		return newGrid;
	}
	// Return Grid objects created on import
	RefList<Grid,int> createdGrids()
	{
		return createdGrids_;
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


	/*
	 * Options
	 */
	protected:
	// Options specific to this plugin
	KVMap pluginOptions_;

	protected:
	// Return conversion of supplied QString to bool
	bool toBool(QString string)
	{
		if ((string.toInt() == 1) || (string.toLower() == "false")) return false;
		return true;
	}

	public:
	// Set plugin option
	bool setOption(QString optionName, QString optionValue)
	{
		// Search for this option in pluginOptions_
		KVPair* pair = pluginOptions_.search(optionName);
		if (pair) pair->setValue(optionValue);
		else
		{
			Messenger::error("Option '" + optionName + "' is not recognised by this plugin.");
			Messenger::error("Available options are: " + pluginOptions_.keys());
			return false;
		}
		return true;
	}
	// Set plugin options
	bool setOptions(KVMap options)
	{
		bool result = true;
		for (KVPair* pair = options.pairs(); pair != NULL; pair = pair->next) if (!setOption(pair->key(), pair->value())) result = false;

		return result;
	}
	// Return options specific to this plugin (read-only)
	const KVMap& pluginOptions()
	{
		return pluginOptions_;
	}


	/*
	 * PluginStore
	 */
	protected:
	// Pointer to main PluginStore
	PluginStore* pluginStore_;

	public:
	// Set pointer to main PluginStore
	void setPluginStore(PluginStore* pluginStore)
	{
		if (pluginStore_) printf("BasePluginInterface - Refusing to set pointer to pluginStore again.\n");
		else pluginStore_ = pluginStore;
	}
	// Return pointer to main PluginStore
	const PluginStore* pluginStore() const
	{
		return pluginStore_;
	}
};

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

#endif
