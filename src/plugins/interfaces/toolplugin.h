/*
        *** Tool Plugin Interface
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

#ifndef ATEN_TOOLPLUGININTERFACE_H
#define ATEN_TOOLPLUGININTERFACE_H

#include "plugins/interfaces/baseplugin.h"
#include "model/model.h"
#include "base/kvmap.h"
#include "base/messenger.h"
#include "base/namespace.h"
#include "templates/reflist.h"
#include <QStringList>
#include <QtPlugin>

ATEN_BEGIN_NAMESPACE

// Forward Declarations
/* none */

// Tool Plugin Interface
class ToolPluginInterface : public BasePluginInterface
{
	public:
	// Constructor
	ToolPluginInterface()
	{
		currentModelOrFrame_ = NULL;
	}
	// Destructor
	virtual ~ToolPluginInterface() {}


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
		ToolPluginInterface* copy = (ToolPluginInterface*) makeCopy();
		copy->setPluginStore(pluginStore_);
		copy->setOptions(pluginOptions_);
		return copy;
	}


	/*
	 * Object Handling
	 */
	private:
	// Current model or frame
	Model* currentModelOrFrame_;
	// List of all loaded models
	RefList<Model,bool> allModels_;

	public:
	// Set current model or frame
	void setCurrentModelOrFrame(Model* model)
	{
		currentModelOrFrame_ = model;
	}
	// Return current model or frame
	Model* currentModelOrFrame() const
	{
		return currentModelOrFrame_;
	}
	// Clear all loaded models
	void clearAllModels()
	{
		currentModelOrFrame_ = NULL;
		allModels_.clear();
	}
	// Add loaded model
	void addModel(Model* model)
	{
		allModels_.add(model);
	}
	// Return all loaded models
	const RefList<Model,bool>& allModels()
	{
		return allModels_;
	}


	/*
	 * Tool Definition
	 */
	protected:
	// Dialog for tool (if there is one)
	QDialog* dialog_;

	public:
	// Return button label to use in GUI
	virtual QString buttonLabel() const = 0;
	// Return icon for button in GUI
	virtual QIcon buttonIcon() const = 0;
	// Return group name for tool (used to group similar tools together)
	virtual QString groupName() const = 0;
	// Return whether the tool has a dialog
	virtual bool hasDialog() const = 0;
	// Show the dialog for the tool
	virtual void showDialog() = 0;
	// Run the tool with the current settings
	virtual bool runTool() = 0;


	/*
	 * QObject / Signals
	 */
	public:
	// Return interface as QObject
	virtual QObject* object() = 0;

	// Derived classes must implement the following signals:
	//   void updateWidgets(int);
};

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

#define ToolPluginInterface_iid "com.projectaten.Aten.ToolPluginInterface.v1"

Q_DECLARE_INTERFACE(AtenSpace::ToolPluginInterface, ToolPluginInterface_iid)

#endif
