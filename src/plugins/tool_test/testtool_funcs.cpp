/*
        *** Test Tool Functions
        *** src/plugins/tool_test/testtool_funcs.cpp
        Copyright T. Youngs 2016-2017

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

#include "plugins/tool_test/testtool.hui"
#include "plugins/tool_test/testtooldialog.h"
#include "model/model.h"

// Constructor
TestToolPlugin::TestToolPlugin()
{
	// Setup plugin options
	pluginOptions_.add("randomise", "false");
	pluginOptions_.add("element", "C");
	pluginOptions_.add("applyToAll", "false");

	// Create dialog if the tool has one
	if (hasDialog()) dialog_ = new TestToolDialog(*this, pluginOptions_);
	else dialog_ = NULL;
}

// Destructor
TestToolPlugin::~TestToolPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* TestToolPlugin::makeCopy() const
{
	return new TestToolPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType TestToolPlugin::type() const
{
	return PluginTypes::ToolPlugin;
}

// Return category of plugin
int TestToolPlugin::category() const
{
	return PluginTypes::GeneralToolPlugin;
}

// Name of plugin
QString TestToolPlugin::name() const
{
	return QString("Test Tool Plugin");
}

// Nickname of plugin
QString TestToolPlugin::nickname() const
{
	return QString("testtool");
}

// Return whether the plugin is enabled
bool TestToolPlugin::enabled() const
{
	return false;
}

// Description (long name) of plugin
QString TestToolPlugin::description() const
{
	return QString("Test tool to illustrate implementation of simple tool with a dialog");
}

/*
 * Tool Definition
 */

// Return button label to use in GUI
QString TestToolPlugin::buttonLabel() const
{
	return QString("Test");
}

// Return icon for button in GUI
QIcon TestToolPlugin::buttonIcon() const
{
	return QIcon(":/testtool_icons/icon.svg");
}

// Return group name for tool (used to group similar tools together)
QString TestToolPlugin::groupName() const
{
	return QString("Test");
}

// Return whether the tool is enabled (appears in the GUI)
bool TestToolPlugin::isEnabled() const
{
	return true;
}

// Return whether the tool has a dialog
bool TestToolPlugin::hasDialog() const
{
	return true;
}

// Show the dialog for the tool
void TestToolPlugin::showDialog()
{
	// Check if a dialog actually exists
	if (dialog_ == NULL)
	{
		Messenger::error("No dialog is associated to the tool '%s'\n", qPrintable(name()));
		return;
	}

	// Cast the dialog_ pointer into our custom class
	TestToolDialog* testToolDialog = (TestToolDialog*) dialog_;
	if (!testToolDialog)
	{
		Messenger::error("Error casting tool dialog into custom class for the tool '%s'\n", qPrintable(name()));
	}
	testToolDialog->applyPluginOptions();
	testToolDialog->exec();
}

// Run the tool with the current settings
bool TestToolPlugin::runTool()
{
	// Get the target models
	RefList<Model,bool> targets;
	if (pluginOptions_.value("applyToAll") == "true") targets = allModels();
	else targets.add(currentModelOrFrame());

	bool randomise = pluginOptions_.value("randomise") == "true";
	int el = ElementMap::find(pluginOptions_.value("element"));

	// Loop over targets
	for (RefListItem<Model,bool>* ri = targets.first(); ri != NULL; ri = ri->next)
	{
		Model* model = ri->item;
		if (!model) continue;

		// Start an undostate
		if (randomise) model->beginUndoState("Randomise all elements");
		else model->beginUndoState("Change all elements to %s\n", ElementMap::symbol(el));

		if (randomise)
		{
			for (Atom* i = model->atoms(); i != NULL; i = i->next)
			{
				el = AtenMath::randomi(ElementMap::nElements()) + 1;
				printf("El = %i.\n", el);
				model->transmuteAtom(i, el);
			}
		}
		else
		{
			for (Atom* i = model->atoms(); i != NULL; i = i->next) model->transmuteAtom(i, el);
		}

		// End the undo state
		model->endUndoState();
	}

	// Update the display
	emit(updateWidgets(0));

	return true;
}

/*
 * QObject / Signals
 */

// Return interface as QObject
QObject* TestToolPlugin::object()
{
	return this;
}