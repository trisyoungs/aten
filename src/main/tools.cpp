/*
	*** Aten Tools Functions
	*** src/main/tools.cpp
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

#include "main/aten.h"
#include "gui/mainwindow.h"

ATEN_USING_NAMESPACE

// Setup and run specified ToolPluginInterface
void Aten::runTool(ToolPluginInterface* plugin, KVMap pluginOptions, bool showDialog)
{
	Messenger::enter("Aten::runTool");

	// Set plugin options
	plugin->setOptions(pluginOptions);

	// Set the current model/frame, and full model list, in the plugin
	plugin->clearAllModels();
	plugin->setCurrentModelOrFrame(currentModelOrFrame());
	for (Model* m = models_.first(); m != NULL; m = m->next) plugin->addModel(m);

	// If requested, and the plugin has one, show the dialog
	if (showDialog && plugin->hasDialog()) plugin->showDialog();
	else plugin->runTool();

	// Update the whole GUI
	atenWindow_->updateWidgets(AtenWindow::AllTargets);

	Messenger::exit("Aten::runTool");
}

