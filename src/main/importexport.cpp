/*
	*** Aten Import/Export Functions
	*** src/main/importexport.cpp
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

#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/savemodel.h"

ATEN_USING_NAMESPACE

// Import model (if it is not loaded already)
bool Aten::importModel(QString filename, FilePluginInterface* plugin, KVMap standardOptions, KVMap pluginOptions)
{
	Messenger::enter("Aten::importModel");

	// Check to see if current list of loaded models contains the filename supplied
	QFileInfo newFileInfo(filename);
	for (Model* model = models_.first(); model != NULL; model = model->next)
	{
		// If there is no filename for this model, carry on
		if (model->filename().isEmpty()) continue;

		// Get file info for the model's filename
		QFileInfo oldFileInfo(model->filename());
		if (newFileInfo == oldFileInfo)
		{
			Messenger::warn("Refusing to load model '%s' since it is already loaded.\n", qPrintable(filename));
			setCurrentModel(model);

			Messenger::exit("Aten::importModel");
			return false;
		}
	}

	// If the current model is empty, has no fileName, and has no modifications to it, delete it after we have finished loading...
	Model* removeAfterLoad = NULL;
	if (current_.m)
	{
		if ((current_.m->nAtoms() == 0) && (current_.m->filename().isEmpty()) && (!current_.m->isModified())) removeAfterLoad = current_.m;
	}

	// If plugin == NULL then we must probe the file first to try and find out how to load it
	bool result = false;
	if (plugin == NULL) plugin = pluginStore_.findFilePlugin(PluginTypes::ModelFilePlugin, PluginTypes::ImportPlugin, filename);
	if (plugin != NULL)
	{
		// Create an instance of the plugin, and open an input file and set options
		FilePluginInterface* interface = plugin->createInstance();
		interface->setOptions(pluginOptions);
		if (!interface->openInput(filename))
		{
			Messenger::exit("Aten::importModel");
			return false;
		}

		if (interface->importData(standardOptions))
		{
			// Finalise any loaded models
			while (interface->createdModels().first())
			{
				Model* m = interface->createdModels().takeFirst();
				m->setType(Model::ParentModelType);

				// Set source filename and plugin interface used
				m->setFilename(filename);
				m->setPlugin(interface);

				// Do various necessary calculations
				if (standardOptions.value(FilePluginInterface::standardOption(FilePluginInterface::CoordinatesInBohrOption)) == "true") m->bohrToAngstrom();
				m->renumberAtoms();
				if (standardOptions.value(FilePluginInterface::standardOption(FilePluginInterface::KeepViewOption)) == "false") m->resetView(atenWindow()->ui.MainView->width(), atenWindow()->ui.MainView->height());
				m->calculateMass();
				m->selectNone();

				// Print out some useful info on the model that we've just read in
				Messenger::print(Messenger::Verbose, "Model  : %s", qPrintable(m->name()));
				Messenger::print(Messenger::Verbose, "Atoms  : %i", m->nAtoms());
				Messenger::print(Messenger::Verbose, "Cell   : %s", UnitCell::cellType(m->cell().type()));
				if (m->cell().type() != UnitCell::NoCell) m->cell().print();

				// If a names forcefield was created, add it to Aten's list 
				if (m->namesForcefield()) ownForcefield(m->namesForcefield());

				// If a trajectory exists for this model, by default we view from trajectory in the GUI
				if (m->hasTrajectory()) m->setRenderSource(Model::TrajectorySource);

				// Lastly, reset all the log points and start afresh
				m->enableUndoRedo();
				m->resetLogs();
				m->updateSavePoint();

				// Pass the model pointer to Aten 
				ownModel(m);
			}

			ReturnValue rv = filename;
			atenWindow_->ui.HomeFileOpenButton->callPopupMethod("addRecentFile", rv);
			result = true;
		}

		interface->closeFiles();
	}
	else Messenger::error("Couldn't determine a suitable plugin to load the file '%s'.", qPrintable(filename));

	// If we loaded something successfully, have we flagged an empty model to delete?
	if (result)
	{
		if (removeAfterLoad) removeModel(removeAfterLoad);
		atenWindow_->updateWidgets(AtenWindow::AllTarget);
	}

	Messenger::exit("Aten::importModel");
	return result;
}

// Export model
bool Aten::exportModel(Model* sourceModel, QString filename, FilePluginInterface* plugin, KVMap standardOptions, KVMap pluginOptions)
{
	Messenger::enter("Aten::exportModel");

	if (filename.isEmpty() || (plugin == NULL) || (plugin->category() != PluginTypes::ModelFilePlugin) || (plugin->canExport()))
	{
		// Need to raise a save file dialog to get a valid name and/or plugin
		static AtenSaveModel saveModelDialog(atenWindow_, workDir_, pluginStore().filePlugins(PluginTypes::ModelFilePlugin));

		if (saveModelDialog.execute(pluginStore_.logPoint(), filename, plugin))
		{
			filename = saveModelDialog.selectedFilename();
			plugin = saveModelDialog.selectedPlugin();
			if (plugin == NULL) plugin = pluginStore_.findFilePlugin(PluginTypes::ModelFilePlugin, PluginTypes::ImportPlugin, filename);
		}
		else
		{
			Messenger::exit("Aten::exportModel");
			return false;
		}
	}

	// Now do we have a valid filename and plugin?
	if ((!filename.isEmpty()) && (plugin) && (plugin->category() == PluginTypes::ModelFilePlugin) && (plugin->canExport()))
	{
		// Temporarily disable undo/redo for the model
		sourceModel->disableUndoRedo();

		// Turn on export type mapping
		if (nTypeExportMappings() > 0) typeExportMapping_ = true;

		// Create an instance of the plugin, and set options and the output file
		FilePluginInterface* interface = plugin->createInstance();
		if (!interface->openOutput(filename))
		{
			Messenger::exit("Aten::exportModel");
			return false;
		}
		interface->setOptions(pluginOptions);
		if (interface->exportData(standardOptions))
		{
			// Set the model's (potentially new) filename and plugin
			sourceModel->setFilename(filename);
			sourceModel->setPlugin(plugin);
			sourceModel->updateSavePoint();

			// Done - tidy up
			interface->closeFiles();

			Messenger::print("Model '%s' saved to file '%s' (%s)", qPrintable(sourceModel->name()), qPrintable(filename), qPrintable(plugin->name()));
		}
		else
		{
			sourceModel->enableUndoRedo();

			Messenger::print("Failed to save model '%s'.", qPrintable(sourceModel->name()));
			Messenger::exit("Aten::exportModel");
			return false;
		}

		typeExportMapping_ = false;

		sourceModel->enableUndoRedo();
	}
	else
	{
		Messenger::print("Model '%s' not saved.\n", qPrintable(sourceModel->name()));
		Messenger::exit("Aten::exportModel");
		return false;
	}
	
	Messenger::exit("Aten::exportModel");

	return true;
}

// Import grid
bool Aten::importGrid(Model* targetModel, QString fileName, FilePluginInterface* plugin, KVMap standardOptions, KVMap pluginOptions)
{
	Messenger::enter("Aten::importGrid");

	// If plugin == NULL then we must probe the file first to try and find out how to load it
	bool result = false;
	if (plugin == NULL) pluginStore_.findFilePlugin(PluginTypes::GridFilePlugin, PluginTypes::ImportPlugin, fileName);
	if (plugin != NULL)
	{
		FilePluginInterface* interface = plugin->createInstance();
		if (!interface->openInput(fileName))
		{
			Messenger::exit("Aten::importGrid");
			return false;
		}
		interface->setOptions(pluginOptions);
		interface->setTargetModel(targetModel);
		if (interface->importData(standardOptions))
		{
			// Finalise any loaded grids
			RefList<Grid,int> createdGrids = interface->createdGrids();
			for (RefListItem<Grid,int>* ri = createdGrids.first(); ri != NULL; ri = ri->next)
			{
				Grid* g = ri->item;

				// Set source filename and plugin interface used
				g->setFilename(fileName);
				g->setPlugin(interface);
			}

			ReturnValue rv = fileName;
			atenWindow_->ui.GridsManageOpenButton->callPopupMethod("addRecentFile", rv);
			result = true;
		}

		interface->closeFiles();
	}
	else Messenger::error("Couldn't determine a suitable plugin to load the file '%s'.", qPrintable(fileName));

	Messenger::exit("Aten::importGrid");
	return result;
}

// Import trajectory
bool Aten::importTrajectory(Model* targetModel, QString fileName, FilePluginInterface* plugin, KVMap standardOptions, KVMap pluginOptions)
{
	Messenger::enter("Aten::importTrajectory");

	// Clear existing trajectory, if there is one
	targetModel->clearTrajectory();

	// If plugin == NULL then we must probe the file first to try and find out how to load it
	bool result = true;
	if (plugin == NULL) plugin = pluginStore_.findFilePlugin(PluginTypes::TrajectoryFilePlugin, PluginTypes::ImportPlugin, fileName);
	if (plugin != NULL)
	{
		// Create a LineParser to open the file, and encapsulate it in a FileParser to give to the interface
		LineParser parser;
		parser.openInput(fileName);
		if (!parser.isFileGoodForReading())
		{
			Messenger::error("Couldn't open file '%s' for reading.\n", qPrintable(fileName));
			Messenger::exit("Aten::importTrajectory");
			return false;
		}

		FilePluginInterface* interface = plugin->createInstance();
		interface->setOptions(pluginOptions);
		FileParser fileParser(parser);

		// Call the importData() function of the interface - this will read any header information present in the file before the first frame
		if (!interface->importData(standardOptions))
		{
			targetModel->clearTrajectory();
			Messenger::error("Failed to import trajectory.");
			result = false;
		}

		// If successful, now read / cache frame data
		if (result)
		{
// 			if (prefs.coordsInBohr()) obj.rs()->bohrToAngstrom();
// 			obj.rs()->renumberAtoms();
// 			if (!prefs.keepView()) obj.rs()->resetView(aten_.atenWindow()->ui.MainView->contextWidth(), aten_.atenWindow()->ui.MainView->contextHeight());
// 			obj.rs()->calculateMass();
// 			obj.rs()->selectNone();
// 			obj.rs()->resetLogs();
// 			obj.rs()->updateSavePoint();
// 			obj.rs()->setFilename("frame");
// 			obj.rs()->enableUndoRedo();
		}

		parser.closeFiles();
	}
	else Messenger::error("Couldn't determine a suitable plugin to load the file '%s'.", qPrintable(fileName));

	Messenger::exit("Aten::importTrajectory");
	return result;
}

// Import expression
bool Aten::importExpression(QString fileName, FilePluginInterface* plugin, KVMap standardOptions, KVMap pluginOptions)
{
	Messenger::enter("Aten::importExpression");

	// If plugin == NULL then we must probe the file first to try and find out how to load it
	bool result = false;
	if (plugin == NULL) plugin = pluginStore_.findFilePlugin(PluginTypes::ExpressionFilePlugin, PluginTypes::ImportPlugin, fileName);
	if (plugin != NULL)
	{
		// Create a LineParser to open the file, and encapsulate it in a FileParser to give to the interface
		LineParser parser;
		parser.openInput(fileName);
		if (!parser.isFileGoodForReading())
		{
			Messenger::error("Couldn't open file '%s' for reading.\n", qPrintable(fileName));
			Messenger::exit("Aten::importExpression");
			return false;
		}

		FilePluginInterface* interface = plugin->createInstance();
		interface->setOptions(pluginOptions);
		FileParser fileParser(parser);
		if (interface->importData(standardOptions))
		{
			result = true;
		}

		parser.closeFiles();
	}
	else Messenger::error("Couldn't determine a suitable plugin to load the file '%s'.", qPrintable(fileName));

	Messenger::exit("Aten::importExpression");
	return result;
}

// Export expression
bool Aten::exportExpression(Model* targetModel, QString filename, FilePluginInterface* plugin, KVMap standardOptions, KVMap pluginOptions)
{
	// ATEN2 TODO ENDOFFILTERS
}
