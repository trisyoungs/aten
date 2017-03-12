/*
	*** Aten Import/Export Functions
	*** src/main/importexport.cpp
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
#include "gui/savemodel.h"
#include <QMessageBox>

ATEN_USING_NAMESPACE

// Process objects created on import
void Aten::processImportedObjects(FilePluginInterface* plugin, QString filename)
{
	// Parent Models
	while (plugin->createdModels().first())
	{
		Model* m = plugin->createdModels().takeFirst();
		m->setType(Model::ParentModelType);

		// Set source filename and plugin interface used
		m->setFilename(filename);
		m->setPlugin(plugin);

		// Do various necessary calculations
		if (plugin->standardOptions().isSetAndOn(FilePluginStandardImportOptions::CoordinatesInBohrSwitch)) m->bohrToAngstrom();
		m->renumberAtoms();
		if (!plugin->standardOptions().isSetAndOn(FilePluginStandardImportOptions::KeepViewSwitch)) m->resetView(atenWindow()->ui.MainView->width(), atenWindow()->ui.MainView->height());
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

	// Trajectory frames
	RefList<Model,int> createdFrames = plugin->createdFrames();
	for (RefListItem<Model,int>* ri = createdFrames.first(); ri != NULL; ri = ri->next)
	{
		Model* frame = ri->item;
		if (!frame) continue;

		frame->renumberAtoms();
		frame->calculateMass();
		frame->selectNone();
		frame->resetLogs();
		frame->updateSavePoint();
		frame->enableUndoRedo();
	}

	// Grids
	while (plugin->createdGrids().first())
	{
		Grid* grid = plugin->createdGrids().takeFirst();

		// Set source filename and plugin interface used
		if (plugin->standardOptions().isSetAndOn(FilePluginStandardImportOptions::CoordinatesInBohrSwitch)) grid->bohrToAngstrom();
		grid->setFilename(filename);
		grid->setPlugin(plugin);

		// If the model pointer is set in the Grid, make sure the Model knows about it!
		// If not, add it to the current model.
		if (grid->parent()) grid->parent()->ownGrid(grid);
		else current_.rs()->ownGrid(grid);

		// Set current object
		current_.g = grid;
	}

	// Forcefields
	while (plugin->createdForcefields().first())
	{
		Forcefield* ff = plugin->createdForcefields().takeFirst();

		// Set source filename and plugin interface used
		ff->setFilename(filename);
		ff->setPlugin(plugin);

		// Pass the model pointer to Aten 
		ownForcefield(ff);
	}
}

// Import model (if it is not loaded already)
bool Aten::importModel(QString filename, const FilePluginInterface* plugin, FilePluginStandardImportOptions standardOptions, KVMap pluginOptions)
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

	// Does the file actually exist?
	if (!QFile::exists(filename))
	{
		Messenger::error("Specified file '" + filename + "' does not exist.");
		return false;
	}

	// If plugin == NULL then we must probe the file first to try and find out how to load it
	bool result = false;
	if (plugin == NULL) plugin = pluginStore_.findFilePlugin(PluginTypes::ModelFilePlugin, PluginTypes::ImportPlugin, filename);
	if (plugin != NULL)
	{
		// Create an instance of the plugin, and open an input file and set options
		FilePluginInterface* pluginInterface = (FilePluginInterface*) plugin->duplicate();
		pluginInterface->applyStandardOptions(standardOptions);
		pluginInterface->setOptions(pluginOptions);
		pluginInterface->setParentModel(current_.m);
		pluginInterface->setTargetModel(current_.rs());
		if (!pluginInterface->openInput(filename))
		{
			Messenger::exit("Aten::importModel");
			return false;
		}

		if (pluginInterface->importData())
		{
			processImportedObjects(pluginInterface, filename);

			ReturnValue rv = filename;
			atenWindow_->ui.HomeFileOpenButton->callPopupMethod("addRecentFile", rv);
			result = true;
		}

		pluginInterface->closeFiles();
	}
	else Messenger::error("Couldn't determine a suitable plugin to load the file '%s'.", qPrintable(filename));

	// If we loaded something successfully, have we flagged an empty model to delete?
	if (result)
	{
		if (removeAfterLoad)
		{
			// Just check that nothing has happened to the model (might have been modified...)
			if ((!removeAfterLoad->atoms()) && (!removeAfterLoad->glyphs()) && (!removeAfterLoad->grids())) removeModel(removeAfterLoad);
		}
		atenWindow_->updateWidgets(AtenWindow::AllTargets);
	}

	Messenger::exit("Aten::importModel");
	return result;
}

// Export model
bool Aten::exportModel(Model* sourceModel, QString filename, const FilePluginInterface* plugin, FilePluginStandardImportOptions standardOptions, KVMap pluginOptions)
{
	Messenger::enter("Aten::exportModel");

	if (filename.isEmpty() || (plugin == NULL) || (plugin->category() != PluginTypes::ModelFilePlugin) || (!plugin->canExport()))
	{
		// Need to raise the save model dialog to get a valid name and/or plugin
		if (atenWindow_->shown() && atenWindow_->saveModelDialog().execute(pluginStore_.logPoint(), filename, plugin))
		{
			filename = atenWindow_->saveModelDialog().selectedFilenames().at(0);

			// Grab current plugin from dialog
			plugin = atenWindow_->saveModelDialog().selectedPlugin();
			if (!plugin)
			{
				QMessageBox::critical(atenWindow_, "Export Failed", "Export format could not be determined.\nCheck the file extension, or explicitly select a type.");
				Messenger::print("Model '%s' not saved.\n", qPrintable(sourceModel->name()));
				Messenger::exit("Aten::exportModel");
				return false;
			}
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
		FilePluginInterface* pluginInterface = (FilePluginInterface*) plugin->duplicate();
		if (!pluginInterface->openOutput(filename))
		{
			Messenger::exit("Aten::exportModel");
			return false;
		}
		pluginInterface->applyStandardOptions(standardOptions);
		pluginInterface->setOptions(pluginOptions);
		pluginInterface->setParentModel(sourceModel);
		if (pluginInterface->exportData())
		{
			// Set the model's (potentially new) filename and plugin
			sourceModel->setFilename(filename);
			sourceModel->setPlugin(pluginInterface);
			sourceModel->updateSavePoint();

			// Done - tidy up
			pluginInterface->closeFiles();

			Messenger::print("Model '%s' saved to file '%s' (%s)", qPrintable(sourceModel->name()), qPrintable(filename), qPrintable(pluginInterface->name()));
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
bool Aten::importGrid(Model* targetModel, QString filename, const FilePluginInterface* plugin, FilePluginStandardImportOptions standardOptions, KVMap pluginOptions)
{
	Messenger::enter("Aten::importGrid");

	// Does the file actually exist?
	if (!QFile::exists(filename))
	{
		Messenger::error("Specified file '" + filename + "' does not exist.");
		return false;
	}

	// If plugin == NULL then we must probe the file first to try and find out how to load it
	bool result = false;
	if (plugin == NULL) plugin = pluginStore_.findFilePlugin(PluginTypes::GridFilePlugin, PluginTypes::ImportPlugin, filename);
	if (plugin != NULL)
	{
		FilePluginInterface* pluginInterface = (FilePluginInterface*) plugin->duplicate();
		if (!pluginInterface->openInput(filename))
		{
			Messenger::exit("Aten::importGrid");
			return false;
		}
		pluginInterface->applyStandardOptions(standardOptions);
		pluginInterface->setOptions(pluginOptions);
		pluginInterface->setParentModel(targetModel->parent() ? targetModel->parent() : targetModel);
		pluginInterface->setTargetModel(targetModel);
		if (pluginInterface->importData())
		{
			processImportedObjects(pluginInterface, filename);

			ReturnValue rv = filename;
			atenWindow_->ui.GridsManageOpenButton->callPopupMethod("addRecentFile", rv);
			result = true;
		}

		pluginInterface->closeFiles();
	}
	else Messenger::error("Couldn't determine a suitable plugin to load the file '%s'.", qPrintable(filename));

	Messenger::exit("Aten::importGrid");
	return result;
}

// Import trajectory
bool Aten::importTrajectory(Model* targetModel, QString filename, const FilePluginInterface* plugin, FilePluginStandardImportOptions standardOptions, KVMap pluginOptions)
{
	Messenger::enter("Aten::importTrajectory");

	// Does the file actually exist?
	if (!QFile::exists(filename))
	{
		Messenger::error("Specified file '" + filename + "' does not exist.");
		return false;
	}

	// Clear existing trajectory, if there is one
	targetModel->setRenderSource(Model::ModelSource);
	targetModel->clearTrajectory();

	// If plugin == NULL then we must probe the file first to try and find out how to load it
	bool result = false;
	if (plugin == NULL) plugin = pluginStore_.findFilePlugin(PluginTypes::TrajectoryFilePlugin, PluginTypes::ImportPlugin, filename);
	if (plugin != NULL)
	{
		FilePluginInterface* pluginInterface = (FilePluginInterface*) plugin->duplicate();
		if (!pluginInterface->openInput(filename))
		{
			Messenger::exit("Aten::importTrajectory");
			return false;
		}
		pluginInterface->applyStandardOptions(standardOptions);
		pluginInterface->setOptions(pluginOptions);
		pluginInterface->setParentModel(targetModel);

		// Call the importData() function of the interface - this will read any header information present in the file before the first frame
		if (!pluginInterface->importData())
		{
			targetModel->setRenderSource(Model::ModelSource);
			targetModel->clearTrajectory();
			Messenger::error("Failed to import trajectory.");
		}
		else
		{
			processImportedObjects(pluginInterface, filename);

			targetModel->setRenderSource(Model::TrajectorySource);
			targetModel->setTrajectoryPlugin(pluginInterface);

			result = true;
		}
	}
	else Messenger::error("Couldn't determine a suitable plugin to load the file '%s'.", qPrintable(filename));

	Messenger::exit("Aten::importTrajectory");
	return result;
}

// Import expression
bool Aten::importExpression(QString filename, const FilePluginInterface* plugin, FilePluginStandardImportOptions standardOptions, KVMap pluginOptions)
{
	Messenger::enter("Aten::importExpression");

	// Does the file actually exist?
	if (!QFile::exists(filename))
	{
		Messenger::error("Specified file '" + filename + "' does not exist.");
		return false;
	}

	// If plugin == NULL then we must probe the file first to try and find out how to load it
	bool result = false;
	if (plugin == NULL) plugin = pluginStore_.findFilePlugin(PluginTypes::ExpressionFilePlugin, PluginTypes::ImportPlugin, filename);
	if (plugin != NULL)
	{
		FilePluginInterface* pluginInterface = (FilePluginInterface*) plugin->duplicate();
		if (!pluginInterface->openInput(filename))
		{
			Messenger::exit("Aten::importExpression");
			return false;
		}
		pluginInterface->applyStandardOptions(standardOptions);
		pluginInterface->setOptions(pluginOptions);

		if (!pluginInterface->importData())
		{
			result = false;
			Messenger::error("Failed to import forcefield/expression.");
		}
		else
		{
			processImportedObjects(pluginInterface, filename);
			result = true;
		}
	}
	else Messenger::error("Couldn't determine a suitable plugin to load the file '%s'.", qPrintable(filename));

	Messenger::exit("Aten::importExpression");
	return result;
}

// Export expression
bool Aten::exportExpression(Model* sourceModel, QString filename, const FilePluginInterface* plugin, FilePluginStandardImportOptions standardOptions, KVMap pluginOptions)
{
	Messenger::enter("Aten::exportExpression");

	if (filename.isEmpty() || (plugin == NULL) || (plugin->category() != PluginTypes::ExpressionFilePlugin) || (!plugin->canExport()))
	{
		// Need to raise the save model dialog to get a valid name and/or plugin
		if (atenWindow_->shown() && atenWindow_->saveExpressionDialog().execute(pluginStore_.logPoint(), filename, plugin))
		{
			filename = atenWindow_->saveExpressionDialog().selectedFilenames().at(0);

			// Grab current plugin from dialog
			plugin = atenWindow_->saveExpressionDialog().selectedPlugin();
			if (!plugin)
			{
				QMessageBox::critical(atenWindow_, "Export Failed", "Export format could not be determined.\nCheck the file extension, or explicitly select a type.");
				Messenger::print("Expression for model '%s' not saved.\n", qPrintable(sourceModel->name()));
				Messenger::exit("Aten::exportExpression");
				return false;
			}
		}
	}

	// Now do we have a valid filename and plugin?
	if ((!filename.isEmpty()) && (plugin) && (plugin->category() == PluginTypes::ExpressionFilePlugin) && (plugin->canExport()))
	{
		// Temporarily disable undo/redo for the model
		sourceModel->disableUndoRedo();

		// Turn on export type mapping
		if (nTypeExportMappings() > 0) typeExportMapping_ = true;

		// Create an instance of the plugin, and set options and the output file
		FilePluginInterface* pluginInterface = (FilePluginInterface*) plugin->duplicate();
		if (!pluginInterface->openOutput(filename))
		{
			Messenger::exit("Aten::exportExpression");
			return false;
		}
		pluginInterface->applyStandardOptions(standardOptions);
		pluginInterface->setOptions(pluginOptions);
		pluginInterface->setParentModel(sourceModel);
		if (pluginInterface->exportData())
		{
			// Set the model's (potentially new) filename and plugin
			sourceModel->setFilename(filename);
			sourceModel->setPlugin(pluginInterface);
			sourceModel->updateSavePoint();

			// Done - tidy up
			pluginInterface->closeFiles();

			Messenger::print("Model '%s' saved to file '%s' (%s)", qPrintable(sourceModel->name()), qPrintable(filename), qPrintable(pluginInterface->name()));
		}
		else
		{
			sourceModel->enableUndoRedo();

			Messenger::print("Failed to save model '%s'.", qPrintable(sourceModel->name()));
			Messenger::exit("Aten::exportExpression");
			return false;
		}

		typeExportMapping_ = false;

		sourceModel->enableUndoRedo();
	}
	else
	{
		Messenger::print("Model '%s' not saved.\n", qPrintable(sourceModel->name()));
		Messenger::exit("Aten::exportExpression");
		return false;
	}
	
	Messenger::exit("Aten::exportExpression");

	return true;
}
