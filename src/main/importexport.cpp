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

ATEN_USING_NAMESPACE

// Import model (if it is not loaded already)
bool Aten::importModel(QString fileName, FilePluginInterface* plugin, QStringList pluginOptions)
{
	Messenger::enter("Aten::importModel");

	// Check to see if current list of loaded models contains the filename supplied
	QFileInfo newFileInfo(fileName);
	for (Model* model = models_.first(); model != NULL; model = model->next)
	{
		// If there is no filename for this model, carry on
		if (model->filename().isEmpty()) continue;

		// Get file info for the model's filename
		QFileInfo oldFileInfo(model->filename());
		if (newFileInfo == oldFileInfo)
		{
			Messenger::warn("Refusing to load model '%s' since it is already loaded.\n", qPrintable(fileName));
			setCurrentModel(model);
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
	if (plugin == NULL) plugin = pluginStore_.findFilePlugin(PluginTypes::ModelFilePlugin, PluginTypes::ImportPlugin, fileName);
	if (plugin != NULL)
	{
		// Create a LineParser to open the file, and encapsulate it in a FileParser to give to the interface
		LineParser parser;
		parser.openInput(fileName);
		if (!parser.isFileGoodForReading())
		{
			Messenger::error("Couldn't open file '%s' for reading.", qPrintable(fileName));
			Messenger::exit("Aten::importModel");
			return false;
		}

		FilePluginInterface* interface = plugin->createInstance();
		FileParser fileParser(parser);
		if (interface->importData(fileParser))
		{
			// Finalise any loaded models
			RefList<Model,int> createdModels = interface->createdModels();
			for (RefListItem<Model,int>* ri = createdModels.first(); ri != NULL; ri = ri->next)
			{
				Model* m = ri->item;

				// Set source filename and plugin interface used
				m->setFilename(fileName);
				m->setPlugin(interface);

				// Do various necessary calculations
				if (prefs.coordsInBohr()) m->bohrToAngstrom();
				m->renumberAtoms();
				if (!prefs.keepView()) m->resetView(atenWindow()->ui.MainView->width(), atenWindow()->ui.MainView->height());
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
				if (m->nTrajectoryFrames() > 0) m->setRenderSource(Model::TrajectorySource);

				// Lastly, reset all the log points and start afresh
				m->enableUndoRedo();
				m->resetLogs();
				m->updateSavePoint();
			}

			ReturnValue rv = fileName;
			atenWindow_->ui.HomeFileOpenButton->callPopupMethod("addRecentFile", rv);
			result = true;
		}

		parser.closeFiles();
	}
	else Messenger::error("Couldn't determine a suitable plugin to load the file '%s'.", qPrintable(fileName));

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
bool Aten::exportModel(Model* model, QString filename, FilePluginInterface* plugin, QStringList pluginOptions)
{
	Messenger::enter("Aten::exportModel");

	if (filename.isEmpty() || (plugin == NULL) || (plugin->category() != PluginTypes::ModelFilePlugin) || (plugin->canExport()))
	{
		// Need to raise a save file dialog to get a valid name and/or plugin
	// ATEN2 TODO ENDOFFILTERS
// 	static QString selectedFilter(aten_.filters(FilterData::ModelExport) == NULL ? NULL : aten_.filters(FilterData::ModelExport)->item->filter.name());
// 	static QDir currentDirectory_(aten_.workDir());
// 	QString filename = QFileDialog::getSaveFileName(this, "Save Model", currentDirectory_.path(), aten_.fileDialogFilters(FilterData::ModelExport), &selectedFilter);
// 	if (!filename.isEmpty())
// 	{
// 		// Store path for next use
// 		currentDirectory_.setPath(filename);
// 		// Grab file extension and search for it in our current lists...
// 		QString ext = QFileInfo(filename).suffix();
// 		RefList<Tree,int> filters;
// 		if (ext.isEmpty())
// 		{
// 			QFileInfo fileInfo( filename );
// 			// Does this filename uniquely identify a specific filter?
// 			for (RefListItem<Tree,int>* ri = aten_.filters(FilterData::ModelExport); ri != NULL; ri = ri->next)
// 			{
// 				if (ri->item->filter.doesNameMatch(qPrintable(fileInfo.fileName()))) filters.add(ri->item);
// 			}
// 			Messenger::print(Messenger::Verbose, "Exact filename '%s' matches %i filters...", qPrintable(filename), filters.nItems());
// 
// 			// If only one filter matched the filename extension, use it. Otherwise, ask for confirmation *or* list all filters.
// 			AtenSelectFilter selectFilter(*this);
// 			if (filters.nItems() != 0) filter = selectFilter.selectFilter("Name matches one or more model export filters.", &filters, aten_.filterList(FilterData::ModelExport));
// 			else
// 			{
// 				filter = selectFilter.selectFilter("Couldn't determine format to save expression in.", NULL, aten_.filterList(FilterData::ModelExport), true);
// 				if ((filter != NULL) && selectFilter.appendExtension())
// 				{
// 					if (filter->filter.extensions().count() != 0) filename += QString(".") + filter->filter.extensions().at(0);
// 				}
// 			}
// 		}
// 		else
// 		{
// 			// Does this extension uniquely identify a specific filter?
// 			for (RefListItem<Tree,int>* ri = aten_.filters(FilterData::ModelExport); ri != NULL; ri = ri->next)
// 			{
// 				if (ri->item->filter.doesExtensionMatch(ext)) filters.add(ri->item);
// 			}
// 			Messenger::print(Messenger::Verbose, "Extension of filename '%s' matches %i filters...", qPrintable(filename), filters.nItems());
// 			// If only one filter matched the filename extension, use it. Otherwise, ask for confirmation *or* list all filters.
// 			if (filters.nItems() == 1) filter = filters.first()->item;
// 			else if (filters.nItems() > 1)
// 			{
// 				AtenSelectFilter selectFilter(*this);
// 				filter = selectFilter.selectFilter("Extension matches one or more model export filters.", &filters, aten_.filterList(FilterData::ModelExport));
// 			}
// 			else
// 			{
// 				AtenSelectFilter selectFilter(*this);
// 				filter = selectFilter.selectFilter("Extension doesn't match any in known model export filters.", NULL, aten_.filterList(FilterData::ModelExport), true);
// 				if ((filter != NULL) && selectFilter.appendExtension())
// 				{
// 					if (filter->filter.extensions().count() != 0) filename += QString(".") + filter->filter.extensions().at(0);
// 				}
// 			}
// 		}
// 		saveModelFilter_ = filter;
// 		saveModelFilename_ = qPrintable(filename);
// 		if (filter == NULL) Messenger::print("No filter selected to save file '%s'. Not saved.", qPrintable(saveModelFilename_));
// 		return (saveModelFilter_ == NULL ? false : true);
// 	}
// 	else return false;
	}

	// Now do we have a valid filename and plugin?
	if ((!filename.isEmpty()) && (plugin) && (plugin->category() == PluginTypes::ModelFilePlugin) && (plugin->canExport()))
	{
		// Construct a LineParser with the relevant information
		LineParser parser;
		parser.openOutput(filename, false);
		if (!parser.isFileGoodForWriting())
		{
			Messenger::print("Failed to open file '%s' for writing.\n", qPrintable(filename));
			Messenger::exit("Aten::exportModel");
			return false;
		}

		// Temporarily disable undo/redo for the model, save, and re-enable
		model->disableUndoRedo();

		// Turn on export type mapping
		if (nTypeExportMappings() > 0) typeExportMapping_ = true;

		// Create an instance of the plugin, and make a FileParser object to give to it
		FilePluginInterface* interface = plugin->createInstance();
		FileParser fileParser(parser, model);
		if (interface->exportData(fileParser))
		{
			// Flush the cache (in case we were using indirect output)
			parser.commitCache();

			// Set the model's (potentially new) filename and plugin
			model->setFilename(filename);
			model->setPlugin(plugin);
			model->updateSavePoint();

			// Done - tidy up
			parser.closeFiles();

			Messenger::print("Model '%s' saved to file '%s' (%s)", qPrintable(model->name()), qPrintable(filename), qPrintable(plugin->name()));
		}
		else
		{
			model->enableUndoRedo();

			Messenger::print("Failed to save model '%s'.", qPrintable(model->name()));
			Messenger::exit("Aten::exportModel");
			return false;
		}

		typeExportMapping_ = false;

		model->enableUndoRedo();
	}
	else
	{
		Messenger::print("Model '%s' not saved.\n", qPrintable(model->name()));
		Messenger::exit("Aten::exportModel");
		return false;
	}
	
	Messenger::exit("Aten::exportModel");

	return true;
}

// Import grid
bool Aten::importGrid(QString fileName, FilePluginInterface* plugin, QStringList pluginOptions)
{
	Messenger::enter("Aten::importGrid");

	// If plugin == NULL then we must probe the file first to try and find out how to load it
	bool result = false;
	if (plugin == NULL) pluginStore_.findFilePlugin(PluginTypes::GridFilePlugin, PluginTypes::ImportPlugin, fileName);
	if (plugin != NULL)
	{
		// Create a LineParser to open the file, and encapsulate it in a FileParser to give to the interface
		LineParser parser;
		parser.openInput(fileName);
		if (!parser.isFileGoodForReading())
		{
			Messenger::error("Couldn't open file '%s' for reading.\n", qPrintable(fileName));
			Messenger::exit("Aten::importGrid");
			return false;
		}

		// Loaded grids will be associated to the current model, so grab it
		Model* m = currentModelOrFrame();

		FilePluginInterface* interface = plugin->createInstance();
		FileParser fileParser(parser, m);
		if (interface->importData(fileParser))
		{
			// Finalise any loaded grids???
			RefList<Model,int> createdModels = interface->createdModels();
			for (RefListItem<Model,int>* ri = createdModels.first(); ri != NULL; ri = ri->next)
			{
				Model* m = ri->item;

				// Set source filename and plugin interface used
				m->setFilename(fileName);
				m->setPlugin(interface);

				// Do various necessary calculations
				if (prefs.coordsInBohr()) m->bohrToAngstrom();
				m->renumberAtoms();
				if (!prefs.keepView()) m->resetView(atenWindow()->ui.MainView->width(), atenWindow()->ui.MainView->height());
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
				if (m->nTrajectoryFrames() > 0) m->setRenderSource(Model::TrajectorySource);

				// Lastly, reset all the log points and start afresh
				m->enableUndoRedo();
				m->resetLogs();
				m->updateSavePoint();
			}

			ReturnValue rv = fileName;
			atenWindow_->ui.HomeFileOpenButton->callPopupMethod("addRecentFile", rv);
			result = true;
		}

		parser.closeFiles();
	}
	else Messenger::error("Couldn't determine a suitable plugin to load the file '%s'.", qPrintable(fileName));

	Messenger::exit("Aten::importGrid");
	return result;
}

// Import trajectory to current model
bool Aten::importTrajectory(QString fileName, FilePluginInterface* plugin, QStringList pluginOptions)
{
	// ATEN2 TODO ENDOFFILTERS
}

// Import expression
bool Aten::importExpression(QString fileName, FilePluginInterface* plugin, QStringList pluginOptions)
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
		FileParser fileParser(parser);
		if (interface->importData(fileParser))
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
bool Aten::exportExpression(Model* model, QString filename, FilePluginInterface* plugin, QStringList pluginOptions)
{
	// ATEN2 TODO ENDOFFILTERS
}
