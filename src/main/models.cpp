/*
	*** Aten Model Functions
	*** src/main/models.cpp
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

// Set usage of working model list
void Aten::setUseWorkingList(bool b)
{
	static Bundle originalBundle;
	if (b)
	{
		originalBundle = current_;
		targetModelList_ = Aten::WorkingModelList;
	}
	else 
	{
		current_ = originalBundle;
		workingModels_.clear();
		targetModelList_ = Aten::MainModelList;
	}
}

// Return list of working models
Model* Aten::workingModels() const
{
	return workingModels_.first();
}

// Return first item in the model list
Model* Aten::models() const
{
	return models_.first();
}

// Return nth item in the model list
Model* Aten::model(int n)
{
	return models_[n];
}

// Return whether model exists
bool Aten::isModel(Model* model) const
{
	return models_.contains(model);
}

// Return the current model's index in the model list
int Aten::currentModelId() const
{
	return models_.indexOf(current_.m);
}

// Return index of specified model
int Aten::modelIndex(Model* model) const
{
	return models_.indexOf(model);
}

// Return the number of models in the model list
int Aten::nModels() const
{
	return models_.nItems();
}

// Add model
Model* Aten::addModel()
{
	Messenger::enter("Aten::addModel");

	// Check current list target for model creation
	Model* m = NULL;
	switch (targetModelList_)
	{
		case (Aten::MainModelList):
			m = models_.add();
			m->setType(Model::ParentModelType);
			m->setName(QString("Unnamed%1").arg(++modelId_, 3, 10, QChar('0')));
			m->resetLogs();
			setSingleModelVisible(m);
			break;
		case (Aten::FragmentLibraryList):
			m = fragments_.add();
			m->setType(Model::ParentModelType);
			m->setName(QString("Fragment%1").arg(++fragmentModelId_, 3, 10, QChar('0')));
			m->resetLogs();
			m->disableUndoRedo();
			break;
		case (Aten::WorkingModelList):
			m = workingModels_.add();
			m->setType(Model::ParentModelType);
			m->setName(QString("TempModel%1").arg(workingModels_.nItems(), 3, 10, QChar('0')));
			m->resetLogs();
			m->disableUndoRedo();
			break;
		default:
			printf("Internal Error: No target list set for model creation.\n");
			break;
	}

	Messenger::exit("Aten::addModel");
	return m;
}

// Remove model
void Aten::removeModel(Model* xmodel)
{
	Messenger::enter("Aten::removeModel");

	// Set next available model
	Model* m = (xmodel->next != NULL ? xmodel->next : xmodel->prev);
	setCurrentModel(m);

	// Delete the old model
	models_.remove(xmodel);
	visibleModels_.remove(xmodel);

	Messenger::exit("Aten::removeModel");
}

// Find model by name
Model* Aten::findModel(QString name) const
{
	Messenger::enter("Aten::findModel");

	Model* result = NULL;
	for (result = models_.first(); result != NULL; result = result->next) if (name == result->name()) break;

	Messenger::exit("Aten::findModel");
	return result ;
}

// Set visible flag for specified model
void Aten::setModelVisible(Model* m, bool visible)
{
	// Check model pointer
	if (m == NULL) return;
	m->setVisible(visible);

	// Search list for specified model
	RefListItem<Model,int>* ri = visibleModels_.contains(m);
	if ((ri == NULL) && visible) visibleModels_.add(m);
	else if ((ri != NULL) && (!visible)) visibleModels_.remove(m);
}

// Set the specified model to be the only one visible
void Aten::setSingleModelVisible(Model* m)
{
	// Check model pointer
	if (m == NULL) return;

	// Make all visible models invisible
	for (RefListItem<Model,int>* ri = visibleModels_.first(); ri != NULL; ri = ri->next) ri->item->setVisible(false);
	visibleModels_.clear();

	setCurrentModel(m);
}

// Return number of visible models
int Aten::nVisibleModels()
{
	return visibleModels_.nItems();
}

// Return reflist of visible models
RefListItem<Model,int>* Aten::visibleModels()
{
	return visibleModels_.first();
}

// Return n'th visible model
Model* Aten::visibleModel(int id)
{
	if ((id < 0) || (id >= visibleModels_.nItems()))
	{
		printf("Index %i is out of range for visible model list.\n", id);
		return NULL;
	}
	return visibleModels_[id]->item;
}

// Log specified change(s) in all models
void Aten::globalLogChange(Log::LogType log)
{
	// Loop over all loaded models and log change in their current rendersource
	for (Model* m = models_.first(); m != NULL; m = m->next) m->renderSourceModel()->logChange(log);
}

// Load model (if it is not loaded already)
bool Aten::loadModel(QString fileName, Tree* filter)
{
	// Check to see if current list of loaded models matches the filename supplied
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

	// If filter == NULL then we didn't match a filter we must probe the file first to try and find its format
	bool result = false;
	if (filter == NULL) filter = probeFile(fileName, FilterData::ModelImport);
	if (filter != NULL)
	{
		if (filter->executeRead(fileName))
		{
			ReturnValue rv = fileName;
			atenWindow_->ui.HomeFileOpenButton->callPopupMethod("addRecentFile", rv);
			result = true;
		}
	}

	// If we loaded something successfully, have we flagged an empty model to delete?
	if (result)
	{
		if (removeAfterLoad) removeModel(removeAfterLoad);
		atenWindow_->updateWidgets(AtenWindow::AllTarget);
	}

	return result;
}

// Open model (if it is not loaded already)
bool Aten::openModel(QString fileName, IOPluginInterface* plugin)
{
	Messenger::enter("Aten::openModel");

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
	if (plugin == NULL) pluginStore_.findPlugin(PluginTypes::IOModelPlugin, PluginTypes::ImportPlugin, fileName);
	if (plugin != NULL)
	{
		// Create a LineParser to open the file, and encapsulate it in a FileParser to give to the interface
		LineParser parser;
		parser.openInput(fileName);
		if (!parser.isFileGoodForReading())
		{
			Messenger::error("Couldn't open file '%s' for reading.\n", qPrintable(fileName));
			Messenger::exit("Aten::openModel");
			return false;
		}

		IOPluginInterface* interface = plugin->createInstance();
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
	else
	{
		Messenger::error("Couldn't determine a suitable plugin to load the file '%s'.\n", qPrintable(fileName));
		Messenger::exit("Aten::openModel");
		return false;
	}

	// If we loaded something successfully, have we flagged an empty model to delete?
	if (result)
	{
		if (removeAfterLoad) removeModel(removeAfterLoad);
		atenWindow_->updateWidgets(AtenWindow::AllTarget);
	}

	Messenger::exit("Aten::openModel");
	return result;
}
