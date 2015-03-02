/*
	*** Aten model functions
	*** src/main/aten.cpp
	Copyright T. Youngs 2007-2015

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

// Set the active model
void Aten::setCurrentModel(Model* m, bool deselectOthers)
{
	Messenger::enter("Aten::setCurrentModel");
	if (m == NULL)
	{
		current_.clear();
		Messenger::exit("Aten::setCurrentModel");
		return;
	}

	// Set Bundle pointers
	current_.m = m;
	current_.p = m->patterns();
	current_.g = m->grids();
	current_.i = NULL;
	// Deselect all other models if specified
	if (deselectOthers)
	{
		// Unset visible flags on all currently-visible models
		for (Refitem<Model,int>* ri = visibleModels_.first(); ri != NULL; ri = ri->next) ri->item->setVisible(FALSE);
		visibleModels_.clear();
	}
	// Its the current model, so it must be visible also... add to visible list
	setModelVisible(m, TRUE);
	Messenger::exit("Aten::setCurrentModel");
}

// Return current active model for editing
Model* Aten::currentModel() const
{
	return current_.m;
}

// Return current active model for editing, accounting for trajectory frames
Model* Aten::currentModelOrFrame() const
{
	return (current_.m == NULL ? NULL : current_.m->renderSourceModel());
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

// Return pointer to the actual model list
const List<Model> *Aten::modelList() const
{
	return &models_;
}

// Return the current model's index in the model list
int Aten::currentModelId() const
{
	return models_.indexOf(current_.m);
}

// Return index of specified model
int Aten::modelIndex(Model* m) const
{
	return models_.indexOf(m);
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
	Dnchar newname;
	Model* m = NULL;
	// Check current list target for model creation
	switch (targetModelList_)
	{
		case (Aten::MainModelList):
			m = models_.add();
			m->setType(Model::ParentModelType);
			newname.sprintf("Unnamed%03i", ++modelId_);
			m->setName(newname);
			m->changeLog.reset();
			setCurrentModel(m, TRUE);
			break;
		case (Aten::FragmentLibraryList):
			m = fragmentModels_.add();
			m->setType(Model::ParentModelType);
			newname.sprintf("Fragment%03i", ++fragmentModelId_);
			m->setName(newname);
			m->changeLog.reset();
			m->disableUndoRedo();
			break;
		case (Aten::WorkingModelList):
			m = workingModels_.add();
			m->setType(Model::ParentModelType);
			newname.sprintf("TempModel%03i", workingModels_.nItems());
			m->setName(newname);
			m->changeLog.reset();
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
	// Remove this model from the model_list in the main window
	Messenger::enter("Aten::removeModel");
	Model* m;
	// Delete the current model, but don't allow there to be zero models...
	if (models_.nItems() == 1) m = addModel();
	else m = (xmodel->next != NULL ? xmodel->next : xmodel->prev);
	setCurrentModel(m);
	// Delete the old model
	models_.remove(xmodel);
	visibleModels_.remove(xmodel);
	Messenger::exit("Aten::removeModel");
}

// Find model by name
Model* Aten::findModel(const char* s) const
{
	// Search model list for name 's' (script function)
	Messenger::enter("Aten::findModel");
	Model* result = NULL;
	for (result = models_.first(); result != NULL; result = result->next) if (strcmp(s,result->name()) == 0) break;
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
	Refitem<Model,int>* ri = visibleModels_.contains(m);
	if ((ri == NULL) && visible) visibleModels_.add(m);
	else if ((ri != NULL) && (!visible)) visibleModels_.remove(m);
}

// Return number of visible models
int Aten::nVisibleModels()
{
	return visibleModels_.nItems();
}

// Return reflist of visible models
Refitem<Model,int> *Aten::visibleModels()
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
	for (Model* m = models_.first(); m != NULL; m = m->next) m->renderSourceModel()->changeLog.add(log);
}
