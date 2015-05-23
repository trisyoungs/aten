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
			setCurrentModel(m);
			break;
		case (Aten::FragmentLibraryList):
			m = fragmentModels_.add();
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
Refitem<Model,int>* Aten::visibleModels()
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
