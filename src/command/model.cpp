/*
	*** Model Commands
	*** src/command/model.cpp
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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "main/aten.h"
#include "ff/forcefield.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include "gui/mainwindow.h"

ATEN_USING_NAMESPACE

// Create 'n' new atoms at once in model
bool Commands::function_CreateAtoms(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Vec3<double> v;
	for (int n = 0; n < c->argi(0); n++) obj.i = obj.rs()->addAtom(0, v);
	rv.reset();
	return true;
}

// Return (or set) the current model
bool Commands::function_CurrentModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// Check the presence of arg(0)
	if (c->hasArg(0))
	{
		Model* model = NULL;
		switch (c->argType(0))
		{
			case (VTypes::IntegerData):
				model = aten_.model(c->argi(0)-1);
				break;
			case (VTypes::StringData):
				model = aten_.findModel(c->argc(0));
				break;
			case (VTypes::ModelData):
				model = (Model*) c->argp(0, VTypes::ModelData);
				break;
			default:
				Messenger::print("Can't convert a variable of type '%s' to a Model.", VTypes::dataType(c->argType(0)));
				break;
		}
		if (model == NULL)
		{
			Messenger::print("Invalid model specified - current model unchanged.");
			return false;
		}
		else
		{
			aten_.setCurrentModel(model);
			Messenger::print("Current model is now '%s'.", qPrintable(aten_.currentModel()->name()));
		}
	}
	else Messenger::print("Current model is '%s'.", qPrintable(aten_.currentModel()->name()));
	rv.set(VTypes::ModelData, aten_.currentModel());
	return true;
}

// Delete current or specified model
bool Commands::function_DeleteModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// If the argument is an integer, get by id. Otherwise, get by name
	Model* model = NULL;
	if (c->hasArg(0))
	{
		switch (c->argType(0))
		{
			case (VTypes::ModelData): 
				model = (Model*) c->argp(0, VTypes::ModelData);
				break;
			case (VTypes::IntegerData): 
				model = aten_.model(c->argi(0) - 1);
				break;
			case (VTypes::StringData): 
				model = aten_.findModel(c->argc(0));
				break;
			default:
				printf("Can't convert %s in to a Model.\n", VTypes::aDataType(c->argType(0)));
				break;
		}
	}
	else model = aten_.currentModel();

	if (model != NULL) 
	{
		aten_.removeModel(model);
		if (aten_.nModels() == 0) aten_.addModel();

		return true;
	}
	else
	{
		Messenger::print("No model named '%s' is available, or integer id %i is out of range.", qPrintable(c->argc(0)), c->argi(0));
		return false;
	}
}

// Set current model to be first loaded/created model
bool Commands::function_FirstModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Model* model = aten_.model(0);

	rv.set(VTypes::ModelData, model);
	aten_.setCurrentModel(model);
	
	return model;
}

// Select working model ('getmodel <name>')
bool Commands::function_GetModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// If the argument is an integer, get by id. Otherwise, get by name
	Model* model = NULL;
	switch (c->argType(0))
	{
		case (VTypes::ModelData): 
			model = (Model*) c->argp(0, VTypes::ModelData);
			break;
		case (VTypes::IntegerData): 
			model = aten_.model(c->argi(0) - 1);
			break;
		case (VTypes::StringData): 
			model = aten_.findModel(c->argc(0));
			break;
		default:
			printf("Can't convert %s in to a Model.\n", VTypes::aDataType(c->argType(0)));
			break;
	}
	rv.set(VTypes::ModelData, model);
	if (model != NULL) 
	{
		aten_.setCurrentModel(model);
		model->setRenderSource(Model::ModelSource);
		return true;
	}
	else
	{
		Messenger::print("No model named '%s' is available, or integer id %i is out of range.", qPrintable(c->argc(0)), c->argi(0));
		return false;
	}
}

// Print all information for model ('info')
bool Commands::function_Info(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->print();
	rv.reset();
	return true;
}

// Set current model to be last loaded/created model
bool Commands::function_LastModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Model* model = aten_.model(aten_.nModels()-1);
	rv.set(VTypes::ModelData, model);
	aten_.setCurrentModel(model);
	
	return model;
}

// Print loaded models ('listmodels')
bool Commands::function_ListModels(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (aten_.nModels() != 0) Messenger::print("Name            NAtoms  Forcefield");
	for (Model* m = aten_.models(); m != NULL; m = m->next)
		Messenger::print("%-15s %5i  %-15s", qPrintable(m->name()), m->nAtoms(), (m->forcefield() != NULL ? qPrintable(m->forcefield()->name()) : "None"));
	rv.reset();
	return true;
}

// Load model from file
bool Commands::function_LoadModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Tree* filter;
	
	// Was a specific filter nickname provided?
	// ATEN2 TODO ENDOFFILTERS
// 	if (c->hasArg(1))
// 	{
// 		LineParser parser;
// 		parser.getArgsDelim(0, c->argc(1));
// 		// First part of argument is nickname
// 		filter = aten_.findFilter(FilterData::ModelImport, parser.argc(0));
// 		// Check that a suitable format was found
// 		if (filter == NULL)
// 		{
// 			// Print list of valid filter nicknames
// 			aten_.printValidNicknames(FilterData::ModelImport);
// 			Messenger::print("Not loaded.");
// 			return false;
// 		}
// 		
// 		// Loop over remaining arguments which are widget/global variable assignments
// 		QStringList items;
// 		for (int n = 1; n < parser.nArgs(); ++n)
// 		{
// 			items = parser.argc(n).split('=');
// 			if (!filter->setAccessibleVariable(items.at(0), items.at(1))) return false;
// 		}
// 	}
// 	else filter = aten_.probeFile(c->argc(0), FilterData::ModelImport);
// 	rv.set(0);
// 	if (filter == NULL) return false;
// 	int oldnmodels = aten_.nModels();
// 	if (filter->executeRead(c->argc(0)))
// 	{
// 		Model* m = aten_.currentModel();
// 		obj.i = m->atoms();
// 		rv.set(VTypes::ModelData, m);
// 	}
// 	else return false;
	return true;
}

// Print log information for model ('loginfo')
bool Commands::function_LogInfo(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->renderSourceModel()->printLogs();
	rv.reset();
	return true;
}

// Use parent model as atom template
bool Commands::function_ModelTemplate(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.m == obj.rs())
	{
		printf("Cannot perform model templating in the parent model.\n");
		return false;
	}
	// Create the atoms template
	Vec3<double> v;
	Atom* j;
	for (obj.i = obj.m->atoms(); obj.i != NULL; obj.i = obj.i->next)
	{
		j = obj.rs()->addAtom(obj.i->element(), v);
		j->copyStyle(obj.i);
	}
	rv.reset();
	return true;
}

// Create new model ('newmodel <name>')
bool Commands::function_NewModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	obj.m = aten_.addModel();
	obj.m->setName(c->argc(0).trimmed());
	Messenger::print(Messenger::Verbose, "Created model '%s'", qPrintable(obj.m->name()));

	obj.m->enableUndoRedo();

	rv.set(VTypes::ModelData, obj.m);
	return true;
}

// Skip to next loaded model ('nextmodel')
bool Commands::function_NextModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.m->next == NULL) Messenger::print("Already at last loaded model.");
	else
	{
		aten_.setCurrentModel(obj.m->next);
		Messenger::print("Current model is now '%s'.", qPrintable(obj.m->name()));
	}
	rv.set(VTypes::ModelData, obj.m);
	return true;
}

// Select trajectory parent model as current model
bool Commands::function_ParentModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Check for parent frame
	if (obj.rs()->parent() == NULL)
	{
		Messenger::print("This model doesn't have a parent.");
		return false;
	}
	obj.m->setRenderSource(Model::ModelSource);
	aten_.setCurrentModel(obj.m);
	return true;
}

// Skip to previous loaded model ('prevmodel')
bool Commands::function_PrevModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.m->prev == NULL) Messenger::print("Already at first loaded model.");
	else
	{
		aten_.setCurrentModel(obj.m->prev);
		Messenger::print("Current model is now '%s'.", qPrintable(obj.m->name()));
	}
	rv.set(VTypes::ModelData, obj.m);
	return true;
}

// Save current model
bool Commands::function_SaveModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Parse the first option so we can get the filter nickname and any filter options
	LineParser parser;
	parser.getArgsDelim(Parser::UseQuotes, c->argc(0));

	// First part of argument zero is nickname (followed by options)
	FilePluginInterface* plugin = aten_.pluginStore().findFilePluginByNickname(PluginTypes::ModelFilePlugin, PluginTypes::ExportPlugin, parser.argc(0));
	
	// Check that a suitable plugin was found
	if (plugin == NULL)
	{
		// Print list of valid plugin nicknames
		aten_.pluginStore().showFilePluginNicknames(PluginTypes::ModelFilePlugin, PluginTypes::ExportPlugin);
		Messenger::print("Not saved.");
		return false;
	}

	// Loop over remaining arguments which are option assignments
	KVMap pluginOptions;
	for (int n = 1; n < parser.nArgs(); ++n) pluginOptions.add(parser.argc(n));

	bool result = aten_.exportModel(obj.m, c->argc(1), plugin, KVMap(), pluginOptions);
	
	rv.set(result);

	return true;
}

// Save selection of current model ('saveselection <format> <filename>')
bool Commands::function_SaveSelection(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Parse the first option so we can get the filter nickname and any filter options
	LineParser parser;
	parser.getArgsDelim(Parser::UseQuotes, c->argc(0));

	// First part of argument is nickname
	FilePluginInterface* plugin = aten_.pluginStore().findFilePluginByNickname(PluginTypes::ModelFilePlugin, PluginTypes::ExportPlugin, parser.argc(0));
	
	// Check that a suitable plugin was found
	if (plugin == NULL)
	{
		// Print list of valid plugin nicknames
		aten_.pluginStore().showFilePluginNicknames(PluginTypes::ModelFilePlugin, PluginTypes::ExportPlugin);
		Messenger::print("Not saved.");
		return false;
	}

	// Is anything selected in the source model?
	if (obj.rs()->nSelected() == 0)
	{
		Messenger::print("Nothing selected in current model - nothing to save.");
		rv.set(false);
		return false;
	}

	// Create a copy of the current basic model info, and then copy the selection of atoms
	Model m;
	Clipboard clip;
	m.setCell(obj.rs()->cell());
	clip.copySelection(obj.rs());
	clip.pasteToModel(&m, false);

	bool result = aten_.exportModel(&m, c->argc(1), plugin);

	rv.set(result);

	return true;
}

// Set name of current model ('setname <name>')
bool Commands::function_SetName(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Rename Model");
	obj.rs()->setName(c->argc(0));
	obj.rs()->endUndoState();
	Messenger::print(Messenger::Verbose, "Renamed model to '%s'", qPrintable(obj.rs()->name()));
	return true;
}

// Show all atoms
bool Commands::function_ShowAll(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Show all atoms", obj.rs()->nSelected());
	for (Atom* i = obj.rs()->atoms(); i != NULL; i = i->next) obj.rs()->atomSetHidden(i,false);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}
