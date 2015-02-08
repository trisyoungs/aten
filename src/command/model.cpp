/*
	*** Model Commands
	*** src/command/model.cpp
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

#include "command/commands.h"
#include "parser/tree.h"
#include "parser/commandnode.h"
#include "main/aten.h"
#include "ff/forcefield.h"
#include "model/model.h"
#include "model/clipboard.h"
#include "classes/prefs.h"
#include "base/sysfunc.h"
#include "base/vibration.h"
#include "gui/gui.h"

// Create 'n' new atoms at once in model
bool Command::function_CreateAtoms(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Vec3<double> v;
	for (int n = 0; n < c->argi(0); n++) obj.i = obj.rs()->addAtom(0, v);
	rv.reset();
	return TRUE;
}

// Return (or set) the current model
bool Command::function_CurrentModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// Check the presence of arg(0)
	if (c->hasArg(0))
	{
		Model *m = NULL;
		switch (c->argType(0))
		{
			case (VTypes::IntegerData):
				m = aten.model(c->argi(0)-1);
				break;
			case (VTypes::StringData):
				m = aten.findModel(c->argc(0));
				break;
			case (VTypes::ModelData):
				m = (Model*) c->argp(0, VTypes::ModelData);
				break;
			default:
				msg.print("Can't convert a variable of type '%s' to a Model.\n", VTypes::dataType(c->argType(0)));
				break;
		}
		if (m == NULL)
		{
			msg.print("Invalid model specified - current model unchanged.\n");
			return FALSE;
		}
		else
		{
			aten.setCurrentModel(m,TRUE);
			msg.print("Current model is now '%s'.\n", aten.current.m->name());
			// Update GUI
			parent_.updateWidgets(AtenWindow::AllTarget);
		}
	}
	else msg.print("Current model is '%s'.\n", aten.current.m->name());
	rv.set(VTypes::ModelData, aten.current.m);
	return TRUE;
}

// Delete current or specified model
bool Command::function_DeleteModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// If the argument is an integer, get by id. Otherwise, get by name
	Model *m = NULL;
	if (c->hasArg(0))
	{
		switch (c->argType(0))
		{
			case (VTypes::ModelData): m = (Model*) c->argp(0, VTypes::ModelData); break;
			case (VTypes::IntegerData): m = aten.model(c->argi(0) - 1); break;
			case (VTypes::StringData): m = aten.findModel(c->argc(0)); break;
			default:
				printf("Can't convert %s in to a Model.\n", VTypes::aDataType(c->argType(0)));
				break;
		}
	}
	else m = aten.currentModel();

	if (m != NULL) 
	{
		aten.removeModel(m);
		// Update GUI
		parent_.updateWidgets(AtenWindow::AllTarget);
		return TRUE;
	}
	else
	{
		msg.print("No model named '%s' is available, or integer id %i is out of range.\n", c->argc(0), c->argi(0));
		return FALSE;
	}
}

// Finalise current model
bool Command::function_FinaliseModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// If this command is being run from a filter, set the output filter in the model.
	if (c->parent()->isFilter())
	{
		Tree *t = c->parent();
// 		if (f->partner() != NULL) obj.m->setFilename(c->parent()->filename());
		obj.m->setFilename(c->parent()->parser()->inputFilename());
		obj.m->setFilter(t->filter.partner());
	}
	// Do various necessary calculations
	if (prefs.coordsInBohr()) obj.m->bohrToAngstrom();
	obj.m->renumberAtoms();
	if (!prefs.keepView()) obj.m->resetView();
	obj.m->calculateMass();
	obj.m->selectNone();
	obj.m->regenerateIcon();
	// Print out some useful info on the model that we've just read in
	msg.print(Messenger::Verbose, "Model  : %s\n", obj.m->name());
	msg.print(Messenger::Verbose, "Atoms  : %i\n", obj.m->nAtoms());
	msg.print(Messenger::Verbose, "Cell   : %s\n", UnitCell::cellType(obj.m->cell()->type()));
	if (obj.m->cell()->type() != UnitCell::NoCell) obj.m->cell()->print();
	// If a trajectory exists for this model, by default we view from trajectory in the GUI
	if (obj.m->nTrajectoryFrames() > 0) obj.m->setRenderSource(Model::TrajectorySource);
	// Lastly, reset all the log points and start afresh
	obj.m->enableUndoRedo();
	obj.m->changeLog.reset();
	obj.m->changeLog.updateSavePoint();
	rv.reset();
	return TRUE;
}

// Set current model to be first loaded/created model
bool Command::function_FirstModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Model *m = aten.model(0);
	rv.set(VTypes::ModelData, m);
	if (m != NULL) 
	{
		aten.setCurrentModel(m,TRUE);
		// Update GUI
		parent_.updateWidgets(AtenWindow::AllTarget);
	}
	else return FALSE;
	return TRUE;
}

// Select working model ('getmodel <name>')
bool Command::function_GetModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// If the argument is an integer, get by id. Otherwise, get by name
	Model *m = NULL;
	switch (c->argType(0))
	{
		case (VTypes::ModelData): m = (Model*) c->argp(0, VTypes::ModelData); break;
		case (VTypes::IntegerData): m = aten.model(c->argi(0) - 1); break;
		case (VTypes::StringData): m = aten.findModel(c->argc(0)); break;
		default:
			printf("Can't convert %s in to a Model.\n", VTypes::aDataType(c->argType(0)));
			break;
	}
	rv.set(VTypes::ModelData, m);
	if (m != NULL) 
	{
		aten.setCurrentModel(m,TRUE);
		m->setRenderSource(Model::ModelSource);
		// Update GUI
		parent_.updateWidgets(AtenWindow::AllTarget);
		return TRUE;
	}
	else
	{
		msg.print("No model named '%s' is available, or integer id %i is out of range.\n", c->argc(0), c->argi(0));
		return FALSE;
	}
}

// Print all information for model ('info')
bool Command::function_Info(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->print();
	rv.reset();
	return TRUE;
}

// Set current model to be last loaded/created model
bool Command::function_LastModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Model *m = aten.model(aten.nModels()-1);
	rv.set(VTypes::ModelData, m);
	if (m != NULL) 
	{
		aten.setCurrentModel(m,TRUE);
		// Update GUI
		parent_.updateWidgets(AtenWindow::AllTarget);
	}
	else return FALSE;
	return TRUE;
}

// Print loaded models ('listmodels')
bool Command::function_ListModels(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (aten.nModels() != 0) msg.print("Name            NAtoms  Forcefield\n");
	for (Model *m = aten.models(); m != NULL; m = m->next)
		msg.print("%-15s %5i  %-15s\n", m->name(),m->nAtoms(),(m->forcefield() != NULL ? m->forcefield()->name() : "None"));
	rv.reset();
	return TRUE;
}

// Load model from file
bool Command::function_LoadModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Tree *filter;
	
	// Was a specific filter nickname provided?
	if (c->hasArg(1))
	{
		LineParser parser;
		parser.getArgsDelim(0, c->argc(1));
		// First part of argument is nickname
		filter = aten.findFilter(FilterData::ModelImport, parser.argc(0));
		// Check that a suitable format was found
		if (filter == NULL)
		{
			// Print list of valid filter nicknames
			aten.printValidNicknames(FilterData::ModelImport);
			msg.print("Not loaded.\n");
			return FALSE;
		}
		
		// Loop over remaining arguments which are widget/global variable assignments
		for (int n = 1; n < parser.nArgs(); ++n) if (!filter->setAccessibleVariable(beforeStr(parser.argc(n),"="), afterStr(parser.argc(n),"="))) return FALSE;
	}
	else filter = aten.probeFile(c->argc(0), FilterData::ModelImport);
	rv.set(0);
	if (filter == NULL) return FALSE;
	int oldnmodels = aten.nModels();
	if (filter->executeRead(c->argc(0)))
	{
		Model *m = aten.currentModel();
		obj.i = m->atoms();
		rv.set(VTypes::ModelData, m);
	}
	else return FALSE;
	return TRUE;
}

// Print log information for model ('loginfo')
bool Command::function_LogInfo(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->renderSourceModel()->printLogs();
	rv.reset();
	return TRUE;
}

// Use parent model as atom template
bool Command::function_ModelTemplate(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m == obj.rs())
	{
		printf("Cannot perform model templating in the parent model.\n");
		return FALSE;
	}
	// Create the atoms template
	Vec3<double> v;
	Atom *j;
	for (obj.i = obj.m->atoms(); obj.i != NULL; obj.i = obj.i->next)
	{
		j = obj.rs()->addAtom(obj.i->element(), v);
		j->copyStyle(obj.i);
	}
	rv.reset();
	return TRUE;
}

// Create new model ('newmodel <name>')
bool Command::function_NewModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	obj.m = aten.addModel();
	obj.m->setName(stripTrailing(c->argc(0)));
	msg.print(Messenger::Verbose, "Created model '%s'\n", obj.m->name());
	// Check to see whether we are using a filter, enabling undo/redo if not
	if (!c->parent()->isFilter()) obj.m->enableUndoRedo();
	rv.set(VTypes::ModelData, obj.m);
	// Update GUI
	parent_.updateWidgets(AtenWindow::AllTarget);
	return TRUE;
}

// Skip to next loaded model ('nextmodel')
bool Command::function_NextModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->next == NULL) msg.print("Already at last loaded model.\n");
	else
	{
		aten.setCurrentModel(obj.m->next, TRUE);
		msg.print("Current model is now '%s'.\n", obj.m->name());
		// Update GUI
		parent_.updateWidgets(AtenWindow::AllTarget);
	}
	rv.set(VTypes::ModelData, obj.m);
	return TRUE;
}

// Select trajectory parent model as current model
bool Command::function_ParentModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Check for parent frame
	if (obj.rs()->parent() == NULL)
	{
		msg.print("This model doesn't have a parent.\n");
		return FALSE;
	}
	obj.m->setRenderSource(Model::ModelSource);
	aten.setCurrentModel(obj.m, TRUE);
	// Update GUI
	parent_.updateWidgets(AtenWindow::AllTarget);
	return TRUE;
}

// Skip to previous loaded model ('prevmodel')
bool Command::function_PrevModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->prev == NULL) msg.print("Already at first loaded model.\n");
	else
	{
		aten.setCurrentModel(obj.m->prev, TRUE);
		msg.print("Current model is now '%s'.\n",obj.m->name());
		// Update GUI
		parent_.updateWidgets(AtenWindow::AllTarget);
	}
	rv.set(VTypes::ModelData, obj.m);
	return TRUE;
}

// Save current model ('savemodel <format> <filename>')
bool Command::function_SaveModel(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;

	// Parse the first option so we can get the filter nickname and any filter options
	LineParser parser;
	parser.getArgsDelim(LineParser::UseQuotes, c->argc(0));
	
	// First part of argument is nickname
	Tree *filter = aten.findFilter(FilterData::ModelExport, parser.argc(0));
	// Check that a suitable format was found
	if (filter == NULL)
	{
		// Print list of valid filter nicknames
		aten.printValidNicknames(FilterData::ModelExport);
		msg.print("Not saved.\n");
		return FALSE;
	}

	// Loop over remaining arguments which are widget/global variable assignments
	for (int n = 1; n < parser.nArgs(); ++n) if (!filter->setAccessibleVariable(beforeStr(parser.argc(n),"="), afterStr(parser.argc(n),"="))) return FALSE;
	
	obj.rs()->setFilter(filter);
	obj.rs()->setFilename(c->argc(1));

	// Temporarily disable undo/redo for the model, save, and re-enable
	obj.rs()->disableUndoRedo();
	bool result = filter->executeWrite(obj.rs()->filename());
	if (result)
	{
		obj.rs()->changeLog.updateSavePoint();
		msg.print("Model '%s' saved to file '%s' (%s)\n", obj.rs()->name(), obj.rs()->filename(), filter->filter.name());
	}
	else msg.print("Failed to save model '%s'.\n", obj.rs()->name());
	obj.rs()->enableUndoRedo();
	rv.set(result);
	return TRUE;
}

// Save selection of current model ('saveselection <format> <filename>')
bool Command::function_SaveSelection(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Find filter with a nickname matching that given in argc(0)
	Tree *filter = aten.findFilter(FilterData::ModelExport, c->argc(0));
	// Check that a suitable format was found
	if (filter == NULL)
	{
		msg.print("Valid model export nicknames are:\n");
		for (Refitem<Tree,int> *ri = aten.filters(FilterData::ModelExport); ri != NULL; ri = ri->next)
			msg.print("  %-15s %s\n", ri->item->filter.nickname(), ri->item->filter.name());
		msg.print("Not saved.\n");
		return FALSE;
	}
	// Is anything selected in the source model?
	if (obj.rs()->nSelected() == 0)
	{
		msg.print("Nothing selected in current model - nothing to save.\n");
		rv.set(FALSE);
		return FALSE;
	}
	// Create a copy of the current basic model info, and then copy the selection of atoms
	Model m;
	Clipboard clip;
	m.setCell(obj.rs()->cell());
	m.setFilter(filter);
	m.setFilename(c->argc(1));
	clip.copySelection(obj.rs());
	clip.pasteToModel(&m, FALSE);
	Bundle oldbundle = obj;
	obj.m = &m;
	// Temporarily disable undo/redo for the model, save, and re-enable
	obj.rs()->disableUndoRedo();
	bool result = filter->executeWrite(m.filename());
	obj.rs()->enableUndoRedo();
	obj = oldbundle;
	if (result) msg.print("Selection from model '%s' saved to file '%s' (%s)\n", m.name(), m.filename(), filter->filter.name());
	else msg.print("Failed to save selection from model '%s'.\n", m.name());
	rv.set(result);
	return TRUE;
}

// Set name of current model ('setname <name>')
bool Command::function_SetName(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Rename Model");
	obj.rs()->setName(c->argc(0));
	obj.rs()->endUndoState();
	msg.print(Messenger::Verbose,"Renamed model to '%s'\n", obj.rs()->name());
	return TRUE;
}

// Show all atoms
bool Command::function_ShowAll(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Show all atoms", obj.rs()->nSelected());
	for (Atom *i = obj.rs()->atoms(); i != NULL; i = i->next) obj.rs()->atomSetHidden(i,FALSE);
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}
