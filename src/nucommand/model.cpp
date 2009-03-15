/*
	*** Model Commands
	*** src/nucommand/model.cpp
	Copyright T. Youngs 2007-2009

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

#include "nucommand/commands.h"
#include "parser/tree.h"
#include "parser/commandnode.h"
#include "main/aten.h"
#include "ff/forcefield.h"
#include "command/filter.h"
#include "model/model.h"
#include "classes/prefs.h"
#include "base/sysfunc.h"

// Create 'n' new atoms at once in model
bool NuCommand::function_CreateAtoms(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Vec3<double> v;
	for (int n = 0; n < c->argi(0); n++) obj.i = obj.rs->addAtom(0, v);
	rv.reset();
	return TRUE;
}

// Return (or set) the current model
bool NuCommand::function_CurrentModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
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
			case (VTypes::CharacterData):
				m = aten.findModel(c->argc(0));
				break;
			case (VTypes::ModelData):
				m = (Model*) c->argp(0, NuVTypes::ModelData);
				break;
		}
		if (m == NULL)
		{
			msg.print("Invalid model specified - current model unchanged.\n");
			return FALSE;
		}
		else
		{
			aten.setCurrentModel(m);
			msg.print("Current model is now '%s'.\n", aten.current.m->name());
		}
	}
	else msg.print("Current model is '%s'.\n", aten.current.m->name());
	rv.set(NuVTypes::ModelData, aten.current.m);
	return TRUE;
}

// Finalise current model
bool NuCommand::function_FinaliseModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// If this command is being run from a filter, set the output filter in the model.
	if (c->parent()->isFilter())
	{
		Tree *filter = c->parent();
// 		if (f->partner() != NULL) obj.m->setFilename(c->parent()->filename());
		obj.m->setFilename(c->parent()->filename());
		obj.m->setFilter(filter->partner());
	}
	// Do various necessary calculations
	if (prefs.coordsInBohr()) obj.m->bohrToAngstrom();
	obj.m->renumberAtoms();
	obj.m->calculateViewMatrix();
	obj.m->resetView();
	obj.m->calculateMass();
	obj.m->calculateDensity();
	obj.m->selectNone();
	// Print out some useful info on the model that we've just read in
	msg.print("Atoms  : %i\n",obj.m->nAtoms());
	msg.print("Cell   : %s\n",Cell::cellType(obj.m->cell()->type()));
	if (obj.m->cell()->type() != Cell::NoCell) obj.m->cell()->print();
	// Lastly, reset all the log points and start afresh
	obj.m->enableUndoRedo();
	obj.m->changeLog.reset();
	obj.m->changeLog.updateSavePoint();
	rv.reset();
	return TRUE;
}

// Set current model to be first loaded/created model
bool NuCommand::function_FirstModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Model *m = aten.model(0);
	rv.set(NuVTypes::ModelData, m);
	if (m != NULL) 
	{
		aten.setCurrentModel(m);
		obj.p = NULL;
		obj.i = m->atoms();
	}
	else return FALSE;
	return TRUE;
}

// Select working model ('getmodel <name>')
bool NuCommand::function_GetModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// If the argument is an integer, get by id. Otherwise, get by name
	Model *m = (c->argType(0) == NuVTypes::IntegerData ? aten.model(c->argi(0)) : aten.findModel(c->argc(0)));
	rv.set(NuVTypes::ModelData, m);
	if (m != NULL) 
	{
		aten.setCurrentModel(m);
		obj.p = NULL;
		obj.i = m->atoms();
		return TRUE;
	}
	else
	{
		msg.print("No model named '%s' is available, or integer id %i is out of range.\n", c->argc(0), c->argi(0));
		return FALSE;
	}
}

// Print all information for model ('info')
bool NuCommand::function_Info(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->renderSource()->print();
	rv.reset();
	return TRUE;
}

// Set current model to be last loaded/created model
bool NuCommand::function_LastModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Model *m = aten.model(aten.nModels()-1);
	rv.set(NuVTypes::ModelData, m);
	if (m != NULL) 
	{
		aten.setCurrentModel(m);
		obj.p = NULL;
		obj.i = m->atoms();
	}
	else return FALSE;
	return TRUE;
}

// Print loaded models ('listmodels')
bool NuCommand::function_ListModels(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (aten.nModels() != 0) msg.print("Name            NAtoms  Forcefield\n");
	for (Model *m = aten.models(); m != NULL; m = m->next)
		msg.print("%-15s %5i  %-15s\n", m->name(),m->nAtoms(),(m->forcefield() != NULL ? m->forcefield()->name() : "None"));
	rv.reset();
	return TRUE;
}

// Load model ('loadmodel <filename> [name]')
bool NuCommand::function_LoadModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Tree *filter = aten.probeFile(c->argc(0), Tree::ModelImport);
	if (filter != NULL)
	{
		if (filter->executeRead(c->argc(0)))
		{
			Model *m = aten.currentModel();
			if (c->hasArg(1)) m->setName(c->argc(1));
			obj.i = m->atoms();
			rv.set(NuVTypes::ModelData, m);
			return TRUE;
		}
		else return FALSE;
	}
	else return FALSE;
}

// Print log information for model ('loginfo')
bool NuCommand::function_LogInfo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->renderSource()->printLogs();
	rv.reset();
	return TRUE;
}

// Use parent model as atom template
bool NuCommand::function_ModelTemplate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m == obj.rs)
	{
		printf("Cannot perform model templating in the parent model.\n");
		return FALSE;
	}
	// Create the atoms template
	Vec3<double> v;
	Atom *j;
	for (obj.i = obj.m->atoms(); obj.i != NULL; obj.i = obj.i->next)
	{
		j = obj.rs->addAtom(obj.i->element(), v);
		j->copyStyle(obj.i);
	}
	rv.reset();
	return TRUE;
}

// Create new model ('newmodel <name>')
bool NuCommand::function_NewModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	obj.m = aten.addModel();
	obj.m->setName(stripTrailing(c->argc(0)));
	msg.print("Created model '%s'\n", obj.m->name());
	if (prefs.keepNames())
	{
		Forcefield *f = aten.addForcefield();
		char s[512];
		sprintf(s,"Names kept from Model %s",obj.m->name());
		f->setName(s);
		obj.m->setNamesForcefield(f);
	}
	// Check to see whether we are using a filter, enabling undo/redo if not
	if (!c->parent()->isFilter()) obj.m->enableUndoRedo();
	rv.set(NuVTypes::ModelData, obj.m);
	return TRUE;
}

// Skip to next loaded model ('nextmodel')
bool NuCommand::function_NextModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->next == NULL) msg.print("Already at last loaded model.\n");
	else
	{
		aten.setCurrentModel(obj.m->next);
		msg.print("Current model is now '%s'.\n", obj.m->name());
	}
	rv.set(NuVTypes::ModelData, obj.m);
	return TRUE;
}

// Skip to previous loaded model ('prevmodel')
bool NuCommand::function_PrevModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->prev == NULL) msg.print("Already at first loaded model.\n");
	else
	{
		aten.setCurrentModel(obj.m->prev);
		msg.print("Current model is now '%s'.\n",obj.m->name());
	}
	rv.set(NuVTypes::ModelData, obj.m);
	return TRUE;
}

// Save current model ('savemodel <format> <filename>')
bool NuCommand::function_SaveModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Find filter with a nickname matching that given in argc(0)
	Tree *filter = aten.findFilter(Tree::ModelExport, c->argc(0));
	// Check that a suitable format was found
	if (filter == NULL)
	{
		msg.print("No model export filter was found that matches the nickname '%s'.\nNot saved.\n", c->argc(0));
		return FALSE;
	}
	obj.rs->setFilter(filter);
	obj.rs->setFilename(c->argc(1));
	bool result = filter->executeWrite(c->argc(1));
	rv.set(result);
	return (result);
}

// Set name of current model ('setname <name>')
bool NuCommand::function_SetName(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Rename Model");
	obj.rs->setName(c->argc(0));
	obj.rs->endUndoState();
	msg.print(Messenger::Verbose,"Renamed model to '%s'\n", obj.rs->name());
	return TRUE;
}

