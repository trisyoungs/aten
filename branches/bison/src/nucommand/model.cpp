/*
	*** Model functions
	*** src/parser/model.cpp
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

#include "command/commands.h"
#include "nucommand/commands.h"
#include "main/aten.h"
#include "ff/forcefield.h"
#include "command/filter.h"
#include "model/model.h"
#include "classes/prefs.h"
#include "base/sysfunc.h"

// Create 'n' new atoms at once in model
bool NuCommand::function_Createatoms(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Vec3<double> v;
	for (int n = 0; n < c->argi(0); n++) obj.i = obj.rs->addAtom(0, v);
	return TRUE;
}

// Return (or set) the current model
bool NuCommand::function_Currentmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check the presence of arg(0)
	if (c->hasArg(0))
	{
		Model *m = NULL;
		switch (c->argt(0))
		{
			case (VTypes::IntegerData):
				m = aten.model(c->argi(0)-1);
				break;
			case (VTypes::CharacterData):
				m = aten.findModel(c->argc(0));
				break;
			case (VTypes::ModelData):
				m = (Model*) c->argp(0, VTypes::ModelData);
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
			msg.print("Current model is now '%s'.\n", aten.current.rs->name());
		}
	}
	else msg.print("Current model is '%s'.\n", aten.current.rs->name());
	return TRUE;
}

// Finalise current model
bool NuCommand::function_Finalisemodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// If this command is being run from a filter, set the output filter in the model.
	Filter *f = c->parent()->filter();
	if (f != NULL)
	{
// 		if (f->partner() != NULL) obj.m->setFilename(c->parent()->filename());
		obj.m->setFilename(c->parent()->filename());
		obj.m->setFilter(f->partner());
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
	return TRUE;
}

// Set current model to be first loaded/created model
bool NuCommand::function_Firstmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Model *m = aten.model(0);
	if (m != NULL) 
	{
		aten.setCurrentModel(m);
		obj.p = NULL;
		obj.i = m->atoms();
	}
	else return FALSE;
	return TRUE;
}

// Select working model ('getmodel <name> [variable]')
bool NuCommand::function_Getmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// If the argument is an integer, get by id. Otherwise, get by name
	Model *m = (c->argt(0) == VTypes::IntegerData ? aten.model(c->argi(0)) : aten.findModel(c->argc(0)));
	if (m != NULL) 
	{
		aten.setCurrentModel(m);
		obj.p = NULL;
		obj.i = m->atoms();
		return TRUE;
	}
	else
	{
		msg.print("No model named '%s' is available, or integer id %i is out of range.\n", c->argc(0),c->argi(0));
		return FALSE;
	}
}

// Print all information for model ('info')
bool NuCommand::function_Info(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->renderSource()->print();
	return TRUE;
}

// Set current model to be last loaded/created model
bool NuCommand::function_Lastmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Model *m = aten.model(aten.nModels()-1);
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
bool NuCommand::function_Listmodels(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (aten.nModels() != 0) msg.print("Name            NAtoms  Forcefield\n");
	for (Model *m = aten.models(); m != NULL; m = m->next)
		msg.print("%-15s %5i  %-15s\n", m->name(),m->nAtoms(),(m->forcefield() != NULL ? m->forcefield()->name() : "None"));
	return TRUE;
}

// Load model ('loadmodel <filename> [name]')
bool NuCommand::function_Loadmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Filter *f = aten.probeFile(c->argc(0), Filter::ModelImport);
	if (f != NULL)
	{
		if (f->execute(c->argc(0)))
		{
			Model *m = aten.currentModel();
			if (c->hasArg(1)) m->setName(c->argc(1));
			obj.i = m->atoms();
			return TRUE;
		}
		else return FALSE;
	} else return FALSE;
}

// Print log information for model ('loginfo')
bool NuCommand::function_Loginfo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->renderSource()->printLogs();
	return TRUE;
}

// Use parent model as atom template
bool NuCommand::function_Modeltemplate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
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
	return TRUE;
}

// Create new model ('newmodel <name>')
bool NuCommand::function_Newmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
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
	if (c->parent()->inputFile() == NULL) obj.m->enableUndoRedo();
	return TRUE;
}

// Skip to next loaded model ('nextmodel')
bool NuCommand::function_Nextmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->next == NULL) msg.print("Already at last loaded model.\n");
	else
	{
		aten.setCurrentModel(obj.m->next);
		msg.print("Current model is now '%s'.\n", obj.m->name());
	}
	return TRUE;
}

// Skip to previous loaded model ('prevmodel')
bool NuCommand::function_Prevmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.m->prev == NULL) msg.print("Already at first loaded model.\n");
	else
	{
		aten.setCurrentModel(obj.m->prev);
		msg.print("Current model is now '%s'.\n",obj.m->name());
	}
	return TRUE;
}

// Save current model ('savemodel <format> <filename>')
bool NuCommand::function_Savemodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Find filter with a nickname matching that given in argc(0)
	Filter *f = aten.findFilter(Filter::ModelExport, c->argc(0));
	// Check that a suitable format was found
	if (f == NULL)
	{
		msg.print("No model export filter was found that matches the nickname '%s'.\nNot saved.\n", c->argc(0));
		return FALSE;
	}
	obj.rs->setFilter(f);
	obj.rs->setFilename(c->argc(1));
	return (f->execute(c->argc(1)) ? TRUE : FALSE);
}

// Set name of current model ('setname <name>')
bool NuCommand::function_Setname(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->setName(c->argc(0));
	msg.print(Messenger::Verbose,"Renamed model to '%s'\n", obj.rs->name());
	return TRUE;
}

