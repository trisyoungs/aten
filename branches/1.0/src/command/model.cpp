/*
	*** Model command functions
	*** src/command/model.cpp
	Copyright T. Youngs 2007,2008

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
#include "command/commandlist.h"
#include "main/aten.h"
#include "ff/forcefield.h"
#include "command/filter.h"
#include "model/model.h"
#include "classes/prefs.h"
#include "base/sysfunc.h"

// Create 'n' new atoms at once in model
int Command::function_CA_CREATEATOMS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Vec3<double> v;
	for (int n = 0; n < c->argi(0); n++) obj.i = obj.rs->addAtom(0, v);
	return Command::Success;
}

// Finalise current model
int Command::function_CA_FINALISEMODEL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// If this command is being run from a filter, set the output filter in the model.
	Filter *f = c->parent()->filter();
	if (f != NULL)
	{
		if (f->partner() != NULL) obj.m->setFilename(c->parent()->filename());
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
	return Command::Success;
}

// Select working model ('getmodel <name> [variable]')
int Command::function_CA_GETMODEL(CommandNode *&c, Bundle &obj)
{
	// If the argument is an integer, get by id. Otherwise, get by name
	Model *m = (c->argt(0) == VTypes::IntegerData ? aten.model(c->argi(0)) : aten.findModel(c->argc(0)));
	if (m != NULL) 
	{
		aten.setCurrentModel(m);
		obj.p = NULL;
		obj.i = m->atoms();
		return Command::Success;
	}
	else
	{
		msg.print("No model named '%s' is available, or integer id %i is out of range.\n", c->argc(0),c->argi(0));
		return Command::Fail;
	}
}

// Print all information for model ('info')
int Command::function_CA_INFO(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->renderSource()->print();
	return Command::Success;
}

// Print loaded models ('listmodels')
int Command::function_CA_LISTMODELS(CommandNode *&c, Bundle &obj)
{
	if (aten.nModels() != 0) msg.print("Name            NAtoms  Forcefield\n");
	for (Model *m = aten.models(); m != NULL; m = m->next)
		msg.print("%-15s %5i  %-15s\n", m->name(),m->nAtoms(),(m->forcefield() != NULL ? m->forcefield()->name() : "None"));
	return Command::Success;
}

// Load model ('loadmodel <filename> [name]')
int Command::function_CA_LOADMODEL(CommandNode *&c, Bundle &obj)
{
	Filter *f = aten.probeFile(c->argc(0), Filter::ModelImport);
	if (f != NULL)
	{
		if (f->execute(c->argc(0)))
		{
			Model *m = aten.currentModel();
			if (c->hasArg(1)) m->setName(c->argc(1));
			obj.i = m->atoms();
			return Command::Success;
		}
		else return Command::Fail;
	} else return Command::Fail;
}

// Print log information for model ('loginfo')
int Command::function_CA_LOGINFO(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->renderSource()->printLogs();
	return Command::Success;
}

// Use parent model as atom template
int Command::function_CA_MODELTEMPLATE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (obj.m == obj.rs)
	{
		printf("Cannot perform model templating in the parent model.\n");
		return Command::Fail;
	}
	// Create the atoms template
	Vec3<double> v;
	Atom *j;
	for (obj.i = obj.m->atoms(); obj.i != NULL; obj.i = obj.i->next)
	{
		j = obj.rs->addAtom(obj.i->element(), v);
		j->copyStyle(obj.i);
	}
	return Command::Success;
}

// Create new model ('newmodel <name>')
int Command::function_CA_NEWMODEL(CommandNode *&c, Bundle &obj)
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
	return Command::Success;
}

// Skip to next loaded model ('nextmodel')
int Command::function_CA_NEXTMODEL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (obj.m->next == NULL) msg.print("Already at last loaded model.\n");
	else
	{
		aten.setCurrentModel(obj.m->next);
		msg.print("Current model is now '%s'.\n", obj.m->name());
	}
	return Command::Success;
}

// Skip to previous loaded model ('prevmodel')
int Command::function_CA_PREVMODEL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (obj.m->prev == NULL) msg.print("Already at first loaded model.\n");
	else
	{
		aten.setCurrentModel(obj.m->prev);
		msg.print("Current model is now '%s'.\n",obj.m->name());
	}
	return Command::Success;
}

// Save current model ('savemodel <format> <filename>')
int Command::function_CA_SAVEMODEL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// Find filter with a nickname matching that given in argc(0)
	Filter *f = aten.findFilter(Filter::ModelExport, c->argc(0));
	// Check that a suitable format was found
	if (f == NULL)
	{
		msg.print("No model export filter was found that matches the nickname '%s'.\nNot saved.\n", c->argc(0));
		return Command::Fail;
	}
	obj.rs->setFilter(f);
	obj.rs->setFilename(c->argc(1));
	return (f->execute(c->argc(1)) ? Command::Success : Command::Fail);
}

// Set name of current model ('setname <name>')
int Command::function_CA_SETNAME(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->setName(c->argc(0));
	msg.print(Messenger::Verbose,"Renamed model to '%s'\n", obj.rs->name());
	return Command::Success;
}

