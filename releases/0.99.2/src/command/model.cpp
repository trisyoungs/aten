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
#include "base/aten.h"
#include "base/messenger.h"
#include "classes/forcefield.h"
#include "parse/filter.h"
#include "model/model.h"

// Create 'n' new atoms at once in model
int CommandData::function_CA_CREATEATOMS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Vec3<double> v;
	for (int n = 0; n < c->argi(0); n++) obj.i = obj.rs->addAtom(0, v);
	return CR_SUCCESS;
}

// Finalise current model
int CommandData::function_CA_FINALISEMODEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
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
	obj.m->resetLogs();
	// Print out some useful info on the model that we've just read in
	msg.print("Atoms  : %i\n",obj.m->nAtoms());
	msg.print("Cell   : %s\n",Cell::cellType(obj.m->cell()->type()));
	if (obj.m->cell()->type() != Cell::NoCell) obj.m->cell()->print();
	// Lastly, reset all the log points and start afresh
	obj.m->resetLogs();
	obj.m->updateSavePoint();
	return CR_SUCCESS;
}

// Select working model ('getmodel <name>')
int CommandData::function_CA_GETMODEL(Command *&c, Bundle &obj)
{
	// If the argument is an integer, get by id. Otherwise, get by name
	Model *m = (c->argt(0) == Variable::IntegerVariable ? aten.model(c->argi(0)) : aten.findModel(c->argc(0)));
	if (m != NULL) 
	{
		aten.setCurrentModel(m);
		//gui.select_model(m);
		c->parent()->setModelVariables(obj.m);
		obj.p = NULL;
		obj.i = m->atoms();
		return CR_SUCCESS;
	}
	else
	{
		msg.print("No model named '%s' is available, or integer id %i is out of range.\n", c->argc(0),c->argi(0));
		return CR_FAIL;
	}
}

// Print all information for model ('info')
int CommandData::function_CA_INFO(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->renderSource()->print();
	return CR_SUCCESS;
}

// Print loaded models ('listmodels')
int CommandData::function_CA_LISTMODELS(Command *&c, Bundle &obj)
{
	if (aten.nModels() != 0) msg.print("Name            NAtoms  Forcefield\n");
	for (Model *m = aten.models(); m != NULL; m = m->next)
		msg.print("%-15s %5i  %-15s\n", m->name(),m->nAtoms(),(m->forcefield() != NULL ? m->forcefield()->name() : "None"));
	return CR_SUCCESS;
}

// Load model ('loadmodel <filename> [name]')
int CommandData::function_CA_LOADMODEL(Command *&c, Bundle &obj)
{
	Filter *f = aten.probeFile(c->argc(0), Filter::ModelImport);
	if (f != NULL)
	{
		if (f->execute(c->argc(0)))
		{
			Model *m = aten.currentModel();
			if (c->hasArg(1)) m->setName(c->argc(1));
			obj.i = m->atoms();
			c->parent()->setModelVariables(m);
			return CR_SUCCESS;
		}
		else return CR_FAIL;
	} else return CR_FAIL;
}

// Print log information for model ('loginfo')
int CommandData::function_CA_LOGINFO(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->renderSource()->printLogs();
	return CR_SUCCESS;
}

// Use parent model as atom template
int CommandData::function_CA_MODELTEMPLATE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (obj.m == obj.rs)
	{
		printf("Cannot perform model templating in the parent model.\n");
		return CR_FAIL;
	}
	// Create the atoms template
	Vec3<double> v;
	Atom *j;
	for (obj.i = obj.m->atoms(); obj.i != NULL; obj.i = obj.i->next)
	{
		j = obj.rs->addAtom(obj.i->element(), v);
		j->copyStyle(obj.i);
	}
	return CR_SUCCESS;
}

// Create new model ('newmodel <name>')
int CommandData::function_CA_NEWMODEL(Command *&c, Bundle &obj)
{
	obj.m = aten.addModel();
	obj.m->setName(stripTrailing(c->argc(0)));
	msg.print("Created model '%s'\n", obj.m->name());
	if (prefs.keepNames())
	{
		Forcefield *f = aten.addForcefield();
		char s[512];
		sprintf(s,"Names: %s",obj.m->name());
		f->setName(s);
		obj.m->setNamesForcefield(f);
	}
	return CR_SUCCESS;
}

// Skip to next loaded model ('nextmodel')
int CommandData::function_CA_NEXTMODEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (obj.m->next == NULL) msg.print("Already at last loaded model.\n");
	else
	{
		aten.setCurrentModel(obj.m->next);
		msg.print("Current model is now '%s'.\n", obj.m->name());
		c->parent()->setModelVariables(obj.m);
	}
	return CR_SUCCESS;
}

// Skip to previous loaded model ('prevmodel')
int CommandData::function_CA_PREVMODEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (obj.m->prev == NULL) msg.print("Already at first loaded model.\n");
	else
	{
		aten.setCurrentModel(obj.m->prev);
		msg.print("Current model is now '%s'.\n",obj.m->name());
		c->parent()->setModelVariables(obj.m);
	}
	return CR_SUCCESS;
}

// Save current model ('savemodel <format> <filename>')
int CommandData::function_CA_SAVEMODEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// Find filter with a nickname matching that given in argc(0)
	Filter *f = aten.findFilter(Filter::ModelExport, c->argc(0));
	// Check that a suitable format was found
	if (f == NULL)
	{
		msg.print("No model export filter was found that matches the nickname '%s'.\nNot saved.\n", c->argc(0));
		return CR_FAIL;
	}
	obj.rs->setFilter(f);
	obj.rs->setFilename(c->argc(1));
	return (f->execute(c->argc(1)) ? CR_SUCCESS : CR_FAIL);
}

// Set name of current model ('setname <name>')
int CommandData::function_CA_SETNAME(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->setName(c->argc(0));
	c->parent()->setModelVariables(obj.m);
	msg.print(Messenger::Verbose,"Renamed model to '%s'\n", obj.rs->name());
	return CR_SUCCESS;
}

