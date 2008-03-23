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
#include "base/master.h"
#include "base/debug.h"
#include "classes/forcefield.h"
#include "parse/filter.h"
#include "model/model.h"

// Create 'n' new atoms at once in model
int CommandData::function_CA_CREATEATOMS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Vec3<double> v;
	for (int n = 0; n < c->argi(0); n++) obj.i = obj.m->addAtom(0, v);
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
	obj.m->resetView();
	obj.m->calculateMass();
	obj.m->calculateDensity();
	obj.m->selectNone();
	obj.m->resetLogs();
	// Print out some useful info on the model that we've just read in
	msg(DM_NONE,"Atoms  : %i\n",obj.m->nAtoms());
	msg(DM_NONE,"Cell   : %s\n",text_from_CT(obj.m->cell()->type()));
	if (obj.m->cell()->type() != CT_NONE) obj.m->cell()->print();
	// Lastly, reset all the log points and start afresh
	obj.m->resetLogs();
	obj.m->updateSavePoint();
	return CR_SUCCESS;
}

// Print loaded models ('listmodels')
int CommandData::function_CA_LISTMODELS(Command *&c, Bundle &obj)
{
	if (master.nModels() != 0) msg(DM_NONE,"Name            NAtoms  Forcefield\n");
	for (Model *m = master.models(); m != NULL; m = m->next)
		msg(DM_NONE,"%-15s %5i  %-15s\n", m->name(),m->nAtoms(),(m->forcefield() != NULL ? m->forcefield()->name() : "None"));
	return CR_SUCCESS;
}

// Load model ('loadmodel <filename> [name]')
int CommandData::function_CA_LOADMODEL(Command *&c, Bundle &obj)
{
	Filter *f = master.probeFile(c->argc(0), FT_MODEL_IMPORT);
	if (f != NULL)
	{
		if (f->execute(c->argc(0)))
		{
			Model *m = master.currentModel();
			if (c->hasArg(1)) m->setName(c->argc(1));
			obj.i = m->atoms();
			return CR_SUCCESS;
		}
		else return CR_FAIL;
	} else return CR_FAIL;
}

// Use parent model as atom template
int CommandData::function_CA_MODELTEMPLATE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Model *parent = obj.m->trajectoryParent();
	if (parent == NULL)
	{
		printf("<<<< SEVERE - No trajectory parent set for 'modeltemplate' >>>>\n");
		return CR_FAIL;
	}
	// Create the atoms template
	Vec3<double> v;
	Atom *j;
	for (obj.i = parent->atoms(); obj.i != NULL; obj.i = obj.i->next)
	{
		j = obj.m->addAtom(obj.i->element(), v);
		j->copyStyle(obj.i);
	}
	return CR_SUCCESS;
}

// Create new model ('newmodel <name>')
int CommandData::function_CA_NEWMODEL(Command *&c, Bundle &obj)
{
	obj.m = master.addModel();
	obj.m->setName(stripTrailing(c->argc(0)));
	msg(DM_NONE,"Created model '%s'\n", obj.m->name());
	return CR_SUCCESS;
}

// Print all information for model ('info')
int CommandData::function_CA_INFO(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->print();
	return CR_SUCCESS;
}

// Save current model ('savemodel <format> <filename>')
int CommandData::function_CA_SAVEMODEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// Find filter with a nickname matching that given in argc(0)
	Filter *f = master.findFilter(FT_MODEL_EXPORT, c->argc(0));
	// Check that a suitable format was found
	if (f == NULL)
	{
		msg(DM_NONE,"No model export filter was found that matches the nickname '%s'.\nNot saved.\n", c->argc(0));
		return CR_FAIL;
	}
	obj.m->setFilter(f);
	obj.m->setFilename(c->argc(1));
	return (f->execute(c->argc(1)) ? CR_SUCCESS : CR_FAIL);
}

// Select working model ('getmodel <name>')
int CommandData::function_CA_GETMODEL(Command *&c, Bundle &obj)
{
	Model *m = master.findModel(c->argc(0));
	if (m != NULL) 
	{
		master.setCurrentModel(m);
		//gui.select_model(m);
		obj.p = NULL;
		obj.i = m->atoms();
		return CR_SUCCESS;
	}
	else
	{
		msg(DM_NONE,"No model named '%s' is available.\n", c->argc(0));
		return CR_FAIL;
	}
}

// Set title of model
int CommandData::function_CA_SETTITLE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->setName(c->argc(0));
	return CR_SUCCESS;
}
