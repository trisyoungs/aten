/*
	*** Model command functions
	*** src/command/model.cpp
	Copyright T. Youngs 2007

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
#include "file/filter.h"

// Finalise current model
int command_functions::function_CA_FINALISEMODEL(command *&c, bundle &obj)
{
	//if (partner != NULL) activemodel->set_filename(filename.get());
	//activemodel->set_filter(partner);
	printf("Model filter needs to be set....\n");
	// Do various necessary calculations
	if (prefs.get_coords_in_bohr()) obj.m->bohr_to_angstrom();
	obj.m->renumber_atoms();
	obj.m->reset_view();
	obj.m->calculate_mass();
	obj.m->calculate_density();
	// Print out some useful info on the model that we've just read in
	msg(DM_NONE,"Atoms  : %i\n",obj.m->get_natoms());
	msg(DM_NONE,"Cell   : %s\n",text_from_CT(obj.m->get_celltype()));
	if (obj.m->get_celltype() != 0) obj.m->get_cell()->print();
	// Lastly, reset all the log points and start afresh
	obj.m->reset_logs();
	obj.m->update_save_point();
	return CR_SUCCESS;
}

// Print loaded models ('listmodels')
int command_functions::function_CA_LISTMODELS(command *&c, bundle &obj)
{
	if (master.get_nmodels() != 0) msg(DM_NONE,"Name            NAtoms  Forcefield\n");
	for (model *m = master.get_models(); m != NULL; m = m->next)
		msg(DM_NONE,"%15s %5i  %15s\n", m->get_name(),m->get_natoms(),(m->get_ff() != NULL ? m->get_ff()->get_name() : "None"));
	return CR_SUCCESS;
}

// Load model ('loadmodel <filename> [name]')
int command_functions::function_CA_LOADMODEL(command *&c, bundle &obj)
{
	filter *f = master.probe_file(c->argc(0), FT_MODEL_IMPORT);
	if (f != NULL)
	{
		if (f->execute(c->argc(0)))
		{
			model *m = master.get_currentmodel();
			if (c->has_arg(1)) m->set_name(c->argc(1));
			obj.i = m->get_atoms();
			msg(DM_NONE,"script : Model '%s' loaded, name '%s'\n", c->argc(0), m->get_name());
			return CR_SUCCESS;
		}
		else
		{
			msg(DM_NONE,"script : Model '%s' couldn't be loaded.'\n", c->argc(1));
			return CR_FAIL;
		}
	}
	else return CR_FAIL;
}

// Create new model ('newmodel <name>')
int command_functions::function_CA_NEWMODEL(command *&c, bundle &obj)
{
	model *m = master.add_model();
	m->set_name(c->argc(0));
	msg(DM_NONE,"script : Create model '%s'\n", m->get_name());
	return CR_SUCCESS;
}

// Print all information for model ('printmodel')
int command_functions::function_CA_PRINTMODEL(command *&c, bundle &obj)
{
	obj.m->print();
	return CR_SUCCESS;
}

// Save current model ('savemodel <format> <filename>')
int command_functions::function_CA_SAVEMODEL(command *&c, bundle &obj)
{
	// Find filter with a nickname matching that given in argc(0)
	filter *f;
	for (f = master.filters[FT_MODEL_EXPORT].first(); f != NULL; f = f->next)
		if (strcmp(f->get_nickname(),c->argc(0)) == 0) break;
	// Check that a suitable format was found
	if (f == NULL)
	{
		msg(DM_NONE,"script : No model export filter was found that matches the nickname '%s'.\nNot saved.\n", c->argc(0));
		return CR_FAIL;
	}
	obj.m->set_filter(f);
	obj.m->set_filename(c->argc(1));
	return (f->execute_with_model(obj.m, c->argc(1)) ? CR_SUCCESS : CR_FAIL);
}

// Select working model ('selectmodel <name>')
int command_functions::function_CA_SELECTMODEL(command *&c, bundle &obj)
{
	model *m = master.find_model(c->argc(0));
	if (m != NULL) 
	{
		master.set_currentmodel(m);
		//gui.select_model(m);
		obj.p = NULL;
		obj.i = m->get_atoms();
		return CR_SUCCESS;
	}
	else
	{
		msg(DM_NONE,"No model named '%s' is loaded.\n", c->argc(0));
		return CR_FAIL;
	}
}
