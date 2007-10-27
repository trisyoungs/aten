/*
	*** Filter model import
	*** src/file/importmodel.cpp
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

#include "file/filter.h"
#include "model/model.h"
#include "base/master.h"

// Import model using filter
model *filter::import_model(const char *filename)
{
	dbg_begin(DM_CALLS,"filter::import_model");
	// Check the obvious first...
	if (type != FT_MODEL_IMPORT)
	{
		printf("filter::import_model <<<< This filter does not provide model importing >>>>\n");
		dbg_end(DM_CALLS,"filter::import_model");
		return NULL;
	}
	msg(DM_NONE,"Load   : %s (%s)\n",filename,name.get());
	// Re-set reserved variables
	commands.variables.set("title","Unnamed");
	set_target(NULL);
	// Set element mapping type to that specified in file
	zmap_type temp_zmap = prefs.get_zmapping();
	if (has_zmapping) prefs.set_zmapping(zmapping);
	// Open file...
	if (!set_input(filename))
	{
		msg(DM_NONE,"Error opening model file '%s'.\n",filename);
		dbg_end(DM_CALLS,"filter::import_model");
		return NULL;
	}
	// Run the import commands on the file...
	command_node<filter_command> *fn = commands.commandlist.first();
	while (fn != NULL)
	{
		msg(DM_FILTERS,"(((( Import Model : Command '%s' ))))\n", (fn->get_basic_command() != BC_OTHER ? text_from_BC(fn->get_basic_command()) : text_from_FC(fn->get_command())));
		if (fn->get_basic_command() == BC_TERMINATE) break;
		// Try flow control commands first, then do filter-specific commands
		if (commands.do_basic(fn, activemodel, inputfile)) continue;
		else if (do_variables(fn)) continue;
		else if (do_readwrite(fn)) continue;
		else if (do_actions(fn)) continue;
		else
		{
			printf("filter::import_model <<<< Command '%s' has no defined action >>>>\n", (fn->get_basic_command() != BC_OTHER ? text_from_BC(fn->get_basic_command()) : text_from_FC(fn->get_command())));
			fn = fn->next;
		}
	}
	close_files();
	// Reset element mapping style
	prefs.set_zmapping(temp_zmap);
	// Post-process file
	finalise_model_import(filename);
	dbg_end(DM_CALLS,"filter::import_model");
	return activemodel;
}

// Apply final actions to imported models
void filter::finalise_model_import(const char *filename)
{
	dbg_begin(DM_CALLS,"filter::finalise_model_import");
	activemodel->set_filename(filename);
	activemodel->set_filter(partner);
	// Do various necessary calculations
	if (prefs.get_coords_in_bohr()) activemodel->bohr_to_angstrom();
	activemodel->renumber_atoms();
	activemodel->reset_view();
	activemodel->calculate_mass();
	activemodel->calculate_density();
	// Print out some useful info on the model that we've just read in
	msg(DM_NONE,"Atoms  : %i\n",activemodel->get_natoms());
	msg(DM_NONE,"Cell   : %s\n",text_from_CT(activemodel->cell.get_type()));
	if (activemodel->cell.get_type() != 0) activemodel->cell.print();
	// Lastly, reset all the log points and start afresh
	activemodel->reset_logs();
	activemodel->update_save_point();
	dbg_end(DM_CALLS,"filter::finalise_model_import");
}
