/*
	*** Filter model export
	*** src/file/exportmodel.cpp
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

// Export model using filter
void filter::export_model(model *sourcemodel)
{
	dbg_begin(DM_CALLS,"filter::export_model");
	// Check the obvious first...
	if (type != FT_MODEL_EXPORT)
	{
		printf("filter::export_model <<<< This file filter does not provide model exporting! >>>>\n");
		dbg_end(DM_CALLS,"filter::export_model");
		return;
	}
	if (sourcemodel == NULL)
	{
		printf("filter::export_model <<<< NULL model pointer passed! >>>>\n");
		dbg_end(DM_CALLS,"filter::export_model");
		return;
	}
	msg(DM_NONE,"Save   : %s (%s)...",sourcemodel->get_filename(),name.get());
	bool advance, done;
	atom *currentatom = NULL;
	int n;
	// Set up variables
	commands.variables.set_model_variables(sourcemodel);
	commands.variables.set_cell_variables(sourcemodel->get_cell());
	// Open file and set target
	if (!set_output(sourcemodel->get_filename()))
	{
		msg(DM_NONE,"Error opening output file '%s'.\n",sourcemodel->get_filename());
		dbg_end(DM_CALLS,"filter::export_model");
		return;
	}
	set_target(sourcemodel);
	done = FALSE;
	command_node<filter_command> *fn = commands.commandlist.first();
	while (fn != NULL)
	{
		msg(DM_FILTERS,"(((( Export Model : Command '%s' ))))\n", (fn->get_basic_command() != BC_OTHER ? text_from_BC(fn->get_basic_command()) : text_from_FC(fn->get_command())));
		if (fn->get_basic_command() == BC_TERMINATE) break;
		// Try commands
		if (commands.do_basic(fn, activemodel, NULL)) continue;
		else if (do_variables(fn)) continue;
		else if (do_readwrite(fn)) continue;
		else if (do_actions(fn)) continue;
		else
		{
			printf("filter::export_model <<<< Command '%s' has no defined action >>>>\n", (fn->get_basic_command() != BC_OTHER ? text_from_BC(fn->get_basic_command()) : text_from_FC(fn->get_command())));
			fn = fn->next;
		}
	}
	msg(DM_NONE,"Done.\n");
	close_files();
	// Set new save point for model
	sourcemodel->update_save_point();
	dbg_end(DM_CALLS,"filter::export_model");
}
