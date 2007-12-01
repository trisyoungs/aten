/*
	*** Filter field export
	*** src/file/exportfield.cpp
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

// Export forcefield spec using filter
void filter::export_field(model *sourcemodel, const char *filename)
{
	dbg_begin(DM_CALLS,"filter::export_field");
	// Check the obvious first...
	if (type != FT_FIELD_EXPORT)
	{
		printf("filter::export_field <<<< This filter does not allow field exporting >>>>\n");
		dbg_end(DM_CALLS,"filter::export_field");
		return;
	}
	msg(DM_NONE,"Save   : %s (%s)\n",filename,name.get());
	// Set model source
	set_target(sourcemodel);
	// Need a valid pattern and energy expression to export
	if (!sourcemodel->autocreate_patterns() || !sourcemodel->create_expression())
	{
		msg(DM_NONE,"filter::export_field - Must have valid pattern and energy expression to export a field file\n.");
		dbg_end(DM_CALLS,"filter::export_field");
		return;
	}
	// Set reserved variables
	commands.variables.set("title",activemodel->get_name());
	commands.variables.set("npatterns",activemodel->get_npatterns());
	commands.variables.set("energyunit",text_from_EU(prefs.get_internal_units()));
	// Open file...
	if (!set_output(filename))
	{
		msg(DM_NONE,"Error opening field file '%s'.\n",filename);
		dbg_end(DM_CALLS,"filter::export_field");
		return;
	}
	// Run the import commands on the file...
	command_node<filter_command> *fn = commands.commandlist.first();
	while (fn != NULL)
	{
		msg(DM_FILTERS,"(((( Export Field : Command '%s' ))))\n", (fn->get_basic_command() != BC_OTHER ? text_from_BC(fn->get_basic_command()) : text_from_FC(fn->get_command())));
		if (fn->get_basic_command() == BC_TERMINATE) break;
		// Try flow control commands
		if (commands.do_basic(fn, activemodel, NULL)) continue;
		else if (do_variables(fn)) continue;
		else if (do_readwrite(fn)) continue;
		else
		{
			printf("filter::export_field <<<< Command '%s' has no defined action >>>>\n", (fn->get_basic_command() != BC_OTHER ? text_from_BC(fn->get_basic_command()) : text_from_FC(fn->get_command())));
			fn = fn->next;
		}
	}
	close_files();
	dbg_end(DM_CALLS,"filter::export_field");
}
