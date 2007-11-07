/*
	*** Filter surface import
	*** src/file/importsurface.cpp
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
#include "base/master.h"

// Import grid using filter
void filter::import_grid(const char *filename)
{
	dbg_begin(DM_CALLS,"filter::import_grid");
	// Check the obvious first...
	if (type != FT_GRID_IMPORT)
	{
		printf("filter::import_grid <<<< This filter does not provide surface importing >>>>\n");
		dbg_end(DM_CALLS,"filter::import_grid");
		return;
	}
	msg(DM_NONE,"Load   : %s (%s)\n",filename,name.get());
	// Open file...
	if (!set_input(filename))
	{
		msg(DM_NONE,"Error opening grid file '%s'.\n",filename);
		dbg_end(DM_CALLS,"filter::import_grid");
		return;
	}
	// Reset target variables
	reset_targets();
	// Set element mapping type to that specified in file
	zmap_type temp_zmap = prefs.get_zmapping();
	if (has_zmapping) prefs.set_zmapping(zmapping);
	// Run the import commands on the file...
	command_node<filter_command> *fn = commands.commandlist.first();
	while (fn != NULL)
	{
		msg(DM_FILTERS,"(((( Import Grid : Command '%s' ))))\n", (fn->get_basic_command() != BC_OTHER ? text_from_BC(fn->get_basic_command()) : text_from_FC(fn->get_command())));
		if (fn->get_basic_command() == BC_TERMINATE) break;
		// Try flow control commands first, then do filter-specific commands
		if (commands.do_basic(fn, activemodel, inputfile)) continue;
		else if (do_surface(fn)) continue;
		else if (do_readwrite(fn)) continue;
		else if (do_actions(fn)) continue;
		else if (do_variables(fn)) continue;
		else
		{
			printf("filter::import_grid <<<< Command '%s' has no defined action >>>>\n", (fn->get_basic_command() != BC_OTHER ? text_from_BC(fn->get_basic_command()) : text_from_FC(fn->get_command())));
			fn = fn->next;
		}
	}
	close_files();
	// Reset element mapping style
	prefs.set_zmapping(temp_zmap);
	dbg_end(DM_CALLS,"filter::import_grid");
}
