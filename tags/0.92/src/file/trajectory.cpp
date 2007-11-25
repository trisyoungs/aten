/*
	*** Trajectory filter commands
	*** src/file/trajectory.cpp
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

// Read from trajectory
bool filter::read_trajectory(model *destmodel, bool readheader)
{
	// Attempts to open the trajectory file specified
	dbg_begin(DM_CALLS,"filter::read_trajectory");
	model *parent;
	// Check the obvious first...
	if (type != FT_TRAJECTORY_IMPORT)
	{
		printf("filter::read_trajectory <<<< Filter does not provide trajectory importing! >>>>\n");
		dbg_end(DM_CALLS,"filter::read_trajectory");
		return FALSE;
	}
	// Open file...
	if (readheader && (!set_input(destmodel->get_trajfile())))
	{
		msg(DM_NONE,"Error getting trajectory file.\n");
		dbg_end(DM_CALLS,"filter::read_trajectory(header)");
		return FALSE;
	}
	// Set variables
	commands.variables.set("header",(readheader ? "true" : "false"));
	commands.variables.set("frame",(readheader ? "false" : "true"));
	// Set model target (if reading a frame)
	if (!readheader)
	{
		parent = destmodel->get_trajparent();
		if (parent == NULL)
		{
			msg(DM_NONE,"filter::read_trajectory <<<< Trajectory parent is not set in frame model >>>>\n");
			dbg_end(DM_CALLS,"filter::read_trajectory(frame)");
			return FALSE;	
		}
		commands.variables.set("natoms",parent->get_natoms());
		commands.variables.set("cell.type",lower_case(text_from_CT(parent->cell.get_type())));
		set_target(destmodel);
		destmodel->clear();
	}
	else
	{
		commands.variables.set("natoms",destmodel->get_natoms());
		commands.variables.set("cell.type",lower_case(text_from_CT(destmodel->cell.get_type())));
	}
	// Run the header import commands on the file...
	command_node<filter_command> *fn = commands.commandlist.first();
	while (fn != NULL)
	{
		msg(DM_FILTERS,"(((( Read Trajectory (%s) : Command '%s' ))))\n", (readheader ? "header" : "frame"), (fn->get_basic_command() != BC_OTHER ? text_from_BC(fn->get_basic_command()) : text_from_FC(fn->get_command())));
		if (fn->get_basic_command() == BC_TERMINATE) break;
		// Try flow control commands
		if (commands.do_basic(fn, activemodel, inputfile)) continue;
		else if (do_variables(fn)) continue;
		else if (do_readwrite(fn)) continue;
		else if (do_actions(fn)) continue;
		else
		{
			printf("filter::read_trajectory <<<< Command '%s' has no defined action >>>>\n", (fn->get_basic_command() != BC_OTHER ? text_from_BC(fn->get_basic_command()) : text_from_FC(fn->get_command())));
			fn = fn->next;
		}
	}
	dbg_end(DM_CALLS,"filter::read_trajectory");
	return TRUE;
}
