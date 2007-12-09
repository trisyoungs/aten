/*
	*** Script trajectory functions
	*** src/command/traj.cpp
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
#include "file/filter.h"

// Trajectory-related script commands (root=SR_TRAJ)
bool script::command_traj(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_traj");
	bool result = TRUE;
	filter *f;
	model *m = check_activemodel(text_from_SC(cmd->get_command()));
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_traj");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		case (SC_LOADTRAJECTORY):	// Open and associate trajectory ('loadtrajectory <file>')
			f = master.probe_file(cmd->argc(0), FT_TRAJECTORY_IMPORT);
			if (f != NULL) result = m->initialise_trajectory(cmd->argc(0),f);
			else result = FALSE;
			break;
		case (SC_FIRSTFRAME):		// Skip to first frame ('firstframe')
			if (check_traj(text_from_SC(cmd->get_command()))) m->seek_first_frame();
			break;
		case (SC_PREVFRAME):		// Go to previous frame ('prevframe')
			if (check_traj(text_from_SC(cmd->get_command()))) m->seek_previous_frame();
			break;
		case (SC_NEXTFRAME):		// Go to next frame ('nextframe')
			if (check_traj(text_from_SC(cmd->get_command()))) m->seek_next_frame();
			break;
		case (SC_LASTFRAME):		// Skip to last frame ('lastframe')
			if (check_traj(text_from_SC(cmd->get_command()))) m->seek_last_frame();
			break;
		default:
			printf("Error - missed traj command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_traj");
	return result;
}
