/*
	*** Trajectory command functions
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

// Skip to first frame ('firstframe')
int command_functions::function_CA_FIRSTFRAME(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (obj.m->get_totalframes() == 0)
	{
		msg(DM_NONE,"No trajectory associated to model '%s'.\n",obj.m->get_name());
		return CR_FAIL;
	}
	obj.m->seek_first_frame();
	return CR_SUCCESS;
}

// Skip to last frame ('lastframe')
int command_functions::function_CA_LASTFRAME(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (obj.m->get_totalframes() == 0)
	{
		msg(DM_NONE,"No trajectory associated to model '%s'.\n",obj.m->get_name());
		return CR_FAIL;
	}
	obj.m->seek_last_frame();
	return CR_SUCCESS;
}

// Open and associate trajectory ('loadtrajectory <file>')
int command_functions::function_CA_LOADTRAJECTORY(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	filter *f = master.probe_file(c->argc(0), FT_TRAJECTORY_IMPORT);
	if (f == NULL) return CR_FAIL;
	return (obj.m->initialise_trajectory(c->argc(0),f) ? CR_SUCCESS : CR_FAIL);
}

// Go to next frame ('nextframe')
int command_functions::function_CA_NEXTFRAME(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (obj.m->get_totalframes() == 0)
	{
		msg(DM_NONE,"No trajectory associated to model '%s'.\n",obj.m->get_name());
		return CR_FAIL;
	}
	obj.m->seek_next_frame();
	return CR_SUCCESS;
}

// Go to previous frame ('prevframe')
int command_functions::function_CA_PREVFRAME(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (obj.m->get_totalframes() == 0)
	{
		msg(DM_NONE,"No trajectory associated to model '%s'.\n",obj.m->get_name());
		return CR_FAIL;
	}
	obj.m->seek_previous_frame();
	return CR_SUCCESS;
}
