/*
	*** Script image functions
	*** src/script/image.cpp

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

#include "script/script.h"
#include "base/debug.h"

// Image-related script commands (root=SR_IMAGE)
bool script::command_image(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_image");
	bool result = TRUE;
	if (!check_activemodel(text_from_SC(cmd->get_command())))
	{
		dbg_end(DM_CALLS,"script::command_image");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		default:
			printf("Error - missed image command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_image");
	return result;
}
