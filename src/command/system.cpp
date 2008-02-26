/*
	*** System control functions
	*** src/command/system.cpp
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

#include "command/commandlist.h"
#include "base/debug.h"
#include "base/master.h"
#include "gui/gui.h"

// Start GUI
int commanddata::function_CA_GUI(command *&c, bundle &obj)
{
	// If we're in interactive mode, just set program mode and let main.cpp handle it.
	if (master.get_program_mode() == PM_INTERACTIVE) master.set_program_mode(PM_GUI);
	else if (!gui.exists())
	{
		// Set program mode and start gui
		master.set_program_mode(PM_GUI);
		// Add empty model if none were specified on the command line
		if (master.get_nmodels() == 0) model *m = master.add_model();
		gui.run(0, NULL);
	}
	return CR_SUCCESS;
}

// Help function
int commanddata::function_CA_HELP(command *&c, bundle &obj)
{
	command_action ca = CA_from_text(c->argc(0));
	if (ca == CA_NITEMS) msg(DM_NONE,"help: Unrecognised command '%s'.\n",c->argc(0));
	else if (CA_data[ca].has_arguments()) msg(DM_NONE,"help:  %s  --  %s\n", CA_data[ca].get_keyword(), CA_data[ca].get_syntax());
	else msg(DM_NONE,"help:  %s %s  --  %s\n", CA_data[ca].get_keyword(), CA_data[ca].get_argtext(), CA_data[ca].get_syntax());
	return CR_SUCCESS;
}

// Quit main program
int commanddata::function_CA_QUIT(command *&c, bundle &obj)
{
	// Set program mode here, in case we are running in PM_COMMAND
	master.set_program_mode(PM_NONE);
	// If the GUI is active, close it...
	if (gui.exists()) gui.save_before_close();
	return CR_EXIT;
}
