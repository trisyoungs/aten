/*
	*** Script minimiser functions
	*** src/command/minimise.cpp
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

#include "base/master.h"
#include "command/commands.h"
#include "base/debug.h"
#include "methods/mc.h"
#include "classes/cell.h"

// Local variables
double econverge = 0.001, fconverge = 0.01, linetolerance = 0.0001;

// Minimiser-related script commands
bool script::command_minimise(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_minimise");
	bool result = TRUE;
	model *m = check_activemodel(text_from_SC(cmd->get_command()));
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_minimise");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		// Set convergence criteria
		case (SC_CONVERGE):
			econverge = cmd->argd(0);
			fconverge = cmd->argd(1);
			break;
		// Set line minimiser tolerance
		case (SC_LINETOL):
			linetolerance = cmd->argd(0);
			break;
		// Minimise current model with Monte-Carlo method ('mcminimise <maxsteps>')
		case (SC_MCMINIMISE):
			master.mc.set_ncycles(cmd->argi(0));
			master.mc.minimise(m, econverge, fconverge);
			break;
		// Minimise current model with Steepest Descent method ('sdminimise <maxsteps>')
		case (SC_SDMINIMISE):
			master.sd.set_tolerance(linetolerance);
			master.sd.set_ncycles(cmd->argi(0));
			master.sd.minimise(m, econverge, fconverge);
			break;
		default:
			printf("Error - missed minimise command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_minimise");
	return result;
}
