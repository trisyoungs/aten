/*
	*** Script Monte Carlo functions
	*** src/script/mc.cpp
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
#include "script/script.h"
#include "methods/mc.h"
#include "base/debug.h"
#include "classes/pattern.h"

// Monte Carlo-related script commands (root=SR_MC)
bool script::command_mc(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_mc");
	bool result = TRUE;
	mc_move mt;
	int cmdi = cmd->get_command();	
	switch (cmdi)
	{
		case (SC_PRINTMC):	// Prints the current MC params ('printmc')
			msg(DM_NONE,"Current Monte Carlo Parameters are:\n");
			msg(DM_NONE,"Move        Allowed  NTrials  MaxStep   EAccept :\n");
			for (int n=0; n<MT_NITEMS; n++)
			{
				mt = (mc_move) n;
				msg(DM_NONE,"%11s   %3s   %4i   %8.3f   %8.2e\n",text_from_MT(mt), (master.mc.get_allowed(mt) ? "Yes" : "No"), master.mc.get_ntrials(mt), master.mc.get_maxstep(mt), master.mc.get_eaccept(mt));
			}
			break;
		case (SC_MCACCEPT):	// Set MC move parameters
		case (SC_MCALLOW):
		case (SC_MCNTRIALS):
		case (SC_MCMAXSTEP):
			// Check that a valid move type has been given
			mt = MT_from_text(cmd->datavar[0]->get_as_char());
			if (mt == MT_NITEMS) break;
			switch (cmdi)
			{
				case (SC_MCMAXSTEP):	// Sets maximum stepsizes for moves ('mc maxstep <move> <stepsize>')
					master.mc.set_maxstep(mt,cmd->datavar[1]->get_as_double());
					break;
				case (SC_MCNTRIALS):	// Sets ntrials for moves ('mc ntrials <move> <ntrials>')
					master.mc.set_ntrials(mt,cmd->datavar[1]->get_as_int());
					break;
				case (SC_MCACCEPT):	// Sets acceptance energy for moves ('mc accept <move> <energy>')
					master.mc.set_eaccept(mt,cmd->datavar[1]->get_as_double());
					break;
				case (SC_MCALLOW):	// Sets allowances for moves ('mc allow <move> <on|off>')
					master.mc.set_allowed(mt,cmd->datavar[1]->get_as_bool());
					break;
			}
			break;
		default:
			printf("Error - missed charge command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_charge");
	return result;
}
