/*
	*** Script energy functions
	*** src/command/energy.cpp
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
#include "base/prefs.h"
#include "base/debug.h"

// Expression-related script commands (root=SR_EXPRESSION)
bool script::command_expr(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_expr");
	bool result = TRUE;
	elec_type et;
	model *m;
	switch (cmd->get_command())
	{
		case (SC_VCUT):		// Set VDW cutoff ('vcut <cut>')
			prefs.set_vdw_cutoff(cmd->datavar[0]->get_as_double());
			break;
		case (SC_ECUT):		// Set electrostatics cutoff ('ecut <cut>')
			prefs.set_elec_cutoff(cmd->datavar[0]->get_as_double());
			break;
		case (SC_VDW):		// Turn on/off calculation of vdw ('vdw on|off')
			prefs.set_calc_vdw(cmd->datavar[0]->get_as_bool());
			break;
		case (SC_ELEC):		// Set electrostatic method to use ('elec none|coulomb|ewald|ewaldauto')
			et = EM_from_text(cmd->datavar[0]->get_as_char());
			if (et == EM_NITEMS)
			{
				result = FALSE;
				break;
			}
			prefs.set_electrostatics(et);
			prefs.set_calc_elec((et == EM_OFF ? FALSE : TRUE));
			switch (et)
			{
				case (EM_EWALD):	// Set ewald sum params ('elec ewald <alpha> <kx ky kz>')
					prefs.set_ewald_alpha(cmd->datavar[1]->get_as_double());
					prefs.set_ewald_kvec(cmd->get_vector3i(2));
					break;
				case (EM_EWALDAUTO):	// Set ewald precision
					prefs.set_ewald_precision(cmd->datavar[1]->get_as_double());
					break;
			}
			break;
		case (SC_CREATEEXPRESSION):	// Create energy expression for current model ('createexpression'}
			m = check_activemodel(text_from_SC(cmd->get_command()));
			if (m == NULL)
			{
				dbg_end(DM_CALLS,"script::command_expr");
				return FALSE;
			}
			else
			{
				m->autocreate_patterns();
				m->create_expression();
			}
			break;
		case (SC_INTRA):	// Turn on/off calculation of intra ('intra on|off')
			prefs.set_calc_intra(cmd->datavar[0]->get_as_bool());
			break;
		case (SC_PRINTEXPRESSION):// Print expression setup ('printexpression')
			msg(DM_NONE,"Current Energy Setup:\n");
			msg(DM_NONE,"Intramolecular Terms : %s\n",(prefs.calc_intra() ? "On" : "Off"));
			msg(DM_NONE,"       van der Waals : %s\n",(prefs.calc_vdw() ? "On" : "Off"));
			msg(DM_NONE,"      Electrostatics : %s (%s)\n",(prefs.calc_elec() ? "On" : "Off"), text_from_EM(prefs.get_electrostatics()));
			msg(DM_NONE,"             Cutoffs : %13.6e (VDW)  %13.6e (elec)\n", prefs.get_vdw_cutoff(), prefs.get_elec_cutoff());
			break;
		default:
			printf("Error - missed expr command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_expr");
	return result;
}

// Energy-related script commands (root=SR_ENERGY)
bool script::command_energy(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_energy");
	bool result = TRUE;
	double energy;
	model *mframe;
	model *m = check_activemodel("Energy commands");
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_energy");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		case (SC_MODELENERGY):		// Calculate energy of current model contents ('modelenergy')
			if (m->create_expression()) energy = m->total_energy(m);
			else result = FALSE;
			break;
		case (SC_FRAMEENERGY):		// Calculate energy of current trajectory frame ('frameenergy')
			mframe = m->get_currentframe();
			if (m->create_expression()) energy = m->total_energy(mframe);
			else result = FALSE;
			break;
		case (SC_PRINTENERGY):		// Print long energy decomposition of model ('printenergy')
			m->energy.print();
			break;
		case (SC_PRINTSUMMARY):		// Print short energy decomposition of model ('printsummary')
			m->energy.print_summary();
			break;
		case (SC_PRINTEWALD):	// Print out Ewald energy decomposition of model ('printewald')
			m->energy.print_ewald();
			break;
		case (SC_PRINTVDW):	// Print out VDW decomposition matrix ('printvdw')
			m->energy.print_vdwmatrix(m);
			break;
		case (SC_PRINTELEC):	// Print out electrostatic decomposition matrix ('printelec')
			m->energy.print_elecmatrix(m);
			break;
		case (SC_PRINTINTER):	// Print out interpattern decomposition matrix ('printinter')
			m->energy.print_intermatrix(m);
			break;
		case (SC_PRINTINTRA):	// Print out intramolecular decomposition matrix ('printintra')
			m->energy.print_intramatrix(m);
			break;
		default:
			printf("Error - missed energy command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_energy");
	return result;
}

// Force-related script commands (root=SR_FORCES)
bool script::command_forces(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_forces");
	bool result = TRUE;
	model *mframe;
	model *m = check_activemodel("Force commands");
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_forces");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		case (SC_MODELFORCES):		// Calculate atomic forces of model ('modelforces')
			if (m->create_expression()) m->calculate_forces(m);
			else result = FALSE;
			break;
		case (SC_FRAMEFORCES):		// Calculate forces at trajectory configuration ('frameforces')
			mframe = m->get_currentframe();
			if (m->create_expression()) m->calculate_forces(mframe);
			else result = FALSE;
			break;
		case (SC_PRINTFORCES):		// Print forces of model ('printforces')
			m->print_forces();
			break;
		default:
			printf("Error - missed expr command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_forces");
	return result;
}
