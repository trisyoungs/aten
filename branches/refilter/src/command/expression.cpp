/*
	*** Energy command functions
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

#include "command/commandlist.h"
#include "base/prefs.h"
#include "base/debug.h"
#include "model/model.h"

// Create energy expression for current model ('createexpression'}
int command_functions::function_CA_CREATEEXPRESSION(command *&c, bundle &obj)
{
	if (!obj.m->autocreate_patterns()) return CR_FAIL;
	if (!obj.m->create_expression()) return CR_FAIL;
	return CR_SUCCESS;
}

// Set electrostatics cutoff ('ecut <cut>')
int command_functions::function_CA_ECUT(command *&c, bundle &obj)
{
	prefs.set_elec_cutoff(c->argd(0));
	return CR_SUCCESS;
}

// Set electrostatic method to use ('elec none|coulomb|ewald|ewaldauto')
int command_functions::function_CA_ELEC(command *&c, bundle &obj)
{
	elec_method em = EM_from_text(c->argc(0));
	if (em == EM_NITEMS) return CR_FAIL;
	prefs.set_electrostatics(em);
	prefs.set_calc_elec(em == EM_OFF ? FALSE : TRUE);
	switch (em)
	{
		// Set ewald sum params ('elec ewald <alpha> <kx ky kz>')
		case (EM_EWALD):
			prefs.set_ewald_alpha(c->argd(1));
			prefs.set_ewald_kvec(c->get_vector3i(2));
			break;
		// Set ewald precision
		case (EM_EWALDAUTO):
			prefs.set_ewald_precision(c->argd(1));
			break;
	}
	return CR_SUCCESS;
}

// Turn on/off calculation of intra ('intra on|off')
int command_functions::function_CA_INTRA(command *&c, bundle &obj)
{
	prefs.set_calc_intra(c->argb(0));
	return CR_SUCCESS;
}

// Print expression setup ('printexpression')
int command_functions::function_CA_PRINTEXPRESSION(command *&c, bundle &obj)
{
	msg(DM_NONE,"Current Energy Setup:\n");
	msg(DM_NONE,"Intramolecular Terms : %s\n",(prefs.calc_intra() ? "On" : "Off"));
	msg(DM_NONE,"       van der Waals : %s\n",(prefs.calc_vdw() ? "On" : "Off"));
	msg(DM_NONE,"      Electrostatics : %s (%s)\n",(prefs.calc_elec() ? "On" : "Off"), text_from_EM(prefs.get_electrostatics()));
	msg(DM_NONE,"             Cutoffs : %13.6e (VDW)  %13.6e (elec)\n", prefs.get_vdw_cutoff(), prefs.get_elec_cutoff());
	return CR_SUCCESS;
}

// Set VDW cutoff ('vcut <cut>')
int command_functions::function_CA_VCUT(command *&c, bundle &obj)
{
	prefs.set_vdw_cutoff(c->argd(0));
	return CR_SUCCESS;

}

// Turn on/off calculation of vdw ('vdw on|off')
int command_functions::function_CA_VDW(command *&c, bundle &obj)
{
	prefs.set_calc_vdw(c->argb(0));
	return CR_SUCCESS;
}
