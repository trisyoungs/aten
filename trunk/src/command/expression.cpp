/*
	*** Expression command functions
	*** src/command/expression.cpp
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
#include "base/master.h"
#include "base/debug.h"
#include "model/model.h"
#include "parse/filter.h"

// Create energy expression for current model ('createexpression'}
int commanddata::function_CA_CREATEEXPRESSION(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (!obj.m->autocreate_patterns()) return CR_FAIL;
	if (!obj.m->create_expression()) return CR_FAIL;
	return CR_SUCCESS;
}

// Print expression setup ('printexpression')
int commanddata::function_CA_PRINTSETUP(command *&c, bundle &obj)
{
	msg(DM_NONE,"Current Energy Setup:\n");
	msg(DM_NONE,"Intramolecular Terms : %s\n",(prefs.calc_intra() ? "On" : "Off"));
	msg(DM_NONE,"       van der Waals : %s\n",(prefs.calc_vdw() ? "On" : "Off"));
	msg(DM_NONE,"      Electrostatics : %s (%s)\n",(prefs.calc_elec() ? "On" : "Off"), text_from_EM(prefs.get_electrostatics()));
	msg(DM_NONE,"             Cutoffs : %13.6e (VDW)  %13.6e (elec)\n", prefs.get_vdw_cutoff(), prefs.get_elec_cutoff());
	return CR_SUCCESS;
}

// Save expression ('saveexpression <format> <file>')
int commanddata::function_CA_SAVEEXPRESSION(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	// Find filter with a nickname matching that given in argc(0)
	filter *f = master.find_filter(FT_EXPRESSION_EXPORT, c->argc(0));
	// Check that a suitable format was found
	if (f == NULL)
	{
		msg(DM_NONE,"script : No expression export filter was found that matches the extension '%s'.\nNot saved.\n",c->argc(0));
		return CR_FAIL;
	}
	f->execute(c->argc(1));
	return CR_SUCCESS;
}
