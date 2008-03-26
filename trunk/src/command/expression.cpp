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
//#include "base/debug.h"
#include "model/model.h"
//#include "parse/filter.h"

// Create energy expression for current model ('createexpression'}
int CommandData::function_CA_CREATEEXPRESSION(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (!obj.m->autocreatePatterns()) return CR_FAIL;
	if (!obj.m->createExpression()) return CR_FAIL;
	return CR_SUCCESS;
}

// Print expression setup ('printexpression')
int CommandData::function_CA_PRINTSETUP(Command *&c, Bundle &obj)
{
	msg(Debug::None,"Current Energy Setup:\n");
	msg(Debug::None,"Intramolecular Terms : %s\n",(prefs.calculateIntra() ? "On" : "Off"));
	msg(Debug::None,"       van der Waals : %s\n",(prefs.calculateVdw() ? "On" : "Off"));
	msg(Debug::None,"      Electrostatics : %s (%s)\n",(prefs.calculateElec() ? "On" : "Off"), text_from_EM(prefs.electrostaticsMethod()));
	msg(Debug::None,"             Cutoffs : %13.6e (VDW)  %13.6e (elec)\n", prefs.vdwCutoff(), prefs.elecCutoff());
	return CR_SUCCESS;
}

// Save expression ('saveexpression <format> <file>')
int CommandData::function_CA_SAVEEXPRESSION(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// Find filter with a nickname matching that given in argc(0)
	Filter *f = master.findFilter(FT_EXPRESSION_EXPORT, c->argc(0));
	// Check that a suitable format was found
	if (f == NULL)
	{
		msg(Debug::None,"script : No expression export filter was found that matches the extension '%s'.\nNot saved.\n",c->argc(0));
		return CR_FAIL;
	}
	f->execute(c->argc(1));
	return CR_SUCCESS;
}
