/*
	*** Disorder Commands
	*** src/command/disorder.cpp
	Copyright T. Youngs 2007-2011

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
#include "parser/commandnode.h"
#include "main/aten.h"
#include "model/model.h"
#include "methods/mc.h"
#include "base/sysfunc.h"

// Performs MC insertion ('disorder <scheme>')
bool Command::function_Disorder(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Parse the first option so we can get at any options
	LineParser parser;
	parser.getArgsDelim(LineParser::Defaults, c->argc(0));
	// First argument should always be the scheme name
	PartitioningScheme *scheme = aten.findPartitioningScheme(parser.argc(0));
	if (scheme == NULL) return FALSE;
	// Loop over remaining arguments
	ReturnValue value;
	for (int n = 1; n < parser.nArgs(); ++n)
	{
		Variable *var = scheme->findVariable(beforeStr(parser.argc(n),"="));
		if (var != NULL)
		{
			value.set(afterStr(parser.argc(n),"="));
			var->set(value);
			var->execute(value);
			msg.print(" Variable '%s' in scheme '%s' now has value %s\n", var->name(), scheme->name(), value.asString());
		}
		else return FALSE;
	}
	msg.print("Performing disordered build for model '%s'\n", obj.m->name());
	rv.reset();
	if (!mc.disorder(obj.m, scheme, c->hasArg(1) ? c->argb(1) : TRUE)) return FALSE;
	return TRUE;
}

// Print current component list ('listcomponents')
bool Command::function_ListComponents(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	msg.print("Current component specification:\n");
	Vec3<double> v1, v2;
	Dnchar text;
	msg.print("Model           Policy       Partition       Population   Density\n");
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
// 		ComponentRegion *r = m->region();
// 		v1 = r->centre();
// 		v2 = r->geometry();
// 		text.sprintf("%-10s  %5i  %s %s %s %s %s  %-12s %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %3s\n",
// 			m->name(),m->nRequested(),
// 			(m->isMoveAllowed(MonteCarlo::Insert) ? "+" : " "),
// 			(m->isMoveAllowed(MonteCarlo::Delete) ? "+" : " "),
// 			(m->isMoveAllowed(MonteCarlo::Translate) ? "+" : " "),
// 			(m->isMoveAllowed(MonteCarlo::Rotate) ? "+" : " "),
// 			(m->isMoveAllowed(MonteCarlo::ZMatrix) ? "+" : " "),
// 			ComponentRegion::regionShape(r->shape()),
// 			v1.x, v1.y, v1.z, v2.x, v2.y, v2.z);
// 		msg.print(text);
	}
	rv.reset();
	return TRUE;
}

// Set vdw radius scaling for method ('vdwscale <scale>')
bool Command::function_VdwScale(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) mc.setVdwScale(c->argd(0));
	rv.set(mc.vdwScale());
	return TRUE;
}

