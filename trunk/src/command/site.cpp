/*
	*** Site command functions
	*** src/command/site.cpp
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
#include "base/pattern.h"
#include "classes/site.h"
#include "model/model.h"

// Add site definition to model ('newsite <name> <pattern> <"atomids...">')
int Command::function_CA_NEWSITE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// First, check that the pattern name provided refers to a pattern of the current model
	Pattern *p = obj.m->findPattern(c->argc(1));
	if (p == NULL) return Command::Fail;
	obj.s = obj.m->sites.add();
	obj.s->setName(c->argc(0));
	obj.s->setPattern(p);
	// Parse the atom list which should have been given as: "1,2,3,4,5......"
	if (c->hasArg(2))
	{
		parser.getArgsDelim(c->argc(2), Parser::Defaults);
		for (int n=0; n<parser.nArgs(); n++)
		{
			ListItem<int> *li = obj.s->atoms.add();
			// Store n-1 since internally we work in 0-n range
			li->data = parser.argi(n) - 1;
		}
	}
	msg.print("New site added for model: '%s', for pattern '%s', %i atoms defined%s", obj.s->name(), p->name(), obj.s->atoms.nItems(), (obj.s->atoms.nItems() == 0 ? " (will use centre of geometry)\n" : "\n"));
	return Command::Success;
}

// Print site definitions for model ('listsites')
int Command::function_CA_LISTSITES(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Site *s = obj.m->sites.first();
	if (s == NULL) msg.print("No sites defined for model '%s'.\n",obj.m->name());
	else
	{
		msg.print("Site list for model '%s':\n",obj.m->name());
		for (s = s; s != NULL; s = s->next)
		{
			msg.print(" %15s %15s  ",s->name(), s->pattern()->name());
			if (s->atoms.nItems() == 0) msg.print("All atoms assumed (none defined)");
			else for (ListItem<int> *li = s->atoms.first(); li != NULL; li = li->next) msg.print(" %i",li->data);
			msg.print("\n");
		}
	}
	return Command::Success;
}

// Select named site from currently defined model sites ('getsite <name>')
int Command::function_CA_GETSITE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Site *s;
	for (s = obj.m->sites.first(); s != NULL; s = s->next) if (strcmp(s->name(),c->argc(0)) == 0) break;
	if (s == NULL) msg.print("No site '%s' defined in model '%s'.\n", c->argc(0), obj.m->name());
	else obj.s = s;
	return Command::Fail;
}

// Set x and y-axis definitions for current site ('siteaxes <"X-atomids..."> <"Y-atomids">')
int Command::function_CA_SITEAXES(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::SitePointer)) return Command::Fail;
	int n;
	ListItem<int> *li;
	// Parse atom list for x-axis
	parser.getArgsDelim(c->argc(0), Parser::Defaults);
	for (n=0; n<parser.nArgs(); n++)
	{
		li = obj.s->xAxisAtoms.add();
		// Store n-1 since internally we work in 0-n range
		li->data = parser.argi(n) - 1;
	}
	// Parse atom list for y-axis
	parser.getArgsDelim(c->argc(1), Parser::Defaults);
	for (n=0; n<parser.nArgs(); n++)
	{
		li = obj.s->yAxisAtoms.add();
		// Store n-1 since internally we work in 0-n range
		li->data = parser.argi(n) - 1;
	}
	return Command::Success;
}
