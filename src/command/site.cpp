/*
	*** Site Commands
	*** src/command/site.cpp
	Copyright T. Youngs 2007-2018

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
#include "model/bundle.h"
#include "model/model.h"
#include "base/pattern.h"
#include "base/site.h"

ATEN_USING_NAMESPACE

// Add site definition to model ('newsite <name> <pattern> <"atomids...">')
bool Commands::function_NewSite(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// First, check that the pattern name provided refers to a pattern of the current model
	Pattern* p = obj.m->findPattern(c->argc(1));
	if (p == NULL) return false;
	obj.s = obj.m->sites.add();
	obj.s->setName(c->argc(0));
	obj.s->setPattern(p);
	// Parse the atom list which should have been given as: "1,2,3,4,5......"
	if (c->hasArg(2))
	{
		LineParser parser;
		parser.getArgsDelim(0, c->argc(2));
		for (int n=0; n<parser.nArgs(); n++)
		{
			// Store n-1 since internally we work in 0-n range
			obj.s->atoms << parser.argi(n) - 1;
		}
	}
	Messenger::print("New site added for model: '%s', for pattern '%s', %i atoms defined%s", qPrintable(obj.s->name()), qPrintable(p->name()), obj.s->atoms.count(), (obj.s->atoms.count() == 0 ? " (will use centre of geometry)\n" : "\n"));
	rv.reset();
	return true;
}

// Print site definitions for model ('listsites')
bool Commands::function_ListSites(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Site* s = obj.m->sites.first();
	if (s == NULL) Messenger::print("No sites defined for model '%s'.", qPrintable(obj.m->name()));
	else
	{
		Messenger::print("Site list for model '%s':", qPrintable(obj.m->name()));
		for (s = s; s != NULL; s = s->next)
		{
			Messenger::print(" %15s %15s  ", qPrintable(s->name()), qPrintable(s->pattern()->name()));
			if (s->atoms.count() == 0) Messenger::print("All atoms assumed (none defined)");
			else for (int n=0; n<s->atoms.count(); ++n) Messenger::print(" %i", s->atoms.at(n));
			Messenger::print("");
		}
	}
	rv.reset();
	return true;
}

// Select named site from currently defined model sites ('getsite <name>')
bool Commands::function_GetSite(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Site* s;
	for (s = obj.m->sites.first(); s != NULL; s = s->next) if (s->name() == c->argc(0)) break;
	if (s == NULL) Messenger::print("No site '%s' defined in model '%s'.", qPrintable(c->argc(0)), qPrintable(obj.m->name()));
	else obj.s = s;
	rv.reset();
	return false;
}

// Set x and y-axis definitions for current site ('siteaxes <"X-atomids..."> <"Y-atomids">')
bool Commands::function_SiteAxes(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::SitePointer)) return false;
	int n;
	LineParser parser;
	// Parse atom list for x-axis
	parser.getArgsDelim(0, c->argc(0));
	for (n=0; n<parser.nArgs(); n++)
	{
		// Store n-1 since internally we work in 0-n range
		obj.s->xAxisAtoms << parser.argi(n) - 1;
	}
	// Parse atom list for y-axis
	parser.getArgsDelim(0, c->argc(1));
	for (n=0; n<parser.nArgs(); n++)
	{
		// Store n-1 since internally we work in 0-n range
		obj.s->yAxisAtoms << parser.argi(n) - 1;
	}
	rv.reset();
	return true;
}

