/*
	*** Selection command functions
	*** src/command/select.cpp
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
#include "model/model.h"
#include "base/elements.h"
#include "base/sysfunc.h"
#include "base/debug.h"
#include "classes/forcefield.h"
#include "classes/pattern.h"

// Select all ('selectall')
int CommandData::function_CA_SELECTALL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->selectAll();
	return CR_SUCCESS;
}

// Select by atom ('selectatom <n>')
int CommandData::function_CA_SELECTATOM(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Atom *i = obj.m->atom(c->argi(0));
	if (i != NULL) obj.m->selectAtom(i);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Select by element ('selectelement <el>')
int CommandData::function_CA_SELECTELEMENT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	int el = elements.find(c->argc(0), Prefs::AlphaZmap);
	for (Atom *i = obj.m->atoms(); i != NULL; i = i->next) if (i->element() == el) obj.m->selectAtom(i);
	return CR_SUCCESS;
}

// Select by forcefield type ('selecffttype <fftype>')
int CommandData::function_CA_SELECTFFTYPE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Forcefield *ff = obj.m->forcefield();
	if (ff == NULL)
	{
		msg(Debug::None,"No forcefield associated to model.\n");
		return CR_FAIL;
	}
	ForcefieldAtom *ffa;
	for (Atom *i = obj.m->atoms(); i != NULL; i = i->next)
	{
		ffa = i->type();
		if (ffa != NULL)
		{
			if (ff->matchType(ffa->name(),c->argc(0)) != 0) obj.m->selectAtom(i);
		}
	}
	return CR_SUCCESS;
}

// Invert selection
int CommandData::function_CA_SELECTINVERT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->selectionInvert();
	return CR_SUCCESS;
}

// Select no atoms ('selectnone')
int CommandData::function_CA_SELECTNONE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->selectNone();
	return CR_SUCCESS;
}

// Detect and select overlapping atoms
int CommandData::function_CA_SELECTOVERLAPS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->selectOverlaps(c->argd(0));
	return CR_SUCCESS;
}

// Select all atoms in current (or named) pattern ('selectpattern [name]')
int CommandData::function_CA_SELECTPATTERN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Pattern *p = NULL;
	if (c->hasArg(0)) p = obj.m->findPattern(c->argc(0));
	else p = obj.p;
	if (p == NULL) msg(Debug::None,"No pattern in which to select atoms.\n");
	else
	{
		Atom *i = p->firstAtom();
		for (int n=0; n<p->totalAtoms(); n++)
		{
			obj.m->selectAtom(i);
			i = i->next;
		}
	}
	return CR_SUCCESS;
}

// Select by supplied atom type description ('selecttype <el> <typedesc>')
int CommandData::function_CA_SELECTTYPE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Atomtype testat;
	testat.setCharacterElement(elements.find(c->argc(0)));
	testat.expand(c->argc(1),NULL,NULL);
	// Apply it to the atoms in the model, selecting atoms that match
	int count = 0, matchscore, atomscore;
	if (obj.m->autocreatePatterns())
	{
		// Prepare for typing
		obj.m->describeAtoms();
		// Loop over patterns and select atoms
		for (Pattern *p = obj.m->patterns(); p != NULL; p = p->next)
		{
			Atom *i = p->firstAtom();
			for (int n=0; n<p->totalAtoms(); n++)
			{
				p->resetTempI(0);
				i->tempi = 1;
				if (i->element() == testat.characterElement())
				{
					atomscore = testat.matchAtom(i,p->ringList(),obj.m,i);
					if (atomscore != 0)
					{
						obj.m->selectAtom(i);
						count ++;
						matchscore = atomscore;
					}
				}
				i = i->next;
			}
		}
		// Write results
		msg(Debug::None,"Type description score = %i. Matched %i atoms.\n", matchscore, count);
		// Update model and delete temporary atomtype
		obj.m->logChange(LOG_SELECTION);
		return CR_SUCCESS;
	}
	else msg(Debug::None,"Can't test atomtype description without a valid pattern definition!\n");
	return CR_FAIL;
}
