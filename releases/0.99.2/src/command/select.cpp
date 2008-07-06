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
#include "base/messenger.h"
#include "classes/forcefield.h"
#include "classes/pattern.h"

void selectAtoms(Model *m, Variable *slxn, bool deselect)
{
	static char from[32], to[32], text[256];
	int i, j, n, plus;
	bool range;
	// If the argument is an atom or integer variable, (de)select the corresponding atom. Otherwise, perform ranged selections
	if (slxn->type() == Variable::AtomVariable)
	{
		Atom *ii = (Atom*) slxn->asPointer();
		deselect ? m->deselectAtom(ii) : m->selectAtom(ii);
	}
	else
	{
		// Copy variable contents into local character array
		strcpy(text, slxn->asCharacter());
		// If arg contains a '-', select by range
		if (strchr(text, '-') != NULL)
		{
			range = TRUE;
			strcpy(from,beforeChar(text,'-'));
			strcpy(to,afterChar(text,'-'));
			// Arguments for ranges cannot have '+' in them
			if ((strchr(from,'+') != NULL) || (strchr(to,'+')))
			{
				msg.print("Invalid range symbol (+) given in static range '%s'-'%s'.\n", from, to);
				return;
			}
		}
		else
		{
			range = FALSE;
			strcpy(from,text);
			if (strchr(from,'+') == NULL) plus = 0;
			else if (from[0] == '+') plus = -1;
			else if (from[strlen(from)-1] == '+') plus = 1;
			else
			{
				msg.print("Invalid range symbol (+) given in middle of selection element '%s'.\n", from);
				return;
			}
		}
		// Do the selection
		if (!range)
		{
			if (Variable::determineType(from) == Variable::IntegerVariable)
			{
				i = atoi(from);
				// Integer atom ID selection
				if (plus == 0) (deselect ? m->deselectAtom(i-1) : m->selectAtom(i-1));
				else if (plus == -1) for (n=0; n < i; n++) (deselect ? m->deselectAtom(n) : m->selectAtom(n));
				else if (plus == 1) for (n=i-1; n < m->nAtoms(); n++) (deselect ? m->deselectAtom(n) : m->selectAtom(n));
			}
			else
			{
				i = elements.find(from);
				if (i == 0)
				{
					msg.print("Unrecognised element (%s) in select.\n", from);
					return;
				}
				if (plus == 0) m->selectElement(i);
				else if (plus == -1) for (n=1; n <= i; n++) (deselect ? m->deselectElement(n) : m->selectElement(n));
				else if (plus == 1) for (n=i; n <= elements.nElements(); n++) (deselect ? m->deselectElement(n) : m->selectElement(n));
			}
		}
		else
		{
			// Range of id's or elements
			if (Variable::determineType(from) == Variable::IntegerVariable)
			{
				i = atoi(from);
				j = atoi(to);
				for (n=i-1; n<j; n++) m->selectAtom(n);
			}
			else
			{
				i = elements.find(from);
				if (i == 0)
				{
					msg.print("Unrecognised element (%s) on left-hand side of range.\n", from);
					return;
				}
				j = elements.find(to);
				if (j == 0)
				{
					msg.print("Unrecognised element (%s) on right-hand side of range.\n", to);
					return;
				}
				for (n=i; n <= j; n++) (deselect ? m->deselectElement(n) : m->selectElement(n));
			}
		}
	}
}

// Deselect atom, range of atoms, or elements ('select <n>')
int CommandData::function_CA_DESELECT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	for (int i=0; i<c->nArgs(); i++) selectAtoms(obj.rs, c->arg(i), TRUE);
	return CR_SUCCESS;
}

// Select all ('selectall')
int CommandData::function_CA_SELECTALL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->selectAll();
	return CR_SUCCESS;
}

// Select atom, range of atoms, or elements ('select <n>')
int CommandData::function_CA_SELECT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// Loop over arguments given to command, passing them in turn to selectAtoms
	for (int i=0; i<c->nArgs(); i++) selectAtoms(obj.rs, c->arg(i), FALSE);
	return CR_SUCCESS;
}

// Select by forcefield type ('selecffttype <fftype>')
int CommandData::function_CA_SELECTFFTYPE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Forcefield *ff = obj.rs->forcefield();
	if (ff == NULL)
	{
		msg.print("No forcefield associated to model.\n");
		return CR_FAIL;
	}
	ForcefieldAtom *ffa;
	for (Atom *i = obj.rs->atoms(); i != NULL; i = i->next)
	{
		ffa = i->type();
		if (ffa != NULL)
		{
			if (ff->matchType(ffa->name(),c->argc(0)) != 0) obj.rs->selectAtom(i);
		}
	}
	return CR_SUCCESS;
}

// Invert selection
int CommandData::function_CA_INVERT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->selectionInvert();
	return CR_SUCCESS;
}

// Select no atoms ('selectnone')
int CommandData::function_CA_SELECTNONE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->selectNone();
	return CR_SUCCESS;
}

// Detect and select overlapping atoms
int CommandData::function_CA_SELECTOVERLAPS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->selectOverlaps(c->argd(0));
	return CR_SUCCESS;
}

// Select all atoms in current (or named/id'd) pattern ('selectpattern [name|id]')
int CommandData::function_CA_SELECTPATTERN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Pattern *p = NULL;
	if (c->hasArg(0)) p = obj.rs->findPattern(c->argc(0));
	else p = obj.p;
	if (p == NULL) msg.print("No pattern in which to select atoms.\n");
	else
	{
		Atom *i = p->firstAtom();
		for (int n=0; n<p->totalAtoms(); n++)
		{
			obj.rs->selectAtom(i);
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
	if (obj.rs->autocreatePatterns())
	{
		// Prepare for typing
		obj.rs->describeAtoms();
		// Loop over patterns and select atoms
		for (Pattern *p = obj.rs->patterns(); p != NULL; p = p->next)
		{
			Atom *i = p->firstAtom();
			for (int n=0; n<p->totalAtoms(); n++)
			{
				p->resetTempI(0);
				i->tempi = 1;
				if (i->element() == testat.characterElement())
				{
					atomscore = testat.matchAtom(i,p->ringList(),obj.rs,i);
					if (atomscore != 0)
					{
						obj.rs->selectAtom(i);
						count ++;
						matchscore = atomscore;
					}
				}
				i = i->next;
			}
		}
		// Write results
		msg.print("Type description score = %i. Matched %i atoms.\n", matchscore, count);
		// Update model and delete temporary atomtype
		obj.rs->logChange(Change::SelectionLog);
		return CR_SUCCESS;
	}
	else msg.print("Can't test atomtype description without a valid pattern definition!\n");
	return CR_FAIL;
}
