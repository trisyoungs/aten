/*
	*** Selection command functions
	*** src/command/select.cpp
	Copyright T. Youngs 2007-2009

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

#include "variables/accesspath.h"
#include "command/commandlist.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "base/pattern.h"
#include "base/sysfunc.h"

void selectAtoms(Model *m, Variable *slxn, bool deselect)
{
	static char from[32], to[32], text[256], s[512];
	int i, j, n, plus;
	bool range;
	// If the argument is an atom or integer variable, (de)select the corresponding atom. Otherwise, perform ranged selections
	if (slxn->type() == VTypes::AtomData)
	{
		Atom *ii = (Atom*) slxn->asPointer(VTypes::AtomData);
		sprintf(s,"%select (%i)", deselect ? "Des" : "S", ii->id()+1);
		m->beginUndoState(s);
		deselect ? m->deselectAtom(ii) : m->selectAtom(ii);
		m->endUndoState();
	}
	else if (slxn->type() == VTypes::PatternData)
	{
		Pattern *pp = (Pattern*) slxn->asPointer(VTypes::PatternData);
		sprintf(s,"%select pattern '%s' (%i atoms)", deselect ? "Des" : "S", pp->name(), pp->totalAtoms());
		m->beginUndoState(s);
		m->selectPattern(pp, FALSE, deselect);
		m->endUndoState();
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
		sprintf(s,"%select (%s)", deselect ? "Des" : "S", slxn->asCharacter());
		m->beginUndoState(s);
		if (!range)
		{
			if (VTypes::determineType(from) == VTypes::IntegerData)
			{
				i = atoi(from);
				// Integer atom ID selection
				if (plus == 0) (deselect ? m->deselectAtom(i-1) : m->selectAtom(i-1));
				else if (plus == -1) for (n=0; n < i; n++) (deselect ? m->deselectAtom(n) : m->selectAtom(n));
				else if (plus == 1) for (n=i-1; n < m->nAtoms(); n++) (deselect ? m->deselectAtom(n) : m->selectAtom(n));
			}
			else
			{
				i = elements().find(from);
				if (i == 0)
				{
					msg.print("Unrecognised element (%s) in select.\n", from);
					return;
				}
				if (plus == 0) m->selectElement(i);
				else if (plus == -1) for (n=1; n <= i; n++) (deselect ? m->deselectElement(n) : m->selectElement(n));
				else if (plus == 1) for (n=i; n <= elements().nElements(); n++) (deselect ? m->deselectElement(n) : m->selectElement(n));
			}
		}
		else
		{
			// Range of id's or elements
			if (VTypes::determineType(from) == VTypes::IntegerData)
			{
				i = atoi(from);
				j = atoi(to);
				for (n=i-1; n<j; n++) m->selectAtom(n);
			}
			else
			{
				i = elements().find(from);
				if (i == 0)
				{
					msg.print("Unrecognised element (%s) on left-hand side of range.\n", from);
					return;
				}
				j = elements().find(to);
				if (j == 0)
				{
					msg.print("Unrecognised element (%s) on right-hand side of range.\n", to);
					return;
				}
				for (n=i; n <= j; n++) (deselect ? m->deselectElement(n) : m->selectElement(n));
			}
		}
		m->endUndoState();
	}
}

// Deselect atom, range of atoms, or elements ('select <n>')
int Command::function_CA_DESELECT(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	for (int i=0; i<c->nArgs(); i++) selectAtoms(obj.rs, c->arg(i), TRUE);
	return Command::Success;
}

// Expand current selection
int Command::function_CA_EXPAND(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->selectionExpand();
	return Command::Success;
}

// Select all ('selectall')
int Command::function_CA_SELECTALL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Select all atoms");
	obj.rs->selectAll();
	obj.rs->endUndoState();
	return Command::Success;
}

// Select atom, range of atoms, or elements ('select <n>')
int Command::function_CA_SELECT(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// Loop over arguments given to command, passing them in turn to selectAtoms
	for (int i=0; i<c->nArgs(); i++) selectAtoms(obj.rs, c->arg(i), FALSE);
	return Command::Success;
}

// Select by forcefield type ('selecffttype <fftype>')
int Command::function_CA_SELECTFFTYPE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Forcefield *ff = obj.rs->forcefield();
	if (ff == NULL)
	{
		msg.print("No forcefield associated to model.\n");
		return Command::Fail;
	}
	ForcefieldAtom *ffa;
	char s[128];
	sprintf(s,"Select by forcefield type (%s)", c->argc(0));
	obj.rs->beginUndoState(s);
	for (Atom *i = obj.rs->atoms(); i != NULL; i = i->next)
	{
		ffa = i->type();
		if (ffa != NULL)
		{
			if (ff->matchType(ffa->name(),c->argc(0)) != 0) obj.rs->selectAtom(i);
		}
	}
	obj.rs->endUndoState();
	return Command::Success;
}

// Invert selection
int Command::function_CA_INVERT(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Invert selection");
	obj.rs->selectionInvert();
	obj.rs->endUndoState();
	return Command::Success;
}

// Select no atoms ('selectnone')
int Command::function_CA_SELECTNONE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Deselect all atoms");
	obj.rs->selectNone();
	obj.rs->endUndoState();
	return Command::Success;
}

// Detect and select overlapping atoms
int Command::function_CA_SELECTOVERLAPS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	char s[128];
	sprintf(s,"Select overlapping atoms (within %f)", c->argd(0));
	obj.rs->beginUndoState(s);
	obj.rs->selectOverlaps(c->argd(0));
	obj.rs->endUndoState();
	return Command::Success;
}

// Select all atoms in current (or named/id'd) pattern ('selectpattern [name|id]')
int Command::function_CA_SELECTPATTERN(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Pattern *p = NULL;
	if (c->hasArg(0))
	{
		if (c->argt(0) == VTypes::IntegerData) p = obj.rs->pattern(c->argi(0)-1);
		else p = obj.rs->findPattern(c->argc(0));
	}
	else p = obj.p;
	if (p == NULL) msg.print("No pattern in which to select atoms.\n");
	else
	{
		char s[256];
		sprintf(s,"Select pattern '%s'", p->name());
		obj.rs->beginUndoState(s);
		Atom *i = p->firstAtom();
		for (int n=0; n<p->totalAtoms(); n++)
		{
			obj.rs->selectAtom(i);
			i = i->next;
		}
		obj.rs->endUndoState();
	}
	return Command::Success;
}

// Select by supplied atom type description ('selecttype <el> <typedesc>')
int Command::function_CA_SELECTTYPE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (obj.rs->autocreatePatterns())
	{
		obj.rs->selectType(elements().find(c->argc(0)), c->argc(1));
		return Command::Success;
	}
	else msg.print("Can't test atomtype description without a valid pattern definition!\n");
	return Command::Fail;
}
