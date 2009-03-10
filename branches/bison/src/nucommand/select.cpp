/*
	*** Selection functions
	*** src/parser/select.cpp
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
#include "nucommand/commands.h"
#include "command/format.h"
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
				i = elements().findAlpha(from);
				if (i == 0)
				{
					msg.print("Unrecognised element (%s) in select.\n", from);
					return;
				}
				if (plus == 0) (deselect ? m->deselectElement(i) : m->selectElement(i));
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
				i = elements().findAlpha(from);
				if (i == 0)
				{
					msg.print("Unrecognised element (%s) on left-hand side of range.\n", from);
					return;
				}
				j = elements().findAlpha(to);
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
bool NuCommand::function_Deselect(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	for (int i=0; i<c->nArgs(); i++) selectAtoms(obj.rs, c->arg(i), TRUE);
	return TRUE;
}

// Deselect by supplied atom type description ('deselecttype <el> <typedesc>')
bool NuCommand::function_Deselecttype(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.rs->autocreatePatterns())
	{
		char *s = new char[strlen(c->argc(1)) + strlen(c->argc(0)) + 30];
		sprintf(s,"Deselect %s by type (%s)", c->argc(0), c->argc(1));
		obj.rs->beginUndoState(s);
		obj.rs->selectType(elements().findAlpha(c->argc(0)), c->argc(1), FALSE, TRUE);
		obj.rs->endUndoState();
		delete[] s;
		return TRUE;
	}
	else msg.print("Can't test atomtype description without a valid pattern definition!\n");
	return FALSE;
}

// Expand current selection
bool NuCommand::function_Expand(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Expand current selection");
	obj.rs->selectionExpand();
	obj.rs->endUndoState();
	return TRUE;
}

// Select all ('selectall')
bool NuCommand::function_Selectall(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Select all atoms");
	obj.rs->selectAll();
	obj.rs->endUndoState();
	return TRUE;
}

// Select atom, range of atoms, or elements ('select <n>')
bool NuCommand::function_Select(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Loop over arguments given to command, passing them in turn to selectAtoms
	CharacterVariable charvar;
	for (int i=0; i<c->nArgs(); i++)
	{
		// If the argument contains a variable delimiter '$' anywhere then we must create a format/string from it to simplify the string
		if (strchr(c->argc(i), '$') == NULL) selectAtoms(obj.rs, c->arg(i), FALSE);	
		else
		{
			c->createFormat(c->argc(i), FALSE);
			if (c->format()->createString())
			{
				charvar.set(c->format()->createdString());
				selectAtoms(obj.rs, &charvar, FALSE);
			}
			else
			{
				msg.print("Failed to process variables in selection string.\n");
				return FALSE;
			}
		}
	}
	return TRUE;
}

// Select by forcefield type ('selecffttype <fftype>')
bool NuCommand::function_Selectfftype(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Forcefield *ff = obj.rs->forcefield();
	if (ff == NULL)
	{
		msg.print("No forcefield associated to model.\n");
		return FALSE;
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
	return TRUE;
}


// Get selection centre of geometry ('selectioncog [x y z]')
bool NuCommand::function_Selectioncog(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Vec3<double> v = obj.rs->selectionCog();
	if (c->hasArg(2))
	{
		c->arg(0)->set(v.x);
		c->arg(1)->set(v.y);
		c->arg(2)->set(v.z);
	}
	else msg.print("Selection centre of geometry is at %f %f %f.\n", v.x, v.y, v.z);
	return TRUE;
}

// Get selection centre of mass ('selectioncom [x y z]')
bool NuCommand::function_Selectioncom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Vec3<double> v = obj.rs->selectionCom();
	if (c->hasArg(2))
	{
		c->arg(0)->set(v.x);
		c->arg(1)->set(v.y);
		c->arg(2)->set(v.z);
	}
	else msg.print("Selection centre of mass is at %f %f %f.\n", v.x, v.y, v.z);
	return TRUE;
}

// Invert selection
bool NuCommand::function_Invert(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Invert selection");
	obj.rs->selectionInvert();
	obj.rs->endUndoState();
	return TRUE;
}

// Select no atoms ('selectnone')
bool NuCommand::function_Selectnone(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Deselect all atoms");
	obj.rs->selectNone();
	obj.rs->endUndoState();
	return TRUE;
}

// Detect and select overlapping atoms
bool NuCommand::function_Selectoverlaps(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	char s[128];
	sprintf(s,"Select overlapping atoms (within %f)", c->argd(0));
	obj.rs->beginUndoState(s);
	obj.rs->selectOverlaps(c->argd(0));
	obj.rs->endUndoState();
	return TRUE;
}

// Select all atoms in current (or named/id'd) pattern ('selectpattern [name|id]')
bool NuCommand::function_Selectpattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
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
	return TRUE;
}

// Select by supplied atom type description ('selecttype <el> <typedesc>')
bool NuCommand::function_Selecttype(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	if (obj.rs->autocreatePatterns())
	{
		char *s = new char[strlen(c->argc(1)) + strlen(c->argc(0)) + 30];
		sprintf(s,"Select %s by type (%s)", c->argc(0), c->argc(1));
		obj.rs->beginUndoState(s);
		obj.rs->selectType(elements().findAlpha(c->argc(0)), c->argc(1));
		obj.rs->endUndoState();
		delete[] s;
		return TRUE;
	}
	else msg.print("Can't test atomtype description without a valid pattern definition!\n");
	return FALSE;
}
