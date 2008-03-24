/*
	*** Forcefield command functions
	*** src/command/ff.cpp
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
#include "base/elements.h"
#include "classes/forcefield.h"
#include "classes/pattern.h"
#include "model/model.h"

// Clear manual type mapping list ('clearmap')
int CommandData::function_CA_CLEARMAP(Command *&c, Bundle &obj)
{
	master.typeMap.clear();
	return CR_SUCCESS;
}

// Set default forcefield ('defaultff <ff>')
int CommandData::function_CA_DEFAULTFF(Command *&c, Bundle &obj)
{
	// If an argument was supplied, select forcefield by name. Otherwise use current
	master.setDefaultForcefield(master.findForcefield(c->argc(0)));
	return CR_SUCCESS;
}

// Associate current ff to current model ('ffmodel [name]')
int CommandData::function_CA_FFMODEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// If an argument was supplied, select forcefield by name. Otherwise use current
	if (c->hasArg(0)) obj.m->setForcefield(master.findForcefield(c->argc(0)));
	else obj.m->setForcefield(obj.ff);
	return CR_SUCCESS;
}

// Set current forcefield for named pattern ('ffpattern')
int CommandData::function_CA_FFPATTERN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL+BP_FF)) return CR_FAIL;
	obj.p->setForcefield(obj.ff);
	return CR_SUCCESS;
}

// Set current forcefield for pattern id given ('ffpatternid <id>')
int CommandData::function_CA_FFPATTERNID(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL+BP_FF)) return CR_FAIL;
	int nodeid = c->argi(0) - 1;
	if ((nodeid < 0) || (nodeid > obj.m->nPatterns()))
	{
		msg(DM_NONE,"Pattern ID %i is out of range for model (which has %i atterns).\n", nodeid, obj.m->nPatterns());
		return CR_FAIL;
	}
	else obj.m->pattern(nodeid)->setForcefield(obj.ff);
	return CR_SUCCESS;
}

// Load forcefield ('loadff <filename> [nickname]')
int CommandData::function_CA_LOADFF(Command *&c, Bundle &obj)
{
	Forcefield *ff = master.loadForcefield(c->argc(0));
	if (ff != NULL)
	{
		master.setCurrentForcefield(ff);
		if (c->hasArg(1)) ff->setName(c->argc(1));
		msg(DM_NONE,"Forcefield '%s' loaded, name '%s'\n", c->argc(0), ff->name());
		return CR_SUCCESS;
	}
	else return CR_FAIL;
}

// Select current forcefield ('getff <name>')
int CommandData::function_CA_GETFF(Command *&c, Bundle &obj)
{
	Forcefield *ff = master.findForcefield(c->argc(0));
	if (ff != NULL)	master.setCurrentForcefield(ff);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Add manual type mappings ('map <name=element,...>')
int CommandData::function_CA_MAP(Command *&c, Bundle &obj)
{
	// Get the argument and parse it internally
	parser.getArgsDelim(c->argc(0), PO_DEFAULTS);
	int n, el;
	for (n=0; n<parser.nArgs(); n++)
	{
		el = elements.find(afterChar(parser.argc(n), '='));
		if (el == 0) msg(DM_NONE,"Unrecognised element '%s' in type map.\n",afterChar(parser.argc(n),'='));
		else master.typeMap.add(beforeChar(parser.argc(n),'='), el);
	}
	return CR_SUCCESS;
}

// Perform typing on current model
int CommandData::function_CA_TYPEMODEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	return (obj.m->typeAll() ? CR_SUCCESS : CR_FAIL);
}

// Test specified type ID of current forcefield
int CommandData::function_CA_TYPETEST(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL+BP_FF)) return CR_FAIL;
	// Find the specified type...
	ForcefieldAtom *ffa = obj.ff->findType(c->argi(0));
	if (ffa == NULL)
	{
		msg(DM_NONE,"Type ID %i does not exist in the forcefield '%s'.\n",c->argi(0), obj.ff->name());
		return CR_FAIL;
	}
	else
	{
		if (obj.m->autocreatePatterns())
		{
			// Prepare for typing
			obj.m->describeAtoms();
			// Get atom, element, and the atom's pattern
			Atom *i = obj.m->atomArray()[c->argi(1)-1];
			int el = i->element();
			Pattern *p = obj.m->pattern(i);
			int score = ffa->atomType()->matchAtom(i,p->ringList(),obj.m,i);
			if (score != 0) msg(DM_NONE,"Atom %i matched type %i (%s) with score %i.\n", i->id()+1, ffa->typeId(), ffa->name(), score);
			else msg(DM_NONE,"Atom %i did not match type %i (%s).\n", i->id()+1, ffa->typeId(), ffa->name());
		}
		else return CR_FAIL;
	}
	return CR_SUCCESS;
}
