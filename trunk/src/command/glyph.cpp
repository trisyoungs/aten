/*
	*** Glyph command functions
	*** src/command/glyph.cpp
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
#include "model/model.h"
#include "classes/glyph.h"

// Add glyph to current model
int commanddata::function_CA_ADDGLYPH(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	master.current.gl = obj.m->add_glyph();
	return CR_SUCCESS;
}

// Associate atom with current glyph
int commanddata::function_CA_SETGLYPHATOM(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL+BP_GLYPH)) return CR_FAIL;
	// Check range of supplied data item
	int d = c->argi(0);
	if ((d < 0) || (d >= MAXGLYPHDATA))
	{
		msg(DM_NONE,"Data index given to 'setglyphatom' (%i) is out of range.\n", d);
		return CR_FAIL;
	}
	// If second argument was given, it refers to either an atom by pointer or by id
	atom *target = obj.i;
	if (c->has_arg(1))
	{
		if (c->argt(1) == VT_ATOM) target = c->arga(1);
		else
		{
			int n = c->argi(1);
			if ((n < 0) || (n >= obj.m->get_natoms()))
			{
				msg(DM_NONE,"Atom index given to 'setglyphatom' (%i) is not valid for this model.\n", n);
				target = NULL;
			}
			else target = obj.m->get_atom(n);
		}
	}
	// Finally, check pointer currently in target and store it
	obj.gl->data[d].set_atom(target);
	if (target == NULL) msg(DM_NONE,"Warning - NULL atom stored in glyph data %i.\n",d);
	return CR_SUCCESS;
}

// Store vector data in current glyph
int commanddata::function_CA_SETGLYPHDATA(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL+BP_GLYPH)) return CR_FAIL;
	// Check range of supplied data item
	int d = c->argi(0);
	if ((d < 0) || (d >= MAXGLYPHDATA))
	{
		msg(DM_NONE,"Data index given to 'setglyphatom' (%i) is out of range.\n", d);
		return CR_FAIL;
	}
	obj.gl->data[d].set_vector(c->argd(1), c->argd(2), c->argd(3));
	return CR_SUCCESS;
}
