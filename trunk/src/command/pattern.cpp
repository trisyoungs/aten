/*
	*** Pattern command functions
	*** src/command/pattern.cpp
	Copyright T. Youngs 2007

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
#include "base/debug.h"
#include "base/master.h"
#include "model/model.h"
#include "classes/pattern.h"

// Add manual pattern definition ('addpattern <name> <nmols> <natoms>')
int commanddata::function_CA_ADDPATTERN(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->add_pattern(c->argi(1), c->argi(2), c->argc(0));
	// TODO Add 'check_pattern(pattern*) method to model*
	return CR_SUCCESS;
}

// Clear current pattern definition ('clearpatterns')
int commanddata::function_CA_CLEARPATTERNS(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->clear_patterns();
	return CR_SUCCESS;
}

// Autocreate pattern definition ('createpatterns')
int commanddata::function_CA_CREATEPATTERNS(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->autocreate_patterns();
	return CR_SUCCESS;
}

// Print pattern definition for current model ('printpatterns')
int commanddata::function_CA_PRINTPATTERNS(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->print_patterns();
	return CR_SUCCESS;
}

// Select working pattern from model ('selectpattern <name>')
int commanddata::function_CA_SELECTPATTERN(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	pattern *p = obj.m->find_pattern(c->argc(0));
	if (p != NULL) master.current.p = p;
	else return CR_FAIL;
	return CR_SUCCESS;
}
