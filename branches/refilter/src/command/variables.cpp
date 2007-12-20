/*
	*** Variable command functions
	*** src/command/transform.cpp
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
#include "model/model.h"
#include "classes/atom.h"



int command_functions::function_CA_DECREASE(command *&c, bundle &obj)
{


// Decrease variable by 1
case (BC_DECREASE):
	fn->datavar[0]->decrease(1);
	fn = fn->next;
	break;
}

// Evaluate expression and assign to variable
int command_functions::function_CA_EVAL(command *&c, bundle &obj)
{
	fn->datavar[0]->set(evaluate(fn->argc(2), &variables));
	fn = fn->next;
	break;
}

// Set variable to value or variable
int command_functions::function_CA_LET(command *&c, bundle &obj)
{
	// If the first var is a pointer, second must be a pointer!
	if (c->datavar[0]->get_type() >= VT_ATOM)
	{
		if (c->datavar[0]->get_type() != c->datavar[2]->get_type())
			msg(DM_NONE,"Incompatible pointer types for variable assignment of contents of '%s' to '%s'.\n", c->datavar[0]->get_name(), c->datavar[2]->get_name());
		else c->datavar[0]->copy_pointer(c->datavar[2]);
	}
	else fn->datavar[0]->set(fn->argc(2));
	return CR_SUCCESS;
}

// Increase variable
int command_functions::function_CA_INCREASE(command *&c, bundle &obj)
{
	c->datavar[0]->increase(1);
	return CR_SUCCESS;
}
