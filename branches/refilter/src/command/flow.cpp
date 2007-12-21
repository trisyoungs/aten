/*
	*** Flow control functions
	*** src/command/flow.cpp
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
#include "base/debug.h"
#include "classes/pattern.h"
#include <fstream>

// Else statement
int command_functions::function_CA_ELSE(command *&c, bundle &obj)
{
	c = c->get_branch_commands();
	return CR_SUCCESSNOMOVE;
}

// Elseif statement
int command_functions::function_CA_ELSEIF(command *&c, bundle &obj)
{
	if (c->if_evaluate()) c = c->get_branch_commands();
	else c = c->next;
	return CR_SUCCESSNOMOVE;
}

int command_functions::function_CA_END(command *&c, bundle &obj)
{
}

// Loop over atoms
int command_functions::function_CA_FOR(command *&c, bundle &obj)
{
	if (c->get_loopactive())
	{
		if (c->loop_iterate()) c = c->get_branch_commands();
		else c = c->next;
	}
	else
	{
	// Initialise loop variable in arg(0), depending on its type
	//msg(DM_VERBOSE,"Initialising loop : count variable is '%s', type = '%s'\n", countvar->get_name(), text_from_VT(counttype));
	switch (c->argt(0))
	{
		// Integer loop: 1 arg  - loop from 1 until end of file or termination
		//		 3 args - loop from arg 2 (int) to arg 3 (int)
		case (VT_INT):
			if (!c->has_arg(2)) c->arg(0)->set(1);
			else c->arg(0)->set(c->argi(1));
			break;
		// Atom loop:	1 arg  - loop over all atoms in model
		//		2 args - loop over all atoms in arg 2 (pattern)
		//		3 args - loop over atoms in molecule arg 3 in pattern arg 2
		case (VT_ATOM):
			// If no second variable is given, loop over all atoms
			if (c->has_arg(1))
			{
				// Second argument determines pattern
				pattern *p;
				if (c->argt(1) == VT_PATTERN) p = c->argp(1);
				else if (c->argt(1) == VT_INT) p = obj.m->get_pattern(c->argi(1));
				else
				{
					printf("Atom loop argument 2 must be of type 'pattern' or 'int'.\n");
					return CR_FAIL;
				}
				// Must have a valid pattern pointer here
				if (p == NULL)
				{
					printf("Atom loop was not given a valid pattern.\n");
					return CR_FAIL;
				}
				// Check on third argument - if provided, must be an int
				if (c->has_arg(2))
				{
					if (c->argt(2) == VT_INT)
					{
						int i = c->argi(2);
						// Check molecule range
						if ((i < 1) || (i > p->get_nmols()))
						{
							printf("Atom loop pattern molecule is out of range.\n");
							return CR_FAIL;
						}
						int m = p->get_startatom();
						m += (i-1) * p->get_natoms();
						c->arg(0)->set(obj.m->get_atom(m));
					}
					else
					{
						printf("Atom loop argument 3 must be an 'int'\n.");
						return CR_FAIL;
					}
				}
				else c->arg(0)->set(p->get_firstatom());

			}
			else c->arg(0)->set(obj.m->get_atoms());
			// Set secondary variables from atom loop variable
			c->parent->variables.set_atom_variables(c->arg(0)->get_name(), c->arga(0));
			break;
		// Pattern loop	 1 arg  - loop over patterns in model
		case (VT_PATTERN):

			countvar->set(m->get_patterns());
			// Set pattern variables from the pattern pointer
			vars.set_pattern_variables(countvar->get_name(), (pattern*) countvar->get_as_pointer(VT_PATTERN));
			break;
		// Loop over forcefield terms of pattern
		case (VT_PATBOUND):
			// Second variable supplied must be a pattern variable from which we take the ff terms
			if (rangetype == VT_PATTERN)
			{
				p = (pattern*) rangevar->get_as_pointer(VT_PATTERN);
				switch (basiccommand)
				{
					case (CA_FORFFBONDS):
						countvar->set(p->bonds.first());
						break;
					case (CA_FORFFANGLES):
						countvar->set(p->angles.first());
						break;
					case (CA_FORFFTORSIONS):
						countvar->set(p->torsions.first());
						break;
					default:
						printf("command::loop_initialise <<<< ffbound interaction not in list >>>>\n");
						break;
				}
			}
			else
			{
				msg(DM_NONE,"Range variable '%s' is not of suitable type (%s) for 'ffbound' loop.\n", rangevar->get_name(), text_from_VT(rangevar->get_type()));
				dbg_end(DM_CALLS,"command::loop_initialise");
				return FALSE;
			}
			// Set patbound variables from the patbound pointer
			vars.set_patbound_variables(countvar->get_name(), (patbound*) countvar->get_as_pointer(VT_PATBOUND));
			break;
		default:
			printf("Kick Developer - Loops over '%s' are missing.\n",text_from_VT(counttype));
			dbg_end(DM_CALLS,"command::loop_initialise");
			return FALSE;
			break;
	}
	loop_running = TRUE;
	loopcount = 1;
	// Check on niterations
	bool result = loop_check();
	if (result) msg(DM_VERBOSE,"Loop is initialised and running.\n");
	else msg(DM_VERBOSE,"Loop terminated on initialisation.\n");
	dbg_end(DM_CALLS,"command::loop_initialise");
	return result;
}


		if (c->loop_initialise(obj.m)) c = c->get_branch_commands();
		else c = c->next;
	}
	return CR_SUCCESSNOMOVE;
}

// Jump to specified node
int command_functions::function_CA_GOTO(command *&c, bundle &obj)
{
	c = c->get_pointer();
	return CR_SUCCESSNOMOVE;
}

// Jump to next node in current list that is *not* an ELSE(IF)
int command_functions::function_CA_GOTONONIF(command *&c, bundle &obj)
{
	//printf("Searching for next non-if node...\n");
	c = c->get_pointer();
	// Skip to node after the source node and begin search.
	c = c->next;
	// If we find an BC_IF node we stop and continue on from there.
	//printf("Skipped back to node %li (%s)\n",fn,text_from_FC(fn->get_command()));
	command_action ca = c->get_command();
	while ((ca == CA_ELSEIF) || (ca == CA_ELSE))
	{
		c = c->next;
		ca = c->get_command();
	}
	return CR_SUCCESSNOMOVE;
}

// If statement
int command_functions::function_CA_IF(command *&c, bundle &obj)
{
	if (c->if_evaluate()) c = c->get_branch_commands();
	else c = c->next;
	return CR_SUCCESS;
}

// Loop for n iterations or forever (quit if EOF)
int command_functions::function_CA_REPEAT(command *&c, bundle &obj)
{
	if (c->parent->get_infile() != NULL)
		if (c->parent->get_infile()->peek() == -1)
		{
			msg(DM_VERBOSE,"commandlist::do_basic - 'repeat' reached end of file.\n");
			c->set_loopactive(FALSE);
			c = c->next;
			return CR_SUCCESSNOMOVE;
		}
	// If the loop is not running, start it. Otherwise, check loop iteration number
	if (c->get_loopactive())
	{
		// Quit if loop has done all its iterations
		if (!c->loop_iterate()) c = c->next;
		else c = c->get_branch_commands();
	}
	else
	{
		if (!c->loop_initialise(obj.m)) c = c->next;
		else c = c->get_branch_commands();
	}
	return CR_SUCCESS;
}

int command_functions::function_CA_TERMINATE(command *&c, bundle &obj)
{
}





// Do loop iteration
bool command::loop_iterate(variable_list &vars)
{
	dbg_begin(DM_CALLS,"command::loop_iterate");
	atom *i;
	pattern *p;
	// Grab variables
	variable *countvar = args[0];
	if (countvar == NULL)
	{
		printf("command::loop_iterate <<<< Count var in loop has not been set >>>>\n");
		dbg_end(DM_CALLS,"command::loop_iterate");
		return FALSE;
	}
	// Increase loop counter
	countvar->increase(1);
	loopcount ++;
	// Set new variables from loop variable
	switch (countvar->get_type())
	{
		case (VT_GENERIC):
			break;
		case (VT_ATOM):
			// Set atom variables from the atom pointer
			vars.set_atom_variables(countvar->get_name(), (atom*) countvar->get_as_pointer(VT_ATOM));
			break;
		case (VT_PATTERN):
			// Set patbound variables from the patbound pointer
			vars.set_pattern_variables(countvar->get_name(), (pattern*) countvar->get_as_pointer(VT_PATTERN));
			break;
		case (VT_PATBOUND):
			// Set patbound variables from the patbound pointer
			vars.set_patbound_variables(countvar->get_name(), (patbound*) countvar->get_as_pointer(VT_PATBOUND));
			break;
		default:
			printf("command::loop_iterate <<<< Don't know how to set variables from var of type '%s' >>>>\n", text_from_VT(countvar->get_type()));
			break;
	}
	// Check for completed loop on exit
	dbg_end(DM_CALLS,"command::loop_iterate");
	return loop_check();
}

// Check for loop termination
bool command::loop_check()
{
	dbg_begin(DM_CALLS,"command::loop_check");
	// Grab variables
	variable *countvar, *rangevar;
	atom *i;
	pattern *p;
	patbound *pb;
	countvar = args[0];
	if (countvar == NULL)
	{
		printf("command::loop_check <<<< Count var in loop has not been set >>>>\n");
		dbg_end(DM_CALLS,"command::loop_check");
		return FALSE;
	}
	rangevar = args[1];
	switch (countvar->get_type())
	{
		// Integer loop - goes forever, unless a loop maximum is given in rangevar
		case (VT_GENERIC):
			if (rangevar != NULL)
			{
				if (countvar->get_as_int() > rangevar->get_as_int()) loop_running = FALSE;
			}
			break;
		// Atom loops - end with pointer equal to NULL (model) or last atom in specified pattern/molecule
		case (VT_ATOM):
			// Check type of rangevar (if there is one)
			i = (atom*) countvar->get_as_pointer(VT_ATOM);
			//printf("atom variable = %li\n",i);
			if (rangevar == NULL)
			{
				if (i == NULL) loop_running = FALSE;
			}
			else if (rangevar->get_type() == VT_PATTERN)
			{
				p = (pattern*) rangevar->get_as_pointer(VT_PATTERN);
				// If no molecule variable is specified, check for last atom in pattern
				if (args[2] == NULL)
				{
					if (loopcount > p->get_totalatoms()) loop_running = FALSE;
				}
				else
				{
					if (loopcount > p->get_natoms()) loop_running = FALSE;
				}
			}
			break;
		// Pattern loops - end with pointer equal to NULL
		case (VT_PATTERN):
			p = (pattern*) countvar->get_as_pointer(VT_PATTERN);
			if (p == NULL) loop_running = FALSE;
			break;
		// Pattern ffbound loops - end with pointer equal to NULL
		case (VT_PATBOUND):
			pb = (patbound*) countvar->get_as_pointer(VT_PATBOUND);
			if (pb == NULL) loop_running = FALSE;
			break;
		default:
			printf("Don't know how to check loop of type '%s'\n",text_from_VT(countvar->get_type()));
			break;
	}
	dbg_end(DM_CALLS,"command::loop_check");
	return loop_running;
}
