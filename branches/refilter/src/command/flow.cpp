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
	// Grab variable list from command's parent list
	variable_list &vars = c->parent->variables;
	bool status = TRUE;
	if (c->get_loopactive())
	{
//// START
		// Do loop iteration.
		// Increase count and iteration variables
		c->arg(0)->increase(1);
		c->increase_iterations();
		// Set new variables from loop variable and check for termination
		switch (c->argt(0))
		{
			case (VT_INT):
				// If 1 argument was provided, check for end of file. If three, check for limit
				if (!c->has_arg(2) && (c->parent->get_infile() != NULL))
				{
					// Check for end of file...
					if (c->parent->get_infile()->peek() == -1)
					{
						msg(DM_VERBOSE,"Infinite 'for' reached end of file.\n");
						status = FALSE;
					}
				}
				else if (c->argi(0) > c->argi(2)) status = FALSE;
				break;
			case (VT_ATOM):
				// If only one argument, check for NULL. If two, check for last atom in pattern.
				// If three, check for last atom in molecule
				if (c->has_arg(2))
				{
					if (c->get_loopiterations() > c->argp(1)->get_natoms()) status = FALSE;
				}
				else if (c->has_arg(1))
				{
					if (c->get_loopiterations() > c->argp(1)->get_totalatoms()) status = FALSE;
				}
				else if (c->arga(0) == NULL) status = FALSE;
				vars.set_atom_variables(c->arg(0)->get_name(), c->arga(0));
				break;
			case (VT_PATTERN):
				if (c->argp(0) == NULL) status = FALSE;
				vars.set_pattern_variables(c->arg(0)->get_name(), c->argp(0));
				break;
//			case (VT_PATBOUND):
//				vars.set_patbound_variables(c->arg(0)->get_name(), c->argf(0));
//				break;
			default:
				printf("Don't know how to set iterate loop with variable of type '%s'.\n", text_from_VT(c->argt(0)));
				return CR_FAIL;
		}
		if (status == TRUE) c = c->get_branch_commands();
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
				if (!c->has_arg(2))
				{
					c->arg(0)->set(1);
					// Check for end of file...
					if (c->parent->get_infile() != NULL)
						if (c->parent->get_infile()->peek() == -1)
						{
							msg(DM_VERBOSE,"Command 'repeat' reached end of file.\n");
							status = FALSE;
						}
				}
				else
				{
					c->arg(0)->set(c->argi(1));
					if (c->argi(1) > c->argi(2)) status = FALSE;
				}
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
				if (c->arga(0) == NULL) status = FALSE;
				// Set secondary variables from atom loop variable
				vars.set_atom_variables(c->arg(0)->get_name(), c->arga(0));
				break;
			// Pattern loop	 1 arg  - loop over patterns in model
			case (VT_PATTERN):
				if (c->argt(0) == VT_PATTERN) c->arg(0)->set(obj.m->get_patterns());
				else
				{
					printf("Pattern loop variable must be a 'pattern'.\n");
					return CR_FAIL;
				}
				if (c->argp(0) == NULL) status = FALSE;
				// Set pattern variables from the pattern pointer
				vars.set_pattern_variables(c->arg(0)->get_name(),c->argp(0));
				break;
			/* Loop over forcefield terms of pattern
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
			*/
			default:
				printf("Kick Developer - Loops over '%s' are missing.\n",text_from_VT(c->argt(0)));
				return CR_FAIL;
		}
		// Check loop's starting status
		if (status)
		{
			c->set_loopactive(TRUE);
			c->set_loopiterations(1);
			msg(DM_VERBOSE,"Loop is initialised and running.\n");
			c = c->get_branch_commands();
		}
		else
		{
			c->set_loopactive(FALSE);
			c->set_loopiterations(0);
			msg(DM_VERBOSE,"Loop terminated on initialisation.\n");
			c = c->next;
		}
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

int command_functions::function_CA_TERMINATE(command *&c, bundle &obj)
{
	return CR_EXIT;
}

int command_functions::function_CA_QUIT(command *&c, bundle &obj)
{
	return CR_EXIT;
}
