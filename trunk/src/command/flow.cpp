/*
	*** Flow control functions
	*** src/command/flow.cpp
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
#include "base/debug.h"
#include "base/master.h"
#include "classes/pattern.h"
#include <fstream>

// Root node (no action)
int commanddata::function_CA_ROOTNODE(command *&c, bundle &obj)
{
	return CR_SUCCESS;
}

// Else statement
int commanddata::function_CA_ELSE(command *&c, bundle &obj)
{
	c = c->get_branch_commands();
	return CR_SUCCESSNOMOVE;
}

// Elseif statement
int commanddata::function_CA_ELSEIF(command *&c, bundle &obj)
{
	if (c->if_evaluate()) c = c->get_branch_commands();
	else c = c->next;
	return CR_SUCCESSNOMOVE;
}

int commanddata::function_CA_END(command *&c, bundle &obj)
{
	// This should never be called....
	return CR_SUCCESS;
}

// Loop over atoms
int commanddata::function_CA_FOR(command *&c, bundle &obj)
{
	// Grab variable list from command's parent list
	variable_list &vars = c->get_parent()->variables;
	bool status = TRUE;
	if (c->get_loopactive())
	{
		// Do loop iteration.
		// Increase count and iteration variables
		c->arg(0)->increase(1);
		c->increase_iterations();
		// Set new variables from loop variable and check for termination
		switch (c->argt(0))
		{
			case (VT_INTEGER):
				// If 1 argument was provided, check for end of file. If three, check for limit
				if (!c->has_arg(2) && (c->get_parent()->get_infile() != NULL))
				{
					// Check for end of file...
					if (c->get_parent()->get_infile()->peek() == -1)
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
				c->get_parent()->set_atom_variables(c->arg(0)->get_name(), c->arga(0));
				break;
			case (VT_PATTERN):
				if (c->argp(0) == NULL) status = FALSE;
				c->get_parent()->set_pattern_variables(c->arg(0)->get_name(), c->argp(0));
				break;
			case (VT_BOND):
			case (VT_ANGLE):
			case (VT_TORSION):
				if (c->argpb(0) == NULL) status = FALSE;
				c->get_parent()->set_patbound_variables(c->arg(0)->get_name(), c->argpb(0));
				break;
			case (VT_ATOMTYPE):
				if (c->argffa(0) == NULL) status = FALSE;
				c->get_parent()->set_atomtype_variables(c->arg(0)->get_name(), c->argffa(0));
				break;
			default:
				printf("Don't know how to set iterate loop with variable of type '%s'.\n", text_from_VT(c->argt(0)));
				return CR_FAIL;
		}
		if (status == TRUE) c = c->get_branch_commands();
		else
		{
			c->set_loopactive(FALSE);
			c = c->next;
		}
	}
	else
	{
		// Initialise loop variable in arg(0), depending on its type
		//msg(DM_VERBOSE,"Initialising loop : count variable is '%s', type = '%s'\n", countvar->get_name(), text_from_VT(counttype));
		switch (c->argt(0))
		{
			// Integer loop: 1 arg  - loop from 1 until end of file or termination
			//		 3 args - loop from arg 2 (int) to arg 3 (int)
			case (VT_INTEGER):
				if (!c->has_arg(2))
				{
					c->arg(0)->set(1);
					// Check for end of file...
					if (c->get_parent()->get_infile() != NULL)
						if (c->get_parent()->get_infile()->peek() == -1)
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
				if (obj.notify_null(BP_MODEL)) return CR_FAIL;
				// If no second variable is given, loop over all atoms
				if (c->has_arg(1))
				{
					// Second argument determines pattern
					pattern *p;
					if (c->argt(1) == VT_PATTERN) p = c->argp(1);
					else if (c->argt(1) == VT_INTEGER) p = obj.m->get_pattern(c->argi(1));
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
						if (c->argt(2) == VT_INTEGER)
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
				c->get_parent()->set_atom_variables(c->arg(0)->get_name(), c->arga(0));
				break;
			// Pattern loop	 1 arg  - loop over patterns in model
			case (VT_PATTERN):
				if (obj.notify_null(BP_MODEL)) return CR_FAIL;
				if (c->argt(0) == VT_PATTERN) c->arg(0)->set(obj.m->get_patterns());
				else
				{
					printf("Pattern loop variable must be a 'pattern'.\n");
					return CR_FAIL;
				}
				if (c->argp(0) == NULL) status = FALSE;
				// Set pattern variables from the pattern pointer
				c->get_parent()->set_pattern_variables(c->arg(0)->get_name(), c->argp(0));
				break;
			// Loop over forcefield bond terms of pattern
			case (VT_BOND):
				if (obj.notify_null(BP_MODEL)) return CR_FAIL;
				if (c->argt(1) != VT_PATTERN)
				{
					printf("Bond loop must be given a 'pattern'.\n");
					return CR_FAIL;
				}
				c->arg(0)->set(c->argp(1)->bonds.first());
				c->get_parent()->set_patbound_variables(c->arg(0)->get_name(), (patbound*) c->arg(0)->get_as_pointer());
				if (c->argpb(0) == NULL) status = FALSE;
				break;
			// Loop over forcefield angle terms of pattern
			case (VT_ANGLE):
				if (obj.notify_null(BP_MODEL)) return CR_FAIL;
				if (c->argt(1) != VT_PATTERN)
				{
					printf("Angle loop must be given a 'pattern'.\n");
					return CR_FAIL;
				}
				c->arg(0)->set(c->argp(1)->angles.first());
				c->get_parent()->set_patbound_variables(c->arg(0)->get_name(), (patbound*) c->arg(0)->get_as_pointer());
				if (c->argpb(0) == NULL) status = FALSE;
				break;
			// Loop over forcefield torsion terms of pattern
			case (VT_TORSION):
				if (obj.notify_null(BP_MODEL)) return CR_FAIL;
				if (c->argt(1) != VT_PATTERN)
				{
					printf("Torsion loop must be given a 'pattern'.\n");
					return CR_FAIL;
				}
				c->arg(0)->set(c->argp(1)->torsions.first());
				c->get_parent()->set_patbound_variables(c->arg(0)->get_name(), (patbound*) c->arg(0)->get_as_pointer());
				if (c->argpb(0) == NULL) status = FALSE;
				break;
			// Loop over unique ffatoms in model
			case (VT_ATOMTYPE):
				if (obj.notify_null(BP_MODEL)) return CR_FAIL;
				if (c->has_arg(1))
				{
					if (c->argt(1) != VT_ATOMTYPE)
					{
						printf("Second argument to atomtype loop must be also be an 'atomtype' variable.\n");
						return CR_FAIL;
					}
					// Start atomtype loop at type given instead of first
					c->arg(0)->set((ffatom*) c->arg(1)->get_as_pointer());
				}
				else c->arg(0)->set(obj.m->get_uniquetypes());
				c->get_parent()->set_atomtype_variables(c->arg(0)->get_name(), (ffatom*) c->arg(0)->get_as_pointer());
				if (c->argpb(0) == NULL) status = FALSE;
				break;
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
int commanddata::function_CA_GOTO(command *&c, bundle &obj)
{
	c = c->get_pointer();
	return CR_SUCCESSNOMOVE;
}

// Jump to next node in current list that is *not* an ELSE(IF)
int commanddata::function_CA_GOTONONIF(command *&c, bundle &obj)
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
int commanddata::function_CA_IF(command *&c, bundle &obj)
{
	if (c->if_evaluate()) c = c->get_branch_commands();
	else c = c->next;
	return CR_SUCCESSNOMOVE;
}

// Internal TERMINATE command for flow control
int commanddata::function_CA_TERMINATE(command *&c, bundle &obj)
{
	return CR_EXIT;
}
