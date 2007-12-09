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
#include "base/debug.h"

// Initialise For Loop
bool command::loop_initialise(variable_list &vars, model *m)
{
	dbg_begin(DM_CALLS,"command::loop_initialise");
	// Variable that we're iterating is args[0].
	// args[1] contains a pointer to another variable type which may influence the range of the loop
	variable *countvar, *rangevar;
	variable_type counttype, rangetype;
	pattern *p;
	atom *i;
	int n;
	// Obvious check first - is this a loop node?
	if (action >= CA_IF)
	{
		printf("command::loop_initialise <<<< This is not a loop node! >>>>\n");
		dbg_end(DM_CALLS,"command::loop_initialise");
		return FALSE;
	}
	// Grab pointers to count and range variables
	countvar = args[0];
	rangevar = args[1];
	// Check vars...
	if (countvar == NULL)
	{
		printf("command::loop_initialise <<<< Count variable in loop has not been set >>>>\n");
		dbg_end(DM_CALLS,"command::loop_initialise");
		return FALSE;
	}
	counttype = countvar->get_type();
	//countvar->print();
	if (rangevar == NULL) rangetype = VT_NITEMS;
	else rangetype = rangevar->get_type();
	msg(DM_VERBOSE,"Initialising loop : count variable is '%s', type = '%s'\n", countvar->get_name(), text_from_VT(counttype));
	// Start off the loop
	switch (counttype)
	{
		// Integer loop - starts at 1
		case (VT_INT):
			countvar->set(1);
			break;
		// Atom loop - start depends on second variable.
		case (VT_ATOM):
			// If no second variable is given, use the source model
			if (rangevar == NULL) countvar->set(m->get_atoms());
			//else if (rangetype == VT_MODEL) countvar->set(rangevar->get_as_model()->get_atoms());
			else if (rangetype == VT_PATTERN)
			{
				p = (pattern*) rangevar->get_as_pointer(VT_PATTERN);
				if (p == NULL)
				{
					printf("command::loop_initialise <<<< atom:pattern rangevar is NULL >>>>\n");
					dbg_end(DM_CALLS,"command::loop_initialise");
					return FALSE;
				}
				msg(DM_VERBOSE,"                  : range variable is '%s', type = 'pattern*' (%s)\n", rangevar->get_name(), p->get_name());
				// Check for subrange variable (integer molecule)
				if (args[2] == NULL) countvar->set(p->get_firstatom());
				else
				{
					// Get first atom and skip on nmolatoms * (args[2]-1)
					i = p->get_firstatom();
					for (n = 0; n < p->get_natoms() * (args[2]->get_as_int() - 1); n++) i = i->next;
					countvar->set(i);
				}
			}
			else
			{
				msg(DM_NONE,"Range variable '%s' is not of suitable type (%s) for atom loop.\n", rangevar->get_name(), text_from_VT(rangevar->get_type()));
				dbg_end(DM_CALLS,"command::loop_initialise");
				return FALSE;
			}
			// Set atom variables from the atom pointer
			vars.set_atom_variables(countvar->get_name(), (atom*) countvar->get_as_pointer(VT_ATOM));
			break;
		// Pattern loop over patterns in model
		case (VT_PATTERN):
			/* if (rangetype != VT_MODEL)
			{
				printf("filter::loop_initialise <<<< Range variable '%s' is not of suitable type for pattern loop >>>>\n",rangevar->get_name());
				dbg_end(DM_CALLS,"filter::loop_initialise");
				return FALSE;
			}
			*/
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

// Flow control commands
bool commandlist::do_basic(command *&fn, model *m, ifstream *srcfile)
{
	dbg_begin(DM_CALLS,"commandlist::do_basic");
	bool result = TRUE;
	bool done = FALSE;
	static format varformat;
	static dnchar printstr, var;
	static char srcstr[512];
	static char *c;
	static bool vardone;
	command_action cmd = fn->get_command_action(), cmd2;
	switch (cmd)
	{
		// Loop for n iterations or forever (quit if EOF)
		case (BC_REPEAT):
			if (srcfile != NULL)
				if (srcfile->peek() == -1)
				{
					msg(DM_VERBOSE,"commandlist::do_basic - 'repeat' reached end of file.\n");
					fn->set_loop_running(FALSE);
					fn = fn->next;
					break;
				}
			// If the loop is not running, start it. Otherwise, check loop iteration number
			if (fn->get_loop_running())
			{
				// Quit if loop has done all its iterations
				if (!fn->loop_iterate(variables)) fn = fn->next;
				else fn = fn->get_branch_commands();
			}
			else
			{
				if (!fn->loop_initialise(variables, m)) fn = fn->next;
				else fn = fn->get_branch_commands();
			}
			break;
		// If / Elseif statements
		case (BC_IF):	
		case (BC_ELSEIF):
			if (fn->if_evaluate()) fn = fn->get_branch_commands();
			else fn = fn->next;
			break;
		// Else statement
		case (BC_ELSE):	
			fn = fn->get_branch_commands();
			break;
		// Loops over something
		case (BC_FORATOMS):
		case (BC_FORPATTERNS):
		//case (BC_FORMOLECULES):
		case (BC_FORFFBONDS):
		case (BC_FORFFANGLES):
		case (BC_FORFFTORSIONS):
		/*	if (useconfig)
			{
				result = FALSE;
				printf("commandlist::do_basic <<<< Can't use '%s' loop for config >>>>\n",text_from_FC(cmd));
				break;
			} */
			if (fn->get_loop_running())
			{
				//msg(DM_VERBOSE,"'%s' loop is already running...\n",text_from_FC(cmd));
				// Check if loop has finished
				if (fn->loop_iterate(variables)) fn = fn->get_branch_commands();
				else fn = fn->next;
			}
			else
			{
				//msg(DM_VERBOSE,"Initialising '%s' loop...\n",text_from_FC(cmd));
				if (fn->loop_initialise(variables, m)) fn = fn->get_branch_commands();
				else fn = fn->next;
			}
			break;
		// Jump to specified node
		case (BC_GOTO):
			//printf("Performing generic GOTO\n");
			fn = fn->get_pointer();
			break;
		// Jump to next node in current list that is *not* an ELSE(IF)
		case (BC_GOTONONIF):
			//printf("Searching for next non-if node...\n");
			fn = fn->get_pointer();
			// Skip to node after the source node and begin search.
			fn = fn->next;
			// If we find an BC_IF node we stop and continue on from there.
			//printf("Skipped back to node %li (%s)\n",fn,text_from_FC(fn->get_command()));
			cmd2 = fn->get_command_action();
			while ((cmd2 == BC_ELSEIF) || (cmd2 == BC_ELSE))
			{
				fn = fn->next;
				cmd2 = fn->get_command_action();
			}
			break;
		// Set variable to value or variable
		case (BC_LET):
			// If the first var is a pointer, second must be a pointer!
			if (fn->datavar[0]->get_type() >= VT_ATOM)
			{
				if (fn->datavar[0]->get_type() != fn->datavar[2]->get_type())
					msg(DM_NONE,"Incompatible pointer types for variable assignment of contents of '%s' to '%s'.\n", fn->datavar[0]->get_name(), fn->datavar[2]->get_name());
				else fn->datavar[0]->copy_pointer(fn->datavar[2]);
			}
			else fn->datavar[0]->set(fn->argc(2));
			fn = fn->next;
			break;
		// Evaluate expression and assign to variable
		case (BC_EVAL):
			fn->datavar[0]->set(evaluate(fn->argc(2), &variables));
			fn = fn->next;
			break;
		case (BC_EVALI):
			fn->datavar[0]->set(atoi(evaluate(fn->argc(2), &variables)));
			fn = fn->next;
			break;
		// Increase variable by '1'
		case (BC_INCREASE):
			fn->datavar[0]->increase(1);
			fn = fn->next;
			break;
		// Decrease variable by 1
		case (BC_DECREASE):
			fn->datavar[0]->decrease(1);
			fn = fn->next;
			break;
		// Print formatted string
		case (BC_PRINT):
			// Go through supplied string, converting variables as we go
			printstr.create_empty(512);
			strcpy(srcstr, fn->argc(0));
			for (c = srcstr; *c != '\0'; c++)
			{
				// If the character is not '$', just add it to printstr
				if (*c != '$')
				{
					printstr += *c;
					continue;
				}
				// This is the start of a variable format
				// Clear the variable string and skip past the '$'
				var.create_empty(64);
				var += '$';
				c++;
				// Add characters to 'var' until we find the end of the format
				// Demand that vars are formatted as ${name[@format]}, or terminated by a space
				vardone = FALSE;
				while (*c != ' ')
				{
					switch (*c)
					{
						case ('{'):
							c++;
							break;
						case ('\0'):
							c--;
						case ('}'):
							vardone = TRUE;
							break;
						case (' '):
							vardone = TRUE;
							c--;
							break;
						default:
							var += *c;
							c++;
							break;
					}
					if (vardone) break;
				}
				// Now have variable (and format) in 'var'.
				// Create a quick format and add this to the printstr.
				varformat.create(var.get(), variables);
				//printf("Variable part = [%s]\n",var.get());
				//printf("Current PRINTSTR = [%s]\n",printstr.get());
				printstr.cat(varformat.create_string());
			}
			// Final string to print is now in printstr...
			msg(DM_NONE,"%s\n",printstr.get());
			fn = fn->next;
			break;
		default:
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"commandlist::do_basic");
	return result;
}