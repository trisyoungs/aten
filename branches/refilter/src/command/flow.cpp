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

#include "command/commands.h"
#include "base/debug.h"

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
			else fn->datavar[0]->set(fn->datavar[2]->get_as_char());
			fn = fn->next;
			break;
		// Evaluate expression and assign to variable
		case (BC_EVAL):
			fn->datavar[0]->set(evaluate(fn->datavar[2]->get_as_char(), &variables));
			fn = fn->next;
			break;
		case (BC_EVALI):
			fn->datavar[0]->set(atoi(evaluate(fn->datavar[2]->get_as_char(), &variables)));
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
			strcpy(srcstr, fn->datavar[0]->get_as_char());
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