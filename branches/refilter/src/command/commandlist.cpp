/*
	*** Command list functions
	*** src/templates/command.cpp
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

#include "templates/command.h"
#include "base/sysfunc.h"

// If Conditions
const char *IC_strings[6] = { "eq", "l", "le", "g", "ge", "neq" };
const char *text_from_IC(if_condition i)
	{ return IC_strings[i-1]; }

// Constructor
command::command()
{
	next = NULL;
	prev = NULL;
	for (int i=0; i<MAXDATAVARS; i++) datavar[i] = NULL;
	action = CA_ROOTNODE;
	function = NULL;
	ptr = NULL;
	branch = NULL;
	fmt = NULL;
	loopcount = 0;
	loop_running = FALSE;
	#ifdef MEMDEBUG
		memdbg.create[MD_COMMANDNODE] ++;
	#endif
}

// Destructor
command::~command()
{
	if (branch != NULL) delete branch;
	if (fmt != NULL) delete fmt;
	#ifdef MEMDEBUG
		memdbg.destroy[MD_COMMANDNODE] ++;
	#endif
}

// Clear command list and reinitialise
void commandlist::clear()
{
	commandlist.clear();
	branchstack.clear();
	branchcmdstack.clear();
	push_branch(&commandlist, BC_ROOTNODE, NULL);
}

// Print data variables
template <class T> void command::print_datavars()
{
	dbg_begin(DM_CALLS,"command::print_datavars");
	int i;
	for (int i=0; i<MAXDATAVARS; i++)
	{
		printf("%2i %20li",i,datavar[i]);
		if (datavar[i] == NULL) printf ("None.\n");
		else
		{
			printf("%12s [%10s]",datavar[i]->get_name(), text_from_VT(datavar[i]->get_type()));
			if (datavar[i]->get_type() < VT_ATOM) printf("%20s\n",datavar[i]->get_as_char());
			else printf("%li\n",datavar[i]->get_as_pointer(VT_UNDEFINED));
		}
	}
	dbg_end(DM_CALLS,"command::print_datavars");
}


// Return arguments as vec3<double>
template <class T> vec3<double> command::get_vector3d(int i)
{
	dbg_begin(DM_CALLS,"command::get_vector3d");
        static vec3<double> result;
        if (i > (MAXDATAVARS-3)) printf("command::get_vector3d - Starting point too close to MAXDATAVARS.\n");
        result.set(datavar[i]->get_as_double(),datavar[i+1]->get_as_double(),datavar[i+2]->get_as_double());
	dbg_end(DM_CALLS,"command::get_vector3d");
        return result;
}

// Return arguments as vec3<float>
template <class T> vec3<float> command::get_vector3f(int i)
{
	dbg_begin(DM_CALLS,"command::get_vector3f");
        static vec3<float> result;
        if (i > (MAXDATAVARS-3)) printf("command::get_vector3f - Starting point too close to MAXDATAVARS.\n");
        result.set(datavar[i]->get_as_float(),datavar[i+1]->get_as_float(),datavar[i+2]->get_as_float());
	dbg_end(DM_CALLS,"command::get_vector3f");
        return result;
}

// Return arguments as vec3<int>
template <class T> vec3<int> command::get_vector3i(int i)
{
	dbg_begin(DM_CALLS,"command::get_vector3i");
	static vec3<int> result;
	if (i > (MAXDATAVARS-3)) printf("command::get_vector3i - Starting point too close to MAXDATAVARS.\n");
        result.set(datavar[i]->get_as_int(),datavar[i+1]->get_as_int(),datavar[i+2]->get_as_int());
	dbg_end(DM_CALLS,"command::get_vector3i");
	return result;
}

// Create branch
template <class T> list< command > *command::create_branch()
{
	dbg_begin(DM_CALLS,"command::create_branch");
	if (branch != NULL) printf("command::create_branch <<<< Already has a branch >>>>\n");
	branch = new list< command >;
	dbg_end(DM_CALLS,"command::create_branch");
	return branch;
}

// Create branch
template <class T> void command::create_format(const char *s, variable_list &vars)
{
	dbg_begin(DM_CALLS,"command::create_format");
	if (fmt != NULL) printf("command::create_branch <<<< Already has a format >>>>\n");
	else
	{
		fmt = new format;
		fmt->create(s, vars);
	}
	dbg_end(DM_CALLS,"command::create_format");
}

// Initialise For Loop
template <class T> bool command::loop_initialise(variable_list &vars, model *m)
{
	dbg_begin(DM_CALLS,"command::loop_initialise");
	// Variable that we're iterating is datavar[0].
	// datavar[1] contains a pointer to another variable type which may influence the range of the loop
	variable *countvar, *rangevar;
	variable_type counttype, rangetype;
	pattern *p;
	atom *i;
	int n;
	// Obvious check first - is this a loop node?
	if (basiccommand >= BC_IF)
	{
		printf("command::loop_initialise <<<< This is not a loop node! >>>>\n");
		dbg_end(DM_CALLS,"command::loop_initialise");
		return FALSE;
	}
	// Grab pointers to count and range variables
	countvar = datavar[0];
	rangevar = datavar[1];
	// Check vars...
	if (countvar == NULL)
	{
		printf("command::loop_initialise <<<< Count variable in loop has not been set >>>>\n");
		dbg_end(DM_CALLS,"command::loop_initialise");
		return FALSE;
	}
	counttype = countvar->get_type();
	//countvar->print();
	if (rangevar == NULL) rangetype = VT_UNDEFINED;
	else rangetype = rangevar->get_type();
	msg(DM_VERBOSE,"Initialising loop : count variable is '%s', type = '%s'\n", countvar->get_name(), text_from_VT(counttype));
	// Start off the loop
	switch (counttype)
	{
		// Integer loop - starts at 1
		case (VT_GENERIC):
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
				if (datavar[2] == NULL) countvar->set(p->get_firstatom());
				else
				{
					// Get first atom and skip on nmolatoms * (datavar[2]-1)
					i = p->get_firstatom();
					for (n = 0; n < p->get_natoms() * (datavar[2]->get_as_int() - 1); n++) i = i->next;
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
					case (BC_FORFFBONDS):
						countvar->set(p->bonds.first());
						break;
					case (BC_FORFFANGLES):
						countvar->set(p->angles.first());
						break;
					case (BC_FORFFTORSIONS):
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
template <class T> bool command::loop_iterate(variable_list &vars)
{
	dbg_begin(DM_CALLS,"command::loop_iterate");
	atom *i;
	pattern *p;
	// Grab variables
	variable *countvar = datavar[0];
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
template <class T> bool command::loop_check()
{
	dbg_begin(DM_CALLS,"command::loop_check");
	// Grab variables
	variable *countvar, *rangevar;
	atom *i;
	pattern *p;
	patbound *pb;
	countvar = datavar[0];
	if (countvar == NULL)
	{
		printf("command::loop_check <<<< Count var in loop has not been set >>>>\n");
		dbg_end(DM_CALLS,"command::loop_check");
		return FALSE;
	}
	rangevar = datavar[1];
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
				if (datavar[2] == NULL)
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

// Set if condition test
template <class T> bool command::set_iftest(const char *s)
{
	dbg_begin(DM_CALLS,"command::set_iftest");
	bool result = TRUE;
	int n, m;
	m = 0;
	for (n=0; s[n] != '\0'; n++)
		switch (s[n])
		{
			case ('='):
				m += 1;
				break;
			case ('<'):
				m += 2;
				break;
			case ('>'):
				m += 4;
				break;
			default:
				printf("Unrecognised character '%c' in 'if' condition\n",s[n]);
				result = FALSE;
				break;
		}
	if (result >= IF_NITEMS) result = FALSE;
	else iftest = (if_condition) m;
	dbg_end(DM_CALLS,"command::set_iftest");
	return result;
}

// Evaluate condition
template <class T> bool command::if_evaluate()
{
	dbg_begin(DM_CALLS,"command::if_evaluate");
	// Do all as comparisons as floats, except for equalities
	bool result;
	static dnchar value1, value2;
	static double d1, d2;
	//print_datavars();
	if ((iftest == IF_EQUAL) || (iftest == IF_NEQUAL))
	{
		// Grab current variable values into the value1/value2 character arrays (if var != NULL)
		value1 = datavar[0]->get_as_char();
		value2 = datavar[2]->get_as_char();
	}
	else
	{
		d1 = datavar[0]->get_as_double();
		d2 = datavar[2]->get_as_double();
	}
	msg(DM_VERBOSE,"IF TEST = var1(%s)=[%s] (%s) var2(%s)=[%s]\n", datavar[0]->get_name(), datavar[0]->get_as_char(), text_from_IC(iftest), datavar[2]->get_name(), datavar[2]->get_as_char());
	// Do comparison
	switch (iftest)
	{
		case (IF_EQUAL):
			result = (value1 == value2 ? TRUE : FALSE);
			break;
		case (IF_LESS):
			result = (d1 < d2 ? TRUE : FALSE);
			break;
		case (IF_LEQUAL):
			result = (d1 <= d2 ? TRUE : FALSE);
			break;
		case (IF_GREATER):
			result = (d1 > d2 ? TRUE : FALSE);
			break;
		case (IF_GEQUAL):
			result = (d1 >= d2 ? TRUE : FALSE);
			break;
		case (IF_NEQUAL):
			result = (value1 != value2 ? TRUE : FALSE);
			break;
	}
	//printf("IF TEST : [%s] [%i] [%s] = %s\n",value1,type,value2,(result ? "TRUE" : "FALSE"));
	dbg_end(DM_CALLS,"command::if_evaluate");
	return result;
}

// Add variables to command
template <class T> bool command::add_variables(const char *cmd, const char *v, variable_list &vars)
{
	dbg_begin(DM_CALLS,"command::add_variables");
	bool result = TRUE, required = TRUE;
	int n, argcount, varcount;
	dnchar arg;
	variable_type vt;
	//printf("DOING VARIABLES (%s) FOR COMMAND '%s'\n",v,cmd);
	// Are there arguments in the parser that we shouldn't have been given.
	if ((parser.get_nargs() - 1) > (strlen(v) - (index(v,'|') != NULL ? 1 : 0)))
	{
		printf("Too many arguments (%i) given to command '%s' (which expects %i at most).\n", (parser.get_nargs()-1), cmd, strlen(v));
		dbg_end(DM_CALLS,"command::add_variables");
		return FALSE;
	}
	argcount = 0;
	varcount = -1;
	for (n = 0; v[n] != '\0'; n++)
	{
		// Check for bar, which we take to mean 'start of optional variables'
		if (v[n] == '|')
		{
			required = FALSE;
			continue;
		}
		argcount ++;
		varcount ++;
		//printf("Adding variable %c which should have value %s\n", v[n], parser.argc(argcount));
		// Is this a required argument?
		//if ((parser.is_blank(argcount)) || (argcount >= parser.get_nargs()))
		if (argcount >= parser.get_nargs())
		{
			if (required)
			{
				printf("Command '%s' requires argument %i\n", cmd, argcount);
				result = FALSE;
				break;
			}
			else break;	// No more arguments, so may as well quit.
		}
		arg = parser.argc(argcount);
		// Check for specifiers that don't require variables to be created...
		switch (v[n])
		{
			// Formats
			case ('F'):
			case ('f'):
				create_format(arg.get(), vars);
				continue;
			// Discard
			case ('X'):
			case ('x'):
				continue;
			// String as-is
			case ('s'):
			case ('S'):
				datavar[varcount] = vars.add();
				datavar[varcount]->set(arg.get());
				continue;
			case ('='):
				if (strcmp(arg.get(),"=") != 0)
				{
					printf("Expected '=' after argument %i for command '%s'.\n", argcount, cmd);
					result = FALSE;
					dbg_end(DM_CALLS,"command::add_variables");
					return FALSE;
				}
				else continue;
		}
		// Now for specifiers that require variables of a certain type.
		// Capital letters enforce type onto a variable
		datavar[varcount] = vars.get(arg.get());
		// If datavar[varcount] is NULL then the argument was probably blank.
		if (datavar[varcount] == NULL) datavar[varcount] = vars.add();
		vt = datavar[varcount]->get_type();
		switch (v[n])
		{
			// Atom - may be 'atom*' or 'integer' (or forced as 'atom*')
			case ('A'):
				result = datavar[varcount]->set_type(VT_ATOM);
				break;
			case ('a'):
				if ((vt != VT_ATOM) && (vt != VT_GENERIC))
				{
					printf("Argument %i (%s) to command '%s' must be a variable of type 'atom*' or 'integer'.\n", argcount, arg.get(), cmd);
					result = FALSE;
				}
				break;
			case ('B'):
				result = datavar[varcount]->set_type(VT_PATBOUND);
				break;
			case ('b'):
				if (vt != VT_PATBOUND)
				{
					printf("Argument %i (%s) to command '%s' must be a variable of type 'patbound*'.\n", argcount, arg.get(), cmd);
					result = FALSE;
				}
				break;
			case ('G'):
				if (vt != VT_GENERICCONSTANT) result = datavar[varcount]->set_type(VT_GENERIC);
				break;
			case ('g'):
				if (vt != VT_GENERICCONSTANT)
				{
					result = datavar[varcount]->set_type(VT_GENERIC);
					vt = datavar[varcount]->get_type();
				}
				if ((vt != VT_GENERIC) && (vt != VT_GENERICCONSTANT))
				{
					printf("Argument %i (%s) to command '%s' must be a variable of type 'generic' or a constant value.\n", argcount, arg.get(), cmd);
					result = FALSE;
				}
				break;
			case ('M'):
				result = datavar[varcount]->set_type(VT_MODEL);
				break;
			case ('m'):
				if ((vt != VT_MODEL) && (vt != VT_GENERIC) && (vt != VT_GENERICCONSTANT))
				{
					printf("Argument %i (%s) to command '%s' must be a variable of type 'model*' or 'generic', or a constant value.\n", argcount, arg.get(), cmd);
					result = FALSE;
				}
				break;
			case ('P'):
				result = datavar[varcount]->set_type(VT_PATTERN);
				break;
			case ('p'):
				if (vt != VT_PATTERN)
				{
					printf("Argument %i (%s) to command '%s' must be a variable of type 'pattern*'.\n", varcount, arg.get(), cmd);
					result = FALSE;
				}
				break;
			case ('V'):
			case ('v'):
				if (vt == VT_GENERICCONSTANT)
				{
					printf("Argument %i (%s) to command '%s' must be a variable.\n", argcount, arg.get(), cmd);
					result = FALSE;
				}
				break;
			case ('?'):
				break;
			default:
				printf("command::add_variables <<<< Unrecognised internal variable specifier '%c' >>>>\n", v[n]);
				result = FALSE;
				break;
		}
		// Exit if we encountered an error
		if (!result) break;
	}
	dbg_end(DM_CALLS,"command::add_variables");
	return result;
}

// Push branch on to stack
void commandlist::push_branch(list<command> *branch, command_action ca, command *basenode)
{
	branchstack.add(branch);
	command *cn = branchcmdstack.add();
	cn->set_command_action(bc);
	cn->set_pointer(basenode);
}

// Pop topmost branch on stack
void commandlist::pop_branch()
{
	if (branchstack.size() == 0)
	{
		printf("commandlist::pop_branch <<<< No branches in branch list! >>>>\n");
		return;
	}
	branchstack.remove(branchstack.last());
	branchcmdstack.remove(branchcmdstack.last());
}

// Return basic command type of topmost branch
command_action commandlist::get_topbranch_type()
{
	if (branchcmdstack.size() == 0)
	{
		printf("commandlist::get_topbranch_type <<<< No branches in branch list! >>>>\n");
		return CA_NITEMS;
	}
	else return branchcmdstack.last()->get_command_action();
}

// Return base node of topmost branch
command* commandlist::get_topbranch_basenode()
{
	if (branchcmdstack.size() == 0)
	{
		printf("commandlist::get_topbranch_basenode <<<< No branches in branch list! >>>>\n");
		return NULL;
	}
	else return branchcmdstack.last()->get_pointer();
}

// Add command to topmost branch
command* commandlist::add_topbranch_command(command_action bc, command *nodeptr)
{
	if (branchstack.size() == 0)
	{
		printf("commandlist::add_topbranch_command <<<< No branches in branch list! >>>>\n");
		return NULL;
	}
	command *cn = branchstack.last()->item->add();
	cn->set_command_action(bc);
	cn->set_pointer(nodeptr);
	return cn;
}

// Add basic command
bool commandlist::add_command(command_action ca)
{
	dbg_begin(DM_CALLS,"commandlist::add_command");
	// Pointers to command nodes
	command *fn, *fn2, *fn3;
	command_action branchca;
	variable_type vt;
	int n;
	variable *v;
	bool result = TRUE, varresult = TRUE;
	switch (ca)
	{
		// 'If' statement (if 'x condition y')
		case (BC_IF):
			fn = add_topbranch_command(BC_IF, NULL);
			push_branch(fn->create_branch(), BC_IF, fn);
			varresult = fn->add_variables(text_from_BC(ca), vars_from_BC(ca), variables);
			if (!fn->set_iftest(parser.argc(2))) result = FALSE;
			break;
		// 'Else If' statement (acts as BC_END to previous 'if' or 'elseif' branch.
		case (BC_ELSEIF):
			// If the previous branch was an 'if' or 'elseif', set the *ptr of that node to this node
			branchca = get_topbranch_type();
			if ((branchca != BC_IF) && (branchca != BC_ELSEIF))
			{
				msg(DM_NONE,"Error: 'elseif' used without previous if/elseif.\n");
				result = FALSE;
				break;
			}
			// Add GOTONONIF command to topmost branch to end the if sequence
			fn = add_topbranch_command(BC_GOTONONIF, get_topbranch_basenode());
			// Pop topmost (previous IF/ELSEIF) branch
			pop_branch();
			// Add new command node to new topmost branch and get variables
			fn = add_topbranch_command(BC_ELSEIF, NULL);
			//printf("New node is %li, command = %s\n",fn,BC_keywords[cmd]);
			// Add new branch to this node for new if test to run
			push_branch(fn->create_branch(), BC_ELSEIF, fn);
			varresult = fn->add_variables(text_from_BC(ca), vars_from_BC(ca), variables);
			if (!fn->set_iftest(parser.argc(2))) result = FALSE;
			break;
		// 'Else' statement (acts as BC_END to previous 'if' or 'elseif' branch.
		case (BC_ELSE):
			// If the previous branch was an 'if' or 'elseif', set the *ptr of that node to this node
			branchca = get_topbranch_type();
			if ((branchca != BC_IF) && (branchca != BC_ELSEIF))
			{
				msg(DM_NONE,"Error: 'else' used without previous if/elseif.\n");
				result = FALSE;
				break;
			}
			// Add GOTONONIF command to current topbranch to terminate that branch
			fn = add_topbranch_command(BC_GOTONONIF, get_topbranch_basenode());
			// Pop previous branch from stack and add new command to new topmost branch
			pop_branch();
			// Add new node to new top branch
			fn = add_topbranch_command(BC_ELSE, NULL);
			//printf("New node is %li, command = %s\n",fn,BC_keywords[cmd]);
			// Add new branch to this node for new if test to run
			push_branch(fn->create_branch(), BC_ELSE, fn);
			break;
		// Loop for n iterations (or until file ends)
		case (BC_REPEAT):
			//fn->datavar[0]->set(0);
			fn = add_topbranch_command(BC_REPEAT, NULL);
			push_branch(fn->create_branch(), BC_REPEAT, fn);
			varresult = fn->add_variables(text_from_BC(ca), vars_from_BC(ca), variables);
			break;
		// Loops over items
		case (BC_FORPATTERNS):		// Loop over patterns in model
		case (BC_FORMOLECULES):		// Loop over molecules in pattern
		case (BC_FORATOMS):		// Loop over atoms
		case (BC_FORBONDS):		// Loop over bonds in model
		case (BC_FORFFBONDS):		// Loop over pattern's ff bonds
		case (BC_FORFFANGLES):		// Loop over pattern's ff angles
		case (BC_FORFFTORSIONS):	// Loop over pattern's ff torsions
			fn = add_topbranch_command(ca, NULL);
			push_branch(fn->create_branch(), ca, fn);
			varresult = fn->add_variables(text_from_BC(ca), vars_from_BC(ca), variables);
			break;
		// End the topmost branch in the stack
		case (BC_END):
			if (branchstack.size() == 0)
			{
				msg(DM_NONE,"commandlist::add_command - 'end' does not end a block.\n");
				result = FALSE;
				break;
			}
			// Check command stack to choose list ending pointer
			branchca = get_topbranch_type();
			switch (branchca)
			{
				// For repeats, jump back to node at start of loop (the branch owner)
				case (BC_REPEAT):
				case (BC_FORATOMS):
				case (BC_FORPATTERNS):
				case (BC_FORMOLECULES):
				case (BC_FORFFBONDS):
				case (BC_FORFFANGLES):
				case (BC_FORFFTORSIONS):
					add_topbranch_command(BC_GOTO, get_topbranch_basenode());
					break;
				// For IFs, jump to node containing IF/ELSEIF/ELSE branch (the branch owner)
				case (BC_IF):
				case (BC_ELSEIF):
				case (BC_ELSE):
					add_topbranch_command(BC_GOTONONIF, get_topbranch_basenode());
					break;
				case (BC_OTHER):
				case (BC_ROOTNODE):
					add_topbranch_command(BC_TERMINATE, NULL);
					break;
				default:
					printf("commandlist::add_basic <<<< No END action defined for command '%s' >>>>\n",text_from_BC(branchca));
					result = FALSE;
					break;
			}
			// Remove the topmost branch from the stack
			pop_branch();
			break;
		// All other commands do not alter the flow of the commandlist...
		default:
			fn = add_topbranch_command(ca, NULL);
			varresult = fn->add_variables(text_from_BC(ca), vars_from_BC(ca), variables);
			break;
	}
	// Check variable assignment result
	if (!varresult)
	{
		msg(DM_NONE,"Error: Command '%s' was not given the correct variables.\n", text_from_BC(ca));
		result = FALSE;
	}
	dbg_end(DM_CALLS,"commandlist::add_command");
	return result;
}

// Cache script commands from line containing semicolon-separated commands
bool commandlist::cache_line(const char *s)
{
	dbg_begin(DM_CALLS,"commandlist::cache_line");
	// Use a local parser to split up the semi-colon'd line into individual commands
	static line_parser lines;
	lines.get_lines_delim(s);
	for (int n=0; n<lines.get_nargs(); n++)
	{
		// Parse the argument in our local line_parser and call cache_command())
		parser.get_args_delim(lines.argc(n), PO_USEQUOTES+PO_SKIPBLANKS);
		if (!cache_command())
		{
			dbg_end(DM_CALLS,"commandlist::cache_line");
			return FALSE;
		}
	}
	dbg_end(DM_CALLS,"commandlist::cache_line");
	return TRUE;
}


// Cache command arguments in line_parser
bool commandlist::cache_command()
{
	dbg_begin(DM_CALLS,"commandlist::cache_command");
	script_command sc;
	basic_command bc;
	command *fn;
	int success;
	bool result = TRUE;
	// Assume that the main parser object contains the data we require.
	// Check for basic commands (local to command_nodes) first.
	bc = BC_from_text(parser.argc(0));
	if (bc != BC_NITEMS)
	{
		// Add the command to the list
		if (!commands.add_basic(bc))
		{
			msg(DM_NONE,"commandlist::load - Error adding basic command '%s'.\n", parser.argc(0));
			result = FALSE;
		}
	}
	else
	{
		// Find the script command and add this to the command list
		sc = SC_from_text(parser.argc(0));
		if (sc != SC_NITEMS)
		{
			// If add_other() returns FALSE then we encountered an error
			if (!commands.add_other(sc, text_from_SC(sc), vars_from_SC(sc)))
			{
				msg(DM_NONE,"Error adding script command '%s'.\n", parser.argc(0));
				msg(DM_NONE,  "Command usage is '%s'.\n", syntax_from_SC(sc));
				result = FALSE;
			}
		}
		else
		{
			msg(DM_NONE,"Unrecognised command '%s' in script.\n", parser.argc(0));
			result = FALSE;
		}
	}
	dbg_end(DM_CALLS,"commandlist::cache_command");
	return result;
}
