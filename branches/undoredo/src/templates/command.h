/*
	*** Command list functions
	*** src/templates/command.h
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

#ifndef H_COMMAND_H
#define H_COMMAND_H

#define MAXDATAVARS 10

#include "templates/list.h"
#include "classes/variables.h"
#include "file/format.h"
#include "model/model.h"
#include "classes/pattern.h"
#include "file/parse.h"
#include "base/sysfunc.h"

// Command node basic commands
enum basic_command {
	BC_ROOTNODE,

	BC_REPEAT,
	BC_FORATOMS,
	BC_FORBONDS,
	BC_FORPATTERNS,
	BC_FORMOLECULES,
	BC_FORFFBONDS,
	BC_FORFFANGLES,
	BC_FORFFTORSIONS,

	BC_IF,
	BC_ELSEIF,
	BC_ELSE,
	BC_END,

	BC_LET,
	BC_INCREASE,
	BC_DECREASE,
	BC_EVAL,
	BC_EVALI,

	BC_PRINT,

	BC_GOTO,
	BC_GOTONONIF,
	BC_TERMINATE,
	BC_OTHER,
	BC_NITEMS };
basic_command BC_from_text(const char*);
const char *text_from_BC(basic_command);
const char *vars_from_BC(basic_command);

// If Conditions
enum if_condition { IF_EQUAL=1, IF_LESS=2, IF_LEQUAL=3, IF_GREATER=4, IF_GEQUAL=5, IF_NEQUAL=6, IF_NITEMS };
const char *text_from_IC(if_condition);

// Command node
template <class cmd_type> class command_node
{
	public:
	// Constructor / Destructor
	command_node();
	~command_node();
	// List pointers
	command_node *prev, *next;

	/*
	// Command
	*/
	private:
	// Basic command that this node performs
	basic_command basiccommand;
	// Specific command (when basic = CA_OTHER) to perform
	cmd_type command;

	public:
	// Set basic command
	void set_basic_command(basic_command bc) { basiccommand = bc; }
	// Get basic command
	basic_command get_basic_command() { return basiccommand; }
	// Set command
	void set_command(cmd_type f) { command = f; }
	// Get command
	cmd_type get_command() { return command; }

	/*
	// Format
	*/
	private:
	// Associated format (if any)
	format *fmt;

	public:
	// Create format structure
	void create_format(const char*, variable_list&);
	// Returns the formatter
	format *get_format() { return fmt; }

	/*
	// Loop Data
	*/
	private:
	// Internal counter for loop
	int loopcount;
	// Whether the loop is currently executing
	bool loop_running;

	public:
	// Set loop running status
	void set_loop_running(bool b) { loop_running = b; }
	// Get loop running status
	bool get_loop_running() { return loop_running; }
	// Initialise loop
	bool loop_initialise(variable_list&, model*);
	// Iterate loop
	bool loop_iterate(variable_list&);
	// Check loop for termination
	bool loop_check();

	/*
	// Command Branch
	*/
	private:
	// Lists for branched commands (if any)
	list<command_node> *branch;
	// Pointer for use by flow control nodes
	command_node *ptr;

	public:
	// Create branch for the node
	list<command_node> *create_branch();
	// Returns branch list structure
	list<command_node> *get_branch() { return branch; }
	// Returns first item in branch 
	command_node *get_branch_commands() { return (branch != NULL ? branch->first() : NULL); }
	// Set format_node pointer variable
	void set_pointer(command_node *f) { ptr = f; }
	// Return format_node pointer variable
	command_node *get_pointer() { return ptr; }

	/*
	// If Test Data
	*/
	private:
	// If condition structure
	if_condition iftest;

	public:
	// Set if test type
	bool set_iftest(const char*);
	// Evaluate the if expression
	bool if_evaluate();

	/*
	// Data Variables
	*/
	public:
	// Data variables
	variable *datavar[MAXDATAVARS];
	// Set variables from parser arguments
	bool add_variables(const char*, const char*, variable_list&);
	// Return arguments as vec3<double>
	vec3<double> get_vector3d(int);
	// Return arguments as vec3<float>
	vec3<float> get_vector3f(int);
	// Return arguments as vec3<int>
	vec3<int> get_vector3i(int);
	// Print data variables
	void print_datavars();
};

// Command List Structore
template <class T> class command_list
{
	/*
	// Command List
	*/
	private:
	// List of pointers to stacked branches
	reflist< list<command_node<T> > > branchstack;
	// Basic command types of stacked branches
	list<command_node<T> > branchcmdstack;
	// Add specified branch to stack
	void push_branch(list< command_node<T> >*, basic_command, command_node<T>*);
	// Pop topmost branch from stack
	void pop_branch();
	// Add command to topmost branch
	command_node<T>* add_topbranch_command(basic_command, command_node<T>*);

	public:
	// Return size of branch stack
	int get_branchstack_size() { return branchstack.size(); }
	// Return type of topmost branch on stack
	basic_command get_topbranch_type();
	// Return basenode pointer of topmost branch on stack
	command_node<T>* get_topbranch_basenode();
	// List of commands
	list< command_node<T> > commandlist;
	// Add basic command node
	bool add_basic(basic_command);
	// Add other command node
	bool add_other(T, const char*, const char*);
	// Perform basic (internal) control commands
	bool do_basic(command_node<T>*&, model*, ifstream*);
	// Clear and reinitialise command list
	void clear();

	/*
	// Variables
	*/
	public:
	// Associative variable list
	variable_list variables;
};

// Constructor
template <class T> command_node<T>::command_node()
{
	next = NULL;
	prev = NULL;
	for (int i=0; i<MAXDATAVARS; i++) datavar[i] = NULL;
	basiccommand = BC_OTHER;
	//command = CA_NITEMS;
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
template <class T> command_node<T>::~command_node()
{
	if (branch != NULL) delete branch;
	if (fmt != NULL) delete fmt;
	#ifdef MEMDEBUG
		memdbg.destroy[MD_COMMANDNODE] ++;
	#endif
}

// Clear command list and reinitialise
template <class T> void command_list<T>::clear()
{
	commandlist.clear();
	branchstack.clear();
	branchcmdstack.clear();
	push_branch(&commandlist, BC_ROOTNODE, NULL);
}

// Print data variables
template <class T> void command_node<T>::print_datavars()
{
	dbg_begin(DM_CALLS,"command_node::print_datavars");
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
	dbg_end(DM_CALLS,"command_node::print_datavars");
}


// Return arguments as vec3<double>
template <class T> vec3<double> command_node<T>::get_vector3d(int i)
{
	dbg_begin(DM_CALLS,"command_node::get_vector3d");
        static vec3<double> result;
        if (i > (MAXDATAVARS-3)) printf("command_node::get_vector3d - Starting point too close to MAXDATAVARS.\n");
        result.set(datavar[i]->get_as_double(),datavar[i+1]->get_as_double(),datavar[i+2]->get_as_double());
	dbg_end(DM_CALLS,"command_node::get_vector3d");
        return result;
}

// Return arguments as vec3<float>
template <class T> vec3<float> command_node<T>::get_vector3f(int i)
{
	dbg_begin(DM_CALLS,"command_node::get_vector3f");
        static vec3<float> result;
        if (i > (MAXDATAVARS-3)) printf("command_node::get_vector3f - Starting point too close to MAXDATAVARS.\n");
        result.set(datavar[i]->get_as_float(),datavar[i+1]->get_as_float(),datavar[i+2]->get_as_float());
	dbg_end(DM_CALLS,"command_node::get_vector3f");
        return result;
}

// Return arguments as vec3<int>
template <class T> vec3<int> command_node<T>::get_vector3i(int i)
{
	dbg_begin(DM_CALLS,"command_node::get_vector3i");
	static vec3<int> result;
	if (i > (MAXDATAVARS-3)) printf("command_node::get_vector3i - Starting point too close to MAXDATAVARS.\n");
        result.set(datavar[i]->get_as_int(),datavar[i+1]->get_as_int(),datavar[i+2]->get_as_int());
	dbg_end(DM_CALLS,"command_node::get_vector3i");
	return result;
}

// Create branch
template <class T> list< command_node<T> > *command_node<T>::create_branch()
{
	dbg_begin(DM_CALLS,"command_node::create_branch");
	if (branch != NULL) printf("command_node::create_branch <<<< Already has a branch >>>>\n");
	branch = new list< command_node<T> >;
	dbg_end(DM_CALLS,"command_node::create_branch");
	return branch;
}

// Create branch
template <class T> void command_node<T>::create_format(const char *s, variable_list &vars)
{
	dbg_begin(DM_CALLS,"command_node::create_format");
	if (fmt != NULL) printf("command_node::create_branch <<<< Already has a format >>>>\n");
	else
	{
		fmt = new format;
		fmt->create(s, vars);
	}
	dbg_end(DM_CALLS,"command_node::create_format");
}

// Initialise For Loop
template <class T> bool command_node<T>::loop_initialise(variable_list &vars, model *m)
{
	dbg_begin(DM_CALLS,"command_node::loop_initialise");
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
		printf("command_node::loop_initialise <<<< This is not a loop node! >>>>\n");
		dbg_end(DM_CALLS,"command_node::loop_initialise");
		return FALSE;
	}
	// Grab pointers to count and range variables
	countvar = datavar[0];
	rangevar = datavar[1];
	// Check vars...
	if (countvar == NULL)
	{
		printf("command_node::loop_initialise <<<< Count variable in loop has not been set >>>>\n");
		dbg_end(DM_CALLS,"command_node::loop_initialise");
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
					printf("command_node::loop_initialise <<<< atom:pattern rangevar is NULL >>>>\n");
					dbg_end(DM_CALLS,"command_node::loop_initialise");
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
				dbg_end(DM_CALLS,"command_node::loop_initialise");
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
						printf("command_node::loop_initialise <<<< ffbound interaction not in list >>>>\n");
						break;
				}
			}
			else
			{
				msg(DM_NONE,"Range variable '%s' is not of suitable type (%s) for 'ffbound' loop.\n", rangevar->get_name(), text_from_VT(rangevar->get_type()));
				dbg_end(DM_CALLS,"command_node::loop_initialise");
				return FALSE;
			}
			// Set patbound variables from the patbound pointer
			vars.set_patbound_variables(countvar->get_name(), (patbound*) countvar->get_as_pointer(VT_PATBOUND));
			break;
		default:
			printf("Kick Developer - Loops over '%s' are missing.\n",text_from_VT(counttype));
			dbg_end(DM_CALLS,"command_node::loop_initialise");
			return FALSE;
			break;
	}
	loop_running = TRUE;
	loopcount = 1;
	// Check on niterations
	bool result = loop_check();
	if (result) msg(DM_VERBOSE,"Loop is initialised and running.\n");
	else msg(DM_VERBOSE,"Loop terminated on initialisation.\n");
	dbg_end(DM_CALLS,"command_node::loop_initialise");
	return result;
}

// Do loop iteration
template <class T> bool command_node<T>::loop_iterate(variable_list &vars)
{
	dbg_begin(DM_CALLS,"command_node::loop_iterate");
	atom *i;
	pattern *p;
	// Grab variables
	variable *countvar = datavar[0];
	if (countvar == NULL)
	{
		printf("command_node::loop_iterate <<<< Count var in loop has not been set >>>>\n");
		dbg_end(DM_CALLS,"command_node::loop_iterate");
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
			printf("command_node::loop_iterate <<<< Don't know how to set variables from var of type '%s' >>>>\n", text_from_VT(countvar->get_type()));
			break;
	}
	// Check for completed loop on exit
	dbg_end(DM_CALLS,"command_node::loop_iterate");
	return loop_check();
}

// Check for loop termination
template <class T> bool command_node<T>::loop_check()
{
	dbg_begin(DM_CALLS,"command_node::loop_check");
	// Grab variables
	variable *countvar, *rangevar;
	atom *i;
	pattern *p;
	patbound *pb;
	countvar = datavar[0];
	if (countvar == NULL)
	{
		printf("command_node::loop_check <<<< Count var in loop has not been set >>>>\n");
		dbg_end(DM_CALLS,"command_node::loop_check");
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
	dbg_end(DM_CALLS,"command_node::loop_check");
	return loop_running;
}

// Set if condition test
template <class T> bool command_node<T>::set_iftest(const char *s)
{
	dbg_begin(DM_CALLS,"command_node::set_iftest");
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
	dbg_end(DM_CALLS,"command_node::set_iftest");
	return result;
}

// Evaluate condition
template <class T> bool command_node<T>::if_evaluate()
{
	dbg_begin(DM_CALLS,"command_node::if_evaluate");
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
	dbg_end(DM_CALLS,"command_node::if_evaluate");
	return result;
}

// Add variables to command
template <class T> bool command_node<T>::add_variables(const char *cmd, const char *v, variable_list &vars)
{
	dbg_begin(DM_CALLS,"command_node::add_variables");
	bool result = TRUE, required = TRUE;
	int n, argcount, varcount;
	dnchar arg;
	variable_type vt;
	//printf("DOING VARIABLES (%s) FOR COMMAND '%s'\n",v,cmd);
	// Are there arguments in the parser that we shouldn't have been given.
	if ((parser.get_nargs() - 1) > (strlen(v) - (index(v,'|') != NULL ? 1 : 0)))
	{
		printf("Too many arguments (%i) given to command '%s' (which expects %i at most).\n", (parser.get_nargs()-1), cmd, strlen(v));
		dbg_end(DM_CALLS,"command_node::add_variables");
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
		if (parser.is_blank(argcount))
		{
			if (required)
			{
				printf("Script command '%s' requires argument %i\n", cmd, argcount);
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
					dbg_end(DM_CALLS,"command_node::add_variables");
					return FALSE;
				}
				else continue;
		}
		// Now for specifiers that require variables of a certain type.
		// Capital letters enforce type onto a variable
		datavar[varcount] = vars.get(arg.get());
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
				printf("command_node::add_variables <<<< Unrecognised internal variable specifier '%c' >>>>\n", v[n]);
				result = FALSE;
				break;
		}
		// Exit if we encountered an error
		if (!result) break;
	}
	dbg_end(DM_CALLS,"command_node::add_variables");
	return result;
}

// Push branch on to stack
template <class T> void command_list<T>::push_branch(list< command_node<T> > *branch, basic_command bc, command_node<T> *basenode)
{
	branchstack.add(branch);
	command_node<T> *cn = branchcmdstack.add();
	cn->set_basic_command(bc);
	cn->set_pointer(basenode);
}

// Pop topmost branch on stack
template <class T> void command_list<T>::pop_branch()
{
	if (branchstack.size() == 0)
	{
		printf("command_list::pop_branch <<<< No branches in branch list! >>>>\n");
		return;
	}
	branchstack.remove(branchstack.last());
	branchcmdstack.remove(branchcmdstack.last());
}

// Return basic command type of topmost branch
template <class T> basic_command command_list<T>::get_topbranch_type()
{
	if (branchcmdstack.size() == 0)
	{
		printf("command_list::get_topbranch_type <<<< No branches in branch list! >>>>\n");
		return BC_NITEMS;
	}
	else return branchcmdstack.last()->get_basic_command();
}

// Return base node of topmost branch
template <class T> command_node<T>* command_list<T>::get_topbranch_basenode()
{
	if (branchcmdstack.size() == 0)
	{
		printf("command_list::get_topbranch_basenode <<<< No branches in branch list! >>>>\n");
		return NULL;
	}
	else return branchcmdstack.last()->get_pointer();
}

// Add command to topmost branch
template <class T> command_node<T>* command_list<T>::add_topbranch_command(basic_command bc, command_node<T> *nodeptr)
{
	if (branchstack.size() == 0)
	{
		printf("command_list::add_topbranch_command <<<< No branches in branch list! >>>>\n");
		return NULL;
	}
	command_node<T> *cn = branchstack.last()->item->add();
	cn->set_basic_command(bc);
	cn->set_pointer(nodeptr);
	return cn;
}

// Add basic command
template <class T> bool command_list<T>::add_basic(basic_command bc)
{
	dbg_begin(DM_CALLS,"command_list::add_basic");
	// Pointers to command nodes
	command_node<T> *fn, *fn2, *fn3;
	basic_command branchbc;
	variable_type vt;
	int n;
	variable *v;
	bool result = TRUE, varresult = TRUE;
	// Perform specific node actions / pointer management for commands...
	switch (bc)
	{
		// 'If' statement (if 'x condition y')
		case (BC_IF):
			fn = add_topbranch_command(BC_IF, NULL);
			push_branch(fn->create_branch(), BC_IF, fn);
			varresult = fn->add_variables(text_from_BC(bc), vars_from_BC(bc), variables);
			if (!fn->set_iftest(parser.argc(2))) result = FALSE;
			break;
		// 'Else If' statement (acts as BC_END to previous 'if' or 'elseif' branch.
		case (BC_ELSEIF):
			// If the previous branch was an 'if' or 'elseif', set the *ptr of that node to this node
			branchbc = get_topbranch_type();
			if ((branchbc != BC_IF) && (branchbc != BC_ELSEIF))
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
			varresult = fn->add_variables(text_from_BC(bc), vars_from_BC(bc), variables);
			if (!fn->set_iftest(parser.argc(2))) result = FALSE;
			break;
		// 'Else' statement (acts as BC_END to previous 'if' or 'elseif' branch.
		case (BC_ELSE):
			// If the previous branch was an 'if' or 'elseif', set the *ptr of that node to this node
			branchbc = get_topbranch_type();
			if ((branchbc != BC_IF) && (branchbc != BC_ELSEIF))
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
			varresult = fn->add_variables(text_from_BC(bc), vars_from_BC(bc), variables);
			break;
		// Loops over items
		case (BC_FORPATTERNS):		// Loop over patterns in model
		case (BC_FORMOLECULES):		// Loop over molecules in pattern
		case (BC_FORATOMS):		// Loop over atoms
		case (BC_FORBONDS):		// Loop over bonds in model
		case (BC_FORFFBONDS):		// Loop over pattern's ff bonds
		case (BC_FORFFANGLES):		// Loop over pattern's ff angles
		case (BC_FORFFTORSIONS):	// Loop over pattern's ff torsions
			fn = add_topbranch_command(bc, NULL);
			push_branch(fn->create_branch(), bc, fn);
			varresult = fn->add_variables(text_from_BC(bc), vars_from_BC(bc), variables);
			break;
		// End the topmost branch in the stack
		case (BC_END):
			if (branchstack.size() == 0)
			{
				msg(DM_NONE,"command_list::add_basic - 'end' does not end a block.\n");
				result = FALSE;
				break;
			}
			// Check command stack to choose list ending pointer
			branchbc = get_topbranch_type();
			switch (branchbc)
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
					printf("command_list::add_basic <<<< No END action defined for command '%s' >>>>\n",text_from_BC(branchbc));
					result = FALSE;
					break;
			}
			// Remove the topmost branch from the stack
			pop_branch();
			break;
		default:
			fn = add_topbranch_command(bc, NULL);
			varresult = fn->add_variables(text_from_BC(bc), vars_from_BC(bc), variables);
			break;
	}
	// Check variable assignment result
	if (!varresult)
	{
		msg(DM_NONE,"Error: Command '%s' was not given the correct variables.\n", text_from_BC(bc));
		result = FALSE;
	}
	dbg_end(DM_CALLS,"command_list::add_basic");
	return result;
}

// Add basic command
template <class T> bool command_list<T>::add_other(T oc, const char *cmd, const char *v)
{
	dbg_begin(DM_CALLS,"command_list::add_other");
	// Pointers to command nodes
	command_node<T> *fn;
	bool result = TRUE;
	fn = branchstack.last()->item->add();
	//printf("New node is %li, command = %s\n",fn,FC_keywords[cmd]);
	fn->set_command(oc);
	// Add variables to command pointed by 'fn'
	if (!fn->add_variables(cmd, v, variables))
	{
		msg(DM_NONE,"Error: Command '%s' was not given the correct variables.\n", cmd);
		result = FALSE;
	}
	dbg_end(DM_CALLS,"command_list::add_other");
	return result;
}

// Flow control commands
template <class T> bool command_list<T>::do_basic(command_node<T> *&fn, model *m, ifstream *srcfile)
{
	dbg_begin(DM_CALLS,"command_list::do_basic");
	bool result = TRUE;
	bool done = FALSE;
	static format varformat;
	static dnchar printstr, var;
	static char srcstr[512];
	static char *c;
	static bool vardone;
	basic_command cmd = fn->get_basic_command(), cmd2;
	switch (cmd)
	{
		// Loop for n iterations or forever (quit if EOF)
		case (BC_REPEAT):
			if (srcfile != NULL)
				if (srcfile->peek() == -1)
				{
					msg(DM_VERBOSE,"command_list::do_basic - 'repeat' reached end of file.\n");
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
				printf("command_list::do_basic <<<< Can't use '%s' loop for config >>>>\n",text_from_FC(cmd));
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
			cmd2 = fn->get_basic_command();
			while ((cmd2 == BC_ELSEIF) || (cmd2 == BC_ELSE))
			{
				fn = fn->next;
				cmd2 = fn->get_basic_command();
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
	dbg_end(DM_CALLS,"command_list::do_basic");
	return result;
}

#endif
