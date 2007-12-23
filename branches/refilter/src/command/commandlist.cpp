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

#include "command/commandlist.h"
#include "file/format.h"
#include "file/parse.h"
#include "base/sysfunc.h"
#include "base/master.h"
#include "base/constants.h"

// Static variables
command_functions command::functions;

// If Conditions
const char *IC_strings[6] = { "eq", "l", "le", "g", "ge", "neq" };
const char *text_from_IC(if_condition i)
	{ return IC_strings[i-1]; }

// Constructors
command::command()
{
	next = NULL;
	prev = NULL;
	for (int i=0; i<MAXDATAVARS; i++) args[i] = NULL;
	action = CA_ROOTNODE;
	function = NULL;
	ptr = NULL;
	branch = NULL;
	fmt = NULL;
	loopactive = FALSE;
	#ifdef MEMDEBUG
		memdbg.create[MD_COMMANDNODE] ++;
	#endif
}

commandlist::commandlist()
{
	infile = NULL;
	outfile = NULL;
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

// Set command and function
void command::set_command(command_action ca)
{
	action = ca;
	function = functions.action[ca];
}

// Clear command list and reinitialise
void commandlist::clear()
{
	commands.clear();
	branchstack.clear();
	branchcmdstack.clear();
	push_branch(&commands, CA_ROOTNODE, NULL);
}

// Print data variables
void command::print_args()
{
	dbg_begin(DM_CALLS,"command::print_args");
	int i;
	for (int i=0; i<MAXDATAVARS; i++)
	{
		printf("%2i %20li",i,args[i]);
		if (args[i] == NULL) printf ("None.\n");
		else
		{
			printf("%12s [%10s]",args[i]->get_name(), text_from_VT(args[i]->get_type()));
			if (args[i]->get_type() < VT_ATOM) printf("%20s\n",args[i]->get_as_char());
			else printf("%li\n",args[i]->get_as_pointer(VT_ATOM));
		}
	}
	dbg_end(DM_CALLS,"command::print_args");
}


// Return arguments as vec3<double>
vec3<double> command::arg3d(int i)
{
	dbg_begin(DM_CALLS,"command::get_vector3d");
        static vec3<double> result;
        if (i > (MAXDATAVARS-3)) printf("command::get_vector3d - Starting point too close to MAXDATAVARS.\n");
        result.set(args[i]->get_as_double(),args[i+1]->get_as_double(),args[i+2]->get_as_double());
	dbg_end(DM_CALLS,"command::get_vector3d");
        return result;
}

// Return arguments as vec3<float>
vec3<float> command::arg3f(int i)
{
	dbg_begin(DM_CALLS,"command::get_vector3f");
        static vec3<float> result;
        if (i > (MAXDATAVARS-3)) printf("command::get_vector3f - Starting point too close to MAXDATAVARS.\n");
        result.set(args[i]->get_as_float(),args[i+1]->get_as_float(),args[i+2]->get_as_float());
	dbg_end(DM_CALLS,"command::get_vector3f");
        return result;
}

// Return arguments as vec3<int>
vec3<int> command::arg3i(int i)
{
	dbg_begin(DM_CALLS,"command::get_vector3i");
	static vec3<int> result;
	if (i > (MAXDATAVARS-3)) printf("command::get_vector3i - Starting point too close to MAXDATAVARS.\n");
        result.set(args[i]->get_as_int(),args[i+1]->get_as_int(),args[i+2]->get_as_int());
	dbg_end(DM_CALLS,"command::get_vector3i");
	return result;
}

// Create branch
list<command> *command::create_branch()
{
	dbg_begin(DM_CALLS,"command::create_branch");
	if (branch != NULL) printf("command::create_branch <<<< Already has a branch >>>>\n");
	branch = new list< command >;
	dbg_end(DM_CALLS,"command::create_branch");
	return branch;
}

// Create branch
void command::create_format(const char *s, variable_list &vars)
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

// Set if condition test
bool command::set_iftest(const char *s)
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
bool command::if_evaluate()
{
	dbg_begin(DM_CALLS,"command::if_evaluate");
	// Do all as comparisons as floats, except for equalities
	bool result;
	static dnchar value1, value2;
	static double d1, d2;
	//print_argss();
	if ((iftest == IF_EQUAL) || (iftest == IF_NEQUAL))
	{
		// Grab current variable values into the value1/value2 character arrays (if var != NULL)
		value1 = args[0]->get_as_char();
		value2 = args[2]->get_as_char();
	}
	else
	{
		d1 = args[0]->get_as_double();
		d2 = args[2]->get_as_double();
	}
	msg(DM_VERBOSE,"IF TEST = var1(%s)=[%s] (%s) var2(%s)=[%s]\n", args[0]->get_name(), args[0]->get_as_char(), text_from_IC(iftest), args[2]->get_name(), args[2]->get_as_char());
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
bool command::add_variables(const char *cmd, const char *v, variable_list &vars)
{
	dbg_begin(DM_CALLS,"command::add_variables");
	bool required = TRUE;
	int n, argcount, varcount;
	dnchar arg;
	variable_type vt;
	printf("DOING VARIABLES (%s) FOR COMMAND '%s'\n",v,cmd);
	// Are there arguments in the parser that we shouldn't have been given.
	if ((parser.get_nargs() - 1) > strlen(v))
	{
		printf("Too many arguments (%i) given to command '%s' (which expects %i at most).\n", (parser.get_nargs()-1), cmd, strlen(v));
		dbg_end(DM_CALLS,"command::add_variables");
		return FALSE;
	}
	argcount = 0;
	varcount = -1;
	for (n = 0; v[n] != '\0'; n++)
	{
		// Check for lowercase letter (optional argument)
		required = (v[n] > 90 ? FALSE : TRUE);
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
				dbg_end(DM_CALLS,"command::add_variables");
				return FALSE;
			}
			else break;	// No more arguments, so may as well quit.
		}
		arg = parser.argc(argcount);
		// Check for specifiers that don't require variables to be created...
		switch (v[n])
		{
			// Formats
			case ('f'):
				create_format(arg.get(), vars);
				continue;
			// Discard
			case ('x'):
				continue;
			// String as-is
			case ('s'):
				args[varcount] = vars.add();
				args[varcount]->set(arg.get());
				continue;
			case ('='):
				if (strcmp(arg.get(),"=") != 0)
				{
					printf("Expected '=' after argument %i for command '%s'.\n", argcount, cmd);
					dbg_end(DM_CALLS,"command::add_variables");
					return FALSE;
				}
				else continue;
		}
		// Now for variable specifiers.
		// First, check that the variable has been declared
		args[varcount] = vars.find(arg.get());
		if (args[varcount] == NULL)
		{
			printf("Variable '%s' has not been declared.\n", arg.get());
			dbg_end(DM_CALLS,"command::add_variables");
			return FALSE;
		}
		// We are not concerned about the variable's type here - this will just generate errors (or warnings) later on as commands are run
	}
	dbg_end(DM_CALLS,"command::add_variables");
	return TRUE;
}

// Push branch on to stack
void commandlist::push_branch(list<command> *branch, command_action ca, command *basenode)
{
	branchstack.add(branch);
	command *cn = branchcmdstack.add();
	cn->set_command(ca);
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
	else return branchcmdstack.last()->get_command();
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
command* commandlist::add_topbranch_command(command_action ca, command *nodeptr)
{
	if (branchstack.size() == 0)
	{
		printf("commandlist::add_topbranch_command <<<< No branches in branch list! >>>>\n");
		return NULL;
	}
	command *cn = branchstack.last()->item->add();
	cn->set_command(ca);
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
		/*
		// Variable Declaration
		// All arguments to commands are names of variables to create
		*/
		case (CA_CHAR):
		case (CA_INT):
		case (CA_DOUBLE):
		case (CA_ATOM):
		case (CA_BOND):
		case (CA_PATTERN):
		case (CA_MODEL):
		case (CA_PATBOUND):
			for (n=1; n<parser.get_nargs(); n++)
			{
				// Check for existing variable with same name
				v = variables.find(parser.argc(n));
				if (v != NULL)
				{
					printf("Variable '%s': redeclared as type [%s], was [%s].\n", parser.argc(n), text_from_VT((variable_type) (ca - CA_CHAR)),  text_from_VT(v->get_type()));
					result = FALSE;
				}
				else
				{
					v = variables.get(parser.argc(n));
					v->set_type((variable_type) (ca - CA_CHAR));
				}
			}
		// 'If' statement (if 'x condition y')
		case (CA_IF):
			fn = add_topbranch_command(CA_IF, NULL);
			push_branch(fn->create_branch(), CA_IF, fn);
			varresult = fn->add_variables(text_from_CA(ca), vars_from_CA(ca), variables);
			if (!fn->set_iftest(parser.argc(2))) result = FALSE;
			break;
		// 'Else If' statement (acts as CA_END to previous 'if' or 'elseif' branch.
		case (CA_ELSEIF):
			// If the previous branch was an 'if' or 'elseif', set the *ptr of that node to this node
			branchca = get_topbranch_type();
			if ((branchca != CA_IF) && (branchca != CA_ELSEIF))
			{
				msg(DM_NONE,"Error: 'elseif' used without previous if/elseif.\n");
				result = FALSE;
				break;
			}
			// Add GOTONONIF command to topmost branch to end the if sequence
			fn = add_topbranch_command(CA_GOTONONIF, get_topbranch_basenode());
			// Pop topmost (previous IF/ELSEIF) branch
			pop_branch();
			// Add new command node to new topmost branch and get variables
			fn = add_topbranch_command(CA_ELSEIF, NULL);
			//printf("New node is %li, command = %s\n",fn,CA_keywords[cmd]);
			// Add new branch to this node for new if test to run
			push_branch(fn->create_branch(), CA_ELSEIF, fn);
			varresult = fn->add_variables(text_from_CA(ca), vars_from_CA(ca), variables);
			if (!fn->set_iftest(parser.argc(2))) result = FALSE;
			break;
		// 'Else' statement (acts as CA_END to previous 'if' or 'elseif' branch.
		case (CA_ELSE):
			// If the previous branch was an 'if' or 'elseif', set the *ptr of that node to this node
			branchca = get_topbranch_type();
			if ((branchca != CA_IF) && (branchca != CA_ELSEIF))
			{
				msg(DM_NONE,"Error: 'else' used without previous if/elseif.\n");
				result = FALSE;
				break;
			}
			// Add GOTONONIF command to current topbranch to terminate that branch
			fn = add_topbranch_command(CA_GOTONONIF, get_topbranch_basenode());
			// Pop previous branch from stack and add new command to new topmost branch
			pop_branch();
			// Add new node to new top branch
			fn = add_topbranch_command(CA_ELSE, NULL);
			//printf("New node is %li, command = %s\n",fn,CA_keywords[cmd]);
			// Add new branch to this node for new if test to run
			push_branch(fn->create_branch(), CA_ELSE, fn);
			break;
		// Loop for n iterations (or until file ends) or over items
		case (CA_FOR):
			fn = add_topbranch_command(ca, NULL);
			push_branch(fn->create_branch(), ca, fn);
			varresult = fn->add_variables(text_from_CA(ca), vars_from_CA(ca), variables);
			break;
		// End the topmost branch in the stack
		case (CA_END):
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
				case (CA_FOR):
					add_topbranch_command(CA_GOTO, get_topbranch_basenode());
					break;
				// For IFs, jump to node containing IF/ELSEIF/ELSE branch (the branch owner)
				case (CA_IF):
				case (CA_ELSEIF):
				case (CA_ELSE):
					add_topbranch_command(CA_GOTONONIF, get_topbranch_basenode());
					break;
				case (CA_ROOTNODE):
					add_topbranch_command(CA_TERMINATE, NULL);
					break;
				default:
					printf("commandlist::add_basic <<<< No END action defined for command '%s' >>>>\n",text_from_CA(branchca));
					result = FALSE;
					break;
			}
			// Remove the topmost branch from the stack
			pop_branch();
			break;
		// All other commands do not alter the flow of the commandlist...
		default:
			fn = add_topbranch_command(ca, NULL);
			varresult = fn->add_variables(text_from_CA(ca), vars_from_CA(ca), variables);
			break;
	}
	// Store function pointer for command
	if (result) 
	// Check variable assignment result
	if (!varresult)
	{
		msg(DM_NONE,"Error: Command '%s' was not given the correct variables.\n", text_from_CA(ca));
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
	command_action ca;
	int success;
	bool result = TRUE;
	// Assume that the main parser object contains the data we require.
	ca = CA_from_text(parser.argc(0));
	if (ca != CA_NITEMS)
	{
		// If add_command() returns FALSE then we encountered an error
		if (!add_command(ca))
		{
			msg(DM_NONE,"Error adding command '%s'.\n", parser.argc(0));
			msg(DM_NONE,  "Command usage is '%s'.\n", syntax_from_CA(ca));
			result = FALSE;
		}
	}
	else
	{
		msg(DM_NONE,"Unrecognised command '%s' in script.\n", parser.argc(0));
		result = FALSE;
	}
	dbg_end(DM_CALLS,"commandlist::cache_command");
	return result;
}

// Set input file (pointer)
bool commandlist::set_infile(const char *sourcefile)
{
	dbg_begin(DM_CALLS,"filter::set_infile");
        if (infile != NULL) printf("commandlist::set_infile <<<< Inputfile already set >>>>\n");
        infile = new ifstream(sourcefile,ios::in);
        dbg_end(DM_CALLS,"filter::set_infile");
        if (!infile->good()) return FALSE;
        else return TRUE;
}

// Set output file
bool commandlist::set_outfile(const char *destfile)
{
	dbg_begin(DM_CALLS,"commandlist::set_output");
	outfile = new ofstream(destfile,ios::out);
	dbg_end(DM_CALLS,"commandlist::set_output");
	if (!outfile->good()) return FALSE;
	else return TRUE;
}

// Close files
void commandlist::close_files()
{
	dbg_begin(DM_CALLS,"commandlist::close_files");
	if (infile != NULL)
	{
		infile->close();
		delete infile;
	}
	if (outfile != NULL)
	{
		outfile->close();
		delete outfile;
	}
	infile = NULL;
	outfile = NULL;
	dbg_end(DM_CALLS,"commandlist::close_files");
}

// Execute command
int command::execute(command *&c)
{
	return (functions.*function)(c, master.current);
}

// Execute commands in command list
bool commandlist::execute()
{
	// Get first command in list
	command *c = commands.first();
	int result;
	while (c != NULL)
	{
		// Run command and get return value
		result = c->execute(c);
	}
}
