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
#include "templates/reflist.h"
#include "templates/vector3.h"
#include "command/commands.h"
#include "command/functions.h"
#include "classes/variables.h"
#include "base/constants.h"
#include "parse/parse.h"

// If Conditions
enum if_condition { IF_EQUAL=1, IF_LESS=2, IF_LEQUAL=3, IF_GREATER=4, IF_GEQUAL=5, IF_NEQUAL=6, IF_NITEMS };
const char *text_from_IC(if_condition);

// Forward declarations
class commandlist;
class format;

// Command node
class command
{
	public:
	// Constructor / Destructor
	command();
	~command();
	// List pointers
	command *prev, *next;

	/*
	// Command
	*/
	private:
	// Static class containing pointers to command functions
	static command_functions functions;
	// Command that this node performs
	command_action action;
	// Pointer to action function
	commandfunc function;

	public:
	// Parent list
	commandlist *parent;
	// Set command
	void set_command(command_action ca);
	// Get command
	command_action get_command() { return action; }
	// Execute command
	int execute(command *&c, model *alttarget);

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
	// Whether the loop is currently executing
	bool loopactive;
	// NUmber of iterations performed by loop
	int loopiterations;

	public:
	// Set status of loop
	void set_loopactive(bool b) { loopactive = b; }
	// Get status of loop
	bool get_loopactive() { return loopactive; }
	// Set iteration count
	void set_loopiterations(int n) { loopiterations = n; }
	// Get iteration count
	int get_loopiterations() { return loopiterations; }
	// Increase interation count
	void increase_iterations() { loopiterations ++; }

	/*
	// Command Branch
	*/
	private:
	// Lists for branched commands (if any)
	list<command> *branch;
	// Pointer for use by flow control nodes
	command *ptr;

	public:
	// Create branch for the node
	list<command> *create_branch();
	// Returns branch list structure
	list<command> *get_branch() { return branch; }
	// Returns first item in branch 
	command *get_branch_commands() { return (branch != NULL ? branch->first() : NULL); }
	// Set format_node pointer variable
	void set_pointer(command *f) { ptr = f; }
	// Return format_node pointer variable
	command *get_pointer() { return ptr; }

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
	private:
	// Data variables
	variable *args[MAXDATAVARS];

	public:
	// Set variables from parser arguments
	bool add_variables(const char*, const char*, variable_list&);
	// Return variable argument
	variable *arg(int argno) { return args[argno]; }
	// Return argument as character
	const char *argc(int argno) { return (args[argno] == NULL ?  "NULL" : args[argno]->get_as_char()); }
	// Return argument as integer
	int argi(int argno) { return (args[argno] == NULL ?  0 : args[argno]->get_as_int()); }
	// Return argument as double
	double argd(int argno) { return (args[argno] == NULL ? 0.0 : args[argno]->get_as_double()); }
	// Return argument as bool
	bool argb(int argno) { return (args[argno] == NULL ? -1 : args[argno]->get_as_bool()); }
	// Return arguments as vec3<double>
	vec3<double> arg3d(int);
	// Return arguments as vec3<float>
	vec3<float> arg3f(int);
	// Return arguments as vec3<int>
	vec3<int> arg3i(int);
	// Return argument as atom pointer
	atom *arga(int argno) { return (args[argno] == NULL ? NULL : (atom*) args[argno]->get_as_pointer(VT_ATOM)); }
	// Return argument as pattern pointer
	pattern *argp(int argno) { return (args[argno] == NULL ? NULL : (pattern*) args[argno]->get_as_pointer(VT_PATTERN)); }
	// Returns whether argument 'n' was provided
	bool has_arg(int argno) { return (args[argno] == NULL ? FALSE : TRUE); }
	// Return variable type of argument
	variable_type argt(int argno) { return (args[argno] == NULL ? VT_NITEMS : args[argno]->get_type()); }
	// Print data variables
	void print_args();
};

// Command List Structore
class commandlist
{
	public:
	// Constructor / Destructor
	commandlist();
	~commandlist();
	// List pointers
	commandlist *prev, *next;

	/*
	// Command List
	*/
	private:
	// List of commands
	list<command> commands;
	// List of pointers to stacked branches
	reflist< list<command> > branchstack;
	// Basic command types of stacked branches
	list<command> branchcmdstack;
	// Add specified branch to stack
	void push_branch(list<command>*, command_action, command*);
	// Pop topmost branch from stack
	void pop_branch();
	// Add command to topmost branch
	command* add_topbranch_command(command_action, command*);

	public:
	// Return size of branch stack
	int get_branchstack_size() { return branchstack.size(); }
	// Return type of topmost branch on stack
	command_action get_topbranch_type();
	// Return basenode pointer of topmost branch on stack
	command* get_topbranch_basenode();
	// Add action to lst node
	bool add_command(command_action);
	// Clear and reinitialise command list
	void clear();
	// Read semicolon-separated commands from string
	bool cache_line(const char *s);
	// Cache command arguments in main parser
	bool cache_command();
	// Load commands from file
	bool load(const char *filename);
	// Execute command list
	bool execute(model *alttarget = NULL, ifstream *altfile = NULL);
	// Check structure of command list
	bool validate();

	/*
	// Variables
	*/
	public:
	// Associative variable list
	variable_list variables;

	public:
	// Set model variables
	void set_model_variables(model*);
	// Set cell variables
	void set_cell_variables(unitcell*);
	// Set atom variables
	void set_atom_variables(const char*, atom*);
	void set_atom_variables(const char*, int);
	// Set pattern variables
	void set_pattern_variables(const char*, pattern*);
	// Set pattern (ff) term variables
	void set_patbound_variables(const char*, patbound*);

	/*
	// Local Variables
	*/
	public:
	// Pen orientation matrix
	mat3<double> penorient;
	// Pen position
	vec3<double> penpos;

	/*
	// Files
	*/
	private:
	// Input file stream
	ifstream *infile;
	// Output file stream
	ofstream *outfile;
	// Parser read options for this commandlist
	int readopts;
	// Close files
	void close_files();

	public:
	// Set input stream
	bool set_infile(const char *infile);
	// Get input stream
	ifstream *get_infile() { return infile; }
	// Set output stream
	bool set_outfile(const char *outfile);
	// Get output stream
	ofstream *get_outfile() { return outfile; }
	// Add read option
	void add_readoption(parse_option po) { if (!(readopts&po)) readopts += po; }
	// Remove read option
	void remove_readoption(parse_option po) { if (readopts&po) readopts -= po; }
	// Return read options
	int get_readoptions() { return readopts; }
};

#endif
