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
#include "classes/variables.h"
#include "base/constants.h"

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
	// Basic command that this node performs
	command_action action;
	// Pointer to action function
	commandfunc function;

	public:
	// Parent list
	commandlist *parent;
	// Set command
	void set_command(command_action ca) { action = ca; }
	// Get command
	command_action get_command() { return action; }
	// Execute command
	int execute();

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
	// Return argument as character
	const char *argc(int argno) { return (args[argno] == NULL ?  "NULL" : args[argno]->get_as_char()); }
	// Return argument as integer
	int argi(int argno) { return (args[argno] == NULL ?  0 : args[argno]->get_as_int()); }
	// Return argument as double
	double argd(int argno) { return (args[argno] == NULL ? 0.0 : args[argno]->get_as_double()); }
	// Return argument as bool
	bool argb(int argno) { return (args[argno] == NULL ? -1 : args[argno]->get_as_bool()); }
	// Return arguments as vec3<double>
	vec3<double> get_vector3d(int);
	// Return arguments as vec3<float>
	vec3<float> get_vector3f(int);
	// Return arguments as vec3<int>
	vec3<int> get_vector3i(int);
	// Returns whether argument was provided
	bool was_given(int argno) { return (args[argno] == NULL ? FALSE : TRUE); }
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
	// Static class containing pointers to command functions
	static command_functions functions;
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
	bool execute(const char *infile, const char *outfile);
	// Check structure of command list
	bool validate();

	/*
	// Variables
	*/
	public:
	// Associative variable list
	variable_list variables;

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

	public:
	// Open input file
	bool set_input(const char *filename);
	// Set file pointer
	bool set_input(ifstream *file);
	// Set output file
	bool set_output(const char *filename);
	// Close input/output file(s)
	void close_files();
};

#endif
