/*
	*** Command list functions
	*** src/command/commandlist.cpp
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
#include "command/commands.h"
#include "parse/format.h"
#include "base/master.h"
#include "classes/forcefield.h"
#include "base/elements.h"
#include "classes/pattern.h"
#include "model/model.h"

// If Conditions
const char *IC_strings[6] = { "eq", "l", "le", "g", "ge", "neq" };
const char *text_from_IC(IfTest i)
	{ return IC_strings[i-1]; }

// Constructors
Command::Command()
{
	// Private variables
	for (int i=0; i<MAXDATAVARS; i++) args_[i] = NULL;
	action_ = CA_ROOTNODE;
	parent_ = NULL;
	function_ = NULL;
	ptr_ = NULL;
	branch_ = NULL;
	format_ = NULL;
	loopActive_ = FALSE;
	nArgs_ = 0;
	// Public variables
	next = NULL;
	prev = NULL;
}

CommandList::CommandList()
{
	// Private variables
	inputFile_ = NULL;
	outputFile_ = NULL;
	readOptions_ = 0;
	pushBranch(&commands_, CA_ROOTNODE, NULL);
	// Public variables
	next = NULL;
	prev = NULL;
}

// Destructors
Command::~Command()
{
	if (branch_ != NULL) delete branch_;
	if (format_ != NULL) delete format_;
}

/*
// Command
*/

// Set parent CommandList
void Command::setParent(CommandList *cl)
{
	parent_ = cl;
}

// Get parent CommandList
CommandList *Command::parent()
{
	return parent_;
}

// Get command
CommandAction Command::command()
{
	return action_;
}

// Returns the formatter
Format *Command::format()
{
	return format_;
}

// Set status of loop
void Command::setLoopActive(bool b)
{
	loopActive_ = b;
}

// Get status of loop
bool Command::isLoopActive()
{
	return loopActive_;
}

// Set iteration count
void Command::setLoopIterations(int n)
{
	loopIterations_ = n;
}

// Get iteration count
int Command::loopIterations()
{
	return loopIterations_;
}

// Increase interation count
void Command::increaseIterations()
{
	loopIterations_ ++;
}

// Returns branch list structure
List<Command> *Command::branch()
{
	return branch_;
}

// Returns first item in branch 
Command *Command::branchCommands()
{
	return (branch_ != NULL ? branch_->first() : NULL);
}

// Set FormatNode pointer variable
void Command::setPointer(Command *f)
{
	ptr_ = f;
}

// Return FormatNode pointer variable
Command *Command::pointer()
{
	return ptr_;
}

// Return variable argument
Variable *Command::arg(int argno)
{
	return args_[argno];
}

// Return argument as character
const char *Command::argc(int argno)
{
	return (args_[argno] == NULL ?  "NULL" : args_[argno]->asCharacter());
}

// Return argument as integer
int Command::argi(int argno)
{
	return (args_[argno] == NULL ?  0 : args_[argno]->asInteger());
}

// Return argument as double
double Command::argd(int argno)
{
	return (args_[argno] == NULL ? 0.0 : args_[argno]->asDouble());
}

// Return argument as float
double Command::argf(int argno)
{
	return (float) argd(argno);
}

// Return argument as bool
bool Command::argb(int argno)
{
	return (args_[argno] == NULL ? -1 : args_[argno]->asBool());
}

// Return argument as atom pointer
Atom *Command::arga(int argno)
{
	return (args_[argno] == NULL ? NULL : (Atom*) args_[argno]->asPointer());
}

// Return argument as pattern pointer
Pattern *Command::argp(int argno)
{
	return (args_[argno] == NULL ? NULL : (Pattern*) args_[argno]->asPointer());
}

// Return argument as PatternBound pointer
PatternBound *Command::argpb(int argno)
{
	return (args_[argno] == NULL ? NULL : (PatternBound*) args_[argno]->asPointer());
}

// Return argument as ForcefieldAtom pointer
ForcefieldAtom *Command::argffa(int argno)
{
	return (args_[argno] == NULL ? NULL : (ForcefieldAtom*) args_[argno]->asPointer());
}

// Returns whether argument 'n' was provided
bool Command::hasArg(int argno)
{
	return (args_[argno] == NULL ? FALSE : TRUE);
}

// Return variable type of argument
Variable::VariableType Command::argt(int argno)
{
	return (args_[argno] == NULL ? Variable::nVariableTypes : args_[argno]->type());
}

// Set command and function
void Command::setCommand(CommandAction ca)
{
	action_ = ca;
	function_ = CA_data[ca].function;
}

// Clear command list and reinitialise
void CommandList::clear()
{
	commands_.clear();
	branchStack_.clear();
	branchCommandStack_.clear();
	pushBranch(&commands_, CA_ROOTNODE, NULL);
}

// Print data variables
void Command::printArgs()
{
	dbgBegin(Debug::Calls,"Command::printArgs");
	int i;
	for (int i=0; i<MAXDATAVARS; i++)
	{
		printf("%2i %20li",i,args_[i]);
		if (args_[i] == NULL) printf ("None.\n");
		else
		{
			printf("%12s [%10s]",args_[i]->name(), Variable::variableType(args_[i]->type()));
			if (args_[i]->type() < Variable::AtomVariable) printf("%20s\n",args_[i]->asCharacter());
			else printf("%li\n",args_[i]->asPointer());
		}
	}
	dbgEnd(Debug::Calls,"Command::printArgs");
}

// Return arguments as Vec3<double>
Vec3<double> Command::arg3d(int i)
{
	dbgBegin(Debug::Calls,"Command::arg3d");
        static Vec3<double> result;
        if (i > (MAXDATAVARS-3)) printf("Command::get_vector3d - Starting point too close to MAXDATAVARS.\n");
        result.set(args_[i]->asDouble(),args_[i+1]->asDouble(),args_[i+2]->asDouble());
	dbgEnd(Debug::Calls,"Command::arg3d");
        return result;
}

// Return arguments as Vec3<float>
Vec3<float> Command::arg3f(int i)
{
	dbgBegin(Debug::Calls,"Command::arg3f");
        static Vec3<float> result;
        if (i > (MAXDATAVARS-3)) printf("Command::get_vector3f - Starting point too close to MAXDATAVARS.\n");
        result.set(args_[i]->asFloat(),args_[i+1]->asFloat(),args_[i+2]->asFloat());
	dbgEnd(Debug::Calls,"Command::arg3f");
        return result;
}

// Return arguments as Vec3<int>
Vec3<int> Command::arg3i(int i)
{
	dbgBegin(Debug::Calls,"Command::arg3i");
	static Vec3<int> result;
	if (i > (MAXDATAVARS-3)) printf("Command::get_vector3i - Starting point too close to MAXDATAVARS.\n");
        result.set(args_[i]->asInteger(),args_[i+1]->asInteger(),args_[i+2]->asInteger());
	dbgEnd(Debug::Calls,"Command::arg3i");
	return result;
}

// Create branch
List<Command> *Command::createBranch()
{
	dbgBegin(Debug::Calls,"Command::createBranch");
	if (branch_ != NULL) printf("Command::createBranch <<<< Already has a branch >>>>\n");
	branch_ = new List< Command >;
	dbgEnd(Debug::Calls,"Command::createBranch");
	return branch_;
}

// Create branch
bool Command::createFormat(const char *s, VariableList &vars, bool delimited)
{
	dbgBegin(Debug::Calls,"Command::create_format");
	bool result = FALSE;
	if (format_ != NULL) printf("Command::createBranch <<<< Already has a format >>>>\n");
	else
	{
		format_ = new Format;
		result = format_->create(s, vars, delimited);
	}
	dbgEnd(Debug::Calls,"Command::create_format");
	return result;
}

// Set if condition test
bool Command::setIfTest(const char *s)
{
	dbgBegin(Debug::Calls,"Command::setIfTest");
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
	else ifTest_ = (IfTest) m;
	dbgEnd(Debug::Calls,"Command::setIfTest");
	return result;
}

// Evaluate condition
bool Command::ifEvaluate()
{
	dbgBegin(Debug::Calls,"Command::ifEvaluate");
	// Do all as comparisons as floats, except for equalities
	bool result;
	static Dnchar value1, value2;
	static double d1, d2;
	//print_argss();
	if ((ifTest_ == IF_EQUAL) || (ifTest_ == IF_NEQUAL))
	{
		// Grab current variable values into the value1/value2 character arrays (if var != NULL)
		value1 = args_[0]->asCharacter();
		value2 = args_[2]->asCharacter();
	}
	else
	{
		d1 = args_[0]->asDouble();
		d2 = args_[2]->asDouble();
	}
	msg(Debug::Verbose,"IF TEST = var1(%s)=[%s] (%s) var2(%s)=[%s]\n", args_[0]->name(), args_[0]->asCharacter(), text_from_IC(ifTest_), args_[2]->name(), args_[2]->asCharacter());
	// Do comparison
	switch (ifTest_)
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
	dbgEnd(Debug::Calls,"Command::ifEvaluate");
	return result;
}

// Add variables to command
bool Command::addVariables(const char *cmd, const char *v, VariableList &vars)
{
	dbgBegin(Debug::Calls,"Command::addVariables");
	bool required = TRUE;
	int n, m, argcount, varcount;
	Variable *b;
	static char arg[512];
	char *c;
	Variable::VariableType vt;
	//printf("DOING VARIABLES (%s) FOR COMMAND '%s'\n",v,cmd);
	// Are there arguments in the parser that we shouldn't have been given?
	// We don't care about too many variables being given if we want the whole line.
	if (((parser.nArgs() - 1) > strlen(v)) && (v[0] != 'L'))
	{
		printf("Too many arguments (%i) given to command '%s' (which expects %li at most).\n", (parser.nArgs()-1), cmd, strlen(v));
		dbgEnd(Debug::Calls,"Command::addVariables");
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
		//if ((parser.is_blank(argcount)) || (argcount >= parser.nArgs()))
		if (argcount >= parser.nArgs())
		{
			if (required)
			{
				msg(Debug::None,"Command '%s' requires argument %i\n", cmd, argcount);
				dbgEnd(Debug::Calls,"Command::addVariables");
				return FALSE;
			}
			else break;	// No more arguments, so may as well quit.
		}
		strcpy(arg,parser.argc(argcount));
		// Go through possible specifiers
		switch (v[n])
		{
			// Formats (delimited)
			case ('f'):
			case ('F'):
				if (!createFormat(arg, vars, TRUE)) return FALSE;
				break;
			// Formats (exact)
			case ('g'):
			case ('G'):
				if (!createFormat(arg, vars, FALSE)) return FALSE;
				break;
			// Expressions
			case ('e'):
			case ('E'):
				args_[varcount] = vars.addConstant(arg);
				break;
			// Discard
			case ('x'):
			case ('X'):
				break;
			// String as-is
			case ('s'):
			case ('S'):
				args_[varcount] = vars.addConstant(arg);
				break;
			// Equals
			case ('='):
				if (strcmp(arg,"=") != 0)
				{
					printf("Expected '=' after argument %i for command '%s'.\n", argcount, cmd);
					dbgEnd(Debug::Calls,"Command::addVariables");
					return FALSE;
				}
				break;
			// Variable
			case ('v'):
			case ('V'):
				// If first character is '$', find variable pointer.
				// If '*' set to the dummy variable.
				// Otherwise, add constant variable.
				if (arg[0] == '$')
				{
					// See if it has been declared
					args_[varcount] = parent_->variables.get(&arg[1]);
					if (args_[varcount] == NULL)
					{
						printf("Variable '%s' has not been declared.\n", &arg[1]);
						return FALSE;
					}
				}
				else if (arg[0] == '*') args_[varcount] = parent_->variables.dummy();
				else args_[varcount] = parent_->variables.addConstant(arg);
				break;
			// Rest of line (reconstructed)
			case ('L'):
				arg[0] = '\0';
				for (m=argcount; m< parser.nArgs(); m++)
				{
					strcat(arg, parser.argc(m));
					strcat(arg, " ");
				}
				args_[varcount] = parent_->variables.addConstant("abcde");
				args_[varcount]->set(arg);
				break;
		}
	}
	nArgs_ = varcount + 1;
	dbgEnd(Debug::Calls,"Command::addVariables");
	return TRUE;
}

// Return number of arguments given to command
int Command::nArgs()
{
	return nArgs_;
}

/*
// CommandList
*/

// Set name of CommandList
void CommandList::setName(const char *s)
{
	name_ = s;
}

// Return name of CommandList
const char *CommandList::name()
{
	return name_.get();
}

// Return filename of CommandList
const char *CommandList::scriptFilename()
{
	return scriptFilename_.get();
}

// Return size of branch stack
int CommandList::nBranches()
{
	return branchStack_.nItems();
}

// Set parent filter
void CommandList::setFilter(Filter *f)
{
	parentFilter_ = f;
}

// Return parent filter
Filter *CommandList::filter()
{
	return parentFilter_;
}

// Push branch on to stack
void CommandList::pushBranch(List<Command> *branch, CommandAction ca, Command *basenode)
{
	branchStack_.add(branch);
	Command *cn = branchCommandStack_.add();
	cn->setCommand(ca);
	cn->setPointer(basenode);
	cn->setParent(this);
}

// Pop topmost branch on stack
void CommandList::popBranch()
{
	if (branchStack_.nItems() == 0)
	{
		printf("CommandList::popBranch <<<< No branches in branch list! >>>>\n");
		return;
	}
	branchStack_.remove(branchStack_.last());
	branchCommandStack_.remove(branchCommandStack_.last());
}

// Return basic command type of topmost branch
CommandAction CommandList::topBranchType()
{
	if (branchCommandStack_.nItems() == 0)
	{
		printf("CommandList::topBranchType <<<< No branches in branch list! >>>>\n");
		return CA_NITEMS;
	}
	else return branchCommandStack_.last()->command();
}

// Return base node of topmost branch
Command* CommandList::topBranchBaseNode()
{
	if (branchCommandStack_.nItems() == 0)
	{
		printf("CommandList::topBranchBaseNode <<<< No branches in branch list! >>>>\n");
		return NULL;
	}
	else return branchCommandStack_.last()->pointer();
}

// Add command to topmost branch
Command* CommandList::addTopBranchCommand(CommandAction ca, Command *nodeptr)
{
	if (branchStack_.nItems() == 0)
	{
		printf("CommandList::addTopBranchCommand <<<< No branches in branch list! >>>>\n");
		return NULL;
	}
	Command *cn = branchStack_.last()->item->add();
	cn->setCommand(ca);
	cn->setPointer(nodeptr);
	cn->setParent(this);
	return cn;
}

// Add basic command
bool CommandList::addCommand(CommandAction ca)
{
	dbgBegin(Debug::Calls,"CommandList::addCommand");
	// Pointers to command nodes
	Command *fn, *fn2, *fn3;
	CommandAction branchca;
	Variable::VariableType vt;
	int n;
	Variable *v;
	bool result = TRUE, varresult = TRUE;
	switch (ca)
	{
		/*
		// Variable Declaration
		// All arguments to commands are names of variables to create
		*/
		case (CA_CHAR):
		case (CA_INT):
		case (CA_FLOAT):
		case (CA_ATOM):
		case (CA_PATTERN):
		case (CA_MODEL):
		case (CA_BOND):
		case (CA_ANGLE):
		case (CA_TORSION):
		case (CA_ATOMTYPE):
			for (n=1; n<parser.nArgs(); n++)
			{
				// Check for existing variable with same name
				v = variables.get(parser.argc(n));
				if (v != NULL)
				{
					printf("Variable '%s': redeclared as type [%s] (was [%s]).\n", parser.argc(n), Variable::variableType((Variable::VariableType) ca),  Variable::variableType(v->type()));
					result = FALSE;
				}
				else
				{
					vt = (Variable::VariableType) ca;
					v = variables.addVariable(parser.argc(n), vt);
				}
			}
			break;
		// 'If' statement (if 'x condition y')
		case (CA_IF):
			fn = addTopBranchCommand(CA_IF, NULL);
			pushBranch(fn->createBranch(), CA_IF, fn);
			varresult = fn->addVariables(CA_data[ca].keyword, CA_data[ca].arguments, variables);
			if (!fn->setIfTest(parser.argc(2))) result = FALSE;
			break;
		// 'Else If' statement (acts as CA_END to previous 'if' or 'elseif' branch.
		case (CA_ELSEIF):
			// If the previous branch was an 'if' or 'elseif', set the *ptr of that node to this node
			branchca = topBranchType();
			if ((branchca != CA_IF) && (branchca != CA_ELSEIF))
			{
				msg(Debug::None,"Error: 'elseif' used without previous if/elseif.\n");
				result = FALSE;
				break;
			}
			// Add GOTONONIF command to topmost branch to end the if sequence
			fn = addTopBranchCommand(CA_GOTONONIF, topBranchBaseNode());
			// Pop topmost (previous IF/ELSEIF) branch
			popBranch();
			// Add new command node to new topmost branch and get variables
			fn = addTopBranchCommand(CA_ELSEIF, NULL);
			//printf("New node is %li, command = %s\n",fn,CA_keywords[cmd]);
			// Add new branch to this node for new if test to run
			pushBranch(fn->createBranch(), CA_ELSEIF, fn);
			varresult = fn->addVariables(CA_data[ca].keyword, CA_data[ca].arguments, variables);
			if (!fn->setIfTest(parser.argc(2))) result = FALSE;
			break;
		// 'Else' statement (acts as CA_END to previous 'if' or 'elseif' branch.
		case (CA_ELSE):
			// If the previous branch was an 'if' or 'elseif', set the *ptr of that node to this node
			branchca = topBranchType();
			if ((branchca != CA_IF) && (branchca != CA_ELSEIF))
			{
				msg(Debug::None,"Error: 'else' used without previous if/elseif.\n");
				result = FALSE;
				break;
			}
			// Add GOTONONIF command to current topbranch to terminate that branch
			fn = addTopBranchCommand(CA_GOTONONIF, topBranchBaseNode());
			// Pop previous branch from stack and add new command to new topmost branch
			popBranch();
			// Add new node to new top branch
			fn = addTopBranchCommand(CA_ELSE, NULL);
			//printf("New node is %li, command = %s\n",fn,CA_keywords[cmd]);
			// Add new branch to this node for new if test to run
			pushBranch(fn->createBranch(), CA_ELSE, fn);
			break;
		// Loop for n iterations (or until file ends) or over items
		case (CA_FOR):
			fn = addTopBranchCommand(ca, NULL);
			pushBranch(fn->createBranch(), ca, fn);
			varresult = fn->addVariables(CA_data[ca].keyword, CA_data[ca].arguments, variables);
			// Here, we must also add relevant variables to the list
			if (varresult)
			{
				switch (fn->argt(0))
				{
					case (Variable::AtomVariable):
						varresult = createAtomVariables(fn->arg(0)->name());
						break;
					case (Variable::PatternVariable):
						varresult = createPatternVariables(fn->arg(0)->name());
						break;
					case (Variable::BondVariable):
					case (Variable::AngleVariable):
					case (Variable::TorsionVariable):
						varresult = createPatternBoundVariables(fn->arg(0)->name());
						break;
					case (Variable::AtomtypeVariable):
						varresult = createAtomtypeVariables(fn->arg(0)->name());
						break;
				}
			}
			break;
		// End the topmost branch in the stack
		case (CA_END):
			if (branchStack_.nItems() == 0)
			{
				msg(Debug::None,"CommandList::addCommand - 'end' does not end a block.\n");
				result = FALSE;
				break;
			}
			// Check command stack to choose list ending pointer
			branchca = topBranchType();
			switch (branchca)
			{
				// For repeats, jump back to node at start of loop (the branch owner)
				case (CA_FOR):
					addTopBranchCommand(CA_GOTO, topBranchBaseNode());
					break;
				// For IFs, jump to node containing IF/ELSEIF/ELSE branch (the branch owner)
				case (CA_IF):
				case (CA_ELSEIF):
				case (CA_ELSE):
					addTopBranchCommand(CA_GOTONONIF, topBranchBaseNode());
					break;
				case (CA_ROOTNODE):
					addTopBranchCommand(CA_TERMINATE, NULL);
					break;
				default:
					printf("CommandList::add_basic <<<< No END action defined for command '%s' >>>>\n", CA_data[branchca].keyword);
					result = FALSE;
					break;
			}
			// Remove the topmost branch from the stack
			popBranch();
			break;
		// Unrecognised command
		case (CA_NITEMS):
			printf("Unrecognised command in CommandList::addCommand()\n");
			result = FALSE;
			break;
		// All other commands do not alter the flow of the CommandList...
		default:
			fn = addTopBranchCommand(ca, NULL);
			varresult = fn->addVariables(CA_data[ca].keyword, CA_data[ca].arguments, variables);
			break;
	}
	// Check variable assignment result
	if (!varresult)
	{
		msg(Debug::None,"Error: Command '%s' was not given the correct variables.\n", CA_data[ca].keyword);
		result = FALSE;
	}
	dbgEnd(Debug::Calls,"CommandList::addCommand");
	return result;
}

// Cache script commands from line containing semicolon-separated commands
bool CommandList::cacheLine(const char *s)
{
	dbgBegin(Debug::Calls,"CommandList::cacheLine");
	// Use a local parser to split up the semi-colon'd line into individual commands
	static Parser lines;
	lines.getLinesDelim(s);
	for (int n=0; n<lines.nArgs(); n++)
	{
		// Parse the argument in our local line_parser and call cache_command())
		parser.getArgsDelim(lines.argc(n), Parser::UseQuotes+Parser::SkipBlanks);
		if (!cacheCommand())
		{
			dbgEnd(Debug::Calls,"CommandList::cacheLine");
			return FALSE;
		}
	}
	dbgEnd(Debug::Calls,"CommandList::cacheLine");
	return TRUE;
}

// Cache command arguments in line_parser
bool CommandList::cacheCommand()
{
	dbgBegin(Debug::Calls,"CommandList::cacheCommand");
	CommandAction ca;
	int success;
	bool result = TRUE;
	// Assume that the main parser object contains the data we require.
	ca = CA_from_text(parser.argc(0));
	if (ca != CA_NITEMS)
	{
		// If addCommand() returns FALSE then we encountered an error
		if (!addCommand(ca))
		{
			msg(Debug::None,"Error adding command '%s'.\n", parser.argc(0));
			msg(Debug::None,"Command usage is: %s %s\n", CA_data[ca].keyword, CA_data[ca].argText);
			result = FALSE;
		}
	}
	else
	{
		msg(Debug::None,"Unrecognised command '%s'.\n", parser.argc(0));
		result = FALSE;
	}
	dbgEnd(Debug::Calls,"CommandList::cacheCommand");
	return result;
}

/*
// Files
*/

// Get input stream
ifstream *CommandList::inputFile() {
	return inputFile_;
}

// Get output stream
ofstream *CommandList::outputFile() {
	return outputFile_;
}

// Return filename associated to infile/outfile
const char *CommandList::filename() {
	return filename_.get();
}

// Add read option
void CommandList::addReadOption(Parser::ParseOption po) {
	if (!(readOptions_&po)) readOptions_ += po;
}

// Remove read option
void CommandList::removeReadOption(Parser::ParseOption po) {
	if (readOptions_&po) readOptions_ -= po;
}

// Return read options
int CommandList::readOptions() {
	return readOptions_;
}

// Load commands from file
bool CommandList::load(const char *filename)
{
	dbgBegin(Debug::Calls,"CommandList::load");
	scriptFilename_ = filename;
	ifstream cmdfile(filename,ios::in);
	Command *c;
	CommandAction ca;
	int success;
	clear();
	name_ = filename;
	// Read in commands
	while (!cmdfile.eof())
	{
		success = parser.getArgsDelim(&cmdfile,Parser::UseQuotes+Parser::SkipBlanks);
		if (success == 1)
		{
			msg(Debug::None,"CommandList::load - Error reading command file.\n");
			dbgEnd(Debug::Calls,"CommandList::load");
			return FALSE;
		}
		else if (success == -1) break;
		// See if we found a legitimate command
		ca = CA_from_text(parser.argc(0));
		if (ca != CA_NITEMS)
		{
			// Add the command to the list
			if (addCommand(ca)) continue;
			else
			{
				msg(Debug::None,"CommandList::load <<< Error adding command '%s' >>>>\n", parser.argc(0));
				dbgEnd(Debug::Calls,"CommandList::load");
				return FALSE;
			}
		}
		else
		{
			msg(Debug::None,"Unrecognised command '%s' in file.\n", parser.argc(0));
			dbgEnd(Debug::Calls,"CommandList::load");
			return FALSE;
		}
	}
	// Check the flowstack - it should be empty...
	int itemsleft = branchStack_.nItems();
	if (itemsleft != 1)
	{
		printf("CommandList::load <<<< %i block%s not been terminated >>>>\n", itemsleft, (itemsleft == 1 ? " has" : "s have"));
		dbgEnd(Debug::Calls,"CommandList::load");
		return FALSE;
	}
	dbgEnd(Debug::Calls,"CommandList::load");
	return TRUE;
}

// Set input file (pointer)
bool CommandList::setInputFile(const char *sourcefile)
{
	dbgBegin(Debug::Calls,"CommandList::setInFile");
	if (inputFile_ != NULL) printf("CommandList::setInFile <<<< Inputfile already set >>>>\n");
        inputFile_ = new ifstream(sourcefile,ios::in);
	filename_ = sourcefile;
	dbgEnd(Debug::Calls,"CommandList::setInFile");
	return (!inputFile_->good() ? FALSE : TRUE);
}

// Set output file
bool CommandList::setOutputFile(const char *destfile)
{
	dbgBegin(Debug::Calls,"CommandList::setOutputFile");
	outputFile_ = new ofstream(destfile,ios::out);
	filename_ = destfile;
	dbgEnd(Debug::Calls,"CommandList::setOutputFile");
	if (!outputFile_->good()) return FALSE;
	else return TRUE;
}

// Close files
void CommandList::closeFiles()
{
	dbgBegin(Debug::Calls,"CommandList::closeFiles");
	if (inputFile_ != NULL)
	{
		inputFile_->close();
		delete inputFile_;
	}
	if (outputFile_ != NULL)
	{
		outputFile_->close();
		delete outputFile_;
	}
	inputFile_ = NULL;
	outputFile_ = NULL;
	dbgEnd(Debug::Calls,"CommandList::closeFiles");
}

// Execute command
int Command::execute(Command *&c, Model *alttarget)
{
	static Bundle obj;
	// Grab master's pointer Bundle
	obj = master.current;
	// Set destination model to that provided if not NULL
	if (alttarget != NULL) obj.m = alttarget;
	return CALL_COMMAND(CA_data[action_],function_)(c, obj);
}

// Execute commands in command list
bool CommandList::execute(Model *alttarget, ifstream *sourcefile)
{
	dbgBegin(Debug::Calls,"CommandList::execute");
	// Set alternative input file if one was supplied
	if (sourcefile != NULL)
	{
		if ((inputFile_ != NULL) && (inputFile_ != sourcefile)) printf("Warning - supplied ifstream overrides file in CommandList.\n");
		inputFile_ = sourcefile;
	}
	static bool result;
	result = TRUE;
	// Get first command in list and execute
	Command *c = commands_.first();
	while (c != NULL)
	{
		// Run command and get return value
		msg(Debug::Parse,"Commandlist executing command '%s'...\n",CA_data[c->command()].keyword);
		switch (c->execute(c, alttarget))
		{
			// Command succeeded - get following command
			case (CR_SUCCESS):
				c = c->next;
				break;
			// Command succeeded - new command already set
			case (CR_SUCCESSNOMOVE):
				break;
			// Command failed - show message and quit
			case (CR_FAIL):
				printf("Command list failed at '%s'.\n", CA_data[c->command()].keyword);
				c = NULL;
				result = FALSE;
				break;
			// Command failed - show message and continue to next command
			case (CR_FAILCONTINUE):
				printf("Continuing past failed command '%s'...\n", CA_data[c->command()].keyword);
				c = c->next;
				break;
			// Exit with error
			case (CR_EXITWITHERROR):
				c = NULL;
				result = FALSE;
				break;
			// Exit - we're done
			case (CR_EXIT):
				c = NULL;
				break;
		}
	}
	dbgEnd(Debug::Calls,"CommandList::execute");
	return result;
}

// Set variables for model
void CommandList::setModelVariables(Model *m)
{
	dbgBegin(Debug::Calls,"CommandList::setModelVariables");
	if (m != NULL)
	{
		variables.set("title","",m->name());
		variables.set("natoms","",m->nAtoms());
	}
	dbgEnd(Debug::Calls,"CommandList::setModelVariables");
}

// Set variables for cell
void CommandList::setCellVariables(Cell *c)
{
	dbgBegin(Debug::Calls,"CommandList::setCellVariables");
	Mat3<double> mat;
	Vec3<double> vec;
	if (c != NULL)
	{
		variables.set("cell","type",lowerCase(Cell::cellType(c->type())));
		mat = c->axes();
		variables.set("cell","ax",mat.rows[0].x);
		variables.set("cell","ay",mat.rows[0].y);
		variables.set("cell","az",mat.rows[0].z);
		variables.set("cell","bx",mat.rows[1].x);
		variables.set("cell","by",mat.rows[1].y);
		variables.set("cell","bz",mat.rows[1].z);
		variables.set("cell","cx",mat.rows[2].x);
		variables.set("cell","cy",mat.rows[2].y);
		variables.set("cell","cz",mat.rows[2].z);
		vec = c->lengths();
		variables.set("cell","a",vec.x);
		variables.set("cell","b",vec.y);
		variables.set("cell","c",vec.z);
		vec = c->angles();
		variables.set("cell","alpha",vec.x);
		variables.set("cell","beta",vec.y);
		variables.set("cell","gamma",vec.z);
		vec = c->centre();
		variables.set("cell","centrex",vec.x);
		variables.set("cell","centrey",vec.y);
		variables.set("cell","centrez",vec.z);
	}
	else
	{
		variables.reset("cell.type","cell.ax","cell.ay","cell.az","cell.bx","cell.by","cell.bz","cell.cx","cell.cy","cell.cz","");
		variables.reset("cell.a","cell.b","cell.c","cell.alpha","cell.beta","cell.gamma","cell.ox","cell.oy","cell.oz","");
	}
	dbgEnd(Debug::Calls,"CommandList::setCellVariables");
}

// Create atom parameter variables
bool CommandList::createAtomVariables(const char *base)
{
	Variable *v;
	v = variables.createVariable(base,"symbol",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"mass",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"name",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"z",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"id",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fftype",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ffequiv",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"q",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"rx",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ry",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"rz",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fx",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fy",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"fz",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vx",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vy",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vz",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	return TRUE;
}

// Set variable values for atom
void CommandList::setAtomVariables(const char *varname, Atom *i)
{
	dbgBegin(Debug::Calls,"CommandList::setAtomVariables");
	Vec3<double> v;
	if (i != NULL)
	{
		// Element and ff type
		variables.set(varname,"symbol",elements.symbol(i));
		variables.set(varname,"mass",elements.atomicMass(i));
		variables.set(varname,"name",elements.name(i));
		variables.set(varname,"z",i->element());
		variables.set(varname,"id",i->id()+1);
		ForcefieldAtom *ffa = i->type();
		variables.set(varname,"fftype",(ffa == NULL ? elements.symbol(i) : ffa->name()));
		variables.set(varname,"ffequiv",(ffa == NULL ? elements.symbol(i) : ffa->equivalent()));
		v = i->r();
		variables.set(varname,"rx",v.x);
		variables.set(varname,"ry",v.y);
		variables.set(varname,"rz",v.z);
		v = i->f();
		variables.set(varname,"fx",v.x);
		variables.set(varname,"fy",v.y);
		variables.set(varname,"fz",v.z);
		v = i->v();
		variables.set(varname,"vx",v.x);
		variables.set(varname,"vy",v.y);
		variables.set(varname,"vz",v.z);
		variables.set(varname,"q",i->charge());
	}
	dbgEnd(Debug::Calls,"CommandList::setAtomVariables");
}

// Create pattern parameter variables
bool CommandList::createPatternVariables(const char *base)
{
	Variable *v;
	v = variables.createVariable(base,"name",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nmols",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nmolatoms",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"natoms",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nbonds",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"nangles",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ntorsions",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"ntypes",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	return TRUE;
}

// Set variables for pattern
void CommandList::setPatternVariables(const char *varname, Pattern *p)
{
	dbgBegin(Debug::Calls,"CommandList::setPatternVariables");
	if (p != NULL)
	{
		variables.set(varname,"name",p->name());
		variables.set(varname,"nmols",p->nMols());
		variables.set(varname,"nmolatoms",p->nAtoms());
		variables.set(varname,"natoms",p->totalAtoms());
		variables.set(varname,"nbonds",p->nBonds());
		variables.set(varname,"nangles",p->nAngles());
		variables.set(varname,"ntorsions",p->nTorsions());
	}
	dbgEnd(Debug::Calls,"CommandList::setPatternVariables");
}

// Create pattern bound term variables
bool CommandList::createPatternBoundVariables(const char *base)
{
	Variable *v;
	static char parm[24];
	int i;
	v = variables.createVariable(base,"form",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	strcpy(parm,"id_X");
	for (i = 0; i < MAXFFBOUNDTYPES; i++)
	{
		parm[3] = 105 + i;
		v = variables.createVariable(base,parm,Variable::IntegerVariable);
		if (v == NULL) return FALSE;
	}
	strcpy(parm,"type_X");
	for (i = 0; i < MAXFFBOUNDTYPES; i++)
	{
		parm[5] = 105 + i;
		v = variables.createVariable(base,parm,Variable::CharacterVariable);
		if (v == NULL) return FALSE;
	}
	strcpy(parm,"param_X");
	for (i = 0; i < MAXFFBOUNDTYPES; i++)
	{
		parm[6] = 97 + i;
		v = variables.createVariable(base,parm,Variable::FloatVariable);
		if (v == NULL) return FALSE;
	}
	v = variables.createVariable(base,"escale",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"vscale",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	return TRUE;
}

// Set variables for PatternBound
void CommandList::setPatternBoundVariables(const char *varname, PatternBound *pb)
{
	dbgBegin(Debug::Calls,"CommandList::setPatternBoundVariables");
	static ForcefieldParams ffp;
	static ForcefieldBound *ffb;
	static char parm[24];
	int i;
	if (pb != NULL)
	{
		// Grab ForcefieldBound pointer from pattern bound structure
		ffb = pb->data();
		// Set atom ids involved
		strcpy(parm,"id_X");
		for (i = 0; i < MAXFFBOUNDTYPES; i++)
		{
			parm[3] = 105 + i;
			variables.set(varname,parm,pb->atomId(i)+1);
		}
		// Set type names involved
		strcpy(parm,"type_X");
		for (i = 0; i < MAXFFBOUNDTYPES; i++)
		{
			parm[5] = 105 + i;
			variables.set(varname,parm,ffb->typeName(i));
		}
		// Grab ForcefieldParams data
		ffp = ffb->params();
		strcpy(parm,"param_X");
		for (int i = 0; i < MAXFFPARAMDATA; i++)
		{
			parm[6] = 97 + i;
			variables.set(varname,parm,ffp.data[i]);
		}
		// Set functional form and any additional variables
		switch (ffb->type())
		{
			case (ForcefieldBound::BondInteraction):
				variables.set(varname,"form", BondFunctions::BondFunctions[ffb->functionalForm().bondFunc].keyword);
				break;
			case (ForcefieldBound::AngleInteraction):
				variables.set(varname,"form", AngleFunctions::AngleFunctions[ffb->functionalForm().angleFunc].keyword);
				break;
			case (ForcefieldBound::TorsionInteraction):
				variables.set(varname,"form", TorsionFunctions::TorsionFunctions[ffb->functionalForm().torsionFunc].keyword);
				variables.set(varname,"escale",ffp.data[TF_ESCALE]);
				variables.set(varname,"vscale",ffp.data[TF_VSCALE]);
				break;
			default:	
				printf("CommandList::setPatternBoundVariables <<<< Functional form not defined >>>>\n");
				break;
		}
		
	}
	dbgEnd(Debug::Calls,"CommandList::setPatternBoundVariables");
}

// Create atomtype parameter variables
bool CommandList::createAtomtypeVariables(const char *base)
{
	static char parm[24];
	int i;
	Variable *v;
	strcpy(parm,"param_X");
	for (i = 0; i < MAXFFPARAMDATA; i++)
	{
		parm[6] = 97 + i;
		v = variables.createVariable(base,parm,Variable::FloatVariable);
		if (v == NULL) return FALSE;
	}
	v = variables.createVariable(base,"q",Variable::FloatVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"id",Variable::IntegerVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"name",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"equiv",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	v = variables.createVariable(base,"form",Variable::CharacterVariable);
	if (v == NULL) return FALSE;
	return TRUE;
}


// Set variables for pattern
void CommandList::setAtomtypeVariables(const char *varname, ForcefieldAtom *ffa)
{
	dbgBegin(Debug::Calls,"CommandList::setAtomtypeVariables");
	static char parm[24];
	int i;
	ForcefieldParams ffp;
	if (ffa != NULL)
	{
		ffp = ffa->params();
		strcpy(parm,"param_X");
		for (i = 0; i < MAXFFPARAMDATA; i++)
		{
			parm[6] = 97 + i;
			variables.set(varname,parm,ffp.data[i]);
		}
		variables.set(varname,"q",ffa->charge());
		variables.set(varname,"id",ffa->typeId());
		variables.set(varname,"name",ffa->name());
		variables.set(varname,"equiv",ffa->equivalent());
		variables.set(varname,"form",VdwFunctions::VdwFunctions[ffa->vdwForm()].keyword);
	}
	dbgEnd(Debug::Calls,"CommandList::setAtomtypeVariables");
}
