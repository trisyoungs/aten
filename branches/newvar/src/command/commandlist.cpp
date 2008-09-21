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
#include "command/format.h"
#include "variables/expression.h"
#include "main/aten.h"
#include "model/model.h"
#include "base/sysfunc.h"

// Constructor
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

// Clear command list and reinitialise
void CommandList::clear()
{
	commands_.clear();
	branchStack_.clear();
	branchCommandStack_.clear();
	pushBranch(&commands_, CA_ROOTNODE, NULL);
}

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

// Return basenode of topmost branch of specified type in current stack (if any)
Command *CommandList::topmostBranch(CommandAction ca)
{
	Command *result;
	for (result = branchCommandStack_.last(); result != NULL; result = result->prev)
		if (result->command() == ca) break;
	return result->pointer();
}

// Add basic command
bool CommandList::addCommand(CommandAction ca)
{
	msg.enter("CommandList::addCommand");
	Command *c, *topc;
	CommandAction branchca;
	Parser::ArgumentForm form;
	VTypes::DataType vt;
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
				// First, check that the argument is a plain variable
				form = parser.argumentForm(n, TRUE);
				if (form != Parser::VariableForm)
				{
					msg.print("Invalid variable name '%s' found in declaration.\n", parser.argc(n));
					result = FALSE;
					break;
				}
				// Check for existing variable with same name
				v = variables_.get(parser.argc(n));
				if (v != NULL)
				{
					msg.print("Error: Variable '%s': redeclared as type '%s' (from '%s').\n", parser.argc(n), VTypes::dataType((VTypes::DataType) ca),  VTypes::dataType(v->type()));
					result = FALSE;
				}
				else
				{
					vt = (VTypes::DataType) ca;
					v = variables_.addVariable(parser.argc(n), vt);
				}
			}
			break;
		// 'If' statement (if 'x condition y')
		case (CA_IF):
			c = addTopBranchCommand(CA_IF, NULL);
			pushBranch(c->createBranch(), CA_IF, c);
			varresult = c->setArguments(CA_data[ca].keyword, CA_data[ca].arguments, &variables_);
			if (!c->setIfTest(parser.argc(2))) result = FALSE;
			break;
		// 'Else If' statement (acts as CA_END to previous 'if' or 'elseif' branch.
		case (CA_ELSEIF):
			// If the previous branch was an 'if' or 'elseif', set the *ptr of that node to this node
			branchca = topBranchType();
			if ((branchca != CA_IF) && (branchca != CA_ELSEIF))
			{
				msg.print("Error: 'elseif' used without previous if/elseif.\n");
				result = FALSE;
				break;
			}
			// Add GOTONONIF command to topmost branch to end the if sequence
			c = addTopBranchCommand(CA_GOTONONIF, topBranchBaseNode());
			// Pop topmost (previous IF/ELSEIF) branch
			popBranch();
			// Add new command node to new topmost branch and get variables
			c = addTopBranchCommand(CA_ELSEIF, NULL);
			//printf("New node is %li, command = %s\n",c,CA_keywords[cmd]);
			// Add new branch to this node for new if test to run
			pushBranch(c->createBranch(), CA_ELSEIF, c);
			varresult = c->setArguments(CA_data[ca].keyword, CA_data[ca].arguments, &variables_);
			if (!c->setIfTest(parser.argc(2))) result = FALSE;
			break;
		// 'Else' statement (acts as CA_END to previous 'if' or 'elseif' branch.
		case (CA_ELSE):
			// If the previous branch was an 'if' or 'elseif', set the *ptr of that node to this node
			branchca = topBranchType();
			if ((branchca != CA_IF) && (branchca != CA_ELSEIF))
			{
				msg.print("Error: 'else' used without previous if/elseif.\n");
				result = FALSE;
				break;
			}
			// Add GOTONONIF command to current topbranch to terminate that branch
			c = addTopBranchCommand(CA_GOTONONIF, topBranchBaseNode());
			// Pop previous branch from stack and add new command to new topmost branch
			popBranch();
			// Add new node to new top branch
			c = addTopBranchCommand(CA_ELSE, NULL);
			//printf("New node is %li, command = %s\n",c,CA_keywords[cmd]);
			// Add new branch to this node for new if test to run
			pushBranch(c->createBranch(), CA_ELSE, c);
			break;
		// Loop for n iterations (or until file ends) or over items
		case (CA_FOR):
			c = addTopBranchCommand(CA_FOR, NULL);
			pushBranch(c->createBranch(), ca, c);
			varresult = c->setArguments(CA_data[ca].keyword, CA_data[ca].arguments, &variables_);
			// Create subvariables if necessary
// 			if (varresult) varresult = createSubvariables(c->arg(0)); TGAY
			break;
		// End the topmost branch in the stack
		case (CA_END):
			if (branchStack_.nItems() == 0)
			{
				msg.print("Error: 'end' does not end a block.\n");
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
					printf("CommandList::addCommand <<<< No END action defined for command '%s' >>>>\n", CA_data[branchca].keyword);
					result = FALSE;
					break;
			}
			// Remove the topmost branch from the stack
			popBranch();
			break;
		// Break out from current loop
		case (CA_BREAK):
			// Find the topmost FOR branch
			topc = topmostBranch(CA_FOR);
			if (topc == NULL)
			{
				msg.print("Error: no loop for 'break' to terminate.\n");
				result = FALSE;
				break;
			}
			c = addTopBranchCommand(CA_BREAK, topc);
			break;
		// Cycle current loop
		case (CA_CONTINUE):
			// Find the topmost FOR branch
			topc = topmostBranch(CA_FOR);
			if (topc == NULL)
			{
				msg.print("Error: no loop for 'cycle' to iterate.\n");
				result = FALSE;
				break;
			}
			c = addTopBranchCommand(CA_CONTINUE, topc);
			break;
		// Unrecognised command
		case (CA_NITEMS):
			printf("Internal error: Unrecognised command in CommandList::addCommand()\n");
			result = FALSE;
			break;
		// All other commands do not alter the flow of the CommandList...
		default:
			c = addTopBranchCommand(ca, NULL);
			varresult = c->setArguments(CA_data[ca].keyword, CA_data[ca].arguments, &variables_);
			break;
	}
	// Check variable assignment result
	if (!varresult)
	{
		//msg.print("Error: Command '%s' was not given the correct variables.\n", CA_data[ca].keyword);
		result = FALSE;
	}
	msg.exit("CommandList::addCommand");
	return result;
}

// Cache script commands from line containing semicolon-separated commands
bool CommandList::cacheLine(const char *s)
{
	msg.enter("CommandList::cacheLine");
	// Use a local parser to split up the semi-colon'd line into individual commands
	static Parser lines;
	lines.getLinesDelim(s);
	for (int n=0; n<lines.nArgs(); n++)
	{
		// Parse the argument in our local line_parser and call cache_command())
		parser.getArgsDelim(lines.argc(n), Parser::UseQuotes+Parser::SkipBlanks);
		if (!cacheCommand())
		{
			msg.exit("CommandList::cacheLine");
			return FALSE;
		}
	}
	msg.exit("CommandList::cacheLine");
	return TRUE;
}

// Cache command arguments in global Parser object
bool CommandList::cacheCommand()
{
	msg.enter("CommandList::cacheCommand");
	CommandAction ca;
	bool result = TRUE, addcmd = FALSE;
	/*
	Assume that the main parser object contains the data we require.
	If there is no argument 0 in the parser, then just return true. Otherwise, check for the first argument being a variable (denoted by a '$') and the second being an '='. If so, move the first argument to the second and add a CA_LET2 command.
	*/
	if (parser.isBlank(0)) result = TRUE;
	else if (parser.argc(0)[0] == '$')
	{
		// Shift all arguments up one position in the parser
		parser.shiftArgsUp();
		// Set command based on type of variable
		// TGAY return type of variablepath needs to be checked here.
// 		Variable *v = variables_.get(&parser.argc(1)[1]);
		Variable *v = variables_.addPath(parser.argc(1));
		if (v == NULL)
		{
			msg.print( "Variable '%s' has not been declared.\n", &parser.argc(1)[1]);
			result = FALSE;
		}
		else
		{
			// The command we use depends on the type of the target variable
			addcmd = TRUE;
			switch (v->type())
			{
				case (VTypes::CharacterData):
					addcmd = addCommand(CA_LETCHAR);
					break;
				case (VTypes::IntegerData):
				case (VTypes::RealData):
					addcmd = addCommand(CA_LET);
					break;
				default:
					addcmd = addCommand(CA_LETPTR);
					break;
			}
			if (!addcmd)
			{
				msg.print( "Error adding variable assignment command.\n");
				result = FALSE;
			}
		}
	}
	else
	{
		ca = CA_from_text(parser.argc(0));
		if (ca != CA_NITEMS)
		{
			// If addCommand() returns FALSE then we encountered an error
			if (!addCommand(ca))
			{
				//msg.print("Error parsing command '%s'.\n", parser.argc(0));
				msg.print("Command usage is: %s %s\n", CA_data[ca].keyword, CA_data[ca].argText);
				result = FALSE;
			}
		}
		else
		{
			msg.print("Unrecognised command '%s'.\n", parser.argc(0));
			result = FALSE;
		}
	}
	msg.exit("CommandList::cacheCommand");
	return result;
}

/*
// Files
*/

// Get input stream
ifstream *CommandList::inputFile()
{
	return inputFile_;
}

// Get output stream
ofstream *CommandList::outputFile()
{
	return outputFile_;
}

// Return filename associated to infile/outfile
const char *CommandList::filename()
{
	return filename_.get();
}

// Add read option
void CommandList::addReadOption(Parser::ParseOption po)
{
	if (!(readOptions_&po)) readOptions_ += po;
}

// Remove read option
void CommandList::removeReadOption(Parser::ParseOption po)
{
	if (readOptions_&po) readOptions_ -= po;
}

// Return read options
int CommandList::readOptions()
{
	return readOptions_;
}

// Load commands from file
bool CommandList::load(const char *filename)
{
	msg.enter("CommandList::load");
	scriptFilename_ = filename;
	ifstream cmdfile(filename,ios::in);
	int success;
	clear();
	name_ = filename;
	// Read in commands
	while (!cmdfile.eof())
	{
		success = parser.getArgsDelim(&cmdfile,Parser::UseQuotes+Parser::SkipBlanks);
		if (success == 1)
		{
			msg.print("CommandList::load - Error reading command file.\n");
			msg.exit("CommandList::load");
			return FALSE;
		}
		else if (success == -1) break;
		// Attempt to cache the command
		if (cacheCommand()) continue;
		else
		{
			msg.exit("CommandList::load");
			return FALSE;
		}
	}
	// Check the flowstack - it should be empty...
	int itemsleft = branchStack_.nItems();
	if (itemsleft != 1)
	{
		printf("CommandList::load <<<< %i block%s not been terminated >>>>\n", itemsleft, (itemsleft == 1 ? " has" : "s have"));
		msg.exit("CommandList::load");
		return FALSE;
	}
	msg.exit("CommandList::load");
	return TRUE;
}

// Set input file (pointer)
bool CommandList::setInputFile(const char *sourcefile)
{
	msg.enter("CommandList::setInFile");
	if (inputFile_ != NULL) printf("CommandList::setInFile <<<< Inputfile already set >>>>\n");
        inputFile_ = new ifstream(sourcefile,ios::in);
	filename_ = sourcefile;
	msg.exit("CommandList::setInFile");
	return (!inputFile_->good() ? FALSE : TRUE);
}

// Set output file
bool CommandList::setOutputFile(const char *destfile)
{
	msg.enter("CommandList::setOutputFile");
	outputFile_ = new ofstream(destfile,ios::out);
	filename_ = destfile;
	msg.exit("CommandList::setOutputFile");
	if (!outputFile_->good()) return FALSE;
	else return TRUE;
}

// Close files
void CommandList::closeFiles()
{
	msg.enter("CommandList::closeFiles");
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
	msg.exit("CommandList::closeFiles");
}

// Execute commands in command list
bool CommandList::execute(ifstream *sourcefile)
{
	msg.enter("CommandList::execute");
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
		msg.print(Messenger::Commands, "Commandlist executing command '%s'...\n",CA_data[c->command()].keyword);
		switch (c->execute(c))
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
	msg.exit("CommandList::execute");
	return result;
}
