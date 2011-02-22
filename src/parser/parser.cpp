/*
	*** Parser
	*** src/parser/parser.cpp
	Copyright T. Youngs 2007-2010

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

#include "parser/parser.h"

// External Declarations
CommandParser cmdparser;
int CommandParser_parse();

// Constructor
CommandParser::CommandParser()
{
	// Private variables
	reset();
}

// Destructor
CommandParser::~CommandParser()
{
}

// Reset values in parser, ready for next source
void CommandParser::reset()
{
	stringPos_ = -1;
	tokenStart_ = 0;
	functionStart_ = -1;
	stringSource_.clear();
	stringLength_ = 0;
	expectPathStep_ = FALSE;
	tree_ = NULL;
	stack_.clear();
}

// Print error information and location
void CommandParser::printErrorInfo()
{
	// QUICK'n'DIRTY!
	int i;
	char *temp = new char[stringLength_+32];
	for (i=0; i<stringLength_+32; ++i) temp[i] = '\0';
	for (i=0; i<tokenStart_; i++) temp[i] = (stringSource_[i] == '\t' ? '\t' : ' ');
	if (functionStart_ > -1) for (i=functionStart_; i<stringPos_; i++) if (temp[i] != '\t') temp[i] = '-';
	for (i=tokenStart_; i<stringPos_; i++) temp[i] = '^';
	temp[stringPos_] = '\0';
	// Print current string
	printf(" %s\n", stringSource_.get());
	printf(" %s^\n", temp);
	delete[] temp;
}

/*
// Character Stream Retrieval
*/

// Get next character from current input stream
char CommandParser::getChar()
{
	// Are we at the end of the current string?
	if (stringPos_ == stringLength_) return 0;
	// Return current char
	char c = stringSource_[stringPos_];
	stringPos_++;
	return c;
}

// Peek next character from current input stream
char CommandParser::peekChar()
{
	return (stringPos_ == stringLength_ ? 0 : stringSource_[stringPos_]);
}

// 'Replace' last character read from current input stream
void CommandParser::unGetChar()
{
	stringPos_ --;
}

/*
// Tree Generation
*/

// Populate supplied tree with commands
bool CommandParser::generate(Tree *t, const char *commands)
{
	if (t == NULL)
	{
		printf("NULL Tree passed to CommandParser::generate.\n");
		return FALSE;
	}
	// Set the forest target to be our own local, static Forest
	tree_ = t;
	// 'Push' tree onto the stack
	stack_.add(tree_, FALSE);
	//printf("Added initial tree %p to stack\n", tree_);
	// Store the source string
	stringSource_ = commands;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	//printf("Parser source string is '%s', length is %i\n", stringSource_.get(), stringLength_);
	expectPathStep_ = FALSE;
	// Perform the parsing
	int result = CommandParser_parse();
	//tree_->print();
	if (result != 0) printErrorInfo();
	return (result != 0 ? FALSE : TRUE);
}

// Push function (into topmost tree)
void CommandParser::pushFunction(const char *name, VTypes::DataType returntype)
{
	// If there is no current tree target then we add a Forest-global function...
	if (tree_ != NULL) printf("Pushing function onto tree %p\n", tree_);
	if (tree_ == NULL) printf("Horrible Error: No Tree target set for creation of local function\n");
	else tree_ = tree_->addLocalFunction(name);
	tree_->setReturnType(returntype);
	stack_.add(tree_, FALSE);
	//printf("New function stacked (return type is %s) - %p\n", VTypes::dataType(tree_->returnType()), tree_);
}

// Pop tree
void CommandParser::popTree()
{
	Refitem<Tree,bool> *ri = stack_.last();
	//printf("Removing tree %p from stack (%i remain).\n", ri->item, stack_.nItems()-1);
	stack_.remove( stack_.last() );
	// Set current tree target to the top tree now on the stack
	ri = stack_.last();
	tree_ = ri == NULL ? NULL : ri->item;
	//printf("Current tree is now %p\n", tree_);
}
