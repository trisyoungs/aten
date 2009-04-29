/*
	*** Parser
	*** src/parser/parser.cpp
	Copyright T. Youngs 2007-2009

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
#include "parser/forest.h"

// External Declarations
CommandParser cmdparser;
// Tree *CommandParser::tree_;
int yyparse();

// Constructor
CommandParser::CommandParser()
{
	// Private variables
	isFileSource_ = FALSE;
	stringPos_ = -1;
	tokenStart_ = 0;
	functionStart_ = -1;
	stringLength_ = 0;
	lineNumber_ = 0;
	expectPathStep_ = FALSE;
	tree_ = NULL;
}

// Destructor
CommandParser::~CommandParser()
{
}

// Print error information and location
void CommandParser::printErrorInfo()
{
	if (isFileSource_) msg.print("Error occurred here (line %i in file '%s'):\n", parser_.lastLine(), parser_.filename());
	// QUICK'n'DIRTY!
	char *temp = new char[stringLength_+32];
	int i;
	for (int i=0; i<tokenStart_; i++) temp[i] = (stringSource_[i] == '\t' ? '\t' : ' ');
	if (functionStart_ > -1) for (i=functionStart_; i<stringPos_; i++) if (temp[i] != '\t') temp[i] = '-';
	for (i=tokenStart_; i<stringPos_; i++) temp[i] = '^';
	temp[stringPos_] = '\0';
	// Print current string
	msg.print(" %s\n", stringSource_.get());
	msg.print(" %s^\n", temp);
	delete[] temp;
}

/*
// Character Stream Retrieval
*/

// Return whether the current input stream is a file
bool CommandParser::isFileSource()
{
	return isFileSource_;
}

// Get next character from current input stream
char CommandParser::getChar()
{
	char c = 0;
	if (isFileSource_)
	{
		// If the stringPos_ is equal to the string length, read in another line
		if (stringPos_ == stringLength_)
		{
			if (parser_.getLine() != 0) return 0;
			stringSource_ = parser_.line();
			stringLength_ = stringSource_.length();
			stringPos_ = 0;
		}
	}
	// Return current character
	if (stringPos_ == stringLength_) return '\0';
	c = stringSource_[stringPos_];
	// Increment string position
	stringPos_++;
	return c;
}

// Peek next character from current input stream
char CommandParser::peekChar()
{
	char c = 0;
	if (isFileSource_)
	{
		if (stringPos_ == stringLength_) return parser_.peek();
		c = stringSource_[stringPos_];
	}
	else
	{
		// Return current character
		if (stringPos_ == stringLength_) return '\0';
		c = stringSource_[stringPos_];
	}
	return c;
}

// 'Replace' last character read from current input stream
void CommandParser::unGetChar()
{
	if (isFileSource_)
	{
		// If we are at position 0, then we need the last character from the previous line!
		if (stringPos_ == 0) printf("Fix Required: last character from previous line...\n");
		else stringPos_ --;
	}
	else stringPos_--;
}

/*
// Tree Generation
*/

// Fill target forest from specified character string
bool CommandParser::generate(Forest *f, const char *s)
{
	msg.enter("CommandParser::generate[string]");
	// Clear any data in the existing forest
	if (f == NULL)
	{
		printf("Internal Error: No Forest passed to CommandParser::generate().\n");
		msg.exit("CommandParser::generate[string]");
		return FALSE;
	}
	forest_ = f;
	forest_->clear();
	pushTree();
	// Store the source string
	stringSource_ = s;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	msg.print(Messenger::Parse, "Parser source string is '%s', length is %i\n", stringSource_.get(), stringLength_);
	isFileSource_ = FALSE;
	expectPathStep_ = FALSE;
	// Perform the parsing
	int result = yyparse();
	if (result != 0)
	{
		printErrorInfo();
		forest_->clear();
		forest_ = NULL;
		msg.exit("CommandParser::generate[string]");
		return FALSE;
	}
	forest_ = NULL;
	msg.exit("CommandParser::generate[string]");
	return TRUE;
}

// Fill target forest from specified character string
bool CommandParser::generateFromFile(Forest *f, const char *filename)
{
	msg.enter("CommandParser::generate[file]");
	bool result = TRUE;
	// Clear any data in the existing forest
	if (f == NULL)
	{
		printf("Internal Error: No Forest passed to CommandParser::generate().\n");
		msg.exit("CommandParser::generate[file]");
		return FALSE;
	}
	forest_ = f;
	forest_->clear();
	// Open the file
	parser_.openFile(filename);
	if (!parser_.isFileGood())
	{
		msg.print("Error: File '%s' could not be opened.\n", filename);
		msg.exit("CommandParser::generate[file]");
		return FALSE;
	}
	// Set initial string pos and string length so we read in a line on the first getChar.
	stringPos_ = 0;
	stringLength_ = 0;
	isFileSource_ = TRUE;
	expectPathStep_ = FALSE;
	// Perform the parsing
	if (yyparse() != 0)
	{
		printErrorInfo();
		forest_->clear();
		result = FALSE;
	}
	parser_.closeFile();
	isFileSource_ = NULL;
	forest_ = NULL;
	msg.exit("CommandParser::generate[file]");
	return result;
}

// Push tree
void CommandParser::pushTree(bool isfilter)
{
	tree_ = forest_->addTree(isfilter ? Tree::FilterTree : Tree::CommandTree);
	stack_.add(tree_, isfilter);
	msg.print(Messenger::Parse, "New tree stacked - %li\n", tree_);
}

// Push function (into topmost tree)
void CommandParser::pushFunction(const char *name, VTypes::DataType returntype)
{
	// If there is no current tree target then we add a Forest-global function...
	if (tree_ == NULL) tree_ = forest_->addGlobalFunction(name);
	else tree_ = tree_->addLocalFunction(name);
	tree_->setReturnType(returntype);
	stack_.add(tree_, FALSE);
	msg.print(Messenger::Parse, "New function stacked (return type is %s) - %li\n", VTypes::dataType(tree_->returnType()), tree_);
}

// Pop tree
void CommandParser::popTree()
{
	msg.enter("CommandParser::popTree");
	// If the tree to be popped is a Filter, check that a filter type has been defined
	Refitem<Tree,bool> *ri = stack_.last();
	if (ri->data)
	{
		// Can use the 'isFilter' member function to check for the lack of a proper type
		if (!ri->item->isFilter()) msg.print("WARNING - Filter '%s' has not been provided a filter type.\n", ri->item->filter.name());
	}
	msg.print(Messenger::Parse, "Removing tree %li from stack.\n", ri->item);
	stack_.remove( stack_.last() );
	// Set current tree target to the top tree now on the stack
	ri = stack_.last();
	tree_ = ri == NULL ? NULL : ri->item;
	msg.exit("CommandParser::popTree");
}

// Discard current tree and its contents
void CommandParser::deleteCurrentTree()
{
	// Delete the current tree from its parent forest
	forest_->deleteTree(tree_);
	popTree();
}
