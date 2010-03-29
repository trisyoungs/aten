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
#include "parser/forest.h"

// External Declarations
CommandParser cmdparser;
int CommandParser_parse();

// Constructor
CommandParser::CommandParser()
{
	// Private variables
	source_ = StringSource;
	stringListSource_ = NULL;
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
	if (source_ != CommandParser::StringSource) msg.print("Error occurred here (line %i in file '%s'):\n", parser_.lastLineNo(), parser_.filename());
	// QUICK'n'DIRTY!
	int i;
	char *temp = new char[stringLength_+32];
	for (i=0; i<stringLength_+32; ++i) temp[i] = '\0';
	for (i=0; i<tokenStart_; i++) temp[i] = (stringSource_[i] == '\t' ? '\t' : ' ');
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
CommandParser::ParserSource CommandParser::source()
{
	return source_;
}

// Get next character from current input stream
char CommandParser::getChar()
{
	char c = 0;
	// Are we at the end of the current string?
	if (stringPos_ == stringLength_)
	{
		switch (source_)
		{
			case (CommandParser::FileSource):
				if (parser_.getLine() != 0) return 0;
				stringSource_ = parser_.line();
				stringLength_ = stringSource_.length();
				stringPos_ = 0;
				break;
			case (CommandParser::StringListSource):
				// Are there any more strings to read in?
				if (stringListSource_ == NULL) return 0;
				stringSource_ = stringListSource_->get();
				stringListSource_ = stringListSource_->next;
				stringLength_ = stringSource_.length();
				stringPos_ = 0;
				break;
			case (CommandParser::StringSource):
				return 0;
				break;
		}
	}
	// Return current char
	c = stringSource_[stringPos_];
	stringPos_++;
	return c;
}

// Peek next character from current input stream
char CommandParser::peekChar()
{
	char c = 0;
	switch (source_)
	{
		case (CommandParser::FileSource):
			c = (stringPos_ == stringLength_ ? parser_.peek() : stringSource_[stringPos_]);
			break;
		case (CommandParser::StringListSource):
			if (stringPos_ == stringLength_)
			{
				if (stringListSource_ == NULL) c = 0;
				else c = stringListSource_->get()[0];
			}
			else c = stringSource_[stringPos_];
			break;
		case (CommandParser::StringSource):
			c = (stringPos_ == stringLength_ ? 0 : stringSource_[stringPos_]);
			break;
	}
	return c;
}

// 'Replace' last character read from current input stream
void CommandParser::unGetChar()
{
	switch (source_)
	{
		case (CommandParser::FileSource):
			if (stringPos_ == 0) printf("Fix Required: Last character from previous line required for unGetChar...\n");
			else stringPos_ --;
			break;
		case (CommandParser::StringListSource):
			if (stringPos_ == 0) printf("Fix Required: Last character from previous string required for unGetChar...\n");
			else stringPos_ --;
			break;
		case (CommandParser::StringSource):
			stringPos_ --;
			break;
	}
}

/*
// Tree Generation
*/

// Perform tree generation (base function, called by generateFrom*)
bool CommandParser::generate()
{
	msg.enter("CommandParser::generate");
	expectPathStep_ = FALSE;
	// Perform the parsing
	int result = CommandParser_parse();
	if (result != 0)
	{
		printErrorInfo();
		forest_->clear();
	}
	forest_ = NULL;
	msg.exit("CommandParser::generate");
	return (result != 0 ? FALSE : TRUE);
}

// Fill target forest from specified character string
bool CommandParser::generateFromString(Forest *f, const char *s, bool dontpushtree)
{
	msg.enter("CommandParser::generateFromString");
	// Clear any data in the existing forest
	if (f == NULL)
	{
		printf("Internal Error: No Forest passed to CommandParser::generateFromString.\n");
		msg.exit("CommandParser::generateFromString");
		return FALSE;
	}
	forest_ = f;
	forest_->clear();
	if (!dontpushtree) pushTree();
	// Store the source string
	stringSource_ = s;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	msg.print(Messenger::Parse, "Parser source string is '%s', length is %i\n", stringSource_.get(), stringLength_);
	source_ = CommandParser::StringSource;
	bool result = generate();
	msg.exit("CommandParser::generateFromString");
	return result;
}

// Populate target forest from specified string list
bool CommandParser::generateFromStringList(Forest *f, Dnchar *stringListHead, bool dontpushtree)
{
	msg.enter("CommandParser::generateFromStringList");
	// Clear any data in the existing forest
	if (f == NULL)
	{
		printf("Internal Error: No Forest passed to CommandParser::generateFromStringList.\n");
		msg.exit("CommandParser::generateFromStringList");
		return FALSE;
	}
	forest_ = f;
	forest_->clear();
	if (!dontpushtree) pushTree();
	// Store the source string
	stringListSource_ = stringListHead;
	stringPos_ = 0;
	stringLength_ = 0;
// 	stringListSource_->print();
	msg.print(Messenger::Parse, "Parser source is now string list.\n");
	source_ = CommandParser::StringListSource;
	bool result = generate();
	stringListSource_ = NULL;
	msg.exit("CommandParser::generateFromStringList");
	return result;
}

// Fill target forest from specified character string
bool CommandParser::generateFromFile(Forest *f, const char *filename)
{
	msg.enter("CommandParser::generateFromFile");
	// Clear any data in the existing forest
	if (f == NULL)
	{
		printf("Internal Error: No Forest passed to CommandParser::generateFromFile.\n");
		msg.exit("CommandParser::generateFromFile");
		return FALSE;
	}
	forest_ = f;
	forest_->clear();
	pushTree();
	// Open the file
	parser_.openFile(filename);
	if (!parser_.isFileGood())
	{
		msg.print("Error: File '%s' could not be opened.\n", filename);
		msg.exit("CommandParser::generateFromFile");
		return FALSE;
	}
	// Set initial string pos and string length so we read in a line on the first getChar.
	stringPos_ = 0;
	stringLength_ = 0;
	source_ = CommandParser::FileSource;
	bool result = generate();
	source_ = CommandParser::StringSource;
	msg.exit("CommandParser::generateFromFile");
	return result;
}

// Push tree
void CommandParser::pushTree(bool isfilter)
{
	tree_ = forest_->addTree(isfilter ? Tree::FilterTree : Tree::CommandTree);
	stack_.add(tree_, isfilter);
	msg.print(Messenger::Parse, "New tree stacked - %p\n", tree_);
}

// Push function (into topmost tree)
void CommandParser::pushFunction(const char *name, VTypes::DataType returntype)
{
	// If there is no current tree target then we add a Forest-global function...
	if (tree_ == NULL) tree_ = forest_->addGlobalFunction(name);
	else tree_ = tree_->addLocalFunction(name);
	tree_->setReturnType(returntype);
	stack_.add(tree_, FALSE);
	msg.print(Messenger::Parse, "New function stacked (return type is %s) - %p\n", VTypes::dataType(tree_->returnType()), tree_);
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
	msg.print(Messenger::Parse, "Removing tree %p from stack.\n", ri->item);
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
