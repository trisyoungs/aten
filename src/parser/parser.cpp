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

// External Declarations
NuParser nuparser;
// Tree *NuParser::tree_;
int yyparse();

// Constructor
NuParser::NuParser()
{
	// Private variables
	isFileSource_ = FALSE;
	stringPos_ = -1;
	tokenStart_ = 0;
	stringLength_ = 0;
	lineNumber_ = 0;
	expectPathStep_ = FALSE;
	tree_ = NULL;
}

// Destructor
NuParser::~NuParser()
{
}

// Print error information and location
void NuParser::printErrorInfo()
{
	// QUICK'n'DIRTY!
	char *temp = new char[stringLength_+32];
	int i;
	for (int i=0; i<tokenStart_; i++) temp[i] = (stringSource_[i] == '\t' ? '\t' : ' ');
	for (i=tokenStart_; i<stringPos_; i++) temp[i] = '^';
	temp[stringPos_] = '\0';
	// Print current string
	if (isFileSource_)
	{
		printf("(Line %4i) : %s\n", parser_.lastLine(), stringSource_.get());
		printf("            : %s\n", temp);
	}
	else
	{
		printf(" %s\n", stringSource_.get());
		printf(" %s^\n", temp);
	}
	delete[] temp;
}

/*
// Character Stream Retrieval
*/

// Return whether the current input stream is a file
bool NuParser::isFileSource()
{
	return isFileSource_;
}

// Get next character from current input stream
char NuParser::getChar()
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
char NuParser::peekChar()
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
void NuParser::unGetChar()
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
bool NuParser::generate(Forest *f, const char *s)
{
	msg.enter("NuParser::generate[string]");
	// Clear any data in the existing forest
	if (f == NULL)
	{
		printf("Internal Error: No Forest passed to NuParser::generate().\n");
		msg.exit("NuParser::generate[string]");
		return FALSE;
	}
	forest_ = f;
	forest_->clear();
	tree_ = f->createTree();
	printf("Target tree is %li\n", tree_);
	// Store the source string
	stringSource_ = s;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	printf("Source string is '%s', length is %i\n", stringSource_.get(), stringLength_);
	isFileSource_ = FALSE;
	expectPathStep_ = FALSE;
	// Perform the parsing
	int result = yyparse();
	if (result != 0)
	{
		msg.print("Error occurred here:\n");
		printErrorInfo();
		forest_->clear();
		forest_ = NULL;
		msg.exit("NuParser::generate[string]");
		return FALSE;
	}
	forest_ = NULL;
	msg.exit("NuParser::generate[string]");
	return TRUE;
}

// Fill target forest from specified character string
bool NuParser::generateFromFile(Forest *f, const char *filename)
{
	msg.enter("NuParser::generate[file]");
	// Clear any data in the existing forest
	if (f == NULL)
	{
		printf("Internal Error: No Forest passed to NuParser::generate().\n");
		msg.exit("NuParser::generate[file]");
		return FALSE;
	}
	forest_ = f;
	forest_->clear();
	// Open the file
	parser_.openFile(filename);
	if (!parser_.isFileGood())
	{
		msg.print("Error: File '%s' could not be opened.\n", filename);
		msg.exit("NuParser::generate[file]");
		return FALSE;
	}
	// Set initial string pos and string length so we read in a line on the first getChar.
	stringPos_ = 0;
	stringLength_ = 0;
	isFileSource_ = TRUE;
	expectPathStep_ = FALSE;
	// Perform the parsing
	int result = yyparse();
	if (result != 0)
	{
		msg.print("Error occurred here:\n");
		printErrorInfo();
		forest_->clear();
		forest_ = NULL;
		isFileSource_ = NULL;
		msg.exit("NuParser::generate[file]");
		return FALSE;
	}
	isFileSource_ = NULL;
	forest_ = NULL;
	msg.exit("NuParser::generate[file]");
	return TRUE;
}

// Finish current tree (i.e. nullify tree_)
void NuParser::finishTree()
{
	tree_ = NULL;
}

// Discard current tree and its contents
void NuParser::deleteCurrentTree()
{
	// Delete the current tree from its parent forest
	forest_->deleteTree(tree_);
	tree_ = NULL;
}
