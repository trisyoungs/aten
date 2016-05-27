/*
	*** Parser
	*** src/parser/parser.cpp
	Copyright T. Youngs 2007-2016

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
#include "parser/program.h"

ATEN_BEGIN_NAMESPACE

// Static Members
Aten* CommandParser::aten_ = NULL;
QString CommandParser::stringSource_;
QStringList CommandParser::stringListSource_;
int CommandParser::stringListSourceIndex_;
int CommandParser::stringPos_;
int CommandParser::stringLength_;
int CommandParser::tokenStart_;
int CommandParser::functionStart_;
LineParser CommandParser::parser_;
QString CommandParser::sourceInfo_;
CommandParser::ParserSource CommandParser::source_;
bool CommandParser::expectPathStep_;
QString CommandParser::lexedName_;
Program* CommandParser::program_;
Tree* CommandParser::tree_;
RefList<Tree,bool> CommandParser::stack_;
bool CommandParser::failed_;
bool CommandParser::quiet_;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// External Declarations
int CommandParser_parse();

// Constructor
CommandParser::CommandParser()
{
	reset();
}

// Destructor
CommandParser::~CommandParser()
{
}

/*
 * Link to Aten
 */

// Return pointer to Aten
Aten* CommandParser::aten()
{
	return aten_;
}

// Set pointer to Aten
void CommandParser::setAten(Aten* aten)
{
	aten_ = aten;
}

/*
 * Create / Execute
 */

// Reset values in parser, ready for next source
void CommandParser::reset()
{
	parser_.closeFiles();
	source_ = StringSource;
	stringListSource_.clear();
	stringListSourceIndex_ = -1;
	stringPos_ = -1;
	tokenStart_ = 0;
	functionStart_ = -1;
	stringSource_.clear();
	stringLength_ = 0;
	expectPathStep_ = false;
	program_ = NULL;
	tree_ = NULL;
	failed_ = false;
	parser_.reset();
	stack_.clear();
}

// Print error information and location
void CommandParser::printErrorInfo()
{
	if (source_ != CommandParser::StringSource) Messenger::print("Error occurred here (line %i in file '%s'):", parser_.lastLineNo(), qPrintable(parser_.inputFilename()));
	// QUICK'n'DIRTY!
	int i;
	char *temp = new char[stringLength_+32];
	for (i=0; i<stringLength_+32; ++i) temp[i] = '\0';
	for (i=0; i<tokenStart_; i++) temp[i] = (stringSource_[i] == '\t' ? '\t' : ' ');
	if (functionStart_ > -1) for (i=functionStart_; i<stringPos_; i++) if (temp[i] != '\t') temp[i] = '-';
	for (i=tokenStart_; i<stringPos_; i++) temp[i] = '^';
	temp[stringPos_] = '\0';
	// Print current string
	Messenger::print(" %s", qPrintable(stringSource_));
	Messenger::print(" %s^", temp);
	delete[] temp;
}

// Return short info on the current parsing source (filename, line number etc.)
QString CommandParser::sourceInfo()
{
	// If its a file source, construct a suitable string. Otherwise, just return sourceInfo_ as is.
	if (source_ == CommandParser::FileSource)
	{
		sourceInfo_.clear();
		sourceInfo_.sprintf("file '%s' (line %i)", qPrintable(parser_.inputFilename()), parser_.lastLineNo());
	}
	
	return sourceInfo_;
}

// Return current lexed name (if any)
QString CommandParser::lexedName()
{
	return lexedName_;	
}

/*
 * Character Stream Retrieval
 */

// Return whether the current input stream is a file
CommandParser::ParserSource CommandParser::source()
{
	return source_;
}

// Get next character from current input stream
char CommandParser::getChar()
{
	// Are we at the end of the current string?
	if (stringPos_ == stringLength_)
	{
		switch (source_)
		{
			case (CommandParser::FileSource):
				if (parser_.readNextLine(Parser::StripComments+Parser::SkipBlanks) != 0) return 0;
				stringSource_ = parser_.line();
				stringLength_ = stringSource_.length();
				stringPos_ = 0;
				break;
			case (CommandParser::StringListSource):
				// Are there any more strings to read in?
				if (stringListSourceIndex_ == -1) return 0;

				stringSource_ = stringListSource_.at(stringListSourceIndex_);
				++stringListSourceIndex_;
				if (stringListSourceIndex_ >= stringListSource_.count()) stringListSourceIndex_ = -1;
				stringLength_ = stringSource_.length();
				stringPos_ = 0;
				break;
			case (CommandParser::StringSource):
				return 0;
				break;
			default:
				break;
		}
	}

	// Return current char
	char c = stringSource_.at(stringPos_).toLatin1();
	++stringPos_;
	return c;
}

// Peek next character from current input stream
char CommandParser::peekChar()
{
	char c = 0;
	switch (source_)
	{
		case (CommandParser::FileSource):
			c = (stringPos_ == stringLength_ ? parser_.peek() : stringSource_.at(stringPos_).toLatin1());
			break;
		case (CommandParser::StringListSource):
			if (stringPos_ == stringLength_)
			{
				if (stringListSourceIndex_ == -1) c = 0;
				else c = '\n';
			}
			else c = stringSource_.at(stringPos_).toLatin1();
			break;
		case (CommandParser::StringSource):
			c = (stringPos_ == stringLength_ ? 0 : stringSource_.at(stringPos_).toLatin1());
			break;
		default:
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
		default:
			break;
	}
}

/*
 * Tree Generation
 */

// Perform tree generation (base function, called by generateFrom*)
bool CommandParser::generate()
{
	Messenger::enter("CommandParser::generate");
	expectPathStep_ = false;

	// Perform the parsing
	int result = CommandParser_parse();
	if (result != 0)
	{
		if (!quiet_) printErrorInfo();
// 		program_->clear();
	}
	if (failed_) result = -1;
	program_ = NULL;
	Messenger::exit("CommandParser::generate");
	return (result != 0 ? false : true);
}

// Fill target Program from specified character string
bool CommandParser::generateFromString(Program* prog, QString string, QString sourceInfo, bool pushTree, bool clearExisting, bool quiet)
{
	Messenger::enter("CommandParser::generateFromString");

	// Clear any data in the existing Program (if requested)
	if (prog == NULL)
	{
		printf("Internal Error: No Program passed to CommandParser::generateFromString.\n");
		Messenger::exit("CommandParser::generateFromString");
		return false;
	}
	program_ = prog;
	if (clearExisting) program_->clear();
	quiet_ = quiet;

	// Stack an initial Tree?
	if (pushTree)
	{
		tree_ = program_->mainProgram();
		stack_.add(tree_, false);
		Messenger::print(Messenger::Parse, "Main program stacked - %p", tree_);
	}

	// Store the source string
	sourceInfo_ = sourceInfo;
	stringSource_ = string;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	Messenger::print(Messenger::Parse, "Parser source string is '%s', length is %i", qPrintable(stringSource_), stringLength_);
	source_ = CommandParser::StringSource;
	bool result = generate();
	reset();

	Messenger::exit("CommandParser::generateFromString");
	return result;
}

// Populate target Program from specified string list
bool CommandParser::generateFromStringList(Program* prog, QStringList stringList, QString sourceInfo, bool pushTree, bool clearExisting, bool quiet)
{
	Messenger::enter("CommandParser::generateFromStringList");

	// Clear any data in the existing Program
	if (prog == NULL)
	{
		printf("Internal Error: No Program passed to CommandParser::generateFromStringList.\n");
		Messenger::exit("CommandParser::generateFromStringList");
		return false;
	}
	program_ = prog;
	if (clearExisting) program_->clear();
	quiet_ = quiet;

	// Stack an initial Tree?
	if (pushTree)
	{
		tree_ = program_->mainProgram();
		stack_.add(tree_, false);
		Messenger::print(Messenger::Parse, "Main program stacked - %p", tree_);
	}
	
	// Store the source strings
	sourceInfo_ = sourceInfo;
	stringListSource_ = stringList;
	stringSource_.clear();
	stringListSourceIndex_ = -1;
	if (stringListSource_.count() > 0)
	{
		stringSource_ = stringListSource_.at(0);
		if (stringListSource_.count() > 1) stringListSourceIndex_ = 0;
	}
	
	stringPos_ = 0;
	stringLength_ = 0;
	Messenger::print(Messenger::Parse, "Parser source is now string list.");
	source_ = CommandParser::StringListSource;
	bool result = generate();
	reset();

	Messenger::exit("CommandParser::generateFromStringList");
	return result;
}

// Fill target Program from specified file
bool CommandParser::generateFromFile(Program* prog, QString filename, bool pushTree, bool clearExisting, bool quiet)
{
	Messenger::enter("CommandParser::generateFromFile");

	// Clear any data in the existing Program (if requested)
	if (prog == NULL)
	{
		printf("Internal Error: No Program passed to CommandParser::generateFromFile.\n");
		Messenger::exit("CommandParser::generateFromFile");
		return false;
	}
	program_ = prog;
	if (clearExisting) program_->clear();
	quiet_ = quiet;
	
	// Stack initial Tree?
	if (pushTree)
	{
		tree_ = program_->mainProgram();
		stack_.add(tree_, false);
		Messenger::print(Messenger::Parse, "Main program stacked - %p", tree_);
	}
	
	// Open the file
	parser_.openInput(filename);
	if (!parser_.isFileGoodForReading())
	{
		Messenger::exit("CommandParser::generateFromFile");
		return false;
	}
	// Set initial string pos and string length so we read in a line on the first getChar.
	stringPos_ = 0;
	stringLength_ = 0;
	source_ = CommandParser::FileSource;
	bool result = generate();
	source_ = CommandParser::StringSource;
	reset();
	Messenger::exit("CommandParser::generateFromFile");
	return result;
}

// Return whether to generate program quietly (i.e. don't print any error messages)
bool CommandParser::quiet()
{
	return quiet_;
}

// Return current tree target, raising warning and setting fail flag if no tree is defined...
Tree* CommandParser::tree()
{
// 	static Tree dummyTree;
	if (tree_ == NULL)
	{
		failed_ = true;
		Messenger::print("Internal Error: Parser tried to do something to a non-existent tree.");
		return NULL;
	}
	else return tree_;
}

// Push filter
void CommandParser::pushFilter()
{
	tree_ = program_->addFilter();
	stack_.add(tree_, true);
	Messenger::print(Messenger::Parse, "New filter stacked - %p", tree_);
}

// Push function (into topmost tree)
Tree* CommandParser::pushFunction(QString name, VTypes::DataType returntype)
{
	// If there is no current tree target then we add a global function...
	if (tree_ != NULL) Messenger::print(Messenger::Parse, "Pushing function onto tree %p (%s)", tree_, qPrintable(tree_->name()));
	if (tree_ == NULL) tree_ = program_->addFunction(name);
	else tree_ = tree_->addLocalFunction(name);
	tree_->setReturnType(returntype);
	stack_.add(tree_, false);
	Messenger::print(Messenger::Parse, "New function stacked (return type is %s) - %p", VTypes::dataType(tree_->returnType()), tree_);
	return tree_;
}

// Pop tree
void CommandParser::popTree()
{
	Messenger::enter("CommandParser::popTree");
	// If the tree to be popped is a Filter, check that a filter type has been defined
	RefListItem<Tree,bool>* ri = stack_.last();
	if (ri->data)
	{
		// Can use the 'isFilter' member function to check for the lack of a proper type
		if (!ri->item->isFilter()) Messenger::print("WARNING - Filter '%s' has not been provided a filter type.", qPrintable(ri->item->filter.name()));
	}
	Messenger::print(Messenger::Parse, "Removing tree %p from stack (%i remain).", ri->item, stack_.nItems()-1);
	stack_.remove( stack_.last() );
	// Set current tree target to the top tree now on the stack
	ri = stack_.last();
	tree_ = ri == NULL ? NULL : ri->item;
	Messenger::exit("CommandParser::popTree");
}

// Discard current tree and its contents
void CommandParser::deleteCurrentTree()
{
	// Delete the current tree from its parent Program
	program_->deleteTree(tree_);
	popTree();
}

/*
 * Filters / GUI
 */

// Set filter option
bool CommandParser::setFilterOption(QString name, TreeNode* value)
{
	return tree()->filter.setOption(name, value);	
}
