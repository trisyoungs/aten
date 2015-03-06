/*
	*** Parser
	*** src/parser/parser.cpp
	Copyright T. Youngs 2007-2015

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

// External Declaration and Static Members
CommandParser cmdparser;
Aten* CommandParser::aten_ = NULL;

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
	stringListSource_ = NULL;
	stringPos_ = -1;
	tokenStart_ = 0;
	functionStart_ = -1;
	stringSource_.clear();
	stringLength_ = 0;
	expectPathStep_ = FALSE;
	program_ = NULL;
	tree_ = NULL;
	failed_ = FALSE;
	parser_.reset();
	stack_.clear();
}

// Print error information and location
void CommandParser::printErrorInfo()
{
	if (source_ != CommandParser::StringSource) Messenger::print("Error occurred here (line %i in file '%s'):\n", parser_.lastLineNo(), parser_.inputFilename());
	// QUICK'n'DIRTY!
	int i;
	char *temp = new char[stringLength_+32];
	for (i=0; i<stringLength_+32; ++i) temp[i] = '\0';
	for (i=0; i<tokenStart_; i++) temp[i] = (stringSource_[i] == '\t' ? '\t' : ' ');
	if (functionStart_ > -1) for (i=functionStart_; i<stringPos_; i++) if (temp[i] != '\t') temp[i] = '-';
	for (i=tokenStart_; i<stringPos_; i++) temp[i] = '^';
	temp[stringPos_] = '\0';
	// Print current string
	Messenger::print(" %s\n", stringSource_.get());
	Messenger::print(" %s^\n", temp);
	delete[] temp;
}

// Return short info on the current parsing source (filename, line number etc.)
const char* CommandParser::sourceInfo()
{
	// If its a file source, construct a suitable string. Otherwise, just return sourceInfo_ as is.
	if (source_ == CommandParser::FileSource)
	{
		sourceInfo_.clear();
		sourceInfo_.sprintf("file '%s' (line %i)", parser_.inputFilename(), parser_.lastLineNo());
	}
	
	return sourceInfo_.get();
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
				if (parser_.readNextLine(LineParser::StripComments+LineParser::SkipBlanks) != 0) return 0;
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
			default:
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
				else c = '\n';
			}
			else c = stringSource_[stringPos_];
			break;
		case (CommandParser::StringSource):
			c = (stringPos_ == stringLength_ ? 0 : stringSource_[stringPos_]);
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
// Tree Generation
*/

// Perform tree generation (base function, called by generateFrom*)
bool CommandParser::generate()
{
	Messenger::enter("CommandParser::generate");
	expectPathStep_ = FALSE;
	// Perform the parsing
	int result = CommandParser_parse();
	if (result != 0)
	{
		printErrorInfo();
// 		program_->clear();
	}
	if (failed_) result = -1;
	program_ = NULL;
	Messenger::exit("CommandParser::generate");
	return (result != 0 ? FALSE : TRUE);
}

// Fill target Program from specified character string
bool CommandParser::generateFromString(Program* prog, const char* s, const char* sourceInfo, bool dontPushTree, bool clearExisting)
{
	Messenger::enter("CommandParser::generateFromString");
	// Clear any data in the existing Program (if requested)
	if (prog == NULL)
	{
		printf("Internal Error: No Forest passed to CommandParser::generateFromString.\n");
		Messenger::exit("CommandParser::generateFromString");
		return FALSE;
	}
	program_ = prog;
	if (clearExisting) program_->clear();
		
	// Stack an initial Tree?
	if (!dontPushTree)
	{
		tree_ = program_->mainProgram();
		stack_.add(tree_, FALSE);
		Messenger::print(Messenger::Parse, "Main program stacked - %p\n", tree_);
	}
	
	// Store the source string
	sourceInfo_ = sourceInfo;
	stringSource_ = s;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	Messenger::print(Messenger::Parse, "Parser source string is '%s', length is %i\n", stringSource_.get(), stringLength_);
	source_ = CommandParser::StringSource;
	bool result = generate();
	reset();
	Messenger::exit("CommandParser::generateFromString");
	return result;
}

// Populate target Program from specified string list
bool CommandParser::generateFromStringList(Program* prog, Dnchar* stringListHead, const char* sourceInfo, bool dontPushTree, bool clearExisting)
{
	Messenger::enter("CommandParser::generateFromStringList");
	// Clear any data in the existing Program
	if (prog == NULL)
	{
		printf("Internal Error: No Forest passed to CommandParser::generateFromStringList.\n");
		Messenger::exit("CommandParser::generateFromStringList");
		return FALSE;
	}
	program_ = prog;
	if (clearExisting) program_->clear();
	
	// Stack an initial Tree?
	if (!dontPushTree)
	{
		tree_ = program_->mainProgram();
		stack_.add(tree_, FALSE);
		Messenger::print(Messenger::Parse, "Main program stacked - %p\n", tree_);
	}
	
	// Store the source strings
	sourceInfo_ = sourceInfo;
	stringListSource_ = stringListHead;
	stringPos_ = 0;
	stringLength_ = 0;
	Messenger::print(Messenger::Parse, "Parser source is now string list.\n");
	source_ = CommandParser::StringListSource;
	bool result = generate();
	stringListSource_ = NULL;
	reset();
	Messenger::exit("CommandParser::generateFromStringList");
	return result;
}

// Fill target Program from specified file
bool CommandParser::generateFromFile(Program* prog, const char* filename, bool dontPushTree, bool clearExisting)
{
	Messenger::enter("CommandParser::generateFromFile");
	// Clear any data in the existing Program (if reqeusted)
	if (prog == NULL)
	{
		printf("Internal Error: No Forest passed to CommandParser::generateFromFile.\n");
		Messenger::exit("CommandParser::generateFromFile");
		return FALSE;
	}
	program_ = prog;
	if (clearExisting) program_->clear();
	
	// Stack initial Tree?
	if (!dontPushTree)
	{
		tree_ = program_->mainProgram();
		stack_.add(tree_, FALSE);
		Messenger::print(Messenger::Parse, "Main program stacked - %p\n", tree_);
	}
	
	// Open the file
	parser_.openInput(filename);
	if (!parser_.isFileGoodForReading())
	{
		Messenger::exit("CommandParser::generateFromFile");
		return FALSE;
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

// Return current tree target, raising warning and setting fail flag if no tree is defined...
Tree* CommandParser::tree()
{
// 	static Tree dummyTree;
	if (tree_ == NULL)
	{
		failed_ = TRUE;
		Messenger::print("Internal Error: Parser tried to do something to a non-existent tree.\n");
		return NULL;
	}
	else return tree_;
}

// Push filter
void CommandParser::pushFilter()
{
	tree_ = program_->addFilter();
	stack_.add(tree_, TRUE);
	Messenger::print(Messenger::Parse, "New filter stacked - %p\n", tree_);
}

// Push function (into topmost tree)
Tree* CommandParser::pushFunction(const char* name, VTypes::DataType returntype)
{
	// If there is no current tree target then we add a global function...
	if (tree_ != NULL) Messenger::print(Messenger::Parse, "Pushing function onto tree %p (%s)\n", tree_, tree_->name());
	if (tree_ == NULL) tree_ = program_->addFunction(name);
	else tree_ = tree_->addLocalFunction(name);
	tree_->setReturnType(returntype);
	stack_.add(tree_, FALSE);
	Messenger::print(Messenger::Parse, "New function stacked (return type is %s) - %p\n", VTypes::dataType(tree_->returnType()), tree_);
	return tree_;
}

// Pop tree
void CommandParser::popTree()
{
	Messenger::enter("CommandParser::popTree");
	// If the tree to be popped is a Filter, check that a filter type has been defined
	Refitem<Tree,bool>* ri = stack_.last();
	if (ri->data)
	{
		// Can use the 'isFilter' member function to check for the lack of a proper type
		if (!ri->item->isFilter()) Messenger::print("WARNING - Filter '%s' has not been provided a filter type.\n", ri->item->filter.name());
	}
	Messenger::print(Messenger::Parse, "Removing tree %p from stack (%i remain).\n", ri->item, stack_.nItems()-1);
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
// Pass-Throughs to Tree Functions
*/

// Add integer constant
TreeNode* CommandParser::addConstant(int i)
{
	return tree()->addConstant(i);
}

// Add double constant
TreeNode* CommandParser::addConstant(double d)
{
	return tree()->addConstant(d);
}

// Add string constant
TreeNode* CommandParser::addConstant(const char* s)
{
	return tree()->addConstant(s);
}

// Add Element constant
TreeNode* CommandParser::addElementConstant(int el)
{
	return tree()->addElementConstant(el);
}

// Create a new path on the stack with the specified base 'variable'
TreeNode* CommandParser::createPath(TreeNode* var)
{
	return tree()->createPath(var);
}

// Expand topmost path
bool CommandParser::expandPath(Dnchar* name, TreeNode* arrayIndex, TreeNode* argList)
{
	return tree()->expandPath(name, arrayIndex, argList);
}

// Finalise and remove the topmost path on the stack
TreeNode* CommandParser::finalisePath()
{
	return tree()->finalisePath();
}

// Join two commands together
TreeNode* CommandParser::joinCommands(TreeNode* node1, TreeNode* node2)
{
	return tree()->joinCommands(node1, node2);
}

// Add on a new scope to the stack
TreeNode* CommandParser::pushScope(Commands::Function func)
{
	return tree()->pushScope(func);
}

// Pop the topmost scope node
bool CommandParser::popScope()
{
	return tree()->popScope();
}

// Add a node representing a whole statement to the execution list
bool CommandParser::addStatement(TreeNode* leaf)
{
	return tree()->addStatement(leaf);
}

// Add a 'new' node to the Tree
TreeNode* CommandParser::addNew(VTypes::DataType type)
{
	return tree()->addNew(type);
}

// Add an operator to the Tree
TreeNode* CommandParser::addOperator(Commands::Function func, TreeNode* arg1, TreeNode* arg2, TreeNode* arg3)
{
	return tree()->addOperator(func, arg1, arg2, arg3);
}

// Associate a command-based leaf node to the Tree
TreeNode* CommandParser::addFunctionWithArglist(Commands::Function func, TreeNode* argList)
{
	return tree()->addFunctionWithArglist(func, argList);
}

// Add a function node to the list (overloaded to accept simple arguments instead of a list)
TreeNode* CommandParser::addFunction(Commands::Function func, TreeNode* a1, TreeNode* a2, TreeNode* a3, TreeNode* a4)
{
	return tree()->addFunction(func, a1, a2, a3, a4);
}

// Associate a user-defined command-based leaf node to the Tree
TreeNode* CommandParser::addUserFunction(Tree* func, TreeNode* argList)
{
	return tree()->addUserFunction(func, argList);
}

// Add a declaration list
TreeNode* CommandParser::addDeclarations(TreeNode* declist)
{
	return tree()->addDeclarations(declist);
}

// Wrap named variable (and array index)
TreeNode* CommandParser::wrapVariable(Variable *var, TreeNode* arrayIndex)
{
	return tree()->wrapVariable(var, arrayIndex);
}

// Add variable to topmost ScopeNode
TreeNode* CommandParser::addVariable(VTypes::DataType type, Dnchar* name, TreeNode* initialValue, bool global)
{
	return tree()->addVariable(type, name, initialValue, global);
}

// Add array variable to topmost ScopeNode
TreeNode* CommandParser::addArrayVariable(VTypes::DataType type, Dnchar* name, TreeNode* sizeexpr, TreeNode* initialvalue, bool global)
{
	return tree()->addArrayVariable(type, name, sizeexpr, initialvalue, global);
}

// Add array 'constant'
TreeNode* CommandParser::addArrayConstant(TreeNode* values)
{
	return tree()->addArrayConstant(values);
}

/*
// Filters / GUI
*/

// Set filter option
bool CommandParser::setFilterOption(Dnchar* name, TreeNode* value)
{
	return tree()->filter.setOption(name, value);	
}
