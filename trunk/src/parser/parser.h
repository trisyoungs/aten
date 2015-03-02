/*
	*** Command Parser
	*** src/parser/parser.h
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

#ifndef ATEN_COMMANDPARSER_H
#define ATEN_COMMANDPARSER_H

#include "base/lineparser.h"
#include "parser/tree.h"
#include "templates/reflist.h"
#include "base/dnchar.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Program;
class TreeNode;
class Aten;

// Parser
class CommandParser
{
	public:
	// Constructor / Destructor
	CommandParser(Aten& aten);
	~CommandParser();
	// Symbolic tokens - array of corresponding values refers to Bison's tokens
	enum SymbolToken { AssignSymbol, GEQSymbol, LEQSymbol, CNEQSymbol, FNEQSymbol, PlusEqSymbol, MinusEqSymbol, TimesEqSymbol, DivideEqSymbol, PlusPlusSymbol, MinusMinusSymbol, AndSymbol, OrSymbol, nSymbolTokens };
	// Source of parser input
	enum ParserSource { StringSource, StringListSource, FileSource, nParserSources };
	// Friend declarations
	friend class Program;


	/*
	 * Link to Aten
	 */
	private:
	// Reference to Aten
	Aten& aten_;

	public:
	// Return reference to Aten
	Aten& aten();


	/*
	// Create / Execute
	*/
	private:
	// Character string source
	Dnchar stringSource_;
	// Character string list source
	Dnchar* stringListSource_;
	// Integer position in stringSource, total length of string, and starting position of current token/function
	int stringPos_, stringLength_, tokenStart_, functionStart_;
	// Line parser
	LineParser parser_;
	// Local string used to construct source info
	Dnchar sourceInfo_;
	// Current input type to parser
	ParserSource source_;
	// Whether the next token to expect is a path step
	bool expectPathStep_;

	public:
	// Reset structure ready for next source
	void reset();
	// Parser lexer, called by yylex()
	int lex();
	// Return current input source
	ParserSource source();
	// Get next character from current input stream
	char getChar();
	// Peek next character from current input stream
	char peekChar();
	// 'Replace' last character read from current input stream
	void unGetChar();
	// Clear all node data
	void clear();
	// Print layout of current tree
	void print();
	// Print error information and location
	void printErrorInfo();
	// Set source information string (if not from a file)
	void setSourceInfo(const char* s);
	// Return short info on the current parsing source (filename, line number etc.)
	const char* sourceInfo();


	/*
	// Tree Data
	*/
	private:
	// Current program target
	Program* program_;
	// Current tree (target of node creation)
	Tree* tree_;
	// Stack of created trees
	Reflist<Tree,bool> stack_;
	// Flag to indicate failure in tree generation (set by tree() function)
	bool failed_;
	// Perform tree generation (base function, called by generateFrom*)
	bool generate();
	// Populate target Program from specified character string
	bool generateFromString(Program* prog, const char* s, const char* sourceInfo, bool dontPushTree = FALSE, bool clearExisting = TRUE);
	// Populate target Program from specified string list
	bool generateFromStringList(Program* prog, Dnchar* stringListHead, const char* sourceInfo, bool dontPushTree = FALSE, bool clearExisting = TRUE);
	// Populate target Program from specified file(name)
	bool generateFromFile(Program* prog, const char* filename, bool dontPushTree = FALSE, bool clearExisting = TRUE);

	public:
	// Return current Tree target, raising warning and setting fail flag if no tree is defined...
	Tree* tree();
	// Push filter
	void pushFilter();
	// Push function
	Tree* pushFunction(const char* name, VTypes::DataType returntype);
	// Pop tree (or function) from stack
	void popTree();
	// Discard current tree and its contents
	void deleteCurrentTree();


	/*
	// Pass-Throughs to Tree Functions
	*/
	public:	
	// Add integer constant
	TreeNode* addConstant(int i);
	// Add double constant
	TreeNode* addConstant(double d);
	// Add string constant
	TreeNode* addConstant(const char* s);
	// Add Element constant
	TreeNode* addElementConstant(int el);
	// Create a new path on the stack with the specified base 'variable'
	TreeNode* createPath(TreeNode* var);
	// Expand topmost path
	bool expandPath(Dnchar* name, TreeNode* arrayIndex = NULL, TreeNode* argList = NULL);
	// Finalise and remove the topmost path on the stack
	TreeNode* finalisePath();
	// Join two commands together
	TreeNode* joinCommands(TreeNode* node1, TreeNode* node2);
	// Add on a new scope to the stack
	TreeNode* pushScope(Commands::Function func = Commands::NoFunction);
	// Pop the topmost scope node
	bool popScope();
	// Add a node representing a whole statement to the execution list
	bool addStatement(TreeNode* leaf);
	// Add a 'new' node to the Tree
	TreeNode* addNew(VTypes::DataType type);
	// Add an operator to the Tree
	TreeNode* addOperator(Commands::Function func, TreeNode* arg1, TreeNode* arg2 = NULL, TreeNode* arg3 = NULL);
	// Associate a command-based leaf node to the Tree
	TreeNode* addFunctionWithArglist(Commands::Function func, TreeNode* argList);
	// Add a function node to the list (overloaded to accept simple arguments instead of a list)
	TreeNode* addFunction(Commands::Function func, TreeNode* a1 = NULL, TreeNode* a2 = NULL, TreeNode* a3 = NULL, TreeNode* a4 = NULL);
	// Associate a user-defined command-based leaf node to the Tree
	TreeNode* addUserFunction(Tree* func, TreeNode* argList = NULL);
	// Add a declaration list
	TreeNode* addDeclarations(TreeNode* declist);
	// Add a global declaration list
	TreeNode* addGlobalDeclarations(TreeNode* declist);
	// Wrap named variable (and array index)
	TreeNode* wrapVariable(Variable *var, TreeNode* arrayIndex = NULL);
	// Add variable to topmost ScopeNode
	TreeNode* addVariable(VTypes::DataType type, Dnchar* name, TreeNode* initialValue = NULL, bool global = FALSE);
	// Add array variable to topmost ScopeNode
	TreeNode* addArrayVariable(VTypes::DataType type, Dnchar* name, TreeNode* sizeexpr, TreeNode* initialvalue = NULL, bool global = FALSE);
	// Add array 'constant'
	TreeNode* addArrayConstant(TreeNode* values);


	/*
	// Filters / GUI
	*/
	public:
	// Set filter option
	bool setFilterOption(Dnchar* name, TreeNode* value);
};

// External declaration
extern CommandParser cmdparser;

ATEN_END_NAMESPACE

#endif
