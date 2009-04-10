/*
	*** Base Parser
	*** src/parser/parser.h
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

#ifndef ATEN_NUPARSER_H
#define ATEN_NUPARSER_H

#include "base/lineparser.h"
#include "parser/tree.h"
#include "templates/reflist.h"
#include "base/dnchar.h"
#include <fstream>

// Forward declarations
class Forest;
class TreeNode;

// Parser
class NuParser : public Tree
{
	public:
	// Constructor / Destructor
	NuParser();
	~NuParser();
	// Symbolic tokens - array of corresponding values refers to Bison's tokens
	enum SymbolToken { AssignSymbol, GEQSymbol, LEQSymbol, CNEQSymbol, FNEQSymbol, PlusEqSymbol, MinusEqSymbol, TimesEqSymbol, DivideEqSymbol, PlusPlusSymbol, MinusMinusSymbol, nSymbolTokens };
	// Friend declarations
	friend class Forest;

	/*
	// Create / Execute
	*/
	private:
	// Character string source
	Dnchar stringSource_;
	// Integer position in stringSource, total length of string, and starting position of current token/function
	int stringPos_, stringLength_, tokenStart_, functionStart_;
	// Line parser
	LineParser parser_;
	// Line number in source file that we've just read
	int lineNumber_;
	// Whether the current input source is a file or not
	bool isFileSource_;
	// Whether the next token to expect is a path step
	bool expectPathStep_;

	public:
	// Parser lexer, called by yylex()
	int lex();
	// Return whether the current input stream is a file or not
	bool isFileSource();
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


	/*
	// Tree Data
	*/
	private:
	// Current forest target
	Forest *forest_;
	// Current tree (target of node creation)
	Tree *tree_;

	public:
	// Finish current tree (i.e. nullify tree_)
	void finishTree();
	// Discard current tree and its contents
	void deleteCurrentTree();

	/*
	// Tree Generation - Private functions
	*/
	private:
	// Populate target forest from specified character string
	bool generate(Forest *f, const char *s);
	// Populate target forest from specified file
	bool generateFromFile(Forest *f, const char *filename);


	/*
	// Path Generation
	*/
	public:	
	// Flag that the next token to expect is a path step
	bool setExpectPathStep(bool b);
	// Whether to treat the next alphanumeric token as a path step variable
	bool expectPathStep();
	// Create a new path on the stack with the specified base 'variable'
	TreeNode *createPath(TreeNode *var);
	// Expand topmost path
	bool expandPath(Dnchar *name, TreeNode *arrayindex = NULL);
	// Finalise and remove the topmost path on the stack
	TreeNode *finalisePath();


	/*
	// Tree Interface
	*/
	public:
	// Add a node representing a whole statement to the execution list
	bool addStatement(TreeNode *leaf);
	// Add an operator to the Tree
	TreeNode *addOperator(NuCommand::Function func, TreeNode *arg1, TreeNode *arg2 = NULL);
	// Associate a command-based leaf node to the Tree
	TreeNode *addFunctionWithArglist(NuCommand::Function func, TreeNode *arglist);
	// Add a function node to the list (overloaded to accept simple arguments instead of a list)
	TreeNode *addFunction(NuCommand::Function func, TreeNode *a1 = NULL, TreeNode *a2 = NULL, TreeNode *a3 = NULL, TreeNode *a4 = NULL);
	// Join two commands together
	TreeNode *joinCommands(TreeNode *node1, TreeNode *node2);
	// Add on a new scope to the stack
	TreeNode *pushScope(NuCommand::Function func = NuCommand::NoFunction);
	// Pop the topmost scope node
	bool popScope();

	// Variables / Constants
	// Set current type for variable declarations
	bool setDeclaredVariableType(NuVTypes::DataType type);
	// Set declarations assignment flag
	bool setDeclarationAssignment(bool b);
	// Add constant value to tompost scope
	TreeNode *addConstant(NuVTypes::DataType type, Dnchar *token);
	// Add variable to topmost ScopeNode
	TreeNode *addVariable(NuVTypes::DataType type, Dnchar *name, TreeNode *initialValue = NULL);
	// Add variable to topmost ScopeNode using the most recently declared type
	TreeNode *addVariable(Dnchar *name, TreeNode *initialValue = NULL);
	// Add array variable to topmost ScopeNode using the most recently declared type
	TreeNode *addArrayVariable(Dnchar *name, TreeNode *sizeexpr, TreeNode *initialvalue = NULL);
	// Search for variable in current scope
	bool isVariableInScope(const char *name, NuVariable *&result);
	// Wrap named variable (and array index)
	TreeNode *wrapVariable(NuVariable *var, TreeNode *arrayindex = NULL);

	// Filters
	// Set filter option
	bool setFilterOption(Dnchar *name, TreeNode *value);

};

// External declaration
extern NuParser nuparser;

#endif
