/*
	*** Tree
	*** src/parser/tree.h
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

#ifndef ATEN_TREE_H
#define ATEN_TREE_H

#include <iostream>
#include "parser/returnvalue.h"
#include "parser/variable.h"
#include "nucommand/commands.h"
#include "templates/reflist.h"
#include "base/vtypes.h"
#include "base/dnchar.h"

// Forward declarations
class TreeNode;
class ScopeNode;
class VariableNode;
class StepNode; 

// Tree
class Tree
{
	public:
	// Constructor / Destructor
	Tree();
	~Tree();

	/*
	// Create / Execute
	*/
	private:
	// Whether this Tree is being created from string, file etc.
	bool isFileSource_;
	// Character string source
	Dnchar stringSource_;
	// Integer position in stringSource, and total length of string
	int stringPos_, stringLength_;
	// File source
	ifstream *fileSource_;
	// Line number in source file that we've just read
	int lineNumber_;

	public:
	// Return whether the current input stream is a file
	bool isFileSource();
	// Get next character from current input stream
	char getChar();
	// Peek next character from current input stream
	char peekChar();
	// 'Replace' last character read from current input stream
	void unGetChar();
	// Clear all node data
	void clear();
	// Function to create AST, putting result in static member
	bool generate(const char *s);
	// Execute AST, placing result in ReturnValue provided
	bool execute(NuReturnValue &rv);
	// Current tree (target of node creation)
	static Tree *currentTree;
	// Print layout of current tree
	void print();
	// Print error information and location
	void printErrorInfo();


	/*
	// Node Data
	*/
	private:
	// Node list - a disordered reflist of all nodes owned by the Tree
	Reflist<TreeNode,int> ownedNodes_;
	// Reflist of all statements in the Tree, to be executed sequentially
	Reflist<TreeNode,int> statements_;
	// Stack of ScopeNodes
	Reflist<ScopeNode,int> scopeStack_;
	// Stack of variable paths (and last added stepnode)
	Reflist<VariableNode,TreeNode*> pathStack_;
	// Number of syntactic errors encountered
	int nErrors_;
	// Check operator type compatibility
	NuVTypes::DataType checkOperatorTypes(NuCommand::Function func, NuVTypes::DataType type1, NuVTypes::DataType type2);


	/*
	// Statement / Command Addition
	*/
	public:
	// Add a node representing a whole statement to the execution list
	void addStatement(TreeNode *leaf);
	// Add an operator to the Tree
	TreeNode *addOperator(NuCommand::Function func, int typearg, TreeNode *arg1, TreeNode *arg2 = NULL);
	// Add 'if' statement
	TreeNode *addIf(TreeNode *condition, TreeNode *expr1, TreeNode *expr2 = NULL);
	// Add 'for' statement
	TreeNode *addFor(TreeNode *init, TreeNode *condition, TreeNode *action, TreeNode *statements);
	// Associate a command-based leaf node to the Tree
	TreeNode *addFunctionLeaf(NuCommand::Function func, TreeNode *arglist);
	// Associate a scoped command leaf node to the Tree
	TreeNode *addScopedLeaf(NuCommand::Function func, int nargs, ...);
	// Join two nodes together
	static TreeNode *joinArguments(TreeNode *arg1, TreeNode *arg2);
	// Pop the most recent function leaf from the stack and own any stored arguments
	void finaliseFunction();
	// Add joiner
	TreeNode *joinFunctions(TreeNode *node1, TreeNode *node2);
	// Add on a new scope to the stack
	TreeNode *pushScope();
	// Pop the topmost scope node
	void popScope();


	/*
	// Variables / Constants
	*/
	private:
	// Current variable type to use for creating variables
	NuVTypes::DataType declaredType_;
	// Flag to indicate that we are assigning in a declaration, andthe whole variable scope should be searched
	bool declarationAssignment_;

	public:
	// Set current type for variable declarations
	void setDeclaredVariableType(NuVTypes::DataType type);
	// Set declarations assignment flag
	void setDeclarationAssignment(bool b);
	// Add constant value to tompost scope
	TreeNode *addConstant(NuVTypes::DataType type, Dnchar *token);
	// Add variable to topmost ScopeNode
	TreeNode *addVariable(NuVTypes::DataType type, Dnchar *name, TreeNode *initialValue = NULL);
	// Add variable to topmost ScopeNode using the most recently declared type
	TreeNode *addVariable(Dnchar *name, TreeNode *initialValue = NULL);
	// Add array variable to topmost ScopeNode using the most recently declared type
	TreeNode *addArrayVariable(Dnchar *name, TreeNode *sizeexpr, TreeNode *initialvalue = NULL);
	// Add 'constant' vector value
// 	TreeNode *addVecConstant(NuVTypes::DataType type, TreeNode *value, TreeNode *value2, TreeNode *value3);
	// Search for variable in current scope
	bool isVariableInScope(const char *name, NuVariable *&result);
	// Wrap named variable (and array index)
	TreeNode *wrapVariable(NuVariable *var, TreeNode *arrayindex = NULL);


	/*
	// Paths	
	*/
	private:
	// Whether the next token to expect is a path step
	bool expectPathStep_;

	public:
	// Flag that the next token to expect is a path step
	void setExpectPathStep(bool b);
	// Whether to treat the next alphanumeric token as a path step variable
	bool expectPathStep();
	// Create a new path on the stack with the specified base 'variable'
	TreeNode *createPath(TreeNode *var);
	// Expand topmost path
	bool expandPath(Dnchar *name, TreeNode *arrayindex = NULL);
	// Finalise and remove the topmost path on the stack
	TreeNode *finalisePath();
};

#endif
