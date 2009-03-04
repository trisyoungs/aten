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
#include "nucommand/commands.h"
#include "templates/reflist.h"
#include "base/vtypes.h"
#include "base/dnchar.h"

// Forward declarations
class NuVariable;
class TreeNode;
class ScopeNode;
class PathNode;
class AccessNode;

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

	public:
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

	/*
	// Node Data
	*/
	private:
	// Node list - a disordered reflist of all nodes owned by the Tree
	Reflist<TreeNode,int> ownedNodes_;
	// Reflist of all statements in the Tree, to be executed sequentially
	Reflist<TreeNode,int> statements_;
	private:
	// Stack of ScopeNodes
	Reflist<ScopeNode,int> scopeStack_;
	// Stack of variable path nodes
	Reflist<TreeNode,int> pathStack_;
	// Number of syntactic errors encountered
	int nErrors_;

	/*
	// Statement / Command Addition
	*/
	public:
	// Add a node representing a whole statement to the execution list
	void addStatement(TreeNode *leaf);
	// Add an operator to the Tree
	TreeNode *addOperator(NuCommand::Function func, int typearg, TreeNode *arg1, TreeNode *arg2 = NULL);
	// Associate a command-based leaf node to the Tree
	TreeNode *addCommandLeaf(NuCommand::Function funcs, int nargs, ...);
	// Add an argument to the most recently pushed function on the stack
	static TreeNode *joinArguments(TreeNode *arg1, TreeNode *arg2);
	// Pop the most recent function leaf from the stack and own any stored arguments
	void finaliseFunction();
	// Add joiner
	TreeNode *addJoiner(TreeNode *node1, TreeNode *node2);

	/*
	// Variables / Constants
	*/
	public:
	// Add constant to topmost ScopeNode
	void addConstant(NuVariable *v);
	// Add variable to topmost ScopeNode
	bool addVariable(NuVTypes::DataType type, Dnchar *name, TreeNode *initialValue = NULL);
	// Add 'constant' vector value
	TreeNode *addVecConstant(NuVTypes::DataType type, TreeNode *value, TreeNode *value2, TreeNode *value3);
	// Search for variable in current scope
	NuVariable *isVariableInScope(const char *name);

	/*
	// Paths	
	*/
	private:
	// Nested path depth for lexer
	int pathDepth_;
	// Whether the next token to expect is a path step
	bool expectPathStep_;

	public:
	// Flag that the next token to expect is a path step
	void setExpectPathStep(bool b);
	// Whether to treat the next alphanumeric token as a path step variable
	bool expectPathStep();
	// Add node to path stack
	void pushPath(TreeNode *var);
	// Remove last node from path stack
	void popPath();
	// Return topmost path refitem on stack
	Refitem<TreeNode,int> *topPath();
	// Add (begin) a new path putting it on the stack
	TreeNode *addPath(TreeNode *rootvar, TreeNode *path);
	// Expand the topmost path on the stack
	AccessNode *searchAccessors(const char *s);
};

#endif
