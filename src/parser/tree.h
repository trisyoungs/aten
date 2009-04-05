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
#include "parser/filterdata.h"
#include "parser/returnvalue.h"
#include "parser/variable.h"
#include "nucommand/commands.h"
#include "templates/namemap.h"
#include "templates/list.h"
#include "templates/reflist.h"
#include "base/dnchar.h"
#include "base/elements.h"
#include "base/lineparser.h"
#include "base/vtypes.h"

// Forward declarations
class TreeNode;
class ScopeNode;
class VariableNode;
class StepNode;
class Forest;

// Tree
class Tree
{
	public:
	// Constructor / Destructor
	Tree();
	virtual ~Tree();
	// List pointers
	Tree *prev, *next;
	// Friend class (to allow access to node generation calls
	friend class NuParser;


	/*
	// Forest parent
	*/
	private:
	// Parent
	Forest *parent_;

	public:
	// Set parent
	void setParent(Forest *f);
	// Return parent
	Forest *parent();

	/*
	// Node Data
	*/
	private:
	// Clear all data contained in the Tree
	void clear();
	// (Re)Initialise the tree read for node addition
	void initialise();
	// Node list - a disordered list of all nodes owned by the Tree
	List<TreeNode> nodes_;
	// Reflist of all statements in the Tree, to be executed sequentially
	Reflist<TreeNode,int> statements_;
	// Stack of ScopeNodes
	Reflist<ScopeNode,int> scopeStack_;
	// Stack of variable paths (and last added stepnode)
	Reflist<VariableNode,TreeNode*> pathStack_;
	// Number of syntactic errors encountered
	int nErrors_;
	// Check unary operator type compatibility
	NuVTypes::DataType checkUnaryOperatorTypes(NuCommand::Function func, NuVTypes::DataType type);
	// Check binary operator type compatibility
	NuVTypes::DataType checkBinaryOperatorTypes(NuCommand::Function func, NuVTypes::DataType type1, NuVTypes::DataType type2);

	public:
	// Create a new path on the stack with the specified base 'variable'
	virtual TreeNode *createPath(TreeNode *var);
	// Expand topmost path
	virtual bool expandPath(Dnchar *name, TreeNode *arrayindex = NULL);
	// Finalise and remove the topmost path on the stack
	virtual TreeNode *finalisePath();


	/*
	// Statement / Command Addition
	*/
	public:
	// Add a node representing a whole statement to the execution list
	virtual bool addStatement(TreeNode *leaf);
	// Add an operator to the Tree
	virtual TreeNode *addOperator(NuCommand::Function func, int typearg, TreeNode *arg1, TreeNode *arg2 = NULL);
	// Add 'if' statement
	virtual TreeNode *addIf(TreeNode *condition, TreeNode *expr1, TreeNode *expr2 = NULL);
	// Add 'for' statement
	virtual TreeNode *addFor(TreeNode *init, TreeNode *condition, TreeNode *action, TreeNode *statements);
	// Associate a command-based leaf node to the Tree
	virtual TreeNode *addFunction(NuCommand::Function func, TreeNode *arglist);
	// Join two nodes together
	static TreeNode *joinArguments(TreeNode *arg1, TreeNode *arg2);
	// Join two commands together
	virtual TreeNode *joinCommands(TreeNode *node1, TreeNode *node2);
	// Add on a new scope to the stack
	virtual TreeNode *pushScope();
	// Pop the topmost scope node
	virtual bool popScope();
	// Print statement info
	void print();


	/*
	// Variables / Constants
	*/
	private:
	// Current variable type to use for creating variables
	NuVTypes::DataType declaredType_;
	// Flag to indicate that we are assigning in a declaration, and the whole variable scope should be searched
	bool declarationAssignment_;

	public:
	// Set current type for variable declarations
	virtual bool setDeclaredVariableType(NuVTypes::DataType type);
	// Set declarations assignment flag
	virtual bool setDeclarationAssignment(bool b);
	// Add constant value to tompost scope
	virtual TreeNode *addConstant(NuVTypes::DataType type, Dnchar *token);
	// Add variable to topmost ScopeNode
	virtual TreeNode *addVariable(NuVTypes::DataType type, Dnchar *name, TreeNode *initialValue = NULL);
	// Add variable to topmost ScopeNode using the most recently declared type
	virtual TreeNode *addVariable(Dnchar *name, TreeNode *initialValue = NULL);
	// Add array variable to topmost ScopeNode using the most recently declared type
	virtual TreeNode *addArrayVariable(Dnchar *name, TreeNode *sizeexpr, TreeNode *initialvalue = NULL);
	// Add 'constant' vector value
// 	TreeNode *addVecConstant(NuVTypes::DataType type, TreeNode *value, TreeNode *value2, TreeNode *value3);
	// Search for variable in current scope
	virtual bool isVariableInScope(const char *name, NuVariable *&result);
	// Wrap named variable (and array index)
	virtual TreeNode *wrapVariable(NuVariable *var, TreeNode *arrayindex = NULL);


	/*
	// Filter Properties
	*/
	public:
	// Filter data
	FilterData filter;
	// Return whether this tree is a filter
	bool isFilter();


	/*
	// Execution
	*/
	private:
	// Read options for parser
	int readOptions_;
	// Current input stream target, in the form of a LineParser
	LineParser *parser_;

	public:
	// Add read option
	void addReadOption(LineParser::ParseOption po);
	// Remove read option
	void removeReadOption(LineParser::ParseOption po);
	// Return read options
	int readOptions();
	// Return the current LineParser pointer
	LineParser *parser();
	// Execute
	bool execute(NuReturnValue &rv);
	// Execute, opening specified file as input source (no return value)
	bool executeRead(const char *filename);
	// Execute, using specified parser as input source (no return value)
	bool executeRead(LineParser *parser);
	// Execute, with specified filename as data target
	bool executeWrite(const char *filename);
};

#endif
