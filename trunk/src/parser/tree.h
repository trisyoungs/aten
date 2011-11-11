/*
	*** Tree
	*** src/parser/tree.h
	Copyright T. Youngs 2007-2011

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
#include "parser/variablelist.h"
#include "parser/treegui.h"
#include "command/commands.h"
#include "base/dnchar.h"

// Forward declarations
class TreeNode;
class ScopeNode;
class VariableNode;
class StepNode;
class Program;

// Tree
class Tree
{
	public:
	// Constructor / Destructor
	Tree();
	~Tree();
	// List pointers
	Tree *prev, *next;
	// Tree Types
	enum TreeType { UnknownTree, CommandTree, FilterTree, FunctionTree, nTreeTypes };


	/*
	// Tree Character
	*/
	private:
	// Program parent
	Program *parent_;
	// Tree name (if any)
	Dnchar name_;
	// Type of tree
	Tree::TreeType type_;
	// Return type (used if defined as a function)
	VTypes::DataType returnType_;

	public:
	// Set parent
	void setParent(Program *prog);
	// Return parent
	Program *parent() const;
	// Set name of tree
	void setName(const char *s);
	// Return name of tree
	const char *name() const;
	// Set type
	void setType(Tree::TreeType type);
	// Return type
	Tree::TreeType type() const; 
	// Set return type of tree
	void setReturnType(VTypes::DataType dt);
	// Return return-type of tree
	VTypes::DataType returnType() const;
	// Reset Tree, ready for new statement(s) to be added
	void reset(bool clearVariables = TRUE);
	// Finalise the tree contents, searching for specific functions etc.
	bool finalise();


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
	// Argument list - if the tree is a function, this is the expected argument list
	List<TreeNode> arguments_;
	// Reflist of all statements in the Tree, to be executed sequentially
	Reflist<TreeNode,int> statements_;
	// Stack of ScopeNodes
	Reflist<ScopeNode,int> scopeStack_;
	// Stack of variable paths (and last added stepnode)
	Reflist<VariableNode,TreeNode*> pathStack_;
	// Number of syntactic errors encountered
	int nErrors_;
	// Check unary operator type compatibility
	VTypes::DataType checkUnaryOperatorTypes(Command::Function func, VTypes::DataType type, bool array, bool &returnsarray);
	// Check binary operator type compatibility
	VTypes::DataType checkBinaryOperatorTypes(Command::Function func, VTypes::DataType type1, bool array1, VTypes::DataType type2, bool array2, bool &returnsarray);
	// Check ternary operator type compatibility
	VTypes::DataType checkTernaryOperatorTypes(Command::Function func, VTypes::DataType type1, bool array1, VTypes::DataType type2, bool array2, VTypes::DataType type3, bool array3, bool &returnsarray);
	
	public:
	// Create a new path on the stack with the specified base 'variable'
	TreeNode *createPath(TreeNode *var);
	// Expand topmost path
	bool expandPath(Dnchar *name, TreeNode *arrayindex = NULL, TreeNode *arglist = NULL);
	// Finalise and remove the topmost path on the stack
	TreeNode *finalisePath();
	// Return number of arguments defined (for function)
	int nArgs() const;
	// Return first argument defined (for function)
	TreeNode *args() const;
	// Return first in stack of scopenodes
	Refitem<ScopeNode,int> *scopeNodes();
	

	/*
	// Statement / Command Addition
	*/
	public:
	// Add a node representing a whole statement to the execution list
	bool addStatement(TreeNode *leaf);
	// Add an operator to the Tree
	TreeNode *addOperator(Command::Function func, TreeNode *arg1, TreeNode *arg2 = NULL, TreeNode *arg3 = NULL);
	// Associate a command-based leaf node to the Tree
	TreeNode *addFunctionWithArglist(Command::Function func, TreeNode *arglist);
	// Add a function node to the list (overloaded to accept simple arguments instead of a list)
	TreeNode *addFunction(Command::Function func, TreeNode *a1 = NULL, TreeNode *a2 = NULL, TreeNode *a3 = NULL, TreeNode *a4 = NULL);
	// Associate a user-defined command-based leaf node to the Tree
	TreeNode *addUserFunction(Tree *func, TreeNode *arglist = NULL);
	// Add a declaration list
	TreeNode *addDeclarations(TreeNode *declist);
	// Join two nodes together
	static TreeNode *joinArguments(TreeNode *arg1, TreeNode *arg2);
	// Join two commands together
	TreeNode *joinCommands(TreeNode *node1, TreeNode *node2);
	// Add on a new scope to the stack
	TreeNode *pushScope(Command::Function func = Command::NoFunction);
	// Pop the topmost scope node
	bool popScope();
	// Print statement info
	void print();


	/*
	// Variables / Constants
	*/
	private:
	// Pointer to local ScopeNode (for functions)
	ScopeNode *localScope_;

	public:
	// Add constant value to tompost scope
	TreeNode *addConstant(VTypes::DataType type, Dnchar *token);
	// Add integer constant
	TreeNode *addConstant(int i);
	// Add double constant
	TreeNode *addConstant(double d);
	// Add string constant
	TreeNode *addConstant(const char *s);
	// Add Element constant
	TreeNode *addElementConstant(int el);
	// Add variable to topmost ScopeNode
	TreeNode *addVariable(VTypes::DataType type, Dnchar *name, TreeNode *initialValue = NULL, bool global = FALSE);
	// Add array variable to topmost ScopeNode
	TreeNode *addArrayVariable(VTypes::DataType type, Dnchar *name, TreeNode *sizeexpr, TreeNode *initialvalue = NULL, bool global = FALSE);
	// Add array 'constant'
	TreeNode *addArrayConstant(TreeNode *values);
	// Search for variable in current scope
	Variable *findVariableInScope(const char *name, int &scopelevel);
	// Wrap named variable (and array index)
	TreeNode *wrapVariable(Variable *var, TreeNode *arrayindex = NULL);
	// Return local scope's variable list
	const VariableList &localVariables() const;


	/*
	// Local Functions
	*/
	private:
	// User-defined local functions
	List<Tree> functions_;

	public:
	// Search for existing local function
	Tree *findLocalFunction(const char *name) const;
	// Add new local function
	Tree *addLocalFunction(const char *name);
	// Add list of variable arguments to topmost function
	bool addLocalFunctionArguments(TreeNode* arglist);


	/*
	// Filter Properties
	*/
	public:
	// Filter data
	FilterData filter;
	// Return whether this tree is a filter
	bool isFilter() const;


	/*
	// Qt/CLI GUI Definition
	*/
	private:
	// Default GUI dialog, run at start of Program execution
	TreeGui defaultDialog_;
	// Pointer to create function
	Tree *createDefaultDialogFunction_;
	// Whether createfunction has already been run
	bool defaultDialogCreated_;
	// List of additional, temporary dialogs created by the tree
	List<TreeGui> dialogs_;

	public:
	// Return default dialog structure
	TreeGui &defaultDialog();
	// Create and return new, temporary dialog
	TreeGui *createDialog(const char *title = NULL);
	// Delete specified temporary dialogs
	void deleteDialogs();


	/*
	// Execution
	*/
	private:
	// Read options for parser
	int readOptions_;
	// Current input stream target, in the form of a LineParser
	LineParser *parser_;
	// Flag to indicate that recent failure of this token is known and we should continue
	Command::Function acceptedFail_;
	// Number of times tree has been run
	int runCount_;

	public:
	// Add read option
	void addReadOption(LineParser::ParseOption po);
	// Remove read option
	void removeReadOption(LineParser::ParseOption po);
	// Return read options
	int readOptions() const;
	// Return the current LineParser pointer
	LineParser *parser();
	// Return whether the LineParser is ready for file reading
	bool isFileGoodForReading() const;
	// Return whether the LineParser is ready for file writing
	bool isFileGoodForWriting() const;
	// Set function for accepted fail
	void setAcceptedFail(Command::Function func);
	// Return function for accepted fail
	Command::Function acceptedFail() const;
	// Execute
	bool execute(ReturnValue &rv);
	// Execute, using specified parser as input/output source
	bool execute(LineParser *parser, ReturnValue &rv);
	// Execute, opening specified file as input source
	bool executeRead(const char *filename, ReturnValue &rv);
	// Execute, with specified filename as data target
	bool executeWrite(const char *filename, ReturnValue &rv);
	// Execute, opening specified file as input source (no return value)
	bool executeRead(const char *filename);
	// Execute, with specified filename as data target (no return value)
	bool executeWrite(const char *filename);
	// Return number of times tree has been run
	int runCount();
};

#endif
