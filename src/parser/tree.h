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
#include "templates/namemap.h"
#include "templates/list.h"
#include "templates/reflist.h"
#include "base/dnchar.h"
#include "base/elements.h"
#include "base/vtypes.h"

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
	// List pointers
	Tree *prev, *next;
	// Filter Types
	enum FilterType { ModelImport, TrajectoryImport, ExpressionImport, GridImport, ModelExport, TrajectoryExport, ExpressionExport, GridExport, nFilterTypes };
	static const char *filterType(FilterType ft);
	static FilterType filterType(const char *s);
	// Filter commands
	enum FilterCommmand { ExactCommand, ExtensionCommand, GlobCommand, IdCommand, NameCommand, NicknameCommand, WithinCommand, ZMapCommand, nFilterCommands };
	static FilterCommmand filterCommand(const char *s);
	static const char *filterCommand(FilterCommmand fc);


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
	public:
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


	/*
	// Filter Properties
	*/
	private:
	// Filter ID
	int id_;
	// Type of data the filter describes
	FilterType filterType_;
	// Long name of the filter
	Dnchar name_;
	// Nickname for the filter
	Dnchar nickname_;
	// File extension(s)
	List<Dnchar> extensions_;
	// List of 'within' specifications
	List< Namemap<int> > idStrings_;
	// File filter glob (for gui)
	Dnchar glob_;
	// Partner filter
	Tree *partner_;
	// Filter description
	Dnchar description_;
	// Filename alias list
	List<Dnchar> exactNames_;
	// Whether the file has an associated extension
	bool hasExtension_;
	// Whether separate zmapping has been defined
	bool hasZmapping_;
	// Type of element mapping to use
	ElementMap::ZmapType zmapping_;

	public:
	// Return the ID of the filter
	int id();
	// Return the descriptive name of the filter
	const char *name();
	// Return the short nickname of the filter
	const char *nickname();
	// Return the first file extension
	Dnchar *extensions();
	// Return the first alias
	Dnchar *exactNames();
	// Return the number of identifying strings defined
	int nIdStrings();
	// Return the first identifying text string
	Namemap<int> *idStrings();
	// Return whether filter has an extension
	bool hasExtension();
	// Set the partner filter
	void setPartner(Tree *partner);
	// Return the partner filter
	Tree *partner();
	// Return the file filter
	const char *glob();
	// Set the type of filter
	void setType(FilterType ft);
	// Return the type of filter
	FilterType type();
	// Return the long description of the filter (including glob)
	const char *description();


	/*
	// Execution
	*/
	private:
	// File source (if any)
	ifstream *inputFile_;
	// File destination (if any)
	ofstream *outputFile_;

	public:
	// Return whether a current input file is defined
	bool hasFileSource();
	// Execute
	bool execute(NuReturnValue &rv);
	// Execute, with specified file as data source (no return value)
	bool executeRead(const char *filename, ifstream *trajfile = NULL, bool trajheader = FALSE);
	// Execute, with specified file as data target
	bool executeWrite(const char *filename);
};

// Forest
class Forest
{
	public:
	// Constructor / Destructor
	Forest();
	~Forest();

	/*
	// Tree data
	*/
	private:
	// User-defined functions (local to this structure)
	List<Tree> functions_;
	// List of trees belonging to this forest
	List<Tree> trees_;

	public:
	// Clear contents of forest
	void clear();
	// Return number of trees in forest
	int nTrees();
	// Create a new tree
	Tree *createTree();
};

#endif
