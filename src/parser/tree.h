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
#include "base/lineparser.h"
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
	static FilterType filterType(const char *s, bool quiet = FALSE);
	// Filter commands
	enum FilterOption { ExactOption, ExtensionOption, GlobOption, IdOption, NameOption, NicknameOption, SearchOption, WithinOption, ZMapOption, nFilterOptions };
	static FilterOption filterOption(const char *s);
	static const char *filterOption(FilterOption fo);
	// Friend class (to allow access to node generation calls
	friend class NuParser;

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
	// Number of lines to search when looking for any of the searchStrings_
	int nLinesToSearch_;
	// List of identifying search strings
	List<Dnchar> searchStrings_;
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
	ElementMap::ZMapType zMapType_;

	public:
	// Return whether this tree is a filter
	bool isFilter();
	// Set filter option
	virtual bool setFilterOption(Dnchar *name, TreeNode *value);
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
	// Return the number of lines to search for the identifying strings
	int nLinesToSearch();
	// Return the first identifying search string
	Dnchar *searchStrings();
	// Return whether filter has an extension
	bool hasExtension();
	// Set the partner filter
	void setPartner(Tree *partner);
	// Return the partner filter
	Tree *partner();
	// Return the file filter
	const char *glob();
	// Set the type of filter
	void setFilterType(FilterType ft);
	// Return the type of filter
	FilterType filterType();
	// Return the long description of the filter (including glob)
	const char *description();


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

// Forest
class Forest
{
	public:
	// Constructor / Destructor
	Forest();
	~Forest();
	// List pointers
	Forest *prev, *next;

	/*
	// Tree data
	*/
	private:
	// Name, if any
	Dnchar name_;
	// Original source filename, if any
	Dnchar filename_;
	// User-defined functions (local to this structure)
	List<Tree> functions_;
	// List of trees belonging to this forest
	List<Tree> trees_;

	public:
	// Clear contents of forest
	void clear();
	// Set name of forest
	void setName(const char *s);
	// Return name of forest
	const char *name();
	// Return associated filename (if any)
	const char *filename();
	// Generate forest from string 
	bool generate(const char *, const char *name = NULL);
	// Generate forest from input file
	bool generateFromFile(const char *filename, const char *name = NULL);
	// Finalise forest
	void finalise();
	// Return number of trees in forest
	int nTrees();
	// Create a new, generic (script or command) tree
	Tree *createTree();
	// Create a new file filter-style tree
	Tree *createFilter(Tree::FilterType ft);
	// Execute all trees in forest
	bool executeAll(NuReturnValue &rv);
	// Print forest information
	void print();
};

#endif
