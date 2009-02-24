/*
	*** Tree Node
	*** src/parser/treenode.h
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

#ifndef ATEN_TREENODE_H
#define ATEN_TREENODE_H

//#include "base/vtypes.h"

// Forward declarations
//class CommandList;
class VariableList;

// Tree Node
class TreeNode
{
	public:
	// Constructor / Destructor
	TreeNode();
	~TreeNode();
	// List pointers
	TreeNode *prev, *next;

	/*
	// Execute
	*/
	public:
	void execute();

	/*
	// Argument Data
	*/
	private:
	// Add variable argument to reference list, given the name
	bool addArgument(int argid, Parser::ArgumentForm af = Parser::UnknownForm);
	// Variable list from which the command arguments were set
	VariableList *variableList_;
	// Argument list
	List<TreeNode> args_;

	public:
	// Add constant argument
	void addConstant(const char *s, bool forcechar = FALSE);
	// Add integer constant argument
	void addConstant(int i);
	// Add real constant argument
	void addConstant(double d);
	// Return number of arguments given to node
	int nArgs();
	// Return variable argument
	Variable *arg(int argno);
	// Return argument as character
	const char *argc(int argno);
	// Return argument as integer
	int argi(int argno);
	// Return argument as double
	double argd(int argno);
	// Return argument as float
	float argf(int argno);
	// Return argument as bool
	bool argb(int argno);
	// Return arguments as Vec3<double>
	Vec3<double> arg3d(int);
	// Return arguments as Vec3<float>
	Vec3<float> arg3f(int);
	// Return arguments as Vec3<int>
	Vec3<int> arg3i(int);
	// Return argument as pointer
	void *argp(int argno, VTypes::DataType );
	// Returns whether argument 'n' was provided
	bool hasArg(int argno);
	// Return variable type of argument
	VTypes::DataType argt(int argno);
};

#endif
