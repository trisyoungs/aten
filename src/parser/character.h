/*
	*** String (Character) Variable and Array
	*** src/parser/character.h
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

#ifndef ATEN_STRINGVARIABLE_H
#define ATEN_STRINGVARIABLE_H

#include "parser/variable.h"
#include "parser/accessor.h"
#include "base/dnchar.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// String Variable
class StringVariable : public Variable
{
	public:
	// Constructor / Destructor
	StringVariable();
	StringVariable(const char* s, bool constant = FALSE);
	~StringVariable();


	/*
	// Set / Get
	*/
	public:
	// Return value of node
	bool execute(ReturnValue& rv);
	// Set from returnvalue node
	bool set(ReturnValue& rv);
	// Reset variable
	void reset();


	/*
	// Variable Data
	*/
	private:
	// Character data
	Dnchar stringData_;

	private:
	// Print node contents
	void nodePrint(int offset, const char* prefix = "");
};

// String Array Variable
class StringArrayVariable : public Variable
{
	public:
	// Constructor / Destructor
	StringArrayVariable(TreeNode* sizeexpr, bool constant = FALSE);
	~StringArrayVariable();


	/*
	// Set / Get
	*/
	public:
	// Return value of node
	bool execute(ReturnValue& rv);
	// Return value of node as array
	bool executeAsArray(ReturnValue& rv, int arrayIndex);
	// Set from returnvalue node
	bool set(ReturnValue& rv);
	// Set from returnvalue node as array
	bool setAsArray(ReturnValue& rv, int arrayIndex);
	// Reset variable
	void reset();


	/*
	// Variable Data
	*/
	private:
	// TreeNode determining array size on initialisation
	TreeNode* arraySizeExpression_;
	// Array size
	int arraySize_;
	// String data
	Dnchar* stringArrayData_;

	private:
	// Print node contents
	void nodePrint(int offset, const char* prefix);


	/*
	// Inherited Virtuals
	*/
	public:
	// Initialise node (take over from Variable::initialise())
	bool initialise();
};

ATEN_END_NAMESPACE

#endif
