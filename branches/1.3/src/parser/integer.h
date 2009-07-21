/*
	*** Integer Variable and Array
	*** src/parser/integer.h
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

#ifndef ATEN_INTEGERVARIABLE_H
#define ATEN_INTEGERVARIABLE_H

#include "parser/variable.h"
#include "parser/accessor.h"

// Integer Variable
class IntegerVariable : public Variable
{
	public:
	// Constructor / Destructor
	IntegerVariable(int i = 0, bool constant = FALSE);
	~IntegerVariable();

	/*
	// Set / Get
	*/
	public:
	// Return value of node
	bool execute(ReturnValue &rv);
	// Set from returnvalue node
	bool set(ReturnValue &rv);
	// Reset variable
	void reset();

	/*
	// Variable Data
	*/
	private:
	// Integer data
	int integerData_;
	// Print node contents
	void nodePrint(int offset, const char *prefix);
};

// Integer Array Variable
class IntegerArrayVariable : public ArrayVariable
{
	public:
	// Constructor / Destructor
	IntegerArrayVariable(TreeNode *sizeexpr, bool constant = FALSE);
	~IntegerArrayVariable();

	/*
	// Set / Get
	*/
	public:
	// Return value of node
	bool execute(ReturnValue &rv);
	// Return value of node as array
	bool executeAsArray(ReturnValue &rv, int arrayindex);
	// Set from returnvalue node
	bool set(ReturnValue &rv);
	// Set from returnvalue node as array
	bool setAsArray(ReturnValue &rv, int arrayindex);
	// Reset variable
	void reset();

	/*
	// Variable Data
	*/
	private:
	// Integer data
	int *integerArrayData_;
	// Print node contents
	void nodePrint(int offset, const char *prefix);

	public:
	// Return array pointer
	int *arrayData();

	/*
	// Inherited Virtuals
	*/
	public:
	// Initialise node (take over from Variable::initialise())
	bool initialise();
};

#endif

