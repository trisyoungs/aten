/*
	*** Array Variable
	*** src/parser/array.h
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

#ifndef ATEN_NUARRAYVARIABLE_H
#define ATEN_NUARRAYVARIABLE_H

#include "parser/variable.h"
#include "parser/accessor.h"

// Array Variable
class ArrayVariable : public NuVariable
{
	public:
	// Constructors / Destructor
	ArrayVariable(NuVTypes::DataType arraytype, TreeNode *sizeexpr);
	~ArrayVariable();

	/*
	// Set / Get
	*/
	public:
	// Return value of node
	bool execute(NuReturnValue &rv);
	// Return value of node as array
	bool executeAsArray(NuReturnValue &rv, int arrayindex);
	// Set from returnvalue node
	bool set(NuReturnValue &rv);
	// Set from returnvalue node as array
	bool set(NuReturnValue &rv, int arrayindex);
	// Reset array
	void reset();

	/*
	// Variable Data
	*/
	private:
	// TreeNode determining array size on initialisation
	TreeNode *arraySizeExpression_;
	// Current array size
	int arraySize_;
	// Array data
	NuVariable **arrayData_;
	// Print node contents
	void nodePrint(int offset, const char *prefix);

	/*
	// Inherited Virtuals
	*/
	public:
	// Initialise node (take over from Variable::initialise())
	bool initialise();
};

#endif
