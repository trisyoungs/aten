/*
	*** Variable
	*** src/parser/variable.h
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

#ifndef ATEN_NUVARIABLE_H
#define ATEN_NUVARIABLE_H

#include "parser/treenode.h"
#include "base/dnchar.h"
#include "base/vtypes.h"
#include "templates/vector3.h"
#include <stdlib.h>

// Forward declaration

// Variable
class NuVariable : public TreeNode
{
	public:
	// Constructor / Destructor
	NuVariable();
	virtual ~NuVariable();
	// List pointers
	NuVariable *prev, *next;

	/*
	// Variable Character
	*/
	protected:
	// Name of the variable
	Dnchar name_;
	// Initial value of new variable
	TreeNode *initialValue_;

	public:
	// Set name of variable
	void setName(const char* s);
	// Get name of variable
	const char *name();
	// Set initial value expression
	void setInitialValue(TreeNode *node);
	// Return TreeNode corresponding to initial value
	TreeNode *initialValue();
	// Initialise variable
	bool initialise();
	// 
};

#endif
