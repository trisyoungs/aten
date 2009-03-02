/*
	*** Variable Access Path
	*** src/parser/accesspath.h
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

#ifndef ATEN_NUACCESSPATH_H
#define ATEN_NUACCESSPATH_H

#include "parser/treenode.h"
#include "parser/accessstep.h"
// #include "base/vtypes.h"
// #include "base/parser.h"
// #include "base/dnchar.h"
#include "templates/list.h"

// Forward declarations
// class VariableList;
// class ReturnValue;

// Variable Access Path
class NuAccessPath : public TreeNode
{
	public:
	// Constructor / Destructor
	NuAccessPath();
	~NuAccessPath();

	private:
	// Variable 'path'
	List<NuAccessStep> path_;
	// Walk path and get/set/step final target variable
	bool walk(NuReturnValue &rv, NuVariable *srcvar, NuVTypes::DataType dt, int step);
	// Variable to contain character return value
	Dnchar charResult_;

	public:
	// Set path from character constant
	bool setPath(const char *path);
	// Set value of path
	bool set(NuResultValue &rv);
	// Get value of path
	bool asBool(NuResultValue &rv);
};

#endif
