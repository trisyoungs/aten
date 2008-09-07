/*
	*** Variable Access Interface
	*** src/base/vaccess.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_VACCESS_H
#define ATEN_VACCESS_H

#include "templates/list.h"
#include "variables/variable.h"

// Forward declarations
class VariableList;

// Variable access class
class VAccess
{
	protected:
	// Add new accessor
	//void add(const char *name, VTypes::ArrayType lt, VTypes::DataType dt, bool readonly);

	public:
	// Add these accessors to 
	// Find and return accessor by name
	Variable *findAccessor(const char *name);
	// Dig down through accessor list to find the specified value
// 	bool dig(const char *refpath, VariableList &vlist, VResult &result);
	// Get pointer to data referenced by a VObject
// 	virtual void retrieveData(VObject source, int accessorid, VResult &result) = 0;
};

#endif
