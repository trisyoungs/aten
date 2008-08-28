/*
	*** Variable Access Interface
	*** src/parse/vaccess.h
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
#include "parse/vobject.h"

// Forward declarations
class VariableList;

// Variable access class
class VAccess
{
	private:
	// List of variable reference definitions
	List<VObject> accessors_;

	public:
	// Add new accessor
	void addAccessor(const char *name, VObject::ListType lt, VObject::DataType datatype, void *address, bool readonly);
	// Find and return accessor by name
	VObject *accessor(const char *name);
	// Dig down through accessor list to find the specified value
	bool dig(const char *refpath, VariableList &vlist, VObject &result);
};

#endif
