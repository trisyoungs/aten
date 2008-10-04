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
#include "variables/returnvalue.h"
#include "variables/variable.h"
#include "variables/variablelist.h"

// Variable access class
class VAccess
{
	public:
	// Constructor
	VAccess();

	protected:
	// VariableList in which accessors are stored
	VariableList accessors_;
	// Add new variable accessor
	Variable *addAccessor(const char *name, VTypes::DataType dt, bool readonly, int arraysize = -1);
	// Add new list accessor
	Variable *addListAccessor(const char *name, VTypes::DataType dt);
	// Add new list accessor
	Variable *addRefListAccessor(const char *name, VTypes::DataType dt);
	// Check array index in supplied step against target member
	bool checkIndex(int &index, AccessStep *indexsource, Variable *member);

	public:
	// Return address of VariableList
	VariableList *accessors();
	// Find and return named accessor value
	virtual bool retrieve(void *classptr, AccessStep *step, ReturnValue &rv)=0;
	// Find and set named accessor value
	virtual bool set(void *classptr, AccessStep *step, Variable *srcvar)=0;
	// Return 'id' (position in list) of supplied accessor
	int accessorId(Variable *accessor);
};

#endif
