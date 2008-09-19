/*
	*** Variable Return Value
	*** src/variables/returnvalue.h
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

#ifndef ATEN_RETURNVALUE_H
#define ATEN_RETURNVALUE_H

#include "variables/returnvalue.h"
#include "variables/integer.h"
#include "variables/character.h"
#include "variables/real.h"
#include "variables/pointer.h"
#include "base/vtypes.h"
// #include "base/parser.h"
// #include "base/dnchar.h"
// #include "templates/list.h"

// Forward declarations
class AccessStep;

// Variable return class
class ReturnValue
{
	public:
	// Constructor
	ReturnValue();

	private:
	// Data type contained in class
	VTypes::DataType type_;
	// Variable members for returns
	IntegerVariable valueI_;
	RealVariable valueR_;
	CharacterVariable valueC_;
	PointerVariable valueP_;

	public:
	// Reset data
	void reset();
	// Copy variable data from AccessStep
	void set(AccessStep *source);
	// Return local variable containing last stored value
	Variable *value();
	// Return pointer value from local PointerVariable
	void *asPointer();
};

#endif
