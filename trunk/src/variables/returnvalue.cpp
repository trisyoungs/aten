/*
	*** Variable Return Value
	*** src/variables/returnvalue.cpp
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

#include "variables/returnvalue.h"
#include "variables/accessstep.h"
#include "base/messenger.h"
#include "base/sysfunc.h"
#include <stdlib.h>

// Constructor
ReturnValue::ReturnValue()
{
	// Private variables
	type_ = VTypes::NoData;
}

// Reset data
void ReturnValue::reset()
{
	type_ = VTypes::NoData;
}

// Copy variable data from AccessStep
void ReturnValue::set(AccessStep *source)
{
	type_ = source->type();
	switch (type_)
	{
		case (VTypes::NoData):
			msg.print("No datatype set for AccessStep - no data to copy!\n");
			break;
		case (VTypes::IntegerData):
			valueI_.set(source->asInteger());
			break;
		case (VTypes::RealData):
			valueR_.set(source->asDouble());
			break;
		case (VTypes::CharacterData):
			valueC_.set(source->asCharacter());
			break;
		case (VTypes::ConstVectorData):
			valueV_.set(source->asVector());
			break;
		default:
			valueP_.reset(source->asPointer(type_), type_);
			break;
	}
}

// Set from integer value
void ReturnValue::set(int i)
{
	type_ = VTypes::IntegerData;
	valueI_.set(i);
}

// Set from double value
void ReturnValue::set(double d)
{
	type_ = VTypes::RealData;
	valueR_.set(d);
}

// Set from character value
void ReturnValue::set(const char *c)
{
	type_ = VTypes::CharacterData;
	valueC_.set(c);
}

// Set from pointer value
void ReturnValue::set(void *ptr, VTypes::DataType dt)
{
	type_ = dt;
	valueP_.reset(ptr, type_);
}

// Set from vector value
void ReturnValue::set(Vec3<double> v)
{
	type_ = VTypes::ConstVectorData;
	valueV_.set(v);
}

// Return local variable containing last stored value
Variable *ReturnValue::value()
{
	Variable *result;
	switch (type_)
	{
		case (VTypes::NoData):
			msg.print("No datatype set for AccessStep - no data to return!\n");
			result = NULL;
			break;
		case (VTypes::IntegerData):
			result = &valueI_;
			break;
		case (VTypes::RealData):
			result = &valueR_;
			break;
		case (VTypes::CharacterData):
			result = &valueC_;
			break;
		case (VTypes::ConstVectorData):
			result = &valueV_;
			break;
		default:
			result = &valueP_;
			break;
	}
	return result;
}

// Return pointer value from local PointerVariable
void *ReturnValue::asPointer()
{
	if (type_ == VTypes::VectorData) return valueP_.asPointer(type_);
	if ((type_ < VTypes::AtomData) || (type_ >= VTypes::NoData)) msg.print("ReturnValue contains no pointer to return.\n");
	else return valueP_.asPointer(type_);
	return NULL;
}
