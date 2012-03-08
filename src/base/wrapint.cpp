/*
	*** Wrapped Integer Class
	*** src/base/wrapint.cpp
	Copyright T. Youngs 2007-2012

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

#include "base/wrapint.h"

// Constructor
WrapInt::WrapInt(int minvalue, int maxvalue)
{
	minimum_ = minvalue;
	maximum_ = maxvalue;
	range_ = 1;
	value_ = 0;
}

// Set limits
void WrapInt::setLimits(int minval, int maxval)
{
	minimum_ = minval;
	maximum_ = maxval;
	range_ = (maxval - minval) + 1;
}

// Return value
int WrapInt::value()
{
	return value_;
}

/*
// Operators
*/

// Assignment
void WrapInt::operator=(int i)
{
	int j = (i - minimum_)%range_;
	if (j < 0) j+= range_;
	value_ = (j%range_) + minimum_;
}

// Binary addition
int WrapInt::operator+(int i)
{
	int j = ((value_ + i) - minimum_)%range_;
	if (j < 0) j+= range_;
	return (j%range_) + minimum_;
}

// Binary subtraction
int WrapInt::operator-(int i)
{
	int j = ((value_ - i) - minimum_)%range_;
	if (j < 0) j+= range_;
	return (j%range_) + minimum_;
}

// Conversion
WrapInt::operator int()
{
	return value_;
}
