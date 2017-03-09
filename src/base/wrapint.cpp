/*
	*** Wrapped Integer Class
	*** src/base/wrapint.cpp
	Copyright T. Youngs 2007-2017

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
#include <stdio.h>

ATEN_USING_NAMESPACE

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
 * Operators
 */

// Assignment
WrapInt& WrapInt::operator=(int i)
{
	int j = (i - minimum_)%range_;
	if (j < 0) j+= range_;
	value_ = (j%range_) + minimum_;

	return *this;
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

// +=
int& WrapInt::operator+=(int i)
{
	int j = ((value_ + i) - minimum_)%range_;
	if (j < 0) j+= range_;
	value_ = (j%range_) + minimum_;
	return value_;
}

// Prefix increment
int& WrapInt::operator++()
{
	++value_;
	if (value_ > maximum_) value_ = minimum_;
	return value_;
}

// Prefix decrement
int& WrapInt::operator--()
{
	--value_;
	if (value_ < minimum_) value_ = maximum_;
	return value_;
}

// Conversion
WrapInt::operator int()
{
	return value_;
}
