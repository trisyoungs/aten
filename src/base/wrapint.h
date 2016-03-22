/*
	*** Wrapped Integer Class
	*** src/base/wrapint.h
	Copyright T. Youngs 2007-2016

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

#ifndef ATEN_WRAPINT_H
#define ATEN_WRAPINT_H

#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Wrapped Integer Variable
class WrapInt
{
	public:
	// Constructor
	WrapInt(int minvalue = 0, int maxvalue = 0);

	/*
	 * Data
	 */
	private:
	// Minimum integer value
	int minimum_;
	// Maximum integer value
	int maximum_;
	// Integer range
	int range_;
	// Value
	int value_;

	public:
	// Set limits
	void setLimits(int minvalue, int maxvalue);
	// Return value
	int value();


	/*
	 * Operators
	 */
	public:
	// Assignment
	void operator=(int i);
	// Binary addition
	int operator+(int i);
	// Binary subtraction
	int operator-(int i);
	// +=
	int& operator+=(int i);
	// Prefix increment
	int& operator++();
	// Prefix decrement
	int& operator--();
	// Conversion (int)
	operator int();
};

ATEN_END_NAMESPACE

#endif

