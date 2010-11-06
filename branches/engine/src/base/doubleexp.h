/*
	*** Double/Exponent class
	*** src/base/doubleexp.h
	Copyright T. Youngs 2007-2010

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

#ifndef ATEN_DOUBLEEXP_H
#define ATEN_DOUBLEEXP_H

// Mantissa/exponent class
class DoubleExp
{
	public:
	// Constructor / Destructor
	DoubleExp(double mantissa = 0.0, int exponent = 0);

	/*
	// Data
	*/
	private:
	// Mantissa
	double mantissa_;
	// Exponent
	int exponent_;
	// Value
	double value_;

	/*
	// Functions
	*/
	private:
	// Recalculate value from stored mantissa and exponent
	void recalculate();

	public:
	// Retrieve full, real value
	double value() const;
	// Set mantissa and exponent
	void set(double mantissa, int exponent);
	// Set from normal value
	void set(double value);
	// Set mantissa
	void setMantissa(double mantissa);
	// Return mantissa
	double mantissa() const;
	// Set exponent
	void setExponent(int exponent);
	// Return exponent
	int exponent() const;
	// Operator =
	void operator=(double d);
};

#endif

