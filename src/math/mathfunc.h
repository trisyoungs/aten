/*
	*** Math functions
	*** src/base/mathfunc.h
	Copyright T. Youngs 2007-2015

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

#ifndef ATEN_MATHFUNC_H
#define ATEN_MATHFUNC_H

// Mathematical functions
class AtenMath
{
	// Error functions
	public:
	static double erfc(double);
	static double erf(double);

	// Random number generation
	public:
	static double random();
	static int randomimax();
	static int randomi(int range);

	// Integer power function
	public:
	static int power(int i, int p);
};

#endif
