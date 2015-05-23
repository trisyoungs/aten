/*
	*** Grid Point
	*** src/base/gridpoint.cpp
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

#include "base/gridpoint.h"

ATEN_USING_NAMESPACE

// Constructor
GridPoint::GridPoint() : ListItem<GridPoint>()
{
	// Private variables
	flag_ = 0;
	value_ = 0.0;
}

// Destructor
GridPoint::~GridPoint()
{
}

// Return coordinates of point
Vec3<double>& GridPoint::r()
{
	return r_;
}

// Return value at point
double GridPoint::value() const
{
	return value_;
}

// Set value at point
void GridPoint::setValue(double v)
{
	value_ = v;
}

// Retrieve flag status
int GridPoint::flag() const
{
	return flag_;
}

// Set flag status
void GridPoint::setFlag(int i)
{
	flag_ = i;
}
