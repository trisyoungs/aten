/*
	*** Spacegroup generators
	*** src/base/generator.cpp
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

#include "base/generator.h"
#include "base/master.h"

// Useful Numbers
#define ONSX 0.16666666
#define ONTH 0.33333333
#define TWTH 0.66666667
#define FVSX 0.83333333

// Define spacegroup generators in the master
Generator Master::generators[] = {
	{ "x,y,z",		Mat3<double>( 1, 0, 0, 0, 1, 0, 0, 0, 1), Vec3<double>(   0,   0,   0) },
	{ "-x,-y,-z",		Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0,-1), Vec3<double>(   0,   0,   0) },
	{ "-x,y,-z",		Mat3<double>(-1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>(   0,   0,   0) },
	{ "-x,y+12,-z",		Mat3<double>(-1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>(   0, 0.5,   0) },
	{ "x+12,y+12,z",	Mat3<double>( 1, 0, 0, 0, 1, 0, 0, 0, 1), Vec3<double>( 0.5, 0.5,   0) },
	{ "x,-y,z",		Mat3<double>( 1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>(   0,   0,   0) },
	{ "x,-y,z+12",		Mat3<double>( 1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>(   0,   0, 0.5) },
	{ "-x,y,-z+12",		Mat3<double>(-1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>(   0,   0, 0.5) },
	{ "-x,y+12,-z+12",	Mat3<double>(-1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>(   0, 0.5, 0.5) },
	{ "-x,-y,z",		Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>(   0,   0,   0) },
	{ "-x,-y,z+12",		Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>(   0,   0, 0.5) },
	{ "-x+12,y+12,-z",	Mat3<double>(-1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>( 0.5, 0.5,   0) },
	{ "-x+12,-y,z+12",	Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>( 0.5,   0, 0.5) },
	{ "x,y+12,z+12",	Mat3<double>( 1, 0, 0, 0, 1, 0, 0, 0, 1), Vec3<double>(   0, 0.5, 0.5) },
	{ "x+12,y,z+12",	Mat3<double>( 1, 0, 0, 0, 1, 0, 0, 0, 1), Vec3<double>( 0.5,   0, 0.5) },
	{ "x+12,y+12,z+12",	Mat3<double>( 1, 0, 0, 0, 1, 0, 0, 0, 1), Vec3<double>( 0.5, 0.5, 0.5) },
	{ "x+12,-y,z",		Mat3<double>( 1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>( 0.5,   0,   0) },
	{ "x,-y+12,z+12",	Mat3<double>( 1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>(   0, 0.5, 0.5) },
	{ "x+12,-y,z+12",	Mat3<double>( 1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>( 0.5,   0, 0.5) },
	{ "x+12,-y+12,z",	Mat3<double>( 1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>( 0.5, 0.5,   0) },
	{ "x+12,-y+12,z+12",	Mat3<double>( 1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>( 0.5, 0.5, 0.5) },
	{ "x,-y+12,z",		Mat3<double>( 1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>(   0, 0.5,   0) },
	{ "x+14,-y+14,z+14",	Mat3<double>( 1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>(0.25,0.25,0.25) },
	{ "-x+12,-y+12,-z+12",	Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0,-1), Vec3<double>( 0.5, 0.5, 0.5) },
	{ "-x+12,-y+12,-z",	Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0,-1), Vec3<double>( 0.5, 0.5,   0) },
	{ "-x+12,-y,z",		Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>( 0.5,   0,   0) },
	{ "-x+12,y+12,-z+12",	Mat3<double>(-1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>( 0.5, 0.5, 0.5) },
	{ "-x+12,y,-z+12",	Mat3<double>(-1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>( 0.5,   0, 0.5) },
	{ "-x+12,-y+12,z",	Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>( 0.5, 0.5,   0) },
	{ "-x+12,-y+12,z+12",	Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>( 0.5, 0.5, 0.5) },
	{ "-x,-y+12,z+12",	Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>(   0, 0.5, 0.5) },
	{ "-x,-y+12,z",		Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0, 1), Vec3<double>(   0, 0.5,   0) },
	{ "-x,-y+12,-z+12",	Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0,-1), Vec3<double>(   0, 0.5, 0.5) },
	{ "-x+14,-y+14,-z+14",	Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0,-1), Vec3<double>(0.25,0.25,0.25) },
	{ "-y,x,z",		Mat3<double>( 0,-1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>(   0,   0,   0) },
	{ "-y,x,z+14",		Mat3<double>( 0,-1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>(   0,   0,0.25) },
	{ "-y,x,z+12",		Mat3<double>( 0,-1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>(   0,   0, 0.5) },
	{ "-y,x,z+34",		Mat3<double>( 0,-1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>(   0,   0,0.75) },
	{ "-y,x+12,z+14",	Mat3<double>( 0,-1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>(   0, 0.5,0.25) },
	{ "y,-x,-z",		Mat3<double>( 0, 1, 0,-1, 0, 0, 0, 0,-1), Vec3<double>(   0,   0,   0) },
	{ "-y+12,x+12,z",	Mat3<double>( 0,-1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>( 0.5, 0.5,   0) },
	{ "-y+12,x+12,z+12",	Mat3<double>( 0,-1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>( 0.5, 0.5, 0.5) },
	{ "-x,-y+12,-z+14",	Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0,-1), Vec3<double>(   0, 0.5,0.25) },
	{ "-y+12,x+12,z+14",	Mat3<double>( 0,-1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>( 0.5, 0.5,0.25) },
	{ "-x+12,y+12,-z+14",	Mat3<double>(-1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>( 0.5, 0.5,0.25) },
	{ "-y+12,x+12,z+34",	Mat3<double>( 0,-1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>( 0.5, 0.5,0.75) },
	{ "-x+12,y+12,-z+34",	Mat3<double>(-1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>( 0.5, 0.5,0.75) },
	{ "-x+12,y,-z+34",	Mat3<double>(-1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>( 0.5,   0,0.75) },
	{ "-x+12,y,-z+14",	Mat3<double>(-1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>( 0.5,   0,0.25) },
	{ "-y,x-y,z",		Mat3<double>( 0,-1, 0, 1,-1, 0, 0, 0, 1), Vec3<double>(   0,   0,   0) },
	{ "-y,x-y,z+13",	Mat3<double>( 0,-1, 0, 1,-1, 0, 0, 0, 1), Vec3<double>(   0,   0,ONTH) },
	{ "-y,x-y,z+23",	Mat3<double>( 0,-1, 0, 1,-1, 0, 0, 0, 1), Vec3<double>(   0,   0,TWTH) },
	{ "z,x,y",		Mat3<double>( 0, 0, 1, 1, 0, 0, 0, 1, 0), Vec3<double>(   0,   0,   0) },
	{ "-y,-x,-z",		Mat3<double>( 0,-1, 0,-1, 0, 0, 0, 0,-1), Vec3<double>(   0,   0,   0) },
	{ "y,x,-z",		Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0,-1), Vec3<double>(   0,   0,   0) },
	{ "-y,-x,-z+23",	Mat3<double>( 0,-1, 0,-1, 0, 0, 0, 0,-1), Vec3<double>(   0,   0,TWTH) },
	{ "-y,-x,-z+13",	Mat3<double>( 0,-1, 0,-1, 0, 0, 0, 0,-1), Vec3<double>(   0,   0,ONTH) },
	{ "-z,-y,-x",		Mat3<double>( 0, 0,-1, 0,-1, 0,-1, 0, 0), Vec3<double>(   0,   0,   0) },
	{ "-y,-x,z",		Mat3<double>( 0,-1, 0,-1, 0, 0, 0, 0, 1), Vec3<double>(   0,   0,   0) },
	{ "y,x,z",		Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>(   0,   0,   0) },
	{ "-y,-x,z+12",		Mat3<double>( 0,-1, 0,-1, 0, 0, 0, 0, 1), Vec3<double>(   0,   0, 0.5) },
	{ "y,x,z+12",		Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>(   0,   0, 0.5) },
	{ "z,y,x",		Mat3<double>( 0, 0, 1, 0, 1, 0, 1, 0, 0), Vec3<double>(   0,   0,   0) },
	{ "z+12,y+12,x+12",	Mat3<double>( 0, 0, 1, 0, 1, 0, 1, 0, 0), Vec3<double>( 0.5, 0.5, 0.5) },
	{ "-y,-x,-z+12",	Mat3<double>( 0,-1, 0,-1, 0, 0, 0, 0,-1), Vec3<double>(   0,   0, 0.5) },
	{ "y,x,-z+12",		Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0,-1), Vec3<double>(   0,   0, 0.5) },
	{ "-z+12,-y+12,-x+12",	Mat3<double>( 0, 0,-1, 0,-1, 0,-1, 0, 0), Vec3<double>( 0.5, 0.5, 0.5) },
	{ "x,y,-z",		Mat3<double>( 1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>(   0,   0,   0) },
	{ "y,x,-z+13",		Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0,-1), Vec3<double>(   0,   0,ONTH) },
	{ "y,x,-z+23",		Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0,-1), Vec3<double>(   0,   0,TWTH) },
	{ "x,y,-z+12",		Mat3<double>( 1, 0, 0, 0, 1, 0, 0, 0,-1), Vec3<double>(   0,   0, 0.5) },
	{ "y+12,x+12,-z+12",	Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0,-1), Vec3<double>( 0.5, 0.5, 0.5) },
	{ "y+34,x+14,-z+34",	Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0,-1), Vec3<double>(0.75,0.25,0.75) },
	{ "y+14,x+34,-z+34",	Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0,-1), Vec3<double>(0.25,0.75,0.75) },
	{ "y+34,x+14,-z+14",	Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0,-1), Vec3<double>(0.75,0.25,0.25) },
	{ "y+12,x+12,z+12",	Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>( 0.5, 0.5, 0.5) },
	{ "y+14,x+14,z+14",	Mat3<double>( 0, 1, 0, 1, 0, 0, 0, 0, 1), Vec3<double>(0.25,0.25,0.25) },
	{ "-x+34,-y+34,-z+34",	Mat3<double>(-1, 0, 0, 0,-1, 0, 0, 0,-1), Vec3<double>(0.75,0.75,0.75) }
};
