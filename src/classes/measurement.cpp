/*
	*** Geometry measurement
	*** src/classes/measurement.cpp
	Copyright T. Youngs 2007

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

#include "classes/measurement.h"
#include "classes/atom.h"

// Geometry types
int GT_natoms[GT_NITEMS] = { 0,2,3,4 };
int natoms_from_GT(geom_type gt)
	{ return GT_natoms[gt]; }

// Constructor
measurement::measurement()
{
	type = GT_NONE;
	for (int n=0; n<4; n++) atoms[n] = NULL;
	value = 0.0;
	next = NULL;
	prev = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_MEASUREMENT] ++;
	#endif
}

// Destructor
measurement::~measurement()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_MEASUREMENT] ++;
	#endif
}

