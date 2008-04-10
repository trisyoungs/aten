/*
	*** Grid command functions
	*** src/command/grid.cpp
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

#include "command/commandlist.h"
#include "base/prefs.h"
#include "base/master.h"
#include "classes/grid.h"

// Add grid point data at specified indices
int CommandData::function_CA_ADDGRIDPOINT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	Vec3<int> veci = c->arg3i(0);
	obj.g->setData(veci.x-1, veci.y-1, veci.z-1, c->argd(3));
	return CR_SUCCESS;
}

// Add next gridpoint in sequence
int CommandData::function_CA_ADDNEXTGRIDPOINT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	obj.g->setNextData(c->argd(0));
	return CR_SUCCESS;
}

// Finalise current surface
int CommandData::function_CA_FINALISEGRID(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	if (prefs.coordsInBohr()) obj.g->bohrToAngstrom();
	return CR_SUCCESS;
}

// Create new grid
int CommandData::function_CA_NEWGRID(Command *&c, Bundle &obj)
{
	obj.g = master.addGrid();
	obj.g->setName(stripTrailing(c->argc(0)));
	return CR_SUCCESS;
}

// Set grid axes (nine doubles)
int CommandData::function_CA_GRIDAXES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	Mat3<double> mat;
	mat.set(0, c->arg3d(0));
	mat.set(1, c->arg3d(3));
	mat.set(2, c->arg3d(6));
	obj.g->setAxes(mat);
	return CR_SUCCESS;
}

// Set colour scale for grid
int CommandData::function_CA_GRIDCOLOURSCALE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	int cs = c->argi(0);
	if ((cs < 0) || (cs > 10))
	{
		msg(Debug::None,"ColourScale %i is out of range (1-10, or 0 to use object's internal colour).\n",cs);
		return CR_FAIL;
	}
	obj.g->setColourScale(cs-1);
	return CR_SUCCESS;
}

// Set cubic grid (one double)
int CommandData::function_CA_GRIDCUBIC(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	obj.g->setAxes(c->argd(0));
	return CR_SUCCESS;
}

// Set loop order to use in CA_ADDNEXTPOINT
int CommandData::function_CA_GRIDLOOPORDER(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	if (strlen(c->argc(0)) != 3)
	{
		msg(Debug::None,"A string of three characters must be passed to 'setgridlooporder'.\n");
		return CR_FAIL;
	}
	char ch;
	for (int n=0; n<3; n++)
	{
		ch = c->argc(0)[n];
		switch (ch)
		{
			case ('X'):
			case ('x'):
			case ('1'):
				obj.g->setLoopOrder(n,0);
				break;
			case ('Y'):
			case ('y'):
			case ('2'):
				obj.g->setLoopOrder(n,1);
				break;
			case ('Z'):
			case ('z'):
			case ('3'):
				obj.g->setLoopOrder(n,2);
				break;
			default:
				msg(Debug::None,"Unrecognised character (%c) given to 'setgridlooporder' - default value used.\n",ch);
				obj.g->setLoopOrder(n,n);
				break;
		}
	}
	return CR_SUCCESS;
}

// Set origin (lower-left-hand corner of grid)
int CommandData::function_CA_GRIDORIGIN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	obj.g->setOrigin(c->arg3d(0));
	return CR_SUCCESS;
}

// Set orthorhombic grid (three doubles)
int CommandData::function_CA_GRIDORTHO(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	obj.g->setAxes(c->arg3d(0));
	return CR_SUCCESS;
}

// Set extent of grid (number of points in each direction)
int CommandData::function_CA_GRIDSIZE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	obj.g->setNPoints(c->arg3i(0));
	return CR_SUCCESS;
}




