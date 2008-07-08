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
#include "base/aten.h"
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

// Set (positive) colour for grid
int CommandData::function_CA_GRIDCOLOUR(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	obj.g->setPositiveColour(c->argd(0), c->argd(1), c->argd(2));
	if (c->hasArg(3)) obj.g->setTransparency(c->argd(3));
	return CR_SUCCESS;
}

// Set negative colour for grid
int CommandData::function_CA_GRIDCOLOURNEGATIVE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	obj.g->setNegativeColour(c->argd(0), c->argd(1), c->argd(2));
	if (c->hasArg(3)) obj.g->setTransparency(c->argd(3));
	return CR_SUCCESS;
}

// Set colour scale for grid
int CommandData::function_CA_GRIDCOLOURSCALE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	int cs = c->argi(0);
	if ((cs < 0) || (cs > 10))
	{
		msg.print("ColourScale %i is out of range (1-10, or 0 to use object's internal colour).\n",cs);
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

// Set grid cutoff 
int CommandData::function_CA_GRIDCUTOFF(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	obj.g->setCutoff(c->argd(0));
	return CR_SUCCESS;
}

// Set loop order to use in CA_ADDNEXTPOINT
int CommandData::function_CA_GRIDLOOPORDER(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	if (strlen(c->argc(0)) != 3)
	{
		msg.print("A string of three characters must be passed to 'gridlooporder' (got '%s').\n",c->argc(0));
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
				msg.print("Unrecognised character (%c) given to 'setgridlooporder' - default value used.\n",ch);
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

// Set drawing style of grid
int CommandData::function_CA_GRIDSTYLE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	Grid::SurfaceStyle ss = Grid::surfaceStyle(c->argc(0));
	if (ss != Grid::nSurfaceStyles) obj.g->setStyle(ss);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Set whether the grid has symmetric isovalues
int CommandData::function_CA_GRIDSYMMETRIC(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	obj.g->setSymmetric(c->argb(0));
	return CR_SUCCESS;
}

// Set transparency of grid
int CommandData::function_CA_GRIDTRANSPARENCY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	obj.g->setTransparency(c->argf(0));
	return CR_SUCCESS;
}

// Set whether 2D grid uses data value as height component
int CommandData::function_CA_GRIDUSEZ(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_GRID)) return CR_FAIL;
	obj.g->setUseDataForZ(c->argb(0));
	return CR_SUCCESS;
}

// Load grid ('loadgrid <filename>')
int CommandData::function_CA_LOADGRID(Command *&c, Bundle &obj)
{
	Filter *f = aten.probeFile(c->argc(0), Filter::GridImport);
	if (f != NULL)
	{
		if (f->execute(c->argc(0))) return CR_SUCCESS;
		else return CR_FAIL;
	} else return CR_FAIL;
}

// Create new grid
int CommandData::function_CA_NEWGRID(Command *&c, Bundle &obj)
{
	obj.g = aten.addGrid();
	obj.g->setName(stripTrailing(c->argc(0)));
	return CR_SUCCESS;
}
