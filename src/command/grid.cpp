/*
	*** Grid command functions
	*** src/command/grid.cpp
	Copyright T. Youngs 2007-2009

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

#include "main/aten.h"
#include "command/commandlist.h"
#include "model/model.h"
#include "classes/grid.h"
#include "classes/prefs.h"
#include "base/sysfunc.h"

// Add grid point data at specified indices
int Command::function_CA_ADDGRIDPOINT(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	Vec3<int> veci = c->arg3i(0);
	obj.g->setData(veci.x-1, veci.y-1, veci.z-1, c->argd(3));
	return Command::Success;
}

// Add next gridpoint in sequence
int Command::function_CA_ADDNEXTGRIDPOINT(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	obj.g->setNextData(c->argd(0));
	return Command::Success;
}

// Finalise current surface
int Command::function_CA_FINALISEGRID(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	if (prefs.coordsInBohr()) obj.g->bohrToAngstrom();
	return Command::Success;
}

// Set transparency of grid
int Command::function_CA_GRIDALPHA(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	obj.g->setAlpha(c->argf(0));
	return Command::Success;
}

// Set grid axes (nine doubles)
int Command::function_CA_GRIDAXES(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	Mat3<double> mat;
	mat.set(0, c->arg3d(0));
	mat.set(1, c->arg3d(3));
	mat.set(2, c->arg3d(6));
	obj.g->setAxes(mat);
	return Command::Success;
}

// Set (positive) colour for grid
int Command::function_CA_GRIDCOLOUR(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	obj.g->setPositiveColour(c->argd(0), c->argd(1), c->argd(2));
	if (c->hasArg(3)) obj.g->setAlpha(c->argd(3));
	return Command::Success;
}

// Set negative colour for grid
int Command::function_CA_GRIDCOLOURNEGATIVE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	obj.g->setNegativeColour(c->argd(0), c->argd(1), c->argd(2));
	if (c->hasArg(3)) obj.g->setAlpha(c->argd(3));
	return Command::Success;
}

// Set colour scale for grid
int Command::function_CA_GRIDCOLOURSCALE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	int cs = c->argi(0);
	if ((cs < 0) || (cs > 10))
	{
		msg.print("ColourScale %i is out of range (1-10, or 0 to use object's internal colour).\n",cs);
		return Command::Fail;
	}
	obj.g->setColourScale(cs-1);
	return Command::Success;
}

// Set cubic grid (one double)
int Command::function_CA_GRIDCUBIC(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	obj.g->setAxes(c->argd(0));
	return Command::Success;
}

// Set grid cutoff 
int Command::function_CA_GRIDCUTOFF(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	obj.g->setCutoff(c->argd(0));
	return Command::Success;
}

// Set loop order to use in CA_ADDNEXTPOINT
int Command::function_CA_GRIDLOOPORDER(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	if (strlen(c->argc(0)) != 3)
	{
		msg.print("A string of three characters must be passed to 'gridlooporder' (got '%s').\n",c->argc(0));
		return Command::Fail;
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
	return Command::Success;
}

// Set origin (lower-left-hand corner of grid)
int Command::function_CA_GRIDORIGIN(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	obj.g->setOrigin(c->arg3d(0));
	return Command::Success;
}

// Set orthorhombic grid (three doubles)
int Command::function_CA_GRIDORTHO(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	obj.g->setAxes(c->arg3d(0));
	return Command::Success;
}

// Set extent of grid (number of points in each direction)
int Command::function_CA_GRIDSIZE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	obj.g->setNPoints(c->arg3i(0));
	return Command::Success;
}

// Set drawing style of grid
int Command::function_CA_GRIDSTYLE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	Grid::SurfaceStyle ss = Grid::surfaceStyle(c->argc(0));
	if (ss != Grid::nSurfaceStyles) obj.g->setStyle(ss);
	else return Command::Fail;
	return Command::Success;
}

// Set whether the grid has symmetric isovalues
int Command::function_CA_GRIDSYMMETRIC(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	obj.g->setSymmetric(c->argb(0));
	return Command::Success;
}

// Set whether 2D grid uses data value as height component
int Command::function_CA_GRIDUSEZ(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::GridPointer)) return Command::Fail;
	obj.g->setUseDataForZ(c->argb(0));
	return Command::Success;
}

// Load grid ('loadgrid <filename>')
int Command::function_CA_LOADGRID(CommandNode *&c, Bundle &obj)
{
	Filter *f = aten.probeFile(c->argc(0), Filter::GridImport);
	if (f != NULL)
	{
		if (f->execute(c->argc(0))) return Command::Success;
		else return Command::Fail;
	} else return Command::Fail;
}

// Create new grid in the current model
int Command::function_CA_NEWGRID(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.g = aten.currentModel()->addGrid();
	obj.g->setName(stripTrailing(c->argc(0)));
	return Command::Success;
}
