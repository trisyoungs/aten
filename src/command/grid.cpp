/*
	*** Grid Commands
	*** src/command/grid.cpp
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

#include "main/aten.h"
#include "command/commands.h"
#include "parser/commandnode.h"
#include "model/model.h"
#include "classes/grid.h"
#include "classes/prefs.h"
#include "base/sysfunc.h"

// Add free grid point data at specified coordinates
bool Command::function_AddFreePoint(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	obj.g->addFreePoint(c->argd(0), c->argd(1), c->argd(2), c->argd(3));
	rv.reset();
	return TRUE;
}

// Add grid point data at specified indices
bool Command::function_AddGridPoint(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	Vec3<int> veci = c->arg3i(0);
	obj.g->setData(veci.x-1, veci.y-1, veci.z-1, c->argd(3));
	rv.reset();
	return TRUE;
}

// Add next gridpoint in sequence
bool Command::function_AddNextGridPoint(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	obj.g->setNextData(c->argd(0));
	rv.reset();
	return TRUE;
}

// Return nth grid of model
bool Command::function_CurrentGrid(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Grid *g = NULL;
	switch (c->argType(0))
	{
		case (VTypes::IntegerData):
			g = obj.rs->grid(c->argi(0)-1);
			break;
		case (VTypes::GridData):
			g = (Grid*) c->argp(0, VTypes::GridData);
			break;
		default:
			msg.print("Can't convert a variable of type '%s' to a Grid.\n", VTypes::dataType(c->argType(0)));
		break;
	}
	if (g == NULL) return FALSE;
	obj.g = g;
	rv.set(VTypes::GridData, g);
	return TRUE;
}

// Finalise current surface
bool Command::function_FinaliseGrid(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	if (prefs.coordsInBohr()) obj.g->bohrToAngstrom();
	rv.reset();
	return TRUE;
}

// Return nth grid of model
bool Command::function_GetGrid(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Grid *g = NULL;
	switch (c->argType(0))
	{
		case (VTypes::IntegerData):
			g = obj.rs->grid(c->argi(0)-1);
			break;
		case (VTypes::GridData):
			g = (Grid*) c->argp(0, VTypes::GridData);
			break;
		default:
			msg.print("Can't convert a variable of type '%s' to a Grid.\n", VTypes::dataType(c->argType(0)));
			break;
	}
	if (g == NULL) return FALSE;
	rv.set(VTypes::GridData, g);
	return TRUE;
}

// Set transparency of primary and secondary grid surfaces
bool Command::function_GridAlpha(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	obj.g->setPrimaryAlpha(c->argGLf(0));
	obj.g->setSeondaryAlpha(c->argGLf(0));
	rv.reset();
	return TRUE;
}

// Set grid axes (nine doubles)
bool Command::function_GridAxes(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	Mat3<double> mat;
	mat.set(0, c->arg3d(0));
	mat.set(1, c->arg3d(3));
	mat.set(2, c->arg3d(6));
	obj.g->setAxes(mat);
	rv.reset();
	return TRUE;
}

// Set (positive) colour for grid
bool Command::function_GridColourPrimary(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	if (c->hasArg(3)) obj.g->setPrimaryColour(c->argd(0), c->argd(1), c->argd(2), c->argd(3));
	else obj.g->setPrimaryColour(c->argd(0), c->argd(1), c->argd(2));
	rv.reset();
	return TRUE;
}

// Set negative colour for grid
bool Command::function_GridColourSecondary(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	if (c->hasArg(3)) obj.g->setSecondaryeColour(c->argd(0), c->argd(1), c->argd(2), c->argd(3));
	else obj.g->setSecondaryeColour(c->argd(0), c->argd(1), c->argd(2));
	rv.reset();
	return TRUE;
}

// Set colour scale for grid
bool Command::function_GridColourscale(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	int cs = c->argi(0);
	if ((cs < 0) || (cs > 10))
	{
		msg.print("ColourScale %i is out of range (1-10, or 0 to use object's internal colour).\n",cs);
		return FALSE;
	}
	obj.g->setColourScale(cs-1);
	rv.reset();
	return TRUE;
}

// Set cubic grid (one double)
bool Command::function_GridCubic(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	obj.g->setAxes(c->argd(0));
	rv.reset();
	return TRUE;
}

// Set grid cutoff 
bool Command::function_GridCutoff(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	obj.g->setCutoff(c->argd(0));
	rv.reset();
	return TRUE;
}

// Set loop order to use in 'gridnextpoint'
bool Command::function_GridLoopOrder(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	if (strlen(c->argc(0)) != 3)
	{
		msg.print("A string of three characters must be passed to 'gridlooporder' (got '%s').\n",c->argc(0));
		return FALSE;
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
	rv.reset();
	return TRUE;
}

// Set origin (lower-left-hand corner of grid)
bool Command::function_GridOrigin(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	obj.g->setOrigin(c->arg3d(0));
	rv.reset();
	return TRUE;
}

// Set orthorhombic grid (three doubles)
bool Command::function_GridOrtho(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	obj.g->setAxes(c->arg3d(0));
	rv.reset();
	return TRUE;
}

// Set whether to draw the secondary grid surfce
bool Command::function_GridSecondary(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	if (c->hasArg(0)) obj.g->setUseSecondary(c->argb(0));
	rv.set(obj.g->useSecondary());
	return TRUE;
}

// Set drawing style of grid
bool Command::function_GridStyle(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	Grid::SurfaceStyle ss = Grid::surfaceStyle(c->argc(0));
	if (ss == Grid::nSurfaceStyles) return FALSE;
	obj.g->setStyle(ss);
	rv.reset();
	return TRUE;
}

// Set whether 2D grid uses data value as height component
bool Command::function_GridUseZ(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	if (c->hasArg(0)) obj.g->setUseDataForZ(c->argb(0));
	rv.set(obj.g->useDataForZ());
	return TRUE;
}

// Set visibility of grid data
bool Command::function_GridVisible(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	if (c->hasArg(0)) obj.g->setVisible(c->argb(0));
	rv.set(obj.g->isVisible());
	return TRUE;
}

// Initialise grid, setting extent of grid (number of points in each direction)
bool Command::function_InitialiseGrid(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return FALSE;
	Grid::GridType gt = Grid::gridType(c->argc(0), TRUE);
	if (gt == Grid::nGridTypes) return FALSE;
	rv.set(obj.g->initialise(gt, c->arg3i(1)));
	return TRUE;
}

// Load grid ('loadgrid <filename>')
bool Command::function_LoadGrid(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	Grid *g = NULL;
	Tree *filter = aten.probeFile(c->argc(0), FilterData::GridImport);
	if (filter != NULL) if (filter->executeRead(c->argc(0))) g = obj.g;
	rv.set(VTypes::GridData, g);
	return TRUE;
}

// Create new grid in the current model
bool Command::function_NewGrid(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.g = aten.currentModel()->addGrid();
	obj.g->setName(stripTrailing(c->argc(0)));
	rv.set(VTypes::GridData, obj.g);
	return TRUE;
}

