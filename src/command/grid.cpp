/*
	*** Grid Commands
	*** src/command/grid.cpp
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

#include "command/commands.h"
#include "main/aten.h"
#include "parser/commandnode.h"
#include "base/grid.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Add free grid point data at specified coordinates
bool Commands::function_AddFreePoint(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	obj.g->addFreePoint(c->argd(0), c->argd(1), c->argd(2), c->argd(3));
	rv.reset();
	return true;
}

// Add grid point data at specified indices
bool Commands::function_AddGridPoint(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	Vec3<int> veci = c->arg3i(0);
	obj.g->setData(veci.x-1, veci.y-1, veci.z-1, c->argd(3));
	rv.reset();
	return true;
}

// Add next gridpoint in sequence
bool Commands::function_AddNextGridPoint(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	obj.g->setNextData(c->argd(0));
	rv.reset();
	return true;
}

// Return nth grid of model
bool Commands::function_CurrentGrid(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Grid* g = NULL;
	switch (c->argType(0))
	{
		case (VTypes::IntegerData):
			g = obj.rs()->grid(c->argi(0)-1);
			break;
		case (VTypes::GridData):
			g = (Grid*) c->argp(0, VTypes::GridData);
			break;
		default:
			Messenger::print("Can't convert a variable of type '%s' to a Grid.", VTypes::dataType(c->argType(0)));
		break;
	}
	if (g == NULL) return false;
	obj.g = g;
	rv.set(VTypes::GridData, g);
	return true;
}

// Finalise current grid
bool Commands::function_FinaliseGrid(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;

	// If this command is being run from a filter, set the output filter in the model.
	if (c->parent()->isFilter())
	{
		Tree* t = c->parent();
		obj.g->setFilename(c->parent()->parser()->inputFilename());
	}

	if (prefs.coordsInBohr()) obj.g->bohrToAngstrom();

	rv.reset();
	return true;
}

// Return nth grid of model
bool Commands::function_GetGrid(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Grid* g = NULL;
	switch (c->argType(0))
	{
		case (VTypes::IntegerData):
			g = obj.rs()->grid(c->argi(0)-1);
			break;
		case (VTypes::GridData):
			g = (Grid*) c->argp(0, VTypes::GridData);
			break;
		default:
			Messenger::print("Can't convert a variable of type '%s' to a Grid.", VTypes::dataType(c->argType(0)));
			break;
	}
	if (g == NULL) return false;
	rv.set(VTypes::GridData, g);
	return true;
}

// Set transparency of primary and secondary grid surfaces
bool Commands::function_GridAlpha(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	obj.g->setPrimaryAlpha(c->argGLf(0));
	obj.g->setSecondaryAlpha(c->argGLf(0));
	rv.reset();
	return true;
}

// Set grid axes (nine doubles)
bool Commands::function_GridAxes(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	Matrix mat;
	mat.setColumn(0, c->arg3d(0), 0.0);
	mat.setColumn(1, c->arg3d(3), 0.0);
	mat.setColumn(2, c->arg3d(6), 0.0);
	obj.g->setAxes(mat);
	rv.reset();
	return true;
}

// Set (positive) colour for grid
bool Commands::function_GridColour(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	if (c->hasArg(3)) obj.g->setPrimaryColour(c->argd(0), c->argd(1), c->argd(2), c->argd(3));
	else obj.g->setPrimaryColour(c->argd(0), c->argd(1), c->argd(2));
	rv.reset();
	return true;
}

// Set negative colour for grid
bool Commands::function_GridColourSecondary(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	if (c->hasArg(3)) obj.g->setSecondaryColour(c->argd(0), c->argd(1), c->argd(2), c->argd(3));
	else obj.g->setSecondaryColour(c->argd(0), c->argd(1), c->argd(2));
	rv.reset();
	return true;
}

// Set colour scale for grid
bool Commands::function_GridColourscale(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	int cs = c->argi(0);
	if ((cs < 0) || (cs > 10))
	{
		Messenger::print("ColourScale %i is out of range (1-10, or 0 to use object's internal colour).",cs);
		return false;
	}
	obj.g->setColourScale(cs-1);
	rv.reset();
	return true;
}

// Set cubic grid (one double)
bool Commands::function_GridCubic(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	obj.g->setAxes(c->argd(0));
	rv.reset();
	return true;
}

// Set primary grid cutoff
bool Commands::function_GridCutoff(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	obj.g->setLowerPrimaryCutoff(c->argd(0));
	if (c->hasArg(1)) obj.g->setUpperPrimaryCutoff(c->argd(1));
	rv.reset();
	return true;
}

// Set secondary grid cutoff
bool Commands::function_GridCutoffSecondary(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	obj.g->setLowerSecondaryCutoff(c->argd(0));
	if (c->hasArg(1)) obj.g->setUpperSecondaryCutoff(c->argd(1));
	rv.reset();
	return true;
}

// Set loop order to use in 'gridnextpoint'
bool Commands::function_GridLoopOrder(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	if (c->argc(0).length() != 3)
	{
		Messenger::print("A string of three characters must be passed to 'gridlooporder' (got '%s').", qPrintable(c->argc(0)));
		return false;
	}
	char ch;
	for (int n=0; n<3; n++)
	{
		ch = c->argc(0).at(n).toLatin1();
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
				Messenger::print("Unrecognised character (%c) given to 'setgridlooporder' - default value used.", ch);
				obj.g->setLoopOrder(n,n);
				break;
		}
	}
	rv.reset();
	return true;
}

// Set origin (lower-left-hand corner of grid)
bool Commands::function_GridOrigin(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	obj.g->setOrigin(c->arg3d(0));
	rv.reset();
	return true;
}

// Set orthorhombic grid (three doubles)
bool Commands::function_GridOrtho(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	obj.g->setAxes(c->arg3d(0));
	rv.reset();
	return true;
}

// Set whether to draw volume outline
bool Commands::function_GridOutline(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	obj.g->setOutlineVolume(c->argb(0));
	rv.reset();
	return true;
}

// Set whether grid is periodic
bool Commands::function_GridPeriodic(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	obj.g->setPeriodic(c->argb(0));
	rv.reset();
	return true;
}

// Set whether to draw the secondary grid surfce
bool Commands::function_GridSecondary(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	if (c->hasArg(0)) obj.g->setUseSecondary(c->argb(0));
	rv.set(obj.g->useSecondary());
	return true;
}

// Set drawing style of primary surface
bool Commands::function_GridStyle(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	Grid::SurfaceStyle ss = Grid::surfaceStyle(c->argc(0));
	if (ss == Grid::nSurfaceStyles) return false;
	obj.g->setPrimaryStyle(ss);
	rv.reset();
	return true;
}

// Set drawing style of secondary surface
bool Commands::function_GridStyleSecondary(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	Grid::SurfaceStyle ss = Grid::surfaceStyle(c->argc(0));
	if (ss == Grid::nSurfaceStyles) return false;
	obj.g->setSecondaryStyle(ss);
	rv.reset();
	return true;
}

// Set whether 2D grid uses data value as height component
bool Commands::function_GridUseZ(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	if (c->hasArg(0)) obj.g->setUseDataForZ(c->argb(0));
	rv.set(obj.g->useDataForZ());
	return true;
}

// Set view percentage for primary surface
bool Commands::function_GridViewPercentage(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	if (c->hasArg(0)) obj.g->setPrimaryCutoffAsViewPercentage(c->argd(0), c->hasArg(1) ? c->argb(1) : false);
	rv.set((obj.g->partialPrimarySum() / obj.g->totalAbsoluteSum())*100.0);
	return true;
}

// Set view percentage for secondary surface
bool Commands::function_GridViewPercentageSecondary(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	if (c->hasArg(0)) obj.g->setSecondaryCutoffAsViewPercentage(c->argd(0), c->hasArg(1) ? c->argb(1) : false);
	rv.set((obj.g->partialSecondarySum() / obj.g->totalAbsoluteSum())*100.0);
	return true;
}

// Set visibility of grid data
bool Commands::function_GridVisible(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	if (c->hasArg(0)) obj.g->setVisible(c->argb(0));
	rv.set(obj.g->isVisible());
	return true;
}

// Initialise grid, setting extent of grid (number of points in each direction)
bool Commands::function_InitialiseGrid(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::GridPointer)) return false;
	Grid::GridType gt = Grid::gridType(c->argc(0), true);
	if (gt == Grid::nGridTypes) return false;
	rv.set(obj.g->initialise(gt, c->arg3i(1)));
	return true;
}

// Load grid ('loadgrid <filename>')
bool Commands::function_LoadGrid(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Grid* g = NULL;
	Tree* filter = aten_.probeFile(c->argc(0), FilterData::GridImport);
	if (filter != NULL) if (filter->executeRead(c->argc(0))) g = obj.g;
	rv.set(VTypes::GridData, g);
	return true;
}

// Create new grid in the current model
bool Commands::function_NewGrid(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.g = aten_.currentModel()->addGrid();
	obj.g->setName(c->argc(0).trimmed());
	rv.set(VTypes::GridData, obj.g);
	return true;
}

