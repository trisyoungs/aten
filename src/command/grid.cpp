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
int commanddata::function_CA_ADDGRIDPOINT(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_GRID)) return CR_FAIL;
	vec3<int> veci = c->arg3i(0);
	obj.g->set_data(veci.x-1, veci.y-1, veci.z-1, c->argd(3));
	return CR_SUCCESS;
}

// Add next gridpoint in sequence
int commanddata::function_CA_ADDNEXTGRIDPOINT(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_GRID)) return CR_FAIL;
	obj.g->set_next_data(c->argd(0));
	return CR_SUCCESS;
}

// Finalise current surface
int commanddata::function_CA_FINALISEGRID(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_GRID)) return CR_FAIL;
	if (prefs.get_coords_in_bohr()) obj.g->bohr_to_angstrom();
	return CR_SUCCESS;
}

// Create new grid
int commanddata::function_CA_NEWGRID(command *&c, bundle &obj)
{
	obj.g = master.add_grid();
	obj.g->set_name(strip_trailing(c->argc(0)));
	return CR_SUCCESS;
}

// Set grid axes (nine doubles)
int commanddata::function_CA_SETGRID(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_GRID)) return CR_FAIL;
	mat3<double> mat;
	mat.set(0, c->arg3d(0));
	mat.set(1, c->arg3d(3));
	mat.set(2, c->arg3d(6));
	obj.g->set_axes(mat);
	return CR_SUCCESS;
}

// Set cubic grid (one double)
int commanddata::function_CA_SETGRIDCUBIC(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_GRID)) return CR_FAIL;
	obj.g->set_axes(c->argd(0));
	return CR_SUCCESS;
}

// Set loop order to use in CA_ADDNEXTPOINT
int commanddata::function_CA_SETGRIDLOOPORDER(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_GRID)) return CR_FAIL;
	if (strlen(c->argc(0)) != 3)
	{
		msg(DM_NONE,"A string of three characters must be passed to 'setgridlooporder'.\n");
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
				obj.g->set_looporder(n,0);
				break;
			case ('Y'):
			case ('y'):
			case ('2'):
				obj.g->set_looporder(n,1);
				break;
			case ('Z'):
			case ('z'):
			case ('3'):
				obj.g->set_looporder(n,2);
				break;
			default:
				msg(DM_NONE,"Unrecognised character (%c) given to 'setgridlooporder' - default value used.\n",ch);
				obj.g->set_looporder(n,n);
				break;
		}
	}
	return CR_SUCCESS;
}

// Set origin (lower-left-hand corner of grid)
int commanddata::function_CA_SETGRIDORIGIN(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_GRID)) return CR_FAIL;
	obj.g->set_origin(c->arg3d(0));
	return CR_SUCCESS;
}

// Set orthorhombic grid (three doubles)
int commanddata::function_CA_SETGRIDORTHO(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_GRID)) return CR_FAIL;
	obj.g->set_axes(c->arg3d(0));
	return CR_SUCCESS;
}

// Set extent of grid (number of points in each direction)
int commanddata::function_CA_SETGRIDSIZE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_GRID)) return CR_FAIL;
	obj.g->set_npoints(c->arg3i(0));
	return CR_SUCCESS;
}




