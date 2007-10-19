/*
	*** Surface filter functions
	*** src/file/surface.cpp
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

#include "file/filter.h"
#include "classes/surface.h"

// Surface commands
bool filter::do_surface(command_node<filter_command> *&fn)
{
	dbg_begin(DM_CALLS,"filter::do_surface");
	static vec3<int> veci;
	static vec3<double> vecd;
	static mat3<double> mat;
	bool result = TRUE;
	switch (fn->get_command())
	{
		// Add grid point data at specified indices
		case (FC_ADDGRIDPOINT):
			if (activesurface == NULL) break;
			veci = fn->get_vector3i(0);
			activesurface->set_data(veci.x, veci.y, veci.z, fn->datavar[3]->get_as_double());
			break;
		// Add next gridpoint in sequence
		case (FC_ADDNEXTGRIDPOINT):
			if (activesurface == NULL) break;
			activesurface->set_next_data(fn->datavar[0]->get_as_double());
			break;
		// Set grid axes (nine doubles)
		case (FC_SETGRID):
			if (activesurface == NULL) break;
			mat.set(0, fn->get_vector3d(0));
			mat.set(1, fn->get_vector3d(3));
			mat.set(2, fn->get_vector3d(6));
			activesurface->set_axes(mat);
			break;
		// Set cubic grid (one double)
		case (FC_SETGRIDCUBIC):
			if (activesurface == NULL) break;
			activesurface->set_axes(fn->datavar[0]->get_as_double());
			break;
		// Set origin (lower-left-hand corner of grid)
		case (FC_SETGRIDORIGIN):
			if (activesurface == NULL) break;
			activesurface->set_origin(fn->get_vector3d(0));
			break;
		// Set orthorhombic grid (three doubles)
		case (FC_SETGRIDORTHO):
			if (activesurface == NULL) break;
			activesurface->set_axes(fn->get_vector3d(0));
			break;
		// Set extent of grid (number of points in each direction)
		case (FC_SETGRIDSIZE):
			if (activesurface == NULL) break;
			activesurface->set_npoints(fn->get_vector3i(0));
			break;
		default:
			result = FALSE;
			break;
	}
	if (result) fn = fn->next;
	dbg_end(DM_CALLS,"filter::do_surface");
	return result;
}

