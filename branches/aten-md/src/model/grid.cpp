/*
	*** Model grid functions
	*** src/model/grid.cpp
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

#include "model/model.h"
#include "classes/grid.h"

// Return list of surfaces
Grid *Model::grids() const
{
	return grids_.first();
}

// Return number of surfaces loaded
int Model::nGrids() const
{
	return grids_.nItems();
}

// Return specified surface
Grid *Model::grid(int id)
{
	return grids_[id];
}

// Add new surface
Grid *Model::addGrid()
{
	return grids_.add();
}

// Remove surface
void Model::removeGrid(Grid *xgrid)
{
	grids_.remove(xgrid);
}

// Request rerendering of all grid data
void Model::rerenderGrids()
{
	for (Grid *g = grids_.first(); g != NULL; g = g->next) g->requestRerender();
}
