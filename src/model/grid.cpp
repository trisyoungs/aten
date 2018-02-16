/*
	*** Model grid functions
	*** src/model/grid.cpp
	Copyright T. Youngs 2007-2018

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
#include "base/grid.h"

ATEN_USING_NAMESPACE

// Return list of grids
Grid* Model::grids() const
{
	return grids_.first();
}

// Return number of grids loaded
int Model::nGrids() const
{
	return grids_.nItems();
}

// Return specified grid
Grid* Model::grid(int id)
{
	return grids_[id];
}

// Add new grid
Grid* Model::addGrid()
{
	Grid* grid = grids_.add();
	grid->setParent(this);
	return grid;
}

// Take ownership of existing grid
void Model::ownGrid(Grid* grid)
{
	grids_.own(grid);
	grid->setParent(this);
}

// Remove grid
void Model::removeGrid(Grid* xgrid)
{
	grids_.remove(xgrid);
}

// Update grid axis ordering based on current view
void Model::updateGridAxisOrdering()
{
	int n, ii, jj;

	// Determine which axis/axes overlap most with cardinal Z (i.e. perpendicular to current view)
	Vec3<int> dirs = Vec3<int>(-1, -1, -1);
	Vec3<int> signs = Vec3<int>(1, 1, 1);
	
	// Grab components of the view matrix column vectors in sequential directions/rows, finding the largest component each time
	for (n=0; n<3; ++n)
	{
		// Grab components of the view matrix column vectors in the (2-n) direction/row
		Vec3<double> components = modelViewMatrix().rowAsVec3(2-n);

		// Find next largest component (in a vector direction we haven't used yet...)
		int maxEl = -1;
		double maxVal = -1.0;
		for (ii=0; ii<3; ++ii)
		{
			// For direction 'ii', check that it has not yet been entered into the dirs array
			for (jj=0; jj<n; ++jj) if (dirs[jj] == ii) break;
			if (jj < n) continue;

			// Check absolute value of this element against our stored value
			if (fabs(components[ii]) > maxVal)
			{
				maxVal = fabs(components[ii]);
				maxEl = ii;
			}
		}

		dirs[n] = maxEl;
		signs[n] = components[dirs[n]] < 0 ? -1 : 1;
	}

	// Reorder last two loops so that we are always ordered in x-y-z precedence
	if (dirs.z < dirs.y)
	{
		ii = dirs.z;
		dirs.z = dirs.y;
		dirs.y = ii;
		ii = signs.z;
		signs.z = signs.y;
		signs.y = ii;
	}

	// Set for all grids...
	for (Grid* grid = grids_.first(); grid != NULL; grid = grid->next)
	{
		grid->setAxisLoopOrder(dirs);
		grid->setAxisLoopSigns(signs);
	}
}
