/*
	*** Current Object
	*** src/main/current.cpp
	Copyright T. Youngs 2007-2017

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

ATEN_USING_NAMESPACE

// Return current object Bundle
Bundle& Aten::current()
{
	return current_;
}


// Set the active model
void Aten::setCurrentModel(Model* model)
{
	Messenger::enter("Aten::setCurrentModel");

	if (model == NULL)
	{
		current_.clear();
		Messenger::exit("Aten::setCurrentModel");
		return;
	}

	// Set Bundle pointers
	current_.m = model;
	current_.p = model->patterns();
	current_.g = model->grids();
	current_.i = NULL;

	// Its the current model, so it must be visible also... add to visible list
	setModelVisible(model, true);

	Messenger::exit("Aten::setCurrentModel");
}

// Return current active model for editing
Model* Aten::currentModel() const
{
	return current_.m;
}

// Return current active model for editing, accounting for trajectory frames
Model* Aten::currentModelOrFrame() const
{
	return (current_.m == NULL ? NULL : current_.m->renderSourceModel());
}

// Set current grid for editing
void Aten::setCurrentGrid(Grid* grid)
{
	current_.g = grid;
}

// Return current grid for editing
bool Aten::currentGrid(Grid*& grid) const
{
	grid = current_.g;

	// Check validity of object
	if (grid != NULL)
	{
#ifdef CHECKS
		if (Grid::objectValid(grid)) return true;
		else
		{
			printf("Internal Error: Current Grid object is no longer valid.\n");
			return false;
		}
#endif
	}
	
	return true;
}
