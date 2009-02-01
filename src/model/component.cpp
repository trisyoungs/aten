/*
	*** Model component functions
	*** src/model/component.cpp
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

// Set the Component's pattern
void Model::setComponentPattern(Pattern *p)
{
        componentPattern_ = p;
}

// Return the Component's pattern
Pattern *Model::componentPattern()
{
        return componentPattern_;
}

// Set the requested number of molecules
void Model::setNRequested(int i)
{
        nRequested_ = i;
}

// Return the requested number of molecules
int Model::nRequested()
{
        return nRequested_;
}

// Set a specific move type for the Component
void Model::setMoveAllowed(MonteCarlo::MoveType m, bool b)
{
        moveAllowed_[m] = b;
}

// Set whether the Component may be translated
bool Model::isMoveAllowed(MonteCarlo::MoveType m)
{
        return moveAllowed_[m];
}

