/*
	*** Model component functions
	*** src/model/component.cpp
	Copyright T. Youngs 2007-2011

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

// Set whether component is required in build
void Model::setComponentIsRequired(bool b)
{
	componentIsRequired_ = b;
}

// Return whether component is required in build
bool Model::componentIsRequired()
{
	return componentIsRequired_;
}

// Set target component partition for model
void Model::setComponentPartition(int id)
{
}

// Return region data for model
int Model::componentPartition()
{
}

// Set the Component's pattern
void Model::setComponentPattern(Pattern *p)
{
	componentPattern_ = p;
}

// Return the Component's pattern
Pattern *Model::componentPattern() const
{
	return componentPattern_;
}

// Set the requested number of molecules
void Model::setComponentPopulation(int i)
{
	componentPopulation_ = i;
}

// Return the requested number of molecules
int Model::componentPopulation() const
{
	return componentPopulation_;
}

// Set whether the component is a bulk component
void Model::setComponentIsBulk(bool b)
{
	componentIsBulk_ = b;
}

// Return whether component is a bulk component
bool Model::componentIsBulk()
{
	return componentIsBulk_;
}

// Set the requested density for the component
void Model::setComponentDensity(double d)
{
	componentDensity_ = d;
}

// Return the requested density for the component
double Model::componentDensity() const
{
	return componentDensity_;
}

// Set whether the component has free density or not
void Model::setComponentHasFreeDensity(bool b)
{
	componentHasFreeDensity_ = b;
}

// Return whether component has free density
bool Model::componentHasFreeDensity()
{
	return componentHasFreeDensity_;
}

// Set a specific move type for the Component
void Model::setComponentMoveAllowed(MonteCarlo::MoveType m, bool b)
{
	componentMoveAllowed_[m] = b;
}

// Set whether the Component may be translated
bool Model::componentMoveAllowed(MonteCarlo::MoveType m) const
{
	return componentMoveAllowed_;
}
