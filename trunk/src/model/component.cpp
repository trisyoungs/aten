/*
	*** Model component functions
	*** src/model/component.cpp
	Copyright T. Youngs 2007-2012

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
#include "base/sysfunc.h"

// Insertion Policies
const char *InsertionPolicyKeywords[Model::nInsertionPolicies] = { "none", "number", "density", "both", "relative" };
Model::InsertionPolicy Model::insertionPolicy(const char *s, bool reportError)
{
	Model::InsertionPolicy pol = (Model::InsertionPolicy) enumSearch("insertion policy", Model::nInsertionPolicies, InsertionPolicyKeywords, s, reportError);
	if ((pol == Model::nInsertionPolicies) && reportError) enumPrintValid(Model::nInsertionPolicies,InsertionPolicyKeywords);
	return pol;
}
const char *Model::insertionPolicy(Model::InsertionPolicy pol)
{
	return InsertionPolicyKeywords[pol];
}

// Set the insertion policy for the component
void Model::setComponentInsertionPolicy(Model::InsertionPolicy policy)
{
	componentInsertionPolicy_ = policy;
}

// Return the insertion policy for the component
Model::InsertionPolicy Model::componentInsertionPolicy()
{
	return componentInsertionPolicy_;
}

// Set target component partition for model
void Model::setComponentPartition(int id)
{
	componentPartition_ = id;
}

// Return region data for model
int Model::componentPartition()
{
	return componentPartition_;
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

// Set whether the component is rotatable
void Model::setComponentRotatable(bool b)
{
	componentRotatable_ = b;
}

// Return whether the component is rotatable
bool Model::componentRotatable()
{
	return componentRotatable_;
}
