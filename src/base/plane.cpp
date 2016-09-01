/*
	*** Plane
	*** src/base/plane.cpp
	Copyright T. Youngs 2016-2016

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

#include "base/plane.h"

ATEN_USING_NAMESPACE

// Constructor
Plane::Plane()
{
	// Private variables
	nVertices_ = 0;
}

// Clear data
void Plane::clear()
{
	nVertices_ = 0;
	v_[0].zero();
	v_[1].zero();
	v_[2].zero();
	v_[3].zero();
}

// Set vertex
void Plane::setVertex(int index, Vec3<double> v)
{
	v_[index] = v;
	if (index > nVertices_) nVertices_ = index;
}

// Set vertex triplet
void Plane::setVertices(Vec3<double> v1, Vec3<double> v2, Vec3<double> v3)
{
	v_[0] = v1;
	v_[1] = v2;
	v_[2] = v3;
	nVertices_ = 3;
}

// Set vertex quartet
void Plane::setVertices(Vec3<double> v1, Vec3<double> v2, Vec3<double> v3, Vec3<double> v4)
{
	v_[0] = v1;
	v_[1] = v2;
	v_[2] = v3;
	v_[3] = v4;
	nVertices_ = 4;
}

// Return specified vertex
Vec3<double> Plane::vertex(int index)
{
	return v_[index];
}

// Return number of vertices defined
int Plane::nVertices()
{
	return nVertices_;
}

// Set plane normal
void Plane::setNormal(Vec3<double> normal)
{
	normal_ = normal;
}

// Return plane normal
Vec3<double> Plane::normal()
{
	return normal_;
}

// Calculate and return vertex centroid
Vec3<double> Plane::centroid()
{
	Vec3<double> r;
	for (int n=0; n<nVertices_; ++n) r += v_[n];
	return r/nVertices_;
}
