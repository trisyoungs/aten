/*
	*** Partial Plane
	*** src/base/plane.h
	Copyright T. Youngs 2016-2018

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

#ifndef ATEN_PLANE_H
#define ATEN_PLANE_H

#include "templates/vector3.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
/* none */

// Plane Definition
class Plane
{
	public:
	// Constructor
	Plane();


	/*
	 * Definition
	 */
	private:
	// Vertices for plane (either 3- or 4-point)
	Vec3<double> v_[4];
	// Number of vertices defining plane
	int nVertices_;
	// Surface normal
	Vec3<double> normal_;

	public:
	// Clear data
	void clear();
	// Set vertex
	void setVertex(int index, Vec3<double> v);
	// Set vertex triplet
	void setVertices(Vec3<double> v1, Vec3<double> v2, Vec3<double> v3);
	// Set vertex quartet
	void setVertices(Vec3<double> v1, Vec3<double> v2, Vec3<double> v3, Vec3<double> v4);
	// Return specified vertex
	Vec3<double> vertex(int index);
	// Return number of vertices defined
	int nVertices();
	// Set plane normal
	void setNormal(Vec3<double> normal);
	// Return plane normal
	Vec3<double> normal();
	// Calculate and return vertex centroid
	Vec3<double> centroid();
};

ATEN_END_NAMESPACE

#endif
