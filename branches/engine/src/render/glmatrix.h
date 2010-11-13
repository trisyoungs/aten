/*
	*** Specialized OpenGL 4x4 Matrix class
	*** src/render/glmatrix.h
	Copyright T. Youngs 2007-2010

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

#ifndef ATEN_GLMATRIX4_H
#define ATEN_GLMATRIX4_H

// Prevent complaints for incorrect arguments to 'macro max()' and 'macro min()
#define NOMINMAX

#include <stdio.h>
#include <GL/gl.h>
#include "templates/vector3.h"
using namespace std;

// Column-major 4x4 matrix
class GLMatrix
{
	private:
	// Matrix
	GLdouble matrix_[16];


	/*
	// Operators
	*/
	GLMatrix operator*(const GLMatrix&) const;
	GLMatrix& operator*=(const Mat4<double>&);
	Vec3<double> operator*(const Vec3<double>&) const;


	/*
	// Methods
	*/
	public:
	// Reset the matrix to the identity
	void setIdentity();
	// Create rotation matrix about X
	void createRotationX(double angle);
	// Create XY rotation matrix
	void createRotationXY(double anglex, double angley);
	// Create rotation matrix about Y
	void createRotationY(double angle);
	// Create rotation matrix about Z
	void createRotationZ(double angle);
	// Create axis rotation quaternion
	void createRotationAxis(double ax, double ay, double az, double angle);
	// Apply a general scaling to the matrix (as glScaled would to)
	void scale(double scalex, double scaley, double scalez);
	// Apply an x-scaling to the matrix
	void scaleX(double scale);
	// Apply a y-scaling to the matrix
	void scaleY(double scale);
	// Apply a z-scaling to the matrix
	void scaleZ(double scale);
	// Apply a translation to the matrix (as glTranslated would to)
	void translate(double dx, double dy, double dz);
	// Prints the matrix to stdout
	void print() const;
	// Set the zero matrix
	void zero();
	// Return matrix array
	GLdouble *matrix();
};

#endif

