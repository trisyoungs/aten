/*
	*** Column-Major (OpenGL-friendly) 4x4 Matrix class
	*** src/base/matrix.h
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

#ifndef ATEN_MATRIX_H
#define ATEN_MATRIX_H

// Prevent complaints for incorrect arguments to 'macro max()' and 'macro min()
#define NOMINMAX

#include <stdio.h>
#include "templates/vector3.h"
#include "templates/vector4.h"
#include <GL/gl.h>
using namespace std;

// Column-major 4x4 matrix
class Matrix
{
	private:
	// Matrix
	double matrix_[16];


	/*
	// Operators
	*/
	public:
	Matrix operator*(const Matrix &B) const;
	Vec3<double> operator*(const Vec3<double> &v) const;
	Vec4<double> operator*(const Vec4<double> &v) const;
	Matrix &operator*=(const Matrix &B);
	double &operator[](int);


	/*
	// Basic Set/Get
	*/
	public:
	// Reset the matrix to the identity
	void setIdentity();
	// Prints the matrix to stdout
	void print() const;
	// Set the zero matrix
	void zero();
	// Return matrix array
	double *matrix();
	// Return transpose of current matrix
	Matrix &transpose();
	// Calculate determinant
	double determinant();


	/*
	// Column Operations
	*/
	public:
	// Copy column contents to supplied Vec3
	Vec3<double> columnAsVec3(int col);
	// Copy column contents to supplied Vec4
	Vec4<double> columnAsVec4(int col);
	// Set specified column from supplied values
	void setColumn(int col, double x, double y, double z, double w);
	// Set specified column from supplied Vec3
	void setColumn(int col, Vec3<double> &vec, double w);
	// Set specified column from supplied Vec4
	void setColumn(int col, Vec4<double> &vec);
	// Calculate column magnitude
	double columnMagnitude(int column);
	// Multiply column by single value
	void multiplyColumn(int col, double d);


	/*
	// Rotations
	*/
	public:
	// Create rotation matrix about X
	void createRotationX(double angle);
	// Create XY rotation matrix
	void createRotationXY(double anglex, double angley);
	// Create rotation matrix about Y
	void createRotationY(double angle);
	// Create rotation matrix about Z
	void createRotationZ(double angle);
	// Create axis rotation quaternion
	void createRotationAxis(double ax, double ay, double az, double angle, bool normalise);
	// Apply rotation about X axis
	void applyRotationX(double angle);
	// Apply axis rotation quaternion
	void applyRotationAxis(double ax, double ay, double az, double angle, bool normalise);


	/*
	// Translations
	*/
	public:
	// Create a translation to the matrix (as glTranslated would to)
	void createTranslation(double dx, double dy, double dz);
	// Apply a translation to the matrix (as glTranslated would to)
	void applyTranslation(double dx, double dy, double dz);
	// Apply a translation to the matrix (as glTranslated would to)
	void applyTranslation(Vec3<double> vec);
	// Add a translation to the matrix
	void translate(double dx, double dy, double dz);


	/*
	// Scaling
	*/
	public:
	// Apply a general scaling to the matrix (as glScaled would to)
	void applyScaling(double scalex, double scaley, double scalez);
	// Apply an x-scaling to the matrix
	void applyScalingX(double scale);
	// Apply a y-scaling to the matrix
	void applyScalingY(double scale);
	// Apply a z-scaling to the matrix
	void applyScalingZ(double scale);


	/*
	// Misc
	*/
	public:
	// Apply rotational part of matrix to supplied vector
	Vec3<double> rotateVector(Vec3<double> &v);
	// Multiply against coordinates provided (in GLfloats)
	void multiply(GLfloat *r, GLfloat *transformed);
	// Remove translation and scaling parts, leaving rotation only
	void removeTranslationAndScaling();
};

#endif

