/*
	*** 4x4 Matrix class
	*** src/templates/matrix4.h
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

#ifndef ATEN_MATRIX4_H
#define ATEN_MATRIX4_H

// Prevent complaints for incorrect arguments to 'macro max()' and 'macro min()
#define NOMINMAX

#include "templates/vector4.h"
#include <algorithm>
#include <math.h>
#include <stdio.h>
using namespace std;

// 4x4 matrix
template <class T> class Mat4
{
	public:
	// Constructor
	Mat4();
	Mat4(const Mat3<T>&);

	// 4x4 Matrix, consisting of four vec4's:
	//	{ x.x x.y x.z x.w }  rows[0]
	//	{ y.x y.y y.z y.w }  rows[1]
	//	{ z.x z.y z.z z.w }  rows[2]
	//	{ w.x w.y w.z w.w }  rows[3]
	public:
	// Vectors of matrix
	Vec4<T> rows[4];


	/*
	// Set / Adjust / Get
	*/
	public:
	// Aliases for matrix rows
	Vec4<T> &x();
	Vec4<T> &y();
	Vec4<T> &z();
	Vec4<T> &w();
	// Puts the matrix into the passed 1D-array of type <T>, column-major
	void copyColumnMajor(T*) const;
	// Puts the matrix into the passed 1D-array of type <T>, row-major
	void copyRowMajor(T*) const;
	// Set individual element of matrix
	void set(int row, int col, T value);
	// Set whole row of matrix
	void set(int row, T x, T y, T z, T w);
	// Set whole row of matrix
	void set(int row, Vec3<T> r, T w);
	// Set the matrix from a 1D array of values
	void setFromColumnMajor(T*);
	// Reset the matrix to the identity
	void setIdentity();
	// Get individual element of matrix
	T get(int row, int col);


	/*
	// Operators
	*/
	Mat4 operator*(const Mat4&) const;
	Mat4& operator*=(const Mat4&);
	Mat4& operator*=(const double);
	Vec4<T> operator*(const Vec4<T>&) const;
	Vec3<T> operator*(const Vec3<T>&) const;


	/*
	// Methods
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
	void createRotationAxis(double ax, double ay, double az, double angle);
	// Calculate the determinant of the matrix.
	double determinant() const;
	// Invert the matrix
	void invert();
	// Prints the matrix to stdout
	void print() const;
	// Set the zero matrix
	void zero();
};

// Constructor
template <class T> Mat4<T>::Mat4()
{
	setIdentity();
}

// Create from Mat3<>
template <class T> Mat4<T>::Mat4(const Mat3<T> &m)
{
	rows[0].set(m.rows[0], 0.0);
	rows[1].set(m.rows[1], 0.0);
	rows[2].set(m.rows[2], 0.0);
	rows[3].set(0.0, 0.0, 0.0, 1.0);
}

/*
// Set / Adjust / Get
*/

// Aliases for matrix rows
template <class T> Vec4<T> &Mat4<T>::x()
{
	return rows[0];
}

template <class T> Vec4<T> &Mat4<T>::y()
{
	return rows[1];
}

template <class T> Vec4<T> &Mat4<T>::z()
{
	return rows[2];
}

template <class T> Vec4<T> &Mat4<T>::w()
{
	return rows[3];
}

// Get (array)
template <class T> void Mat4<T>::copyColumnMajor(T *colm) const
{
	// Construct a 1d array of type T with column-major ordering...
	colm[0] = rows[0].x;	colm[4] = rows[0].y;	colm[8] = rows[0].z;	colm[12] = rows[0].w;
	colm[1] = rows[1].x;	colm[5] = rows[1].y;	colm[9] = rows[1].z;	colm[13] = rows[1].w;
	colm[2] = rows[2].x;	colm[6] = rows[2].y;	colm[10] = rows[2].z;	colm[14] = rows[2].w;
	colm[3] = rows[3].x;	colm[7] = rows[3].y;	colm[11] = rows[3].z;	colm[15] = rows[3].w;
}

// Get (array)
template <class T> void Mat4<T>::copyRowMajor(T *rowm) const
{
	// Construct a 1d array of type T with row-major ordering...
	rowm[0] = rows[0].x;	rowm[1] = rows[0].y;	rowm[2] = rows[0].z;	rowm[3] = rows[0].w;
	rowm[4] = rows[1].x;	rowm[5] = rows[1].y;	rowm[6] = rows[1].z;	rowm[7] = rows[1].w;
	rowm[8] = rows[2].x;	rowm[9] = rows[2].y;	rowm[10] = rows[2].z;	rowm[11] = rows[2].w;
	rowm[12] = rows[3].x;	rowm[13] = rows[3].y;	rowm[14] = rows[3].z;	rowm[15] = rows[3].w;
}

// Set individual element of matrix (by row/column)
template <class T> void Mat4<T>::set(int row, int col, T d)
{
	rows[row].set(col,d);
}

// Set individual row of matrix
template <class T> void Mat4<T>::set(int row, T x, T y, T z, T w)
{
	rows[row].set(x, y, z, w);
}

// Set whole row of matrix
template <class T> void Mat4<T>::set(int row, Vec3<T> v, T w)
{
	rows[row].set(v, w);
}

// Set (T[])
template <class T> void Mat4<T>::setFromColumnMajor(T *m)
{
	rows[0].set(m[0],m[4],m[8],m[12]);
	rows[1].set(m[1],m[5],m[9],m[13]);
	rows[2].set(m[2],m[6],m[10],m[14]);
	rows[3].set(m[3],m[7],m[11],m[15]);
}

// Reset to the identity matrix
template <class T> void Mat4<T>::setIdentity()
{
	rows[0].set(1,0,0,0);
	rows[1].set(0,1,0,0);
	rows[2].set(0,0,1,0);
	rows[3].set(0,0,0,1);
}

// Set zero matrix
template <class T> void Mat4<T>::zero()
{
	rows[0].zero();
	rows[1].zero();
	rows[2].zero();
	rows[3].zero();
}

// Get individual element of matrix (by row/column)
template <class T> T Mat4<T>::get(int row, int col)
{
	return rows[row].get(col);
}

/*
// Operators
*/

// Matrix multiply (operator *) (return new matrix)
template <class T> Mat4<T> Mat4<T>::operator*(const Mat4<T> &B) const
{
	// [ row(A|this).column(B) ]
	Mat4 AB;
	AB.rows[0].x = rows[0].x*B.rows[0].x + rows[0].y*B.rows[1].x + rows[0].z*B.rows[2].x + rows[0].w*B.rows[3].x;
	AB.rows[1].x = rows[1].x*B.rows[0].x + rows[1].y*B.rows[1].x + rows[1].z*B.rows[2].x + rows[1].w*B.rows[3].x;
	AB.rows[2].x = rows[2].x*B.rows[0].x + rows[2].y*B.rows[1].x + rows[2].z*B.rows[2].x + rows[2].w*B.rows[3].x;
	AB.rows[3].x = rows[3].x*B.rows[0].x + rows[3].y*B.rows[1].x + rows[3].z*B.rows[2].x + rows[3].w*B.rows[3].x;

	AB.rows[0].y = rows[0].x*B.rows[0].y + rows[0].y*B.rows[1].y + rows[0].z*B.rows[2].y + rows[0].w*B.rows[3].y;
	AB.rows[1].y = rows[1].x*B.rows[0].y + rows[1].y*B.rows[1].y + rows[1].z*B.rows[2].y + rows[1].w*B.rows[3].y;
	AB.rows[2].y = rows[2].x*B.rows[0].y + rows[2].y*B.rows[1].y + rows[2].z*B.rows[2].y + rows[2].w*B.rows[3].y;
	AB.rows[3].y = rows[3].x*B.rows[0].y + rows[3].y*B.rows[1].y + rows[3].z*B.rows[2].y + rows[3].w*B.rows[3].y;

	AB.rows[0].z = rows[0].x*B.rows[0].z + rows[0].y*B.rows[1].z + rows[0].z*B.rows[2].z + rows[0].w*B.rows[3].z;
	AB.rows[1].z = rows[1].x*B.rows[0].z + rows[1].y*B.rows[1].z + rows[1].z*B.rows[2].z + rows[1].w*B.rows[3].z;
	AB.rows[2].z = rows[2].x*B.rows[0].z + rows[2].y*B.rows[1].z + rows[2].z*B.rows[2].z + rows[2].w*B.rows[3].z;
	AB.rows[3].z = rows[3].x*B.rows[0].z + rows[3].y*B.rows[1].z + rows[3].z*B.rows[2].z + rows[3].w*B.rows[3].z;

	AB.rows[0].w = rows[0].x*B.rows[0].w + rows[0].y*B.rows[1].w + rows[0].z*B.rows[2].w + rows[0].w*B.rows[3].w;
	AB.rows[1].w = rows[1].x*B.rows[0].w + rows[1].y*B.rows[1].w + rows[1].z*B.rows[2].w + rows[1].w*B.rows[3].w;
	AB.rows[2].w = rows[2].x*B.rows[0].w + rows[2].y*B.rows[1].w + rows[2].z*B.rows[2].w + rows[2].w*B.rows[3].w;
	AB.rows[3].w = rows[3].x*B.rows[0].w + rows[3].y*B.rows[1].w + rows[3].z*B.rows[2].w + rows[3].w*B.rows[3].w;
	return AB;
}

// Matrix multiply (operator *=)
template <class T> Mat4<T> &Mat4<T>::operator*=(const Mat4<T> &B)
{
	// [ row(A|this).column(B) ]
	Mat4 AB;
	AB.rows[0].x = rows[0].x*B.rows[0].x + rows[0].y*B.rows[1].x + rows[0].z*B.rows[2].x + rows[0].w*B.rows[3].x;
	AB.rows[1].x = rows[1].x*B.rows[0].x + rows[1].y*B.rows[1].x + rows[1].z*B.rows[2].x + rows[1].w*B.rows[3].x;
	AB.rows[2].x = rows[2].x*B.rows[0].x + rows[2].y*B.rows[1].x + rows[2].z*B.rows[2].x + rows[2].w*B.rows[3].x;
	AB.rows[3].x = rows[3].x*B.rows[0].x + rows[3].y*B.rows[1].x + rows[3].z*B.rows[2].x + rows[3].w*B.rows[3].x;

	AB.rows[0].y = rows[0].x*B.rows[0].y + rows[0].y*B.rows[1].y + rows[0].z*B.rows[2].y + rows[0].w*B.rows[3].y;
	AB.rows[1].y = rows[1].x*B.rows[0].y + rows[1].y*B.rows[1].y + rows[1].z*B.rows[2].y + rows[1].w*B.rows[3].y;
	AB.rows[2].y = rows[2].x*B.rows[0].y + rows[2].y*B.rows[1].y + rows[2].z*B.rows[2].y + rows[2].w*B.rows[3].y;
	AB.rows[3].y = rows[3].x*B.rows[0].y + rows[3].y*B.rows[1].y + rows[3].z*B.rows[2].y + rows[3].w*B.rows[3].y;

	AB.rows[0].z = rows[0].x*B.rows[0].z + rows[0].y*B.rows[1].z + rows[0].z*B.rows[2].z + rows[0].w*B.rows[3].z;
	AB.rows[1].z = rows[1].x*B.rows[0].z + rows[1].y*B.rows[1].z + rows[1].z*B.rows[2].z + rows[1].w*B.rows[3].z;
	AB.rows[2].z = rows[2].x*B.rows[0].z + rows[2].y*B.rows[1].z + rows[2].z*B.rows[2].z + rows[2].w*B.rows[3].z;
	AB.rows[3].z = rows[3].x*B.rows[0].z + rows[3].y*B.rows[1].z + rows[3].z*B.rows[2].z + rows[3].w*B.rows[3].z;

	AB.rows[0].w = rows[0].x*B.rows[0].w + rows[0].y*B.rows[1].w + rows[0].z*B.rows[2].w + rows[0].w*B.rows[3].w;
	AB.rows[1].w = rows[1].x*B.rows[0].w + rows[1].y*B.rows[1].w + rows[1].z*B.rows[2].w + rows[1].w*B.rows[3].w;
	AB.rows[2].w = rows[2].x*B.rows[0].w + rows[2].y*B.rows[1].w + rows[2].z*B.rows[2].w + rows[2].w*B.rows[3].w;
	AB.rows[3].w = rows[3].x*B.rows[0].w + rows[3].y*B.rows[1].w + rows[3].z*B.rows[2].w + rows[3].w*B.rows[3].w;
	*this = AB;
	return *this;
}

// Multiply all elements by constant value
template <class T> Mat4<T> &Mat4<T>::operator*=(const double d)
{
	rows[0] *= d;
	rows[1] *= d;
	rows[2] *= d;
	rows[3] *= d;
	return *this;
}

// Operator * (vec4)
template <class T> Vec4<T> Mat4<T>::operator*(const Vec4<T> &v) const
{
	Vec4<T> result;
	result.x = v.x*rows[0].x + v.y*rows[0].y + v.z*rows[0].z + v.w*rows[0].w;
	result.y = v.x*rows[1].x + v.y*rows[1].y + v.z*rows[1].z + v.w*rows[1].w;
	result.z = v.x*rows[2].x + v.y*rows[2].y + v.z*rows[2].z + v.w*rows[2].w;
	result.w = v.x*rows[3].x + v.y*rows[3].y + v.z*rows[3].z + v.w*rows[3].w;
	return result;
}

// Operator * (vec3)
template <class T> Vec3<T> Mat4<T>::operator*(const Vec3<T> &v) const
{
	// Assume vector 'w' element is 1.0
	Vec3<T> result;
	result.x = v.x*rows[0].x + v.y*rows[0].y + v.z*rows[0].z + rows[0].w;
	result.y = v.x*rows[1].x + v.y*rows[1].y + v.z*rows[1].z + rows[1].w;
	result.z = v.x*rows[2].x + v.y*rows[2].y + v.z*rows[2].z + rows[2].w;
	return result;
}

/*
// Methods
*/

// Create rotation matrix about X
template <class T> void Mat4<T>::createRotationX(double angle)
{
	double cosx, sinx, theta = angle/DEGRAD;
	cosx = cos(theta);
	sinx = sin(theta);
	set(0,1.0,0.0,0.0,0.0);
	set(1,0.0,cosx,sinx,0.0);
	set(2,0.0,-sinx,cosx,0.0);
	set(3,0.0,0.0,0.0,1.0);
}

// Create XY rotation matrix
template <class T> void Mat4<T>::createRotationXY(double anglex, double angley)
{
	double cosx, sinx, cosy, siny, thetax = anglex/DEGRAD, thetay = angley/DEGRAD;
	cosx = cos(thetax);
	cosy = cos(thetay);
	sinx = sin(thetax);
	siny = sin(thetay);
	set(0,cosy,0.0,siny,0.0);
	set(1,(-sinx)*(-siny),cosx,(-sinx)*cosy,0.0);
	set(2,cosx*(-siny),sinx,cosx*cosy,0.0);
	set(3,0.0,0.0,0.0,1.0);
}

// Create rotation matrix about Y
template <class T> void Mat4<T>::createRotationY(double angle)
{
	double cosx, sinx, theta = angle/DEGRAD;
	cosx = cos(theta);
	sinx = sin(theta);
	set(0,cosx,0.0,-sinx,0.0);
	set(1,0.0,1.0,0.0,0.0);
	set(2,sinx,0.0,cosx,0.0);
	set(3,0.0,0.0,0.0,1.0);
}

// Create rotation matrix about Z
template <class T> void Mat4<T>::createRotationZ(double angle)
{
	double cosx, sinx, theta = angle/DEGRAD;
	cosx = cos(theta);
	sinx = sin(theta);
	set(0,cosx,sinx,0.0,0.0);
	set(1,-sinx,cosx,0.0,0.0);
	set(2,0.0,0.0,1.0,0.0);
	set(3,0.0,0.0,0.0,1.0);
}

// Create axis rotation quaternion
template <class T> void Mat4<T>::createRotationAxis(double ax, double ay, double az, double angle)
{
	double cosx, sinx, theta = angle/DEGRAD;
	double mag = sqrt(ax*ax + ay*ay + az*az);
	ax /= mag;
	ay /= mag;
	az /= mag;
	cosx = cos(theta);
	sinx = sin(theta);
	set(0,ax*ax*(1.0-cosx) + cosx, ax*ay*(1.0-cosx) - az*sinx, ax*az*(1.0-cosx) + ay*sinx, 0.0);
	set(1,ax*ay*(1.0-cosx) + az*sinx, ay*ay*(1.0-cosx) + cosx, ay*az*(1.0-cosx) - ax*sinx, 0.0);
	set(2,ax*az*(1.0-cosx) - ay*sinx, ay*az*(1.0-cosx) + ax*sinx, az*az*(1.0-cosx) + cosx, 0.0);
	set(3,0.0,0.0,0.0,1.0);
}

template <class T> double Mat4<T>::determinant() const
{
	// Calculate the determinant of the 4x4 matrix.
	// Could write a nice recursive algorithm here, but instead let's hard-code it...
	double a = rows[0].x * (rows[1].y*(rows[2].z*rows[3].w-rows[2].w*rows[3].z) - rows[2].y*(rows[1].z*rows[3].w-rows[1].w*rows[3].z) + rows[3].y*(rows[1].z*rows[2].w-rows[1].w*rows[2].z) );
	double b = rows[1].x * (rows[0].y*(rows[2].z*rows[3].w-rows[2].w*rows[3].z) - rows[2].y*(rows[0].z*rows[3].w-rows[0].w*rows[3].z) + rows[3].y*(rows[0].z*rows[2].w-rows[0].w*rows[2].z) );
	double c = rows[2].x * (rows[0].y*(rows[1].z*rows[3].w-rows[1].w*rows[3].z) - rows[1].y*(rows[0].z*rows[3].w-rows[0].w*rows[3].z) + rows[3].y*(rows[0].z*rows[1].w-rows[0].w*rows[1].z) );
	double d = rows[3].x * (rows[0].y*(rows[1].z*rows[2].w-rows[1].w*rows[2].z) - rows[1].y*(rows[0].z*rows[2].w-rows[0].w*rows[2].z) + rows[2].y*(rows[0].z*rows[1].w-rows[0].w*rows[1].z) );
	return (a-b+c-d);
}

// Calculate matrix inverse
template <class T> void Mat4<T>::invert()
{
	msg.enter("Mat4<T>::invert");
	// Gauss-Jordan Inversion
	// Invert the supplied matrix using Gauss-Jordan elimination
	int pivotrows[4], pivotcols[4], pivotrow = 0, pivotcol = 0;
	bool pivoted[4];
	int row, col, n, m;
	double large, element;
	for (n=0; n<4; ++n)
	{
		pivotrows[n] = 0;
		pivotcols[n] = 0;
		pivoted[n] = FALSE;
	}
	// Loop over columns to be reduced
	for (n=0; n<4; ++n)
	{
		// Locate suitable pivot element - find largest value in the matrix A
		large = 0.0;
		for (row=0; row<4; ++row)
		{
			// Only search this row if it has not previously contained a pivot element
			if (pivoted[row]) continue;
			for (col=0; col<4; ++col)
			{
				// Similarly, only look at the column element if the column hasn't been pivoted yet.
				if (pivoted[col]) continue;
				// Check the size of the element...
				element = fabs(rows[row].get(col));
				if (element > large)
				{
					large = element;
					pivotrow = row;
					pivotcol = col;
				}
			}
		}
		// Mark the pivot row/column as changed
		pivoted[pivotcol] = TRUE;
		pivotrows[n] = pivotrow;
		pivotcols[n] = pivotcol;
		// Exchange rows to put pivot element on the diagonal
		if (pivotrow != pivotcol)
			for (m=0; m<4; m++)
			{
				element = rows[pivotrow].get(m);
				rows[pivotrow].set(m, rows[pivotcol].get(m));
				rows[pivotcol].set(m, element);
			}
		// Now ready to divide through row elements.
		element = 1.0 / rows[pivotcol].get(pivotcol);
		rows[pivotcol].set(pivotcol, 1.0);
		rows[pivotcol] *= element;

		// Divide through other rows by the relevant multiple of the pivot row
		for (row=0; row<4; ++row)
		{
			if (row == pivotcol) continue;
			element = rows[row].get(pivotcol);
			rows[row].set(pivotcol,0.0);
			for (m=0; m<4; ++m) rows[row].set(m, rows[row].get(m) - rows[pivotcol].get(m) * element);
		}
	}
	// Rearrange columns to undo row exchanges performed earlier
	for (n=3; n>=0; --n)
		if (pivotrows[n] != pivotcols[n])
			for (m=0; m<4; m++)
			{
				element = rows[m].get(pivotrows[n]);
				rows[m].set(pivotrows[n], rows[m].get(pivotcols[n]));
				rows[m].set(pivotcols[n], element);
			}
	msg.exit("Mat4<T>::invert");
}

template <class T> void Mat4<T>::print() const
{
	printf("Mat4_X %8.4f %8.4f %8.4f %8.4f\n",rows[0].x,rows[0].y,rows[0].z,rows[0].w);
	printf("Mat4_Y %8.4f %8.4f %8.4f %8.4f\n",rows[1].x,rows[1].y,rows[1].z,rows[1].w);
	printf("Mat4_Z %8.4f %8.4f %8.4f %8.4f\n",rows[2].x,rows[2].y,rows[2].z,rows[2].w);
	printf("Mat4_W %8.4f %8.4f %8.4f %8.4f\n",rows[3].x,rows[3].y,rows[3].z,rows[3].w);
}

#endif

