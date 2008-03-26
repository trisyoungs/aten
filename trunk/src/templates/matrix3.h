/*
	*** 3x3 Matrix class
	*** src/templates/matrix3.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_MATRIX3_H
#define ATEN_MATRIX3_H

#include "templates/vector3.h"
#include "base/debug.h"
#include <algorithm>
#include <math.h>
#include <stdio.h>
using namespace std;

// Forward declarations
template <class T> class Vec3;

// 3x3 matrix
template <class T> class Mat3
{
	public:
	// Constructor / Destructor
	Mat3(T xx = 1, T xy = 0, T xz = 0, T yx = 0, T yy = 1, T yz = 0, T zx = 0, T zy = 0, T zz = 1);

	// 3x3 Matrix, consisting of three vec3's:
	//	{ x.x x.y x.z } rows[0]
	//	{ y.x y.y y.z } rows[1]
	//	{ z.x z.y z.z } rows[2]
	public:
	// Vectors of matrix
	Vec3<T> rows[3];

	/*
	// Set
	*/
	public:
	// Initialise elements of one row
	void set(int row, T a, T b, T c);
	// Initialise elements of one row
	void set(int row, const Vec3<T> &v);
	// Set individual element of matrix (by row/column)
	void set(int row, int col, T d);
	// Set individual element of matrix (straight id)
	void set(int i, T d);
	// Set diagonal elements of matrix (off-diagonals to zero)
	void setDiagonal(T rx, T ry, T rz);

	/*
	// Get
	*/
	public:
	// Return the row specified
	Vec3<T> get(int i);
	// Puts the matrix into the passed 1D-array of type <T>, row-major
	void copyRowMajor(T*);
	// Puts the matrix into the passed 1D-array of type <T>, column-major
	void copyColumnMajor(T*);
	// Returns the specified element of the matrix
	T getElement(int i);

	/*
	// Methods
	*/
	// Swap rows
	void swapRows(int, int);
	// Return transpose of matrix
	Mat3<T> transpose() const;
	// Calculate the determinant of the matrix.
	double determinant();
	// Invert the matrix
	void invert();
	// Reset the matrix to the identity
	void setIdentity();
	// Set the zero matrix
	void zero();
	// Prints the matrix to stdout
	void print() const;
	// Invert the matrix (Gauss-Jordan)
	void matrix3Invert(int, double*);
	// Create matrix of orthogonal vectors from reference
	void createOrthogonal(const Vec3<T>&);
	// Multiply matrix rows by vector elements
	void rowMultiply(const Vec3<T>&);

	/*
	// Operators
	*/
	Mat3 operator*(const Mat3&) const;
	Vec3<T> operator*(const Vec3<T>&) const;
	Mat3& operator*=(const Mat3&);
	Mat3& operator-=(const T);
	Mat3 operator-(const Mat3&) const;
};

// Constructors
template <class T> Mat3<T>::Mat3(T xx, T xy, T xz, T yx, T yy, T yz, T zx, T zy, T zz)
{
	rows[0].set(xx, xy, xz);
	rows[1].set(yx, yy, yz);
	rows[2].set(zx, zy, zz);
}

// Initialise elements of one row
template <class T> void Mat3<T>::set(int row, T a, T b, T c)
{
	rows[row].set(a,b,c);
}

// Initialise elements of one row
template <class T> void Mat3<T>::set(int row, const Vec3<T> &v)
{
	rows[row].set(v.x,v.y,v.z);
}

// Set individual element of matrix (by row/column)
template <class T> void Mat3<T>::set(int row, int col, T d)
{
	rows[row].set(col,d);
}

// Set individual element of matrix (straight id)
template <class T> void Mat3<T>::set(int i, T d)
{
	rows[i/3].set(i%3,d);
}

// Set diagonal elements of matrix (off-diagonals to zero)
template <class T> void Mat3<T>::setDiagonal(T rx, T ry, T rz)
{
	zero();
	rows[0].x = rx;
	rows[1].y = ry;
	rows[2].z = rz;
}

// Return the row specified
template <class T> Vec3<T> Mat3<T>::get(int i)
{
	return rows[i];
}

// Returns the specified element of the matrix
template <class T> T Mat3<T>::getElement(int i)
{
	return rows[i/3].get(i%3);
}

// Get (array)
template <class T> void Mat3<T>::copyRowMajor(T *rowm)
{
	// Construct a 1d array of type T with row-major ordering...
	rowm[0] = rows[0].x;	rowm[1] = rows[0].y;	rowm[2] = rows[0].z;
	rowm[3] = rows[1].x;	rowm[4] = rows[1].y;	rowm[5] = rows[1].z;
	rowm[6] = rows[2].x;	rowm[7] = rows[2].y;	rowm[8] = rows[2].z;
}

// Get (array)
template <class T> void Mat3<T>::copyColumnMajor(T *colm)
{
	// Construct a 1d array of type T with column-major ordering...
	colm[0] = rows[0].x;	colm[3] = rows[0].y;	colm[6] = rows[0].z;
	colm[1] = rows[1].x;	colm[4] = rows[1].y;	colm[7] = rows[1].z;
	colm[2] = rows[2].x;	colm[5] = rows[2].y;	colm[8] = rows[2].z;
}

// Swap rows
template <class T> void Mat3<T>::swapRows(int row1, int row2)
{
	Vec3<T> temp = rows[row2];
	rows[row2] = rows[row1];
	rows[row1] = temp;
}

// Set identity matrix
template <class T> void Mat3<T>::setIdentity()
{
	// Reset to the identity matrix
	rows[0].set(1,0,0);
	rows[1].set(0,1,0);
	rows[2].set(0,0,1);
}

// Set zero matrix
template <class T> void Mat3<T>::zero()
{
	rows[0].zero();
	rows[1].zero();
	rows[2].zero();
}
// Operator * and *=
template <class T> Vec3<T> Mat3<T>::operator*(const Vec3<T> &v) const {
	Vec3<T> result;
	result.x = rows[0].dp(v);
	result.y = rows[1].dp(v);
	result.z = rows[2].dp(v);
	return result;
}


// Operator * and *=
template <class T> Mat3<T> Mat3<T>::operator*(const Mat3<T> &B) const {
	// Multiply matrix A by matrix B. Put result in local matrix
	// [ row(A|this).column(B) ]
	Mat3 AB;
	AB.rows[0].x = rows[0].x*B.rows[0].x + rows[0].y*B.rows[1].x + rows[0].z*B.rows[2].x;
	AB.rows[1].x = rows[1].x*B.rows[0].x + rows[1].y*B.rows[1].x + rows[1].z*B.rows[2].x;
	AB.rows[2].x = rows[2].x*B.rows[0].x + rows[2].y*B.rows[1].x + rows[2].z*B.rows[2].x;

	AB.rows[0].y = rows[0].x*B.rows[0].y + rows[0].y*B.rows[1].y + rows[0].z*B.rows[2].y;
	AB.rows[1].y = rows[1].x*B.rows[0].y + rows[1].y*B.rows[1].y + rows[1].z*B.rows[2].y;
	AB.rows[2].y = rows[2].x*B.rows[0].y + rows[2].y*B.rows[1].y + rows[2].z*B.rows[2].y;

	AB.rows[0].z = rows[0].x*B.rows[0].z + rows[0].y*B.rows[1].z + rows[0].z*B.rows[2].z;
	AB.rows[1].z = rows[1].x*B.rows[0].z + rows[1].y*B.rows[1].z + rows[1].z*B.rows[2].z;
	AB.rows[2].z = rows[2].x*B.rows[0].z + rows[2].y*B.rows[1].z + rows[2].z*B.rows[2].z;
 	return AB;
}

template <class T> Mat3<T> &Mat3<T>::operator*=(const Mat3<T> &B)
{
	// Multiply matrix A by matrix B. Put result in local matrix
	// [ column(A|this).row(B) ]
	Mat3 AB;
	AB.rows[0].x = rows[0].x*B.rows[0].x + rows[0].y*B.rows[1].x + rows[0].z*B.rows[2].x;
	AB.rows[1].x = rows[1].x*B.rows[0].x + rows[1].y*B.rows[1].x + rows[1].z*B.rows[2].x;
	AB.rows[2].x = rows[2].x*B.rows[0].x + rows[2].y*B.rows[1].x + rows[2].z*B.rows[2].x;

	AB.rows[0].y = rows[0].x*B.rows[0].y + rows[0].y*B.rows[1].y + rows[0].z*B.rows[2].y;
	AB.rows[1].y = rows[1].x*B.rows[0].y + rows[1].y*B.rows[1].y + rows[1].z*B.rows[2].y;
	AB.rows[2].y = rows[2].x*B.rows[0].y + rows[2].y*B.rows[1].y + rows[2].z*B.rows[2].y;

	AB.rows[0].z = rows[0].x*B.rows[0].z + rows[0].y*B.rows[1].z + rows[0].z*B.rows[2].z;
	AB.rows[1].z = rows[1].x*B.rows[0].z + rows[1].y*B.rows[1].z + rows[1].z*B.rows[2].z;
	AB.rows[2].z = rows[2].x*B.rows[0].z + rows[2].y*B.rows[1].z + rows[2].z*B.rows[2].z;
	*this = AB;
	return *this;
}

template <class T> Mat3<T> &Mat3<T>::operator-=(const T a)
{
	// Subtract value from each element
	this->rows[0] -= a;
	this->rows[1] -= a;
	this->rows[2] -= a;
	return *this;
}

template <class T> Mat3<T> Mat3<T>::operator-(const Mat3<T> &m) const {
	// Subtract value from each element
	Mat3<T> result;
	result.rows[0] = rows[0] - m.rows[0];
	result.rows[1] = rows[1] - m.rows[1];
	result.rows[2] = rows[2] - m.rows[2];
	return result;
}

// Transpose
template <class T> Mat3<T> Mat3<T>::transpose() const {
	Mat3<T> result;
	result.rows[0].x = rows[0].x;
	result.rows[0].y = rows[1].x;
	result.rows[0].z = rows[2].x;

	result.rows[1].x = rows[0].y;
	result.rows[1].y = rows[1].y;
	result.rows[1].z = rows[2].y;

	result.rows[2].x = rows[0].z;
	result.rows[2].y = rows[1].z;
	result.rows[2].z = rows[2].z;
	return result;
}

// Calculate determinant
template <class T> double Mat3<T>::determinant()
{
	// Hard-coded calculation of determinant of 3x3 matrix
	double det = 0.0;
	det += rows[0].x * (rows[1].y*rows[2].z - rows[2].y*rows[1].z);
	det -= rows[0].y * (rows[1].x*rows[2].z - rows[2].x*rows[1].z);
	det += rows[0].z * (rows[1].x*rows[2].y - rows[2].x*rows[1].y);
	return det;
}

// Calculate matrix inverse
template <class T> void Mat3<T>::invert()
{
	// Must create a T array and pass it to matrix_invert()
	T m[9];
	copyRowMajor(m);
	matrix3Invert(3,m);
	// TODO Don't do this!
	rows[0].set(m[0],m[1],m[2]);
	rows[1].set(m[3],m[4],m[5]);
	rows[2].set(m[6],m[7],m[8]);
}

// Row Multiply
template <class T> void Mat3<T>::rowMultiply(const Vec3<T> &v)
{
	// Multiply matrix rows by vector elements
	rows[0] *= v.x;
	rows[1] *= v.y;
	rows[2] *= v.z;
}

// Print
template <class T> void Mat3<T>::print() const {
	printf("Mat3_X %8.4f %8.4f %8.4f\n",rows[0].x,rows[0].y,rows[0].z);
	printf("Mat3_Y %8.4f %8.4f %8.4f\n",rows[1].x,rows[1].y,rows[1].z);
	printf("Mat3_Z %8.4f %8.4f %8.4f\n",rows[2].x,rows[2].y,rows[2].z);
}

// Create matrix of orthogonal vectors
template <class T> void Mat3<T>::createOrthogonal(const Vec3<T> &vec)
{
	// Set x-vector to be the passed vector.
	rows[0] = vec;
	rows[1].set(rows[0].z,rows[0].x,rows[0].y);
	rows[2].set(rows[0].y,rows[0].z,rows[0].x);
}

// Invert matrix
template <class T> void Mat3<T>::matrix3Invert(int matsize, double *A)
{
	// Invert the supplied matrix using Gauss-Jordan elimination
	int *pivotrows, *pivotcols, pivotrow, pivotcol;
	int *pivoted;
	int row, col, n, m;
	double *B, large, element;
	dbgBegin(Debug::Calls,"invert[GJ]");
	// Create and blank temporary arrays we need
	pivotrows = new int[matsize];
	pivotcols = new int[matsize];
	pivoted = new int[matsize];
	pivotrow = 0;
	pivotcol = 0;
	large = 0.0;
	for (n=0; n<matsize; n++)
	{
		pivotrows[n] = 0;
		pivotcols[n] = 0;
		pivoted[n] = 0;
	}
	// Loop over columns to be reduced
	for (n=0; n<matsize; n++)
	{
		// Locate suitable pivot element - find largest value in the matrix A
		large = 0.0;
		for (row=0; row<matsize; row++)
		{
			// Only search this row if it has not previously contained a pivot element
			if (pivoted[row] == 1) continue;
			for (col=0; col<matsize; col++)
			{
				// Similarly, only look at the column element if the column hasn't been pivoted yet.
				if (pivoted[col] == 1) continue;
				// Check the size of the element...
				element = fabs(A[row + col*matsize]);
				if (element > large)
				{
					large = element;
					pivotrow = row;
					pivotcol = col;
				}
			}
		}
		// Mark the pivot row/column as changed
		pivoted[pivotcol] = 1;
		pivotrows[n] = pivotrow;
		pivotcols[n] = pivotcol;
		// Exchange rows to put pivot element on the diagonal
		if (pivotrow != pivotcol)
			for (m=0; m<matsize; m++) swap( A[pivotrow + m*matsize], A[pivotcol + m*matsize]);
		// Now ready to divide through row elements.
		pivotrow = pivotcol;		// Need to do since pivotrow may not be valid because of row exchange
		element = 1.0/A[pivotrow + matsize*pivotrow];
		for (m=0; m<matsize; m++) A[pivotrow + m*matsize] *= element;
		A[pivotrow + pivotrow*matsize] = element;
		// Divide through other rows by the relevant multiple of the pivot row
		for (row=0; row<matsize; row++)
		{
			if (row == pivotrow) continue;
			element = A[row + matsize*pivotcol];
			A[row + matsize*pivotcol] = 0.0;
			for (col=0; col<matsize; col++) A[row+matsize*col] -= (A[pivotrow+matsize*col] * element);
		}
	}
	// Rearrange columns to undo row exchanges performed earlier
	for (n=matsize-1; n>=0; n--)
		if (pivotrows[n] != pivotcols[n])
			for (m=0; m<matsize; m++) swap( A[m + matsize*pivotrows[n]], A[m + matsize*pivotcols[n]]);
	delete[] pivotrows;
	delete[] pivotcols;
	delete[] pivoted;
	dbgEnd(Debug::Calls,"invert[GJ]");
}

#endif

