/*
	*** 4x4 Matrix class
	*** src/templates/matrix4.h
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

#ifndef H_MATRIX4_H
#define H_MATRIX4_H

using namespace std;
#include "templates/vector3.h"
#include "templates/vector4.h"
#include "base/debug.h"
#include <algorithm>
#include <math.h>
#include <stdio.h>

// Forward declarations
template <class T> class vec3;
template <class T> class vec4;
template <class T> class mat3;
template <class T> class mat4;

// 4x4 matrix
template <class T> struct mat4
{
	public:
	// Constructor / Destructor
	mat4();
	~mat4();
	#ifdef MEMDEBUG
	// Copy constructor
	mat4(const mat4<T>&);
	#endif
	mat4(const mat3<T>&);

	// 4x4 Matrix, consisting of four vec4's:
	//	{ x.x x.y x.z x.w }  rows[0]
	//	{ y.x y.y y.z y.w }  rows[1]
	//	{ z.x z.y z.z z.w }  rows[2]
	//	{ w.x w.y w.z w.w }  rows[3]
	public:
	// Vectors of matrix
	vec4<T> rows[4];

	/*
	// Access
	*/
	public:
	// Set the matrix from a 1D array of values
	void set_from_column_major(T*);
	// Puts the matrix into the passed 1D-array of type <T>, row-major
	void get_row_major(T*) const;
	// Puts the matrix into the passed 1D-array of type <T>, column-major
	void get_column_major(T*) const;

	/*
	// Methods
	*/
	public:
	// Calculate the determinant of the matrix.
	double determinant() const;
	// Invert the matrix
	void invert();
	// Reset the matrix to the identity
	void set_identity();
	// Set the zero matrix
	void zero();
	// Prints the matrix to stdout
	void print() const;
	// TODO 
	void matrix4_invert(int, double*);

	/*
	// Operators
	*/
	mat4 operator*(const mat4&) const;
	mat4& operator*=(const mat4&);
	vec4<T> operator*(const vec4<T>&) const;
	vec3<T> operator*(const vec3<T>&) const;
};

/*
// Mat4
*/

// Constructor
template <class T> mat4<T>::mat4()
{
	set_identity();
	#ifdef MEMDEBUG
	memdbg.create[MD_MAT4] ++;
	#endif
}

#ifdef MEMDEBUG
template <class T> mat4<T>::mat4(const mat4<T> &m)
{
	rows[0] = m.rows[0];
	rows[1] = m.rows[1];
	rows[2] = m.rows[2];
	rows[3] = m.rows[3];
	memdbg.create[MD_MAT4COPY] ++;
}
#endif

// Create from mat3<>
template <class T> mat4<T>::mat4(const mat3<T> &m)
{
	rows[0].set(m.rows[0], 0.0);
	rows[1].set(m.rows[1], 0.0);
	rows[2].set(m.rows[2], 0.0);
	rows[3].set(0.0, 0.0, 0.0, 1.0);
}

// Destructor
template <class T> mat4<T>::~mat4()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_MAT4] ++;
	#endif
}

// Set (T[])
template <class T> void mat4<T>::set_from_column_major(T *m)
{
	rows[0].set(m[0],m[4],m[8],m[12]);
	rows[1].set(m[1],m[5],m[9],m[13]);
	rows[2].set(m[2],m[6],m[10],m[14]);
	rows[3].set(m[3],m[7],m[11],m[15]);
}

// Get (array)
template <class T> void mat4<T>::get_row_major(T *rowm) const
{
	// Construct a 1d array of type T with row-major ordering...
	rowm[0] = rows[0].x;	rowm[1] = rows[0].y;	rowm[2] = rows[0].z;	rowm[3] = rows[0].w;
	rowm[4] = rows[1].x;	rowm[5] = rows[1].y;	rowm[6] = rows[1].z;	rowm[7] = rows[1].w;
	rowm[8] = rows[2].x;	rowm[9] = rows[2].y;	rowm[10] = rows[2].z;	rowm[11] = rows[2].w;
	rowm[12] = rows[3].x;	rowm[13] = rows[3].y;	rowm[14] = rows[3].z;	rowm[15] = rows[3].w;
}


// Get (array)
template <class T> void mat4<T>::get_column_major(T *colm) const
{
	// Construct a 1d array of type T with column-major ordering...
	colm[0] = rows[0].x;	colm[4] = rows[0].y;	colm[8] = rows[0].z;	colm[12] = rows[0].w;
	colm[1] = rows[1].x;	colm[5] = rows[1].y;	colm[9] = rows[1].z;	colm[13] = rows[1].w;
	colm[2] = rows[2].x;	colm[6] = rows[2].y;	colm[10] = rows[2].z;	colm[14] = rows[2].w;
	colm[3] = rows[3].x;	colm[7] = rows[3].y;	colm[11] = rows[3].z;	colm[15] = rows[3].w;
}

// Reset
template <class T> void mat4<T>::set_identity()
{
	// Reset to the identity matrix
	rows[0].set(1,0,0,0);
	rows[1].set(0,1,0,0);
	rows[2].set(0,0,1,0);
	rows[3].set(0,0,0,1);
}

// Set zero matrix
template <class T> void mat4<T>::zero()
{
	rows[0].zero();
	rows[1].zero();
	rows[2].zero();
	rows[3].zero();
}

// Matrix multiply (operator *)
template <class T> mat4<T> mat4<T>::operator*(const mat4<T> &B) const
{
	// Multiply matrix A by matrix B. Put result in local matrix
	// [ row(A|this).column(B) ]

	mat4 AB;
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
template <class T> mat4<T> &mat4<T>::operator*=(const mat4<T> &B)
{
	// Multiply matrix A by matrix B. Put result in local matrix
	// [ column(A|this).row(B) ]
	mat4 AB;
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

// Operator * (vec4)
template <class T> vec4<T> mat4<T>::operator*(const vec4<T> &v) const
{
	vec4<T> result;
	result.x = v.x*rows[0].x + v.y*rows[0].y + v.z*rows[0].z + v.w*rows[0].w;
	result.y = v.x*rows[1].x + v.y*rows[1].y + v.z*rows[1].z + v.w*rows[1].w;
	result.z = v.x*rows[2].x + v.y*rows[2].y + v.z*rows[2].z + v.w*rows[2].w;
	result.w = v.x*rows[3].x + v.y*rows[3].y + v.z*rows[3].z + v.w*rows[3].w;
	return result;
}

// Operator * (vec3)
template <class T> vec3<T> mat4<T>::operator*(const vec3<T> &v) const
{
	// Assume vector 'w' element is 1.0
	vec3<T> result;
	result.x = v.x*rows[0].x + v.y*rows[0].y + v.z*rows[0].z + rows[0].w;
	result.y = v.x*rows[1].x + v.y*rows[1].y + v.z*rows[1].z + rows[1].w;
	result.z = v.x*rows[2].x + v.y*rows[2].y + v.z*rows[2].z + rows[2].w;
	return result;
}

// Calculate matrix inverse
template <class T> void mat4<T>::invert()
{
	// Must create a T array and pass it to matrix_invert()
	T m[16];
	get_row_major(m);
	matrix4_invert(4,m);
	// TODO Don't do this!
	rows[0].set(m[0],m[1],m[2],m[3]);
	rows[1].set(m[4],m[5],m[6],m[7]);
	rows[2].set(m[8],m[9],m[10],m[11]);
	rows[3].set(m[12],m[13],m[14],m[15]);
}

template <class T> void mat4<T>::print() const
{
	printf("Mat4_X %8.4f %8.4f %8.4f %8.4f\n",rows[0].x,rows[0].y,rows[0].z,rows[0].w);
	printf("Mat4_Y %8.4f %8.4f %8.4f %8.4f\n",rows[1].x,rows[1].y,rows[1].z,rows[1].w);
	printf("Mat4_Z %8.4f %8.4f %8.4f %8.4f\n",rows[2].x,rows[2].y,rows[2].z,rows[2].w);
	printf("Mat4_W %8.4f %8.4f %8.4f %8.4f\n",rows[3].x,rows[3].y,rows[3].z,rows[3].w);
}

template <class T> double mat4<T>::determinant() const
{
	// Calculate the determinant of the 4x4 matrix.
	// Could write a nice recursive algorithm here, but instead let's hard-code it...
	double a = rows[0].x * (rows[1].y*(rows[2].z*rows[3].w-rows[2].w*rows[3].z) - rows[2].y*(rows[1].z*rows[3].w-rows[1].w*rows[3].z) + rows[3].y*(rows[1].z*rows[2].w-rows[1].w*rows[2].z) );
	double b = rows[1].x * (rows[0].y*(rows[2].z*rows[3].w-rows[2].w*rows[3].z) - rows[2].y*(rows[0].z*rows[3].w-rows[0].w*rows[3].z) + rows[3].y*(rows[0].z*rows[2].w-rows[0].w*rows[2].z) );
	double c = rows[2].x * (rows[0].y*(rows[1].z*rows[3].w-rows[1].w*rows[3].z) - rows[1].y*(rows[0].z*rows[3].w-rows[0].w*rows[3].z) + rows[3].y*(rows[0].z*rows[1].w-rows[0].w*rows[1].z) );
	double d = rows[3].x * (rows[0].y*(rows[1].z*rows[2].w-rows[1].w*rows[2].z) - rows[1].y*(rows[0].z*rows[2].w-rows[0].w*rows[2].z) + rows[2].y*(rows[0].z*rows[1].w-rows[0].w*rows[1].z) );
	return (a-b+c-d);
}

// Invert matrix
template <class T> void mat4<T>::matrix4_invert(int matsize, double *A)
{
	// Invert the supplied matrix using Gauss-Jordan elimination
	int *pivotrows, *pivotcols, pivotrow, pivotcol;
	int *pivoted;
	int row, col, n, m;
	double *B, large, element;
	dbg_begin(DM_CALLS,"invert[GJ]");
	// Create and blank temporary arrays we need
	pivotrows = new int[matsize];
	pivotcols = new int[matsize];
	pivoted = new int[matsize];
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
	dbg_end(DM_CALLS,"invert[GJ]");
}

#endif

