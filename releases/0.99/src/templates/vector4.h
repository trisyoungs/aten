/*
	*** 4-component vector class
	*** src/templates/vector4.h
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

#ifndef ATEN_VECTOR4_H
#define ATEN_VECTOR4_H

// Prevent complaints for incorrect arguments to 'macro max()' and 'macro min()
#define NOMINMAX

#include "templates/vector3.h"
#include <stdio.h>
#include <math.h>

// 4D vector
template <class T> class Vec4
{
	public:
	// Constructor
	Vec4<T>();

	/*
	// 4-Vector
	*/
	public:
	// Components of vector
	T x,y,z,w;
	// Set the vector to 0,0,0
	void zero();
	// Set all four components simultaneously
	void set(T, T, T, T);
	// Adjust all four components simultaneously
	void add(T, T, T, T);
	// Set all four components simultaneously
	void set(Vec3<T>, T);
	// Print data
	void print() const;
	// Operators + and +=
	Vec4<T>& operator+=(Vec4<T>);
	Vec4<T>& operator+=(Vec3<T>);
	Vec4<T> operator+(Vec4<T>);
	Vec4<T> operator+(Vec3<T>);
	// Operators - and -=
	Vec4<T>& operator-=(Vec4<T>);
	Vec4<T>& operator-=(Vec3<T>);
	Vec4<T> operator-(Vec4<T>);
	Vec4<T> operator-(Vec3<T>);
};

// Constructur
template <class T> Vec4<T>::Vec4()
{
	zero();
}

// Zero
template <class T> void Vec4<T>::zero()
{
	x = 0;
	y = 0;
	z = 0;
	w = 0;
}

// Set
template <class T> void Vec4<T>::set(T a, T b, T c, T d)
{
	x = a;
	y = b;
	z = c;
	w = d;
}

// Add
template <class T> void Vec4<T>::add(T a, T b, T c, T d)
{
	x += a;
	y += b;
	z += c;
	w += d;
}

// Set (vec3, T)
template <class T> void Vec4<T>::set(Vec3<T> v, T a)
{
	x = v.x;
	y = v.y;
	z = v.z;
	w = a;
}

// Print
template <class T> void Vec4<T>::print() const
{
	printf("vec = %8.4f %8.4f %8.4f %8.4f\n",(double)x,(double)y,(double)z,(double)w);
}

// Operator += (Vec4)
template <class T> Vec4<T>& Vec4<T>::operator+=(Vec4<T> v)
{
	x += v.x;
	y += v.y;
	z += v.z;
	w += v.w;
	return *this;
}

// Operator += (vec3)
template <class T> Vec4<T>& Vec4<T>::operator+=(Vec3<T> v)
{
	x += v.x;
	y += v.y;
	z += v.z;
	return *this;
}

// Operator + (Vec4)
template <class T> Vec4<T> Vec4<T>::operator+(Vec4<T> v)
{
	Vec4<T> result;
	result.x = x+v.x;
	result.y = y+v.y;
	result.z = z+v.z;
	result.w = w+v.w;
	return result;
}

// Operator + (vec3)
template <class T> Vec4<T> Vec4<T>::operator+(Vec3<T> v)
{
	Vec4<T> result;
	result.x = x+v.x;
	result.y = y+v.y;
	result.z = z+v.z;
	result.w = w+v.w;
	return result;
}

// Operator -= (Vec4)
template <class T> Vec4<T>& Vec4<T>::operator-=(Vec4<T> v)
{
	x -= v.x;
	y -= v.y;
	z -= v.z;
	w -= v.w;
	return *this;
}

// Operator -= (vec3)
template <class T> Vec4<T>& Vec4<T>::operator-=(Vec3<T> v)
{
	x -= v.x;
	y -= v.y;
	z -= v.z;
	return *this;
}

// Operator - (Vec4)
template <class T> Vec4<T> Vec4<T>::operator-(Vec4<T> v)
{
	Vec4<T> result;
	result.x = x-v.x;
	result.y = y-v.y;
	result.z = z-v.z;
	result.w = w-v.w;
	return result;
}

// Operator - (vec3)
template <class T> Vec4<T> Vec4<T>::operator-(Vec3<T> v)
{
	Vec4<T> result;
	result.x = x-v.x;
	result.y = y-v.y;
	result.z = z-v.z;
	result.w = w-v.w;
	return result;
}

#endif
