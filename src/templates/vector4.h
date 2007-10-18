/*
	*** 4-component vector class
	*** src/templates/vector4.h

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

#ifndef H_VECTOR4_H
#define H_VECTOR4_H

#include "templates/matrix3.h"
#include "templates/matrix4.h"
#include "base/debug.h"
#include <stdio.h>
#include <math.h>

// Forward declarations
template <class T> struct vec3;
template <class T> struct vec4;
template <class T> struct mat3;
template <class T> struct mat4;

// 4D vector
template <class T> struct vec4
{
	public:
	// Constructor / Destructor
	vec4<T>();
	~vec4();
	#ifdef MEMDEBUG
	// Copy constructor
	vec4<T>(const vec4<T>&);
	#endif

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
	void set(vec3<T>, T);
	// Operators + and +=
	vec4<T>& operator+=(vec4<T>);
	vec4<T>& operator+=(vec3<T>);
	vec4<T> operator+(vec4<T>);
	vec4<T> operator+(vec3<T>);
	// Operators - and -=
	vec4<T>& operator-=(vec4<T>);
	vec4<T>& operator-=(vec3<T>);
	vec4<T> operator-(vec4<T>);
	vec4<T> operator-(vec3<T>);
};

// Constructor
template <class T> vec4<T>::vec4()
{
	zero();
	#ifdef MEMDEBUG
	memdbg.create[MD_VEC4] ++;
	#endif
}

// Destructor
template <class T> vec4<T>::~vec4()
{
	#ifdef MEMDEBUG
	memdbg.destroy[MD_VEC4] ++;
	#endif
}

#ifdef MEMDEBUG
// Copy constructor
template <class T> vec4<T>::vec4(const vec4<T> &v)
{
	x = v.x; y = v.y; z = v.z; w = v.w;
	memdbg.create[MD_VEC4COPY] ++;
}
#endif

// Zero
template <class T> void vec4<T>::zero()
{
	x = 0; y = 0; z = 0; w = 0;
}

// Set
template <class T> void vec4<T>::set(T a, T b, T c, T d)
{
	x = a;
	y = b;
	z = c;
	w = d;
}

// Add
template <class T> void vec4<T>::add(T a, T b, T c, T d)
{
	x += a;
	y += b;
	z += c;
	w += d;
}

// Set (vec3, T)
template <class T> void vec4<T>::set(vec3<T> v, T a)
{
	x = v.x; y = v.y; z = v.z; w = a;
}

// Operator += (vec4)
template <class T> vec4<T>& vec4<T>::operator+=(vec4<T> v)
{
	x += v.x; y += v.y; z += v.z; w += v.w;
	return *this;
}

// Operator += (vec3)
template <class T> vec4<T>& vec4<T>::operator+=(vec3<T> v)
{
	x += v.x; y += v.y; z += v.z;
	return *this;
}

// Operator + (vec4)
template <class T> vec4<T> vec4<T>::operator+(vec4<T> v)
{
	vec4<T> result;
	result.x = x+v.x; result.y = y+v.y; result.z = z+v.z; result.w = w+v.w;
	return result;
}

// Operator + (vec3)
template <class T> vec4<T> vec4<T>::operator+(vec3<T> v)
{
	vec4<T> result;
	result.x = x+v.x; result.y = y+v.y; result.z = z+v.z; result.w = w+v.w;
	return result;
}

// Operator -= (vec4)
template <class T> vec4<T>& vec4<T>::operator-=(vec4<T> v)
{
	x -= v.x; y -= v.y; z -= v.z; w -= v.w;
	return *this;
}

// Operator -= (vec3)
template <class T> vec4<T>& vec4<T>::operator-=(vec3<T> v)
{
	x -= v.x; y -= v.y; z -= v.z;
	return *this;
}

// Operator - (vec4)
template <class T> vec4<T> vec4<T>::operator-(vec4<T> v)
{
	vec4<T> result;
	result.x = x-v.x; result.y = y-v.y; result.z = z-v.z; result.w = w-v.w;
	return result;
}

// Operator - (vec3)
template <class T> vec4<T> vec4<T>::operator-(vec3<T> v)
{
	vec4<T> result;
	result.x = x-v.x; result.y = y-v.y; result.z = z-v.z; result.w = w-v.w;
	return result;
}

#endif
