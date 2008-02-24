/*
	*** 3-Component vector class
	*** src/templates/vector3.h
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

#ifndef H_VECTOR3_H
#define H_VECTOR3_H

#include "templates/matrix3.h"
#include "templates/matrix4.h"
#include "base/debug.h"
#include "base/constants.h"
#include "base/mathfunc.h"
#include <stdio.h>
#include <math.h>

// Forward declarations
template <class T> struct vec3;
template <class T> struct vec4;
template <class T> struct mat3;
template <class T> struct mat4;

// 3D vector
template <class T> struct vec3
{
	public:
	// Constructor / Destructor
	vec3<T>(T xx = 0, T yy = 0, T zz = 0);
	~vec3();
	#ifdef MEMDEBUG
	// Copy constructor
	vec3<T>(const vec3<T>&);
	#endif
	
	/*
	// 3-Vector
	*/
	public:
	// Components of vector
	T x,y,z;
	// Set the vector to 0,0,0
	void zero();
	// Set all three components simultaneously
	void set(T, T, T);
	// Add plain values to all three components
	void add(T, T, T);

	// Operators + and +=
	void operator=(const vec4<T>&);
	void operator+=(T);
	void operator+=(const vec3<T>&);
	vec3<T> operator+(T) const;
	vec3<T> operator+(const vec3<T>&) const;

	// Operators - and -=
	void operator-=(T);
	void operator-=(const vec3<T>&);
	vec3<T> operator-(T) const;
	vec3<T> operator-() const;
	vec3<T> operator-(const vec3<T>&) const;

	// Operators / and /=
	void operator/=(const T);
	void operator/=(const vec3<T>&);
	vec3<T> operator/(T) const;
	vec3<T> operator/(const vec3<T>&) const;

	// Operators * and *=
	vec3<T> operator*(T) const;
	void operator*=(T);
	//void operator*=(const vec3<T>&);
	vec3<T> operator*(const vec3<T>&) const;
	vec3<T> operator*(const mat3<T>&) const;
	void operator*=(const mat3<T>&);
	vec3<T> operator*(const mat4<T>&) const;
	void operator*=(const mat4<T> &A);

	// Other functions
	// Normalise the vector to unity
	void normalise();
	// Orthogonalise (Gram-Schmidt) w.r.t. supplied vector
	void orthogonalise(const vec3<T>&);
	// Returns the largest compone&nt of the vector
	T max() const;
	// Returns the smallest component of the vector
	T min() const;
	// Returns the largest absolute component of the vector
	T absmax() const;
	// Returns the smallest absolute component of the vector
	T absmin() const;
	// Calculate vector magnitude
	inline double magnitude() const;
	// Normalise and return original magnitude
	double mag_and_normalise();
	// Dot product between this and supplied vector
	double dp(const vec3<T> &v) const;
	// Returns the minimum absolute-valued element in the vector
	int absminelement() const;
	// Returns the maximum absolute-valued element in the vector
	int absmaxelement() const;
	// Returns the minimum valued element in the vector
	int minelement() const;
	// Returns the maximum valued element in the vector
	int maxelement() const;
	// Returns an orthogonal vector
	vec3<T> get_orthogonal() const;
	// Set the specific element to value
	void set(int, T);
	// Returns the value of the specified element
	T get(int) const;
	// Generate random unit vector
	void random_unit();
	// Prints the contents of the vector
	void print() const;
	// Convert cartesian x,y,z coordinates into spherical (rho,phi/zenith,theta/azimuthal)
	void to_spherical();
	// Convert spherical who,phi,theta coordinates into cartesian x,y,z
	void to_cartesian();
};

// Constructor
template <class T> vec3<T>::vec3(T xx, T yy, T zz)
{
	x = xx;
	y = yy;
	z = zz;
	#ifdef MEMDEBUG
		memdbg.create[MD_VEC3] ++;
	#endif
}

// Destructor
template <class T> vec3<T>::~vec3()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_VEC3] ++;
	#endif
}

#ifdef MEMDEBUG
	// Copy constructor
	template <class T> vec3<T>::vec3(const vec3<T> &v)
	{
		x = v.x; y = v.y; z = v.z;
		memdbg.create[MD_VEC3COPY] ++;
	}
#endif

// Zero
template <class T> void vec3<T>::zero()
{
	x = 0; y = 0; z = 0;
}

// Set
template <class T> void vec3<T>::set(T a, T b, T c)
{
	x = a; y = b; z = c;
}

// Add
template <class T> void vec3<T>::add(T a, T b, T c)
{
	x += a; y += b; z += c;
}

// Operator = (from vec4)
template <class T> void vec3<T>::operator=(const vec4<T> &v)
{
	x = v.x; y = v.y; z = v.z;
}

// Operator += (T)
template <class T> void vec3<T>::operator+=(T v)
{
	x += v; y += v; z += v;
}

// Operator += (vec3)
template <class T> void vec3<T>::operator+=(const vec3<T> &v)
{
	x += v.x; y += v.y; z += v.z;
}

// Operator + (T)
template <class T> vec3<T> vec3<T>::operator+(T v) const
{
	vec3<T> result;
	result.x = x+v; result.y = y+v; result.z = z+v;
	return result;
}

// Operator + (vec3)
template <class T> vec3<T> vec3<T>::operator+(const vec3<T> &v) const
{
	vec3<T> result;
	result.x = x+v.x; result.y = y+v.y; result.z = z+v.z;
	return result;
}

// Operator -= (T)
template <class T> void vec3<T>::operator-=(T v)
{
	x -= v;
	y -= v;
	z -= v;
}

// Operator -= (vec3)
template <class T> void vec3<T>::operator-=(const vec3<T> &v)
{
	x -= v.x;
	y -= v.y;
	z -= v.z;
}

// Unary Operator -
template <class T> vec3<T> vec3<T>::operator-() const
{
	vec3<T> result;
	result.x = -x;
	result.y = -y;
	result.z = -z;
	return result;
}

// Operator - (T)
template <class T> vec3<T> vec3<T>::operator-(T v) const
{
	vec3<T> result;
	result.set(x-v,y-v,z-v);
	return result;
}

// Operator - (vec3)
template <class T> vec3<T> vec3<T>::operator-(const vec3<T> &v) const
{
	vec3<T> result;
	result.set(x-v.x,y-v.y,z-v.z);
	return result;
}

// Operator /= (T)
template <class T> void vec3<T>::operator/=(T v)
{
	x /= v;
	y /= v;
	z /= v;
}

// Operator /= (vec3)
template <class T> void vec3<T>::operator/=(const vec3<T> &v)
{
	x /= v.x;
	y /= v.y;
	z /= v.z;
}

// Operator / (T)
template <class T> vec3<T> vec3<T>::operator/(T v) const
{
	vec3<T> result;
	result.x = x/v;
	result.y = y/v;
	result.z = z/v;
	return result;
}

// Operator / (vec3)
template <class T> vec3<T> vec3<T>::operator/(const vec3<T> &v) const
{
	vec3<T> result;
	result.x = x/v.x;
	result.y = y/v.y;
	result.z = z/v.z;
	return result;
}

// Operator *= (T)
template <class T> void vec3<T>::operator*=(T v)
{
	x *= v;
	y *= v;
	z *= v;
}

// Operator * (T)
template <class T> vec3<T> vec3<T>::operator*(T v) const
{
	vec3<T> result;
	result.x = x*v;
	result.y = y*v;
	result.z = z*v;
	return result;
}

// Operator * (vec3) (Cross product)
template <class T> vec3<T> vec3<T>::operator*(const vec3<T> &v) const
{
	vec3<T> result;
	result.x = y * v.z - z * v.y;
	result.y = z * v.x - x * v.z;
	result.z = x * v.y - y * v.x;
	return result;
}

// Operator *= (mat3)
template <class T> void vec3<T>::operator*=(const mat3<T> &A)
{
	vec3<T> result;
	result.x = x*A.rows[0].x + y*A.rows[0].y + z*A.rows[0].z;
	result.y = x*A.rows[1].x + y*A.rows[1].y + z*A.rows[1].z;
	result.z = x*A.rows[2].x + y*A.rows[2].y + z*A.rows[2].z;
	*this = result; 
}

// Operator *= (mat4)
template <class T> void vec3<T>::operator*=(const mat4<T> &A)
{
	vec3<T> result;
	result.x = x*A.rows[0].x + y*A.rows[0].y + z*A.rows[0].z + A.rows[0].w;
	result.y = x*A.rows[1].x + y*A.rows[1].y + z*A.rows[1].z + A.rows[1].w;
	result.z = x*A.rows[2].x + y*A.rows[2].y + z*A.rows[2].z + A.rows[2].w;
	*this = result;
}

// Operator * (mat3)
template <class T> vec3<T> vec3<T>::operator*(const mat3<T> &A) const
{
	vec3<T> result;
	result.x = x*A.rows[0].x + y*A.rows[0].y + z*A.rows[0].z;
	result.y = x*A.rows[1].x + y*A.rows[1].y + z*A.rows[1].z;
	result.z = x*A.rows[2].x + y*A.rows[2].y + z*A.rows[2].z;
	return result;
}

// Operator * (mat4)
template <class T> vec3<T> vec3<T>::operator*(const mat4<T> &A) const
{
	vec3<T> result;
	result.x = x*A.rows[0].x + y*A.rows[0].y + z*A.rows[0].z + A.rows[0].w;
	result.y = x*A.rows[1].x + y*A.rows[1].y + z*A.rows[1].z + A.rows[1].w;
	result.z = x*A.rows[2].x + y*A.rows[2].y + z*A.rows[2].z + A.rows[2].w;
	return result;
}

// Normalise
template <class T> void vec3<T>::normalise()
{
	double mag = sqrt(x*x + y*y + z*z);
	x /= mag; y /= mag; z /= mag;
}

// Orthogonalise
template <class T> void vec3<T>::orthogonalise(const vec3<T> &source)
{
	double sourcemag = source.magnitude();
	double dpovermagsq = dp(source) / (sourcemag * sourcemag);
	x = x - dpovermagsq * source.x;
	y = y - dpovermagsq * source.y;
	z = z - dpovermagsq * source.z;
}

// Largest value
template <class T> T vec3<T>::max() const
{
	T a = (x < y) ? y : x;
	return (a < z) ? z : a;
}

// Smallest value
template <class T> T vec3<T>::min() const
{
	T a = (x > y) ? y : x;
	return (a > z) ? z : a;
}
// Largest absolute value
template <class T> T vec3<T>::absmax() const
{
	T a = (fabs(x) < fabs(y)) ? fabs(y) : fabs(x);
	return (a < fabs(z)) ? fabs(z) : a;
}

// Smallest absolute value
template <class T> T vec3<T>::absmin() const
{
	T a = (fabs(x) > fabs(y)) ? fabs(y) : fabs(x);
	return (a > fabs(z)) ? fabs(z) : a;
}

// Minimum absolute element
template <class T> int vec3<T>::absminelement() const
{
	if ((fabs(x) <= fabs(y)) && (fabs(x) <= fabs(z))) return 0;
	if ((fabs(y) <= fabs(x)) && (fabs(y) <= fabs(z))) return 1;
	return 2;
}

// Maximum absolute element
template <class T> int vec3<T>::absmaxelement() const
{
	if ((fabs(x) >= fabs(y)) && (fabs(x) >= fabs(z))) return 0;
	if ((fabs(y) >= fabs(x)) && (fabs(y) >= fabs(z))) return 1;
	return 2;
}

// Minimum element
template <class T> int vec3<T>::minelement() const
{
	if ((x <= y) && (x <= z)) return 0;
	if ((y <= x) && (y <= z)) return 1;
	return 2;
}

// Maximum element
template <class T> int vec3<T>::maxelement() const
{
	if ((x >= y) && (x >= z)) return 0;
	if ((y >= x) && (y >= z)) return 1;
	return 2;
}

// Get orthogonal vector
template <class T> vec3<T> vec3<T>::get_orthogonal() const
{
	// Returns a vector orthogonal to this vector
	// Find largest element and then select one of the others
	vec3<T> result;
	//int a = absmaxelement();
	//int b = (a == 0 ? 1 : 0);
	result = *this;
	result.set(get(1),get(2),get(0));
	//result.set(a,get(b));
	//result.set(b,get(a));
	return result;
}

// Vector magnitude
template <class T> double vec3<T>::magnitude() const
{
	return sqrt(x*x + y*y + z*z);
}

// Normalise and return original magnitude
template <class T> double vec3<T>::mag_and_normalise()
{
	double mag = sqrt(x*x + y*y + z*z);
	x /= mag; y /= mag; z /= mag;
	return mag;
}

// Dot product
template <class T> double vec3<T>::dp(const vec3<T> &v) const
{
	return (x*v.x + y*v.y + z*v.z);
}

// Set element
template <class T> void vec3<T>::set(int el, T value)
{
	if (el == 0) x = value;
	else if (el == 1) y = value;
	else if (el == 2) z = value;
}

// Get element
template <class T> T vec3<T>::get(int el) const
{
	T result;
	if (el == 0) result = x;
	if (el == 1) result = y;
	if (el == 2) result = z;
	return result;
}

// Print
template <class T> void vec3<T>::print() const
{
	printf("vec = %8.4f %8.4f %8.4f\n",(double)x,(double)y,(double)z);
}

// Generate random unit vector
template <class T> void vec3<T>::random_unit()
{
	// Generates a random unit vector
	x = cs_random()-0.5;
	y = cs_random()-0.5;
	z = cs_random()-0.5;
	normalise();
}

// Convert to spherical
template <class T> void vec3<T>::to_spherical()
{
	T rho, s, phi, theta;
	rho = magnitude();
	s = sqrt(x*x + y*y);
	phi = acos(z / rho);
	if (s < 1e-7) theta = 0.0;
	else theta = (x < 0 ? PI - asin(y / s) : asin(y / s));
	set(rho,phi,theta);
}

// Convert to cartesian
template <class T> void vec3<T>::to_cartesian()
{
	// x = rho, y = phi, z = theta
	T newx,newy,newz;
	newx = x * sin(y) * cos(z);
	newy = x * sin(y) * sin(z);
	newz = x * cos(y);
	set(newx,newy,newz);
}

#endif
