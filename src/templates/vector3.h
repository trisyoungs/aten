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

#ifndef ATEN_VECTOR3_H
#define ATEN_VECTOR3_H

// Prevent complaints for incorrect arguments to 'macro max()' and 'macro min()
#define NOMINMAX

#include "templates/matrix3.h"
#include "templates/matrix4.h"
#include "base/messenger.h"
#include "base/constants.h"
#include "base/mathfunc.h"
#include <stdio.h>
#include <math.h>

// 3D vector
template <class T> class Vec3
{
	public:
	// Constructor
	Vec3<T>(T xx = 0, T yy = 0, T zz = 0);
	
	/*
	// 3-Vector
	*/
	public:
	// Components of vector
	T x,y,z;
	// Set the vector to 0,0,0
	void zero();
	// Set the specific element to value
	void set(int, T);
	// Set all three components simultaneously
	void set(T, T, T);
	// Add value to single component
	void add(int, T);
	// Add values to all three components simultaneously
	void add(T, T, T);

	// Operators + and +=
	////void operator=(const Vec4<T>&);
	void operator+=(T);
	void operator+=(const Vec3<T>&);
	Vec3<T> operator+(T) const;
	Vec3<T> operator+(const Vec3<T>&) const;

	// Operators - and -=
	void operator-=(T);
	void operator-=(const Vec3<T>&);
	Vec3<T> operator-(T) const;
	Vec3<T> operator-() const;
	Vec3<T> operator-(const Vec3<T>&) const;

	// Operators / and /=
	void operator/=(const T);
	void operator/=(const Vec3<T>&);
	Vec3<T> operator/(T) const;
	Vec3<T> operator/(const Vec3<T>&) const;

	// Operators * and *=
	Vec3<T> operator*(T) const;
	void operator*=(T);
	//void operator*=(const Vec3<T>&);
	Vec3<T> operator*(const Vec3<T>&) const;
	Vec3<T> operator*(const Mat3<T>&) const;
	void operator*=(const Mat3<T>&);
	////Vec3<T> operator*(const Mat4<T>&) const;
	void operator*=(const Mat4<T> &A);

	// Normalise the vector to unity
	void normalise();
	// Orthogonalise (Gram-Schmidt) w.r.t. supplied vector
	void orthogonalise(const Vec3<T>&);
	// Returns the largest compone&nt of the vector
	T max() const;
	// Returns the smallest component of the vector
	T min() const;
	// Returns the largest absolute component of the vector
	T absMax() const;
	// Returns the smallest absolute component of the vector
	T absMin() const;
	// Calculate vector magnitude
	inline double magnitude() const;
	// Normalise and return original magnitude
	double magAndNormalise();
	// Dot product between this and supplied vector
	double dp(const Vec3<T> &v) const;
	// Returns the minimum absolute-valued element in the vector
	int absMinElement() const;
	// Returns the maximum absolute-valued element in the vector
	int absMaxElement() const;
	// Returns the minimum valued element in the vector
	int minElement() const;
	// Returns the maximum valued element in the vector
	int maxElement() const;
	// Returns an orthogonal vector
	Vec3<T> orthogonal() const;
	// Returns the value of the specified element
	T get(int) const;
	// Generate random unit vector
	void randomUnit();
	// Prints the contents of the vector
	void print() const;
	// Convert cartesian x,y,z coordinates into spherical (rho,phi/zenith,theta/azimuthal)
	void toSpherical();
	// Convert spherical who,phi,theta coordinates into cartesian x,y,z
	void toCartesian();
};

// Constructor
template <class T> Vec3<T>::Vec3(T xx, T yy, T zz)
{
	x = xx;
	y = yy;
	z = zz;
}

/*
// Set
*/

// Zero
template <class T> void Vec3<T>::zero()
{
	x = 0;
	y = 0;
	z = 0;
}

// Set element
template <class T> void Vec3<T>::set(int el, T value)
{
	if (el == 0) x = value;
	else if (el == 1) y = value;
	else if (el == 2) z = value;
}

// Set
template <class T> void Vec3<T>::set(T a, T b, T c)
{
	x = a;
	y = b;
	z = c;
}

// Adjust element
template <class T> void Vec3<T>::add(int el, T value)
{
	if (el == 0) x += value;
	else if (el == 1) y += value;
	else if (el == 2) z += value;
}

// Add to all
template <class T> void Vec3<T>::add(T a, T b, T c)
{
	x += a;
	y += b;
	z += c;
}

/*
// Operators
*/

// Operator += (T)
template <class T> void Vec3<T>::operator+=(T v)
{
	x += v;
	y += v;
	z += v;
}

// Operator += (Vec3)
template <class T> void Vec3<T>::operator+=(const Vec3<T> &v)
{
	x += v.x;
	y += v.y;
	z += v.z;
}

// Operator + (T)
template <class T> Vec3<T> Vec3<T>::operator+(T v) const
{
	Vec3<T> result;
	result.x = x+v;
	result.y = y+v;
	result.z = z+v;
	return result;
}

// Operator + (Vec3)
template <class T> Vec3<T> Vec3<T>::operator+(const Vec3<T> &v) const
{
	Vec3<T> result;
	result.x = x+v.x;
	result.y = y+v.y;
	result.z = z+v.z;
	return result;
}

// Operator -= (T)
template <class T> void Vec3<T>::operator-=(T v)
{
	x -= v;
	y -= v;
	z -= v;
}

// Operator -= (Vec3)
template <class T> void Vec3<T>::operator-=(const Vec3<T> &v)
{
	x -= v.x;
	y -= v.y;
	z -= v.z;
}

// Unary Operator -
template <class T> Vec3<T> Vec3<T>::operator-() const
{
	Vec3<T> result;
	result.x = -x;
	result.y = -y;
	result.z = -z;
	return result;
}

// Operator - (T)
template <class T> Vec3<T> Vec3<T>::operator-(T v) const
{
	Vec3<T> result;
	result.set(x-v,y-v,z-v);
	return result;
}

// Operator - (Vec3)
template <class T> Vec3<T> Vec3<T>::operator-(const Vec3<T> &v) const
{
	Vec3<T> result;
	result.set(x-v.x,y-v.y,z-v.z);
	return result;
}

// Operator /= (T)
template <class T> void Vec3<T>::operator/=(T v)
{
	x /= v;
	y /= v;
	z /= v;
}

// Operator /= (Vec3)
template <class T> void Vec3<T>::operator/=(const Vec3<T> &v)
{
	x /= v.x;
	y /= v.y;
	z /= v.z;
}

// Operator / (T)
template <class T> Vec3<T> Vec3<T>::operator/(T v) const
{
	Vec3<T> result;
	result.x = x/v;
	result.y = y/v;
	result.z = z/v;
	return result;
}

// Operator / (Vec3)
template <class T> Vec3<T> Vec3<T>::operator/(const Vec3<T> &v) const
{
	Vec3<T> result;
	result.x = x/v.x;
	result.y = y/v.y;
	result.z = z/v.z;
	return result;
}

// Operator *= (T)
template <class T> void Vec3<T>::operator*=(T v)
{
	x *= v;
	y *= v;
	z *= v;
}

// Operator * (T)
template <class T> Vec3<T> Vec3<T>::operator*(T v) const
{
	Vec3<T> result;
	result.x = x*v;
	result.y = y*v;
	result.z = z*v;
	return result;
}

// Operator * (Vec3) (Cross product)
template <class T> Vec3<T> Vec3<T>::operator*(const Vec3<T> &v) const
{
	Vec3<T> result;
	result.x = y * v.z - z * v.y;
	result.y = z * v.x - x * v.z;
	result.z = x * v.y - y * v.x;
	return result;
}

// Operator *= (mat3)
template <class T> void Vec3<T>::operator*=(const Mat3<T> &A)
{
	Vec3<T> result;
	result.x = x*A.rows[0].x + y*A.rows[0].y + z*A.rows[0].z;
	result.y = x*A.rows[1].x + y*A.rows[1].y + z*A.rows[1].z;
	result.z = x*A.rows[2].x + y*A.rows[2].y + z*A.rows[2].z;
	*this = result; 
}

// Operator *= (mat4)
template <class T> void Vec3<T>::operator*=(const Mat4<T> &A)
{
	Vec3<T> result;
	result.x = x*A.rows[0].x + y*A.rows[0].y + z*A.rows[0].z + A.rows[0].w;
	result.y = x*A.rows[1].x + y*A.rows[1].y + z*A.rows[1].z + A.rows[1].w;
	result.z = x*A.rows[2].x + y*A.rows[2].y + z*A.rows[2].z + A.rows[2].w;
	*this = result;
}

// Operator * (mat3)
template <class T> Vec3<T> Vec3<T>::operator*(const Mat3<T> &A) const
{
	Vec3<T> result;
	result.x = x*A.rows[0].x + y*A.rows[0].y + z*A.rows[0].z;
	result.y = x*A.rows[1].x + y*A.rows[1].y + z*A.rows[1].z;
	result.z = x*A.rows[2].x + y*A.rows[2].y + z*A.rows[2].z;
	return result;
}

/* Operator * (mat4)
template <class T> Vec3<T> Vec3<T>::operator*(const Mat4<T> &A) const
{
	Vec3<T> result;
	result.x = x*A.rows[0].x + y*A.rows[0].y + z*A.rows[0].z + A.rows[0].w;
	result.y = x*A.rows[1].x + y*A.rows[1].y + z*A.rows[1].z + A.rows[1].w;
	result.z = x*A.rows[2].x + y*A.rows[2].y + z*A.rows[2].z + A.rows[2].w;
	return result;
} */

/*
// Methods
*/

// Normalise
template <class T> void Vec3<T>::normalise()
{
	double mag = sqrt(x*x + y*y + z*z);
	x /= mag;
	y /= mag;
	z /= mag;
}

// Orthogonalise
template <class T> void Vec3<T>::orthogonalise(const Vec3<T> &source)
{
	double sourcemag = source.magnitude();
	double dpovermagsq = dp(source) / (sourcemag * sourcemag);
	x = x - dpovermagsq * source.x;
	y = y - dpovermagsq * source.y;
	z = z - dpovermagsq * source.z;
}

// Largest value
template <class T> T Vec3<T>::max() const
{
	T a = (x < y) ? y : x;
	return (a < z) ? z : a;
}

// Smallest value
template <class T> T Vec3<T>::min() const
{
	T a = (x > y) ? y : x;
	return (a > z) ? z : a;
}
// Largest absolute value
template <class T> T Vec3<T>::absMax() const
{
	T a = (fabs(x) < fabs(y)) ? fabs(y) : fabs(x);
	return (a < fabs(z)) ? fabs(z) : a;
}

// Smallest absolute value
template <class T> T Vec3<T>::absMin() const
{
	T a = (fabs(x) > fabs(y)) ? fabs(y) : fabs(x);
	return (a > fabs(z)) ? fabs(z) : a;
}

// Minimum absolute element
template <class T> int Vec3<T>::absMinElement() const
{
	if ((fabs(x) <= fabs(y)) && (fabs(x) <= fabs(z))) return 0;
	if ((fabs(y) <= fabs(x)) && (fabs(y) <= fabs(z))) return 1;
	return 2;
}

// Maximum absolute element
template <class T> int Vec3<T>::absMaxElement() const
{
	if ((fabs(x) >= fabs(y)) && (fabs(x) >= fabs(z))) return 0;
	if ((fabs(y) >= fabs(x)) && (fabs(y) >= fabs(z))) return 1;
	return 2;
}

// Minimum element
template <class T> int Vec3<T>::minElement() const
{
	if ((x <= y) && (x <= z)) return 0;
	if ((y <= x) && (y <= z)) return 1;
	return 2;
}

// Maximum element
template <class T> int Vec3<T>::maxElement() const
{
	if ((x >= y) && (x >= z)) return 0;
	if ((y >= x) && (y >= z)) return 1;
	return 2;
}

// Get orthogonal vector
template <class T> Vec3<T> Vec3<T>::orthogonal() const
{
	// Returns a vector orthogonal to this vector
	Vec3<T> result;
	int a = absMaxElement();
	if (a == 0) result.set(-y,x,0);
	else result.set(-get(a),(a == 1 ? x : y),0);
	result = result * (*this);
	result.normalise();
	return result;
}

// Vector magnitude
template <class T> double Vec3<T>::magnitude() const
{
	return sqrt(x*x + y*y + z*z);
}

// Normalise and return original magnitude
template <class T> double Vec3<T>::magAndNormalise()
{
	double mag = sqrt(x*x + y*y + z*z);
	x /= mag;
	y /= mag;
	z /= mag;
	return mag;
}

// Dot product
template <class T> double Vec3<T>::dp(const Vec3<T> &v) const
{
	return (x*v.x + y*v.y + z*v.z);
}

// Get element
template <class T> T Vec3<T>::get(int el) const
{
	T result;
	if (el == 0) result = x;
	if (el == 1) result = y;
	if (el == 2) result = z;
	return result;
}

// Print
template <class T> void Vec3<T>::print() const
{
	printf("vec = %8.4f %8.4f %8.4f\n",(double)x,(double)y,(double)z);
}

// Generate random unit vector
template <class T> void Vec3<T>::randomUnit()
{
	// Generates a random unit vector
	x = csRandom()-0.5;
	y = csRandom()-0.5;
	z = csRandom()-0.5;
	normalise();
}

// Convert to spherical
template <class T> void Vec3<T>::toSpherical()
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
template <class T> void Vec3<T>::toCartesian()
{
	// x = rho, y = phi, z = theta
	T newx,newy,newz;
	newx = x * sin(y) * cos(z);
	newy = x * sin(y) * sin(z);
	newz = x * cos(y);
	set(newx,newy,newz);
}

#endif
