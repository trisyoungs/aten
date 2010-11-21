/*
	*** Specialized OpenGL 4x4 Matrix class
	*** src/render/glmatrix.cpp
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

#include "render/glmatrix.h"

/*
// Operators
*/

// Matrix multiply (operator *) (return new matrix)
GLMatrix GLMatrix::operator*(const GLMatrix &B) const
{
	// [ row(A|this).column(B) ]
	GLMatrix AB;
	AB.matrix_[0] = matrix_[0]*B.matrix_[0] + matrix_[4]*B.matrix_[1] + matrix_[8]*B.matrix_[2] + matrix_[12]*B.matrix_[3];
	AB.matrix_[1] = matrix_[1]*B.matrix_[0] + matrix_[5]*B.matrix_[1] + matrix_[9]*B.matrix_[2] + matrix_[13]*B.matrix_[3];
	AB.matrix_[2] = matrix_[2]*B.matrix_[0] + matrix_[6]*B.matrix_[1] + matrix_[10]*B.matrix_[2] + matrix_[14]*B.matrix_[3];
	AB.matrix_[3] = matrix_[3]*B.matrix_[0] + matrix_[7]*B.matrix_[1] + matrix_[11]*B.matrix_[2] + matrix_[15]*B.matrix_[3];

	AB.matrix_[4] = matrix_[0]*B.matrix_[4] + matrix_[4]*B.matrix_[5] + matrix_[8]*B.matrix_[6] + matrix_[12]*B.matrix_[7];
	AB.matrix_[5] = matrix_[1]*B.matrix_[4] + matrix_[5]*B.matrix_[5] + matrix_[9]*B.matrix_[6] + matrix_[13]*B.matrix_[7];
	AB.matrix_[6] = matrix_[2]*B.matrix_[4] + matrix_[6]*B.matrix_[5] + matrix_[10]*B.matrix_[6] + matrix_[14]*B.matrix_[7];
	AB.matrix_[7] = matrix_[3]*B.matrix_[4] + matrix_[7]*B.matrix_[5] + matrix_[11]*B.matrix_[6] + matrix_[15]*B.matrix_[7];

	AB.matrix_[8] = matrix_[0]*B.matrix_[8] + matrix_[4]*B.matrix_[9] + matrix_[8]*B.matrix_[10] + matrix_[12]*B.matrix_[11];
	AB.matrix_[9] = matrix_[1]*B.matrix_[8] + matrix_[5]*B.matrix_[9] + matrix_[9]*B.matrix_[10] + matrix_[13]*B.matrix_[11];
	AB.matrix_[10] = matrix_[2]*B.matrix_[8] + matrix_[6]*B.matrix_[9] + matrix_[10]*B.matrix_[10] + matrix_[14]*B.matrix_[11];
	AB.matrix_[11] = matrix_[3]*B.matrix_[8] + matrix_[7]*B.matrix_[9] + matrix_[11]*B.matrix_[10] + matrix_[15]*B.matrix_[11];

	AB.matrix_[12] = matrix_[0]*B.matrix_[12] + matrix_[4]*B.matrix_[13] + matrix_[8]*B.matrix_[14] + matrix_[12]*B.matrix_[15];
	AB.matrix_[13] = matrix_[1]*B.matrix_[12] + matrix_[5]*B.matrix_[13] + matrix_[9]*B.matrix_[14] + matrix_[13]*B.matrix_[15];
	AB.matrix_[14] = matrix_[2]*B.matrix_[12] + matrix_[6]*B.matrix_[13] + matrix_[10]*B.matrix_[14] + matrix_[14]*B.matrix_[15];
	AB.matrix_[15] = matrix_[3]*B.matrix_[12] + matrix_[7]*B.matrix_[13] + matrix_[11]*B.matrix_[14] + matrix_[15]*B.matrix_[15];
	return AB;
}

// Matrix multiply (operator *) (return new matrix)
GLMatrix GLMatrix::operator*(const Mat4<double> &B) const
{
	// [ row(A|this).column(B) ]
	GLMatrix AB;
	AB.matrix_[0] = matrix_[0]*B.rows[0].x + matrix_[4]*B.rows[1].x + matrix_[8]*B.rows[2].x + matrix_[12]*B.rows[3].x;
	AB.matrix_[1] = matrix_[1]*B.rows[0].x + matrix_[5]*B.rows[1].x + matrix_[9]*B.rows[2].x + matrix_[13]*B.rows[3].x;
	AB.matrix_[2] = matrix_[2]*B.rows[0].x + matrix_[6]*B.rows[1].x + matrix_[10]*B.rows[2].x + matrix_[14]*B.rows[3].x;
	AB.matrix_[3] = matrix_[3]*B.rows[0].x + matrix_[7]*B.rows[1].x + matrix_[11]*B.rows[2].x + matrix_[15]*B.rows[3].x;

	AB.matrix_[4] = matrix_[0]*B.rows[0].y + matrix_[4]*B.rows[1].y + matrix_[8]*B.rows[2].y + matrix_[12]*B.rows[3].y;
	AB.matrix_[5] = matrix_[1]*B.rows[0].y + matrix_[5]*B.rows[1].y + matrix_[9]*B.rows[2].y + matrix_[13]*B.rows[3].y;
	AB.matrix_[6] = matrix_[2]*B.rows[0].y + matrix_[6]*B.rows[1].y + matrix_[10]*B.rows[2].y + matrix_[14]*B.rows[3].y;
	AB.matrix_[7] = matrix_[3]*B.rows[0].y + matrix_[7]*B.rows[1].y + matrix_[11]*B.rows[2].y + matrix_[15]*B.rows[3].y;

	AB.matrix_[8] = matrix_[0]*B.rows[0].z + matrix_[4]*B.rows[1].z + matrix_[8]*B.rows[2].z + matrix_[12]*B.rows[3].z;
	AB.matrix_[9] = matrix_[1]*B.rows[0].z + matrix_[5]*B.rows[1].z + matrix_[9]*B.rows[2].z + matrix_[13]*B.rows[3].z;
	AB.matrix_[10] = matrix_[2]*B.rows[0].z + matrix_[6]*B.rows[1].z + matrix_[10]*B.rows[2].z + matrix_[14]*B.rows[3].z;
	AB.matrix_[11] = matrix_[3]*B.rows[0].z + matrix_[7]*B.rows[1].z + matrix_[11]*B.rows[2].z + matrix_[15]*B.rows[3].z;

	AB.matrix_[12] = matrix_[0]*B.rows[0].w + matrix_[4]*B.rows[1].w + matrix_[8]*B.rows[2].w + matrix_[12]*B.rows[3].w;
	AB.matrix_[13] = matrix_[1]*B.rows[0].w + matrix_[5]*B.rows[1].w + matrix_[9]*B.rows[2].w + matrix_[13]*B.rows[3].w;
	AB.matrix_[14] = matrix_[2]*B.rows[0].w + matrix_[6]*B.rows[1].w + matrix_[10]*B.rows[2].w + matrix_[14]*B.rows[3].w;
	AB.matrix_[15] = matrix_[3]*B.rows[0].w + matrix_[7]*B.rows[1].w + matrix_[11]*B.rows[2].w + matrix_[15]*B.rows[3].w;
	return AB;
}


Vec3<double> GLMatrix::operator*(const Vec3<double> &v) const
{
	Vec3<double> result;
	result.x = v.x*matrix_[0] + v.y*matrix_[4] + v.z*matrix_[8] + matrix_[12];
	result.y = v.x*matrix_[1] + v.y*matrix_[5] + v.z*matrix_[9] + matrix_[13];
	result.z = v.x*matrix_[2] + v.y*matrix_[6] + v.z*matrix_[10] + matrix_[14];
	return result;
}

Vec4<double> GLMatrix::operator*(const Vec4<double> &v) const
{
	Vec4<double> result;
	result.x = v.x*matrix_[0] + v.y*matrix_[4] + v.z*matrix_[8] + v.w*matrix_[12];
	result.y = v.x*matrix_[1] + v.y*matrix_[5] + v.z*matrix_[9] + v.w*matrix_[13];
	result.z = v.x*matrix_[2] + v.y*matrix_[6] + v.z*matrix_[10] + v.w*matrix_[14];
	result.w = v.x*matrix_[3] + v.y*matrix_[7] + v.z*matrix_[11] + v.w*matrix_[15];
	return result;
}

// Matrix multiply (operator *=)
GLMatrix &GLMatrix::operator*=(const GLMatrix &B)
{
	// [ row(A|this).column(B) ]
	GLMatrix AB;
	AB.matrix_[0] = matrix_[0]*B.matrix_[0] + matrix_[4]*B.matrix_[1] + matrix_[8]*B.matrix_[2] + matrix_[12]*B.matrix_[3];
	AB.matrix_[1] = matrix_[1]*B.matrix_[0] + matrix_[5]*B.matrix_[1] + matrix_[9]*B.matrix_[2] + matrix_[13]*B.matrix_[3];
	AB.matrix_[2] = matrix_[2]*B.matrix_[0] + matrix_[6]*B.matrix_[1] + matrix_[10]*B.matrix_[2] + matrix_[14]*B.matrix_[3];
	AB.matrix_[3] = matrix_[3]*B.matrix_[0] + matrix_[7]*B.matrix_[1] + matrix_[11]*B.matrix_[2] + matrix_[15]*B.matrix_[3];

	AB.matrix_[4] = matrix_[0]*B.matrix_[4] + matrix_[4]*B.matrix_[5] + matrix_[8]*B.matrix_[6] + matrix_[12]*B.matrix_[7];
	AB.matrix_[5] = matrix_[1]*B.matrix_[4] + matrix_[5]*B.matrix_[5] + matrix_[9]*B.matrix_[6] + matrix_[13]*B.matrix_[7];
	AB.matrix_[6] = matrix_[2]*B.matrix_[4] + matrix_[6]*B.matrix_[5] + matrix_[10]*B.matrix_[6] + matrix_[14]*B.matrix_[7];
	AB.matrix_[7] = matrix_[3]*B.matrix_[4] + matrix_[7]*B.matrix_[5] + matrix_[11]*B.matrix_[6] + matrix_[15]*B.matrix_[7];

	AB.matrix_[8] = matrix_[0]*B.matrix_[8] + matrix_[4]*B.matrix_[9] + matrix_[8]*B.matrix_[10] + matrix_[12]*B.matrix_[11];
	AB.matrix_[9] = matrix_[1]*B.matrix_[8] + matrix_[5]*B.matrix_[9] + matrix_[9]*B.matrix_[10] + matrix_[13]*B.matrix_[11];
	AB.matrix_[10] = matrix_[2]*B.matrix_[8] + matrix_[6]*B.matrix_[9] + matrix_[10]*B.matrix_[10] + matrix_[14]*B.matrix_[11];
	AB.matrix_[11] = matrix_[3]*B.matrix_[8] + matrix_[7]*B.matrix_[9] + matrix_[11]*B.matrix_[10] + matrix_[15]*B.matrix_[11];

	AB.matrix_[12] = matrix_[0]*B.matrix_[12] + matrix_[4]*B.matrix_[13] + matrix_[8]*B.matrix_[14] + matrix_[12]*B.matrix_[15];
	AB.matrix_[13] = matrix_[1]*B.matrix_[12] + matrix_[5]*B.matrix_[13] + matrix_[9]*B.matrix_[14] + matrix_[13]*B.matrix_[15];
	AB.matrix_[14] = matrix_[2]*B.matrix_[12] + matrix_[6]*B.matrix_[13] + matrix_[10]*B.matrix_[14] + matrix_[14]*B.matrix_[15];
	AB.matrix_[15] = matrix_[3]*B.matrix_[12] + matrix_[7]*B.matrix_[13] + matrix_[11]*B.matrix_[14] + matrix_[15]*B.matrix_[15];
	*this = AB;
	return *this;
}

void GLMatrix::operator=(const Mat4<double> &B)
{
	matrix_[0] = B.rows[0].x;
	matrix_[1] = B.rows[1].x;
	matrix_[2] = B.rows[2].x;
	matrix_[3] = B.rows[3].x;
	matrix_[4] = B.rows[0].y;
	matrix_[5] = B.rows[1].y;
	matrix_[6] = B.rows[2].y;
	matrix_[7] = B.rows[3].y;
	matrix_[8] = B.rows[0].z;
	matrix_[9] = B.rows[1].z;
	matrix_[10] = B.rows[2].z;
	matrix_[11] = B.rows[3].z;
	matrix_[12] = B.rows[0].w;
	matrix_[13] = B.rows[1].w;
	matrix_[14] = B.rows[2].w;
	matrix_[15] = B.rows[3].w;
}

GLfloat GLMatrix::operator[](int index)
{
	return matrix_[index];
}

/*
// Methods
*/

// Reset to the identity matrix
void GLMatrix::setIdentity()
{
	matrix_[0] = 1.0;
	matrix_[1] = 0.0;
	matrix_[2] = 0.0;
	matrix_[3] = 0.0;
	matrix_[4] = 0.0;
	matrix_[5] = 1.0;
	matrix_[6] = 0.0;
	matrix_[7] = 0.0;
	matrix_[8] = 0.0;
	matrix_[9] = 0.0;
	matrix_[10] = 1.0;
	matrix_[11] = 0.0;
	matrix_[12] = 0.0;
	matrix_[13] = 0.0;
	matrix_[14] = 0.0;
	matrix_[15] = 1.0;
}

// Set zero matrix
void GLMatrix::zero()
{
	matrix_[0] = 0.0;
	matrix_[1] = 0.0;
	matrix_[2] = 0.0;
	matrix_[3] = 0.0;
	matrix_[4] = 0.0;
	matrix_[5] = 0.0;
	matrix_[6] = 0.0;
	matrix_[7] = 0.0;
	matrix_[8] = 0.0;
	matrix_[9] = 0.0;
	matrix_[10] = 0.0;
	matrix_[11] = 0.0;
	matrix_[12] = 0.0;
	matrix_[13] = 0.0;
	matrix_[14] = 0.0;
	matrix_[15] = 0.0;
}

// Create rotation matrix about X
void GLMatrix::createRotationX(double angle)
{
	double cosx, sinx, theta = angle/DEGRAD;
	cosx = cos(theta);
	sinx = sin(theta);
	matrix_[0] = 1.0;
	matrix_[1] = 0.0;
	matrix_[2] = 0.0;
	matrix_[3] = 0.0;
	matrix_[4] = 0.0;
	matrix_[5] = cosx;
	matrix_[6] = -sinx;
	matrix_[7] = 0.0;
	matrix_[8] = 0.0;
	matrix_[9] = sinx;
	matrix_[10] = cosx;
	matrix_[11] = 0.0;
	matrix_[12] = 0.0;
	matrix_[13] = 0.0;
	matrix_[14] = 0.0;
	matrix_[15] = 1.0;
}

// Create XY rotation matrix
void GLMatrix::createRotationXY(double anglex, double angley)
{
	double cosx, sinx, cosy, siny, thetax = anglex/DEGRAD, thetay = angley/DEGRAD;
	cosx = cos(thetax);
	cosy = cos(thetay);
	sinx = sin(thetax);
	siny = sin(thetay);
	matrix_[0] = cosy;
	matrix_[1] = (-sinx)*(-siny);
	matrix_[2] = -siny*cosx;
	matrix_[3] = 0.0;
	matrix_[4] = 0.0;
	matrix_[5] = cosx;
	matrix_[6] = sinx;
	matrix_[7] = 0.0;
	matrix_[8] = siny;
	matrix_[9] = (-sinx)*cosy;
	matrix_[10] = cosx*cosy;
	matrix_[11] = 0.0;
	matrix_[12] = 0.0;
	matrix_[13] = 0.0;
	matrix_[14] = 0.0;
	matrix_[15] = 1.0;
}

// Create rotation matrix about Y
void GLMatrix::createRotationY(double angle)
{
	double cosx, sinx, theta = angle/DEGRAD;
	cosx = cos(theta);
	sinx = sin(theta);
	matrix_[0] = cosx;
	matrix_[1] = 0.0;
	matrix_[2] = sinx;
	matrix_[3] = 0.0;
	matrix_[4] = 0.0;
	matrix_[5] = 1.0;
	matrix_[6] = 0.0;
	matrix_[7] = 0.0;
	matrix_[8] = -sinx;
	matrix_[9] = 0.0;
	matrix_[10] = cosx;
	matrix_[11] = 0.0;
	matrix_[12] = 0.0;
	matrix_[13] = 0.0;
	matrix_[14] = 0.0;
	matrix_[15] = 1.0;
}

// Create rotation matrix about Z
void GLMatrix::createRotationZ(double angle)
{
	double cosx, sinx, theta = angle/DEGRAD;
	cosx = cos(theta);
	sinx = sin(theta);
	matrix_[0] = cosx;
	matrix_[1] = -sinx;
	matrix_[2] = 0.0;
	matrix_[3] = 0.0;
	matrix_[4] = sinx;
	matrix_[5] = cosx;
	matrix_[6] = 0.0;
	matrix_[7] = 0.0;
	matrix_[8] = 0.0;
	matrix_[9] = 0.0;
	matrix_[10] = 1.0;
	matrix_[11] = 0.0;
	matrix_[12] = 0.0;
	matrix_[13] = 0.0;
	matrix_[14] = 0.0;
	matrix_[15] = 1.0;
}

// Create axis rotation quaternion
void GLMatrix::createRotationAxis(double ax, double ay, double az, double angle, bool normalise)
{
	double cosx, sinx, theta = angle/DEGRAD;
	if (normalise)
	{
		double mag = sqrt(ax*ax + ay*ay + az*az);
		ax /= mag;
		ay /= mag;
		az /= mag;
	}
	cosx = cos(theta);
	sinx = sin(theta);
	matrix_[0] = ax*ax*(1.0-cosx) + cosx;
	matrix_[1] = ax*ay*(1.0-cosx) + az*sinx;
	matrix_[2] = ax*az*(1.0-cosx) - ay*sinx;
	matrix_[3] = 0.0;
	matrix_[4] = ax*ay*(1.0-cosx) - az*sinx;
	matrix_[5] = ay*ay*(1.0-cosx) + cosx;
	matrix_[6] = ay*az*(1.0-cosx) + ax*sinx;
	matrix_[7] = 0.0;
	matrix_[8] = ax*az*(1.0-cosx) + ay*sinx;
	matrix_[9] = ay*az*(1.0-cosx) - ax*sinx;
	matrix_[10] = az*az*(1.0-cosx) + cosx;
	matrix_[11] = 0.0;
	matrix_[12] = 0.0;
	matrix_[13] = 0.0;
	matrix_[14] = 0.0;
	matrix_[15] = 1.0;
}

// Apply rotation about X axis
void GLMatrix::applyRotationX(double angle)
{
	double cosx, sinx, theta = angle/DEGRAD, temp[4];
	cosx = cos(theta);
	sinx = sin(theta);

	// Recalculate second column and store in temp values
	temp[0] = matrix_[4]*cosx + matrix_[8]*-sinx;
	temp[1] = matrix_[5]*cosx + matrix_[9]*-sinx;
	temp[2] = matrix_[6]*cosx + matrix_[10]*-sinx;
	temp[3] = matrix_[7]*cosx + matrix_[11]*-sinx;

	matrix_[8] = matrix_[4]*sinx + matrix_[8]*cosx;
	matrix_[9] = matrix_[5]*sinx + matrix_[9]*cosx;
	matrix_[10] = matrix_[6]*sinx + matrix_[10]*cosx;
	matrix_[11] = matrix_[7]*sinx + matrix_[11]*cosx;

	matrix_[4] = temp[0];
	matrix_[5] = temp[1];
	matrix_[6] = temp[2];
	matrix_[7] = temp[3];
}

// Apply axis rotation quaternion
void GLMatrix::applyRotationAxis(double ax, double ay, double az, double angle, bool normalise)
{
	double cosx, sinx, theta = angle/DEGRAD, temp[8], multipliers[16];
	if (normalise)
	{
		double mag = sqrt(ax*ax + ay*ay + az*az);
		ax /= mag;
		ay /= mag;
		az /= mag;
	}
	cosx = cos(theta);
	sinx = sin(theta);
	multipliers[0] = ax*ax*(1.0-cosx) + cosx;
	multipliers[1] = ax*ay*(1.0-cosx) + az*sinx;
	multipliers[2] = ax*az*(1.0-cosx) - ay*sinx;
	multipliers[4] = ax*ay*(1.0-cosx) - az*sinx;
	multipliers[5] = ay*ay*(1.0-cosx) + cosx;
	multipliers[6] = ay*az*(1.0-cosx) + ax*sinx;
	multipliers[8] = ax*az*(1.0-cosx) + ay*sinx;
	multipliers[9] = ay*az*(1.0-cosx) - ax*sinx;
	multipliers[10] = az*az*(1.0-cosx) + cosx;

	temp[0] = matrix_[0]*multipliers[0] + matrix_[4]*multipliers[1] + matrix_[8]*multipliers[2];
	temp[1] = matrix_[1]*multipliers[0] + matrix_[5]*multipliers[1] + matrix_[9]*multipliers[2];
	temp[2] = matrix_[2]*multipliers[0] + matrix_[6]*multipliers[1] + matrix_[10]*multipliers[2];
	temp[3] = matrix_[3]*multipliers[0] + matrix_[7]*multipliers[1] + matrix_[11]*multipliers[2];

	temp[4] = matrix_[0]*multipliers[4] + matrix_[4]*multipliers[5] + matrix_[8]*multipliers[6];
	temp[5] = matrix_[1]*multipliers[4] + matrix_[5]*multipliers[5] + matrix_[9]*multipliers[6];
	temp[6] = matrix_[2]*multipliers[4] + matrix_[6]*multipliers[5] + matrix_[10]*multipliers[6];
	temp[7] = matrix_[3]*multipliers[4] + matrix_[7]*multipliers[5] + matrix_[11]*multipliers[6];

	matrix_[8] = matrix_[0]*multipliers[8] + matrix_[4]*multipliers[9] + matrix_[8]*multipliers[10];
	matrix_[9] = matrix_[1]*multipliers[8] + matrix_[5]*multipliers[9] + matrix_[9]*multipliers[10];
	matrix_[10] = matrix_[2]*multipliers[8] + matrix_[6]*multipliers[9] + matrix_[10]*multipliers[10];
	matrix_[11] = matrix_[3]*multipliers[8] + matrix_[7]*multipliers[9] + matrix_[11]*multipliers[10];

	matrix_[0] = temp[0];
	matrix_[1] = temp[1];
	matrix_[2] = temp[2];
	matrix_[3] = temp[3];
	matrix_[4] = temp[4];
	matrix_[5] = temp[5];
	matrix_[6] = temp[6];
	matrix_[7] = temp[7];
}

// Apply a translation to the matrix (as glTranslated would do)
void GLMatrix::createTranslation(double dx, double dy, double dz)
{
	matrix_[0] = 1.0;
	matrix_[1] = 0.0;
	matrix_[2] = 0.0;
	matrix_[3] = 0.0;
	matrix_[4] = 0.0;
	matrix_[5] = 1.0;
	matrix_[6] = 0.0;
	matrix_[7] = 0.0;
	matrix_[8] = 0.0;
	matrix_[9] = 0.0;
	matrix_[10] = 1.0;
	matrix_[11] = 0.0;
	matrix_[12] = dx;
	matrix_[13] = dy;
	matrix_[14] = dz;
	matrix_[15] = 1.0;
}

// Apply a translation to the matrix (as glTranslated would do)
void GLMatrix::applyTranslation(double dx, double dy, double dz)
{
	matrix_[12] += matrix_[0]*dx + matrix_[4]*dy + matrix_[8]*dz;
	matrix_[13] += matrix_[1]*dx + matrix_[5]*dy + matrix_[9]*dz;
	matrix_[14] += matrix_[2]*dx + matrix_[6]*dy + matrix_[10]*dz;
}

// Apply a translation to the matrix (as glTranslated would to)
void GLMatrix::applyTranslation(Vec3<double> vec)
{
	matrix_[12] += matrix_[0]*vec.x + matrix_[4]*vec.y + matrix_[8]*vec.z;
	matrix_[13] += matrix_[1]*vec.x + matrix_[5]*vec.y + matrix_[9]*vec.z;
	matrix_[14] += matrix_[2]*vec.x + matrix_[6]*vec.y + matrix_[10]*vec.z;
}

// Add a translation to the matrix
void GLMatrix::translate(double dx, double dy, double dz)
{
	matrix_[12] += dx;
	matrix_[13] += dy;
	matrix_[14] += dz;
}

// Apply a general scaling to the matrix (as glScaled would do)
void GLMatrix::applyScaling(double scalex, double scaley, double scalez)
{
	applyScalingX(scalex);
	applyScalingY(scaley);
	applyScalingZ(scalez);
}

// Apply a x scaling to the matrix
void GLMatrix::applyScalingX(double scale)
{
	matrix_[0] *= scale;
	matrix_[1] *= scale;
	matrix_[2] *= scale;
	matrix_[3] *= scale;
}

// Apply a y scaling to the matrix
void GLMatrix::applyScalingY(double scale)
{
	matrix_[4] *= scale;
	matrix_[5] *= scale;
	matrix_[6] *= scale;
	matrix_[7] *= scale;
}

// Apply a z scaling to the matrix
void GLMatrix::applyScalingZ(double scale)
{
	matrix_[8] *= scale;
	matrix_[9] *= scale;
	matrix_[10] *= scale;
	matrix_[11] *= scale;
}

// Print matrix
void GLMatrix::print() const
{
	printf("GLMat      X        Y         Z    Translate\n");
	printf("        %8.4f %8.4f %8.4f %8.4f\n",matrix_[0], matrix_[4], matrix_[8], matrix_[12]);
	printf("        %8.4f %8.4f %8.4f %8.4f\n",matrix_[1], matrix_[5], matrix_[9], matrix_[13]);
	printf("        %8.4f %8.4f %8.4f %8.4f\n",matrix_[2], matrix_[6], matrix_[10], matrix_[14]);
	printf("Scale   %8.4f %8.4f %8.4f %8.4f\n",matrix_[3], matrix_[7], matrix_[11], matrix_[15]);
}

// Return matrix array
GLdouble *GLMatrix::matrix()
{
	return matrix_;
}

// Multiply against coordinates provided
void GLMatrix::multiply(GLfloat *r, GLfloat *transformed)
{
	transformed[0] = r[0]*matrix_[0] + r[1]*matrix_[4] + r[2]*matrix_[8] + matrix_[12];
	transformed[1] = r[0]*matrix_[1] + r[1]*matrix_[5] + r[2]*matrix_[9] + matrix_[13];
	transformed[2] = r[0]*matrix_[2] + r[1]*matrix_[6] + r[2]*matrix_[10] + matrix_[14];
}

// Apply rotational part of matrix to supplied vector
Vec3<double> GLMatrix::rotateVector(Vec3<double> &v)
{
	Vec3<double> result;
	result.x = v.x*matrix_[0] + v.y*matrix_[4] + v.z*matrix_[8];
	result.y = v.x*matrix_[1] + v.y*matrix_[5] + v.z*matrix_[9];
	result.z = v.x*matrix_[2] + v.y*matrix_[6] + v.z*matrix_[10];
	return result;
}