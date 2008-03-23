/*
	*** Periodic cell definition
	*** src/classes/cell.cpp
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

#include "classes/cell.h"
#include "classes/atom.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include <math.h>
#include <algorithm>
using namespace std;

// Cell types
const char *CT_strings[CT_NITEMS] = { "None", "Cubic", "Orthorhombic", "Parallelepiped" };
const char *text_from_CT(CellType i)
	{ return CT_strings[i]; }
CellType CT_from_text(const char *s)
	{ return (CellType) enumSearch("cell type",CT_NITEMS,CT_strings,s); }
const char **get_CT_strings()
	{ return CT_strings; }

// Constructor
Cell::Cell()
{
	// Private variables
	type_ = CT_NONE;
	axes_.zero();
	transpose_.zero();
	itranspose_.zero();
	reciprocal_.zero();
	lengths_.zero();
	angles_.zero();
	centre_.zero();
	volume_ = 0.0;
	reciprocalVolume_ = 0.0;
}

/*
// Set
*/

// Remove the cell definition (i.e. set 'type' to CT_NONE)
void Cell::reset()
{
	type_ = CT_NONE;
	centre_.zero();
}

// Set lengths and calculates matrix
void Cell::setLengths(const Vec3<double> &lengths)
{
	set(lengths,angles_);
}

// Set individual length
void Cell::setLength(int i, double d)
{
	lengths_.set(i,d);
}

// Set individual angle
void Cell::setAngle(int i, double d)
{
	angles_.set(i,d);
}

// Return the type of cell
CellType Cell::type() const
{
	return type_;
}

// Return the cell vector matrix
Mat3<double> Cell::transpose()
{
	return transpose_;
}

// Return the transpose of the cell vector matrix (giving individual axis vectors in rows[])
Mat3<double> Cell::axes()
{
	return axes_;
}

// Return the cell vector matrix as a 4x4 matrix
void Cell::axesForGl(double *glmat)
{
	glmat[0] = axes_.rows[0].x;
	glmat[1] = axes_.rows[0].y;
	glmat[2] = axes_.rows[0].z;
	glmat[3] = 0.0;

	glmat[4] = axes_.rows[1].x;
	glmat[5] = axes_.rows[1].y;
	glmat[6] = axes_.rows[1].z;
	glmat[7] = 0.0;

	glmat[8] = axes_.rows[2].x;
	glmat[9] = axes_.rows[2].y;
	glmat[10] = axes_.rows[2].z;
	glmat[11] = 0.0;

	glmat[12] = 0.0;
	glmat[13] = 0.0;
	glmat[14] = 0.0;
	glmat[15] = 1.0;
}

// Return a matrix of the reciprocal cell vectors
Mat3<double> Cell::reciprocal()
{
	return reciprocal_;
}

// Return the axis lengths of the cell
Vec3<double> Cell::lengths()
{
	return lengths_;
}

// Return the angles the cell
Vec3<double> Cell::angles()
{
	return angles_;
}

// Return the origin the cell
Vec3<double> Cell::centre()
{
	return centre_;
}

// Return the cell vectors as a column-major matrix in a 1D array
void Cell::transposeColumn(double* m)
{
	transpose_.copyColumnMajor(m);
}

// Return the reciprocal vectors as a column-major matrix in a 1D array
void Cell::reciprocalColumn(double* m)
{
	reciprocal_.copyColumnMajor(m);
}

// Return a inverse transpose matrix of cell axes
Mat3<double> Cell::inverseTranspose()
{
	return itranspose_;
}

// Return the inverse of the cell vectors as a column-major matrix in a 1D array
void Cell::inverseTransposeColumn(double *m)
{
	itranspose_.copyColumnMajor(m);
}

// Return the volume of the cell
double Cell::volume() const
{
	return volume_;
}

// Return the volume of the reciprocal cell
double Cell::reciprocalVolume() const
{
	return reciprocalVolume_;
}

// Return the density of the cell
double Cell::density() const
{
	return density_;
}

// Determine Type
void Cell::determineType()
{
	dbgBegin(DM_CALLS,"Cell::determineType");
	// Compare cell angles_....
	double ab, bc, ac;
	int count = 0;
	if (fabs(90.0 - angles_.x) < 1.0e-5) count ++;
	if (fabs(90.0 - angles_.y) < 1.0e-5) count ++;
	if (fabs(90.0 - angles_.z) < 1.0e-5) count ++;
	// If all sides are orthogonal then either cubic or orthorhombic (2 == monoclinic, 0 == triclinic)
	if (count == 3)
	{
		// Must check lengths as well
		count = 0;
		if (fabs(lengths_.x - lengths_.y) < 1.0e-5) count ++;
		if (fabs(lengths_.x - lengths_.z) < 1.0e-5) count ++;
		if (count == 2) type_ = CT_CUBIC;
		else type_ = CT_ORTHORHOMBIC;
	}
	else type_ = CT_PARALLELEPIPED;
	dbgEnd(DM_CALLS,"Cell::determineType");
}

// Set (by parameters)
void Cell::set(const Vec3<double> &newlengths, const Vec3<double> &newangles)
{
	dbgBegin(DM_CALLS,"Cell::set[parameters]");
	double temp;
	// Store cell lengths and angles (in degrees) in structure
	angles_ = newangles;
	lengths_ = newlengths;
	// Work in unit vectors. Assume that A lays along x-axis
	axes_.set(0,1.0,0.0,0.0);
	// Assume that B lays in the xy plane. Since A={1,0,0}, cos(gamma) equals 'x' of the B vector.
	temp = cos(angles_.z/DEGRAD);
	axes_.set(1,temp,sqrt(1.0 - temp*temp),0.0);
	// The C vector can now be determined in parts.
	// It's x-component is equal to cos(beta) since {1,0,0}{x,y,z} = {1}{x} = cos(beta)
	axes_.set(2,cos(angles_.y/DEGRAD),0.0,0.0);
	// The y-component can be determined by completing the dot product between the B and C vectors
	axes_.rows[2].y = ( cos(angles_.x/DEGRAD) - axes_.rows[1].x*axes_.rows[2].x ) / axes_.rows[1].y;
	// The z-component is simply the remainder of the unit vector...
	axes_.rows[2].z = sqrt(1.0 - axes_.rows[2].x*axes_.rows[2].x - axes_.rows[2].y*axes_.rows[2].y);
	// Lastly, adjust these unit vectors to give the proper cell lengths
	axes_.rows[0] *= lengths_.x;
	axes_.rows[1] *= lengths_.y;
	axes_.rows[2] *= lengths_.z;
	transpose_ = axes_.transpose();
	// Determine type of cell
	determineType();
	// Calculate the cell volume
	volume_ = axes_.determinant();
	calculateCentre();
	calculateInverse();
	calculateReciprocal();
	dbgEnd(DM_CALLS,"Cell::set[parameters]");
}

// Set (by matrix)
void Cell::set(const Mat3<double> &newaxes)
{
	dbgBegin(DM_CALLS,"Cell::set[matrix]");
	// Store the supplied matrix and get transpose for vector calculation
	axes_ = newaxes;
	transpose_ = axes_.transpose();
	// Calculate cell lengths
	lengths_.x = axes_.rows[0].magnitude();
	lengths_.y = axes_.rows[1].magnitude();
	lengths_.z = axes_.rows[2].magnitude();
	// Calculate cell angles
	Vec3<double> vecx,vecy,vecz;
	vecx = axes_.rows[0];
	vecy = axes_.rows[1];
	vecz = axes_.rows[2];
	vecx.normalise();
	vecy.normalise();
	vecz.normalise();
	angles_.x = acos(vecy.dp(vecz));
	angles_.y = acos(vecx.dp(vecz));
	angles_.z = acos(vecx.dp(vecy));
	angles_ *= DEGRAD;
	// Determine type of cell
	determineType();
	// Calculate the cell volume
	volume_ = axes_.determinant();
	calculateCentre();
	calculateInverse();
	calculateReciprocal();
	dbgEnd(DM_CALLS,"Cell::set[matrix]");
}

// Calculate reciprocal cell vectors
void Cell::calculateReciprocal()
{
	// Calculate the reciprocal cell of 'this->cell'
	dbgBegin(DM_CALLS,"Cell::calculateReciprocal");
	switch (type_)
	{
		case (CT_NONE):
			msg(DM_NONE,"Cell : Can't calculate reciprocal cell - no cell defined.\n");
			break;
		case (CT_CUBIC):
		case (CT_ORTHORHOMBIC):
			reciprocal_.rows[0].set(TWOPI / axes_.rows[0].x, 0.0, 0.0);
			reciprocal_.rows[1].set(0.0, TWOPI / axes_.rows[1].y, 0.0);
			reciprocal_.rows[2].set(0.0, 0.0, TWOPI / axes_.rows[2].z);
			reciprocalVolume_ = TWOPI / (axes_.rows[0].x * axes_.rows[1].y * axes_.rows[2].z);
			break;
		case (CT_PARALLELEPIPED):
			// Reciprocal cell vectors are perpendicular to normal cell axes_t.
			// Calculate from cross products of normal cell triples
			reciprocal_.rows[0] = axes_.rows[1] * axes_.rows[2];
			reciprocal_.rows[1] = axes_.rows[0] * axes_.rows[2];
			reciprocal_.rows[2] = axes_.rows[0] * axes_.rows[1];
			reciprocalVolume_ = fabs( axes_.rows[0].x*reciprocal_.rows[0].x + axes_.rows[1].y*reciprocal_.rows[1].y + axes_.rows[2].z*reciprocal_.rows[2].z);
			reciprocal_.rows[0] = reciprocal_.rows[0] * TWOPI / reciprocalVolume_;
			reciprocal_.rows[1] = reciprocal_.rows[1] * TWOPI / reciprocalVolume_;
			reciprocal_.rows[2] = reciprocal_.rows[2] * TWOPI / reciprocalVolume_;
			break;
	}
	dbgEnd(DM_CALLS,"Cell::calculateReciprocal");
}

// Calculate centre coordinate of cell
void Cell::calculateCentre()
{
	dbgBegin(DM_CALLS,"Cell::calculateCentre");
	if (type_ != CT_NONE)
	{
		centre_.set(0.5,0.5,0.5);
		centre_ *= transpose_;
	}
	else centre_.set(0.0,0.0,0.0);
	centre_.print();
	dbgEnd(DM_CALLS,"Cell::calculateCentre");
}

// Calculate inverse transpose matrix
void Cell::calculateInverse()
{
	dbgBegin(DM_CALLS,"Cell::calculateInverse");
	itranspose_ = transpose_;
	itranspose_.invert();
	dbgEnd(DM_CALLS,"Cell::calculateInverse");
}

/*
// Minimum Image Routines
*/

// Minimum image position
Vec3<double> Cell::mim(const Vec3<double> &r1, const Vec3<double> &r2) const
{
	// Returns the minimum image coordinates of r1 with respect to r2.
	// Since we work in fractional cell coordinates, can do simple integer subtraction for cubic and pseudo-cubic boundary conditions
	static Vec3<double> R;
	switch (type_)
	{
		// No cell - just return r1
		case (CT_NONE):
			R = r1;
			break;
		// Remaining boundary conditions are all cubic or pseudo-cubic, so can be grouped together...
		default:
			R = r1 - r2;
			R *= itranspose_;
			// TODO Test speed of 'int' version
			if (R.x < -0.5) R.x += 1.0;
			if (R.y < -0.5) R.y += 1.0;
			if (R.z < -0.5) R.z += 1.0;
			if (R.x > 0.5) R.x -= 1.0;
			if (R.y > 0.5) R.y -= 1.0;
			if (R.z > 0.5) R.z -= 1.0;
			// R.x -= int(R.x);
			// R.y -= int(R.y);
			// R.z -= int(R.z);
			R *= transpose_;
			R += r2;
	}
	return R;
}

// Minimum image vector
Vec3<double> Cell::mimd(const Vec3<double> &r1, const Vec3<double> &r2) const
{
	static Vec3<double> R;
	R = mim(r1,r2);
	R -= r2;
	return R;
}

// Minimimum image routines with atom and vector pointers
Vec3<double> Cell::mim(Atom *i, const Vec3<double> &r2) const
{
	return mim(i->r(),r2);
}

Vec3<double> Cell::mimd(Atom *i, const Vec3<double> &r2) const
{
	return mimd(i->r(),r2);
}

// Minimimum image routines with atom pointers
Vec3<double> Cell::mim(Atom *i, Atom *j) const
{
	return mim(i->r(),j->r());
}

Vec3<double> Cell::mimd(Atom *i, Atom *j) const
{
	return mimd(i->r(),j->r());
}

// Fold atom
void Cell::fold(Vec3<double> &r) const
{
	// Folds the coordinates in 'r' into the defined unit cell
	dbgBegin(DM_MORECALLS,"Cell::fold");
	static Vec3<double> newr;
	switch (type_)
	{
		// No cell, so no image to fold into
		case (CT_NONE):
			break;
		// Remaining cell types are cubic or pseudo-cubic, so subtract integer part of position
		default:
			newr = r;
			// Convert these coordinates into fractional cell coordinates...
			newr *= itranspose_;
			if (newr.x < 0.0) newr.x += 1.0;
			else if (newr.x >= 1.0) newr.x -= 1.0;
			if (newr.y < 0.0) newr.y += 1.0;
			else if (newr.y >= 1.0) newr.y -= 1.0;
			if (newr.z < 0.0) newr.z += 1.0;
			else if (newr.z >= 1.0) newr.z -= 1.0;
			//newr.x -= floor(newr.x);
			//newr.y -= floor(newr.y);
			//newr.z -= floor(newr.z);
			// Convert back into world coordinates
			r = newr * transpose_;
			break;
	}
	dbgEnd(DM_MORECALLS,"Cell::fold");
}

void Cell::fold(Atom *i) const
{
	fold(i->r());
}

/*
// Geometry Calculation
*/

double Cell::distance(const Vec3<double> &r1, const Vec3<double> &r2) const
{
	// Calculate the distance between atoms i and j
	dbgBegin(DM_MORECALLS,"Cell::distance");
	static Vec3<double> mimi;
	mimi = mimd(r1,r2);
	dbgEnd(DM_MORECALLS,"Cell::distance");
	return mimi.magnitude();
}

double Cell::distance(Atom *i, Atom *j) const
{
	return distance(i->r(),j->r());
}

double Cell::angle(const Vec3<double> &r1, const Vec3<double> &r2, const Vec3<double> &r3) const
{
	// Calculate the angle formed between atoms i, j, and k
	// Result is returned in radians.
	dbgBegin(DM_MORECALLS,"Cell::angle");
	static Vec3<double> vecij, veckj;
	static double dp, a;
	vecij = mimd(r1,r2);
	veckj = mimd(r3,r2);
	// Normalise vectors and calculate dot product and angle.
	vecij.normalise();
	veckj.normalise();
	dp = vecij.dp(veckj);
	a = acos(dp);
	dbgEnd(DM_MORECALLS,"Cell::angle");
	return a;
}

double Cell::angle(Atom *i, Atom *j, Atom *k) const
{
	return angle(i->r(),j->r(),k->r());
}

double Cell::torsion(const Vec3<double> &i, const Vec3<double> &j, const Vec3<double> &k, const Vec3<double> &l) const
{
	// Calculate the torsion angle formed between the atoms i, j, k, and l.
	// Return result is in radians.
	dbgBegin(DM_MORECALLS,"Cell::torsion");
	static Vec3<double> vecji, veckl, vecjk, veckj, mim_k, xpj, xpk;
	static double dp, angle;
	// Vector j->i (minimum image of i w.r.t. j)
	vecji = mimd(i,j);
	// Vectors j->k and k->j (minimum image of k w.r.t. j)
	mim_k = mim(k,j);
	vecjk = mim_k - j;
	veckj = -vecjk;
	// Vector k->l (minimum image of l w.r.t. k (in turn w.r.t. j))
	veckl = mimd(mim_k,l);
	// Calculate cross products
	xpj = vecjk * vecji;
	xpj.normalise();
	xpk = veckj * veckl;
	xpk.normalise();
	dp = xpj.dp(xpk);
	//dp = (dp < 0 ? (dp < -1 ? : min(dp,1));
	if (dp < -1) dp = -1;
	else if (dp > 1) dp = 1;
	angle = acos(dp);
	// Calculate sign of torsion
	dp = xpj.dp(veckl);
	if (dp > 0) angle = -angle;
	dbgEnd(DM_MORECALLS,"Cell::torsion");
	return angle;
}

double Cell::torsion(Atom *i, Atom *j, Atom *k, Atom *l) const
{
	return torsion(i->r(),j->r(),k->r(),l->r());
}

/*
// Coordinate Transforms
*/

// Return the fractional coordinates of the specified position
Vec3<double> Cell::realToFrac(const Vec3<double> &v) const
{
	// Convert the real coordinates supplied into fractional cell coordinates
	return (v * itranspose_);
}

// Return the real coordinates of the specified fractional cell coordinate
Vec3<double> Cell::fracToReal(const Vec3<double> &v) const
{
	// Convert the fractional cell coordinates supplied into real cell coordinates
	return (v * transpose_);
}

/*
// Misc
*/

// Generate a random position inside the unit cell
Vec3<double> Cell::randomPos() const
{
	// Multiply some random fractional cell coordinates by the unit cell axes
	static Vec3<double> result;
	result.x = csRandom();
	result.y = csRandom();
	result.z = csRandom();
	result *= transpose_;
	return result;
}

// Print
void Cell::print() const
{
	msg(DM_NONE,"\t        x        y        z          l\n");
	msg(DM_NONE,"\t[ A <%8.4f %8.4f %8.4f > %8.4f [alpha=%8.3f]\n", axes_.rows[0].x, axes_.rows[0].y, axes_.rows[0].z, lengths_.x, angles_.x);
	msg(DM_NONE,"\t[ B <%8.4f %8.4f %8.4f > %8.4f [ beta=%8.3f]\n", axes_.rows[1].x, axes_.rows[1].y, axes_.rows[1].z, lengths_.y, angles_.y);
	msg(DM_NONE,"\t[ C <%8.4f %8.4f %8.4f > %8.4f [gamma=%8.3f]\n", axes_.rows[2].x, axes_.rows[2].y, axes_.rows[2].z, lengths_.z, angles_.z);
}
