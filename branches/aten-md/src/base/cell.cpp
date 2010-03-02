/*
	*** Periodic cell definition
	*** src/base/cell.cpp
	Copyright T. Youngs 2007-2009

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

#include "base/cell.h"
#include "base/atom.h"
#include "model/model.h"
#include "base/sysfunc.h"
using namespace std;

// Cell types
const char *CellTypeKeywords[Cell::nCellTypes] = { "None", "Cubic", "Orthorhombic", "Parallelepiped" };
const char *Cell::cellType(Cell::CellType i)
{
	return CellTypeKeywords[i];
}
Cell::CellType Cell::cellType(const char *s, bool reporterror)
{
	Cell::CellType ct = (Cell::CellType) enumSearch("cell type",Cell::nCellTypes,CellTypeKeywords,s);
	if ((ct == Cell::nCellTypes) && reporterror) enumPrintValid(Cell::nCellTypes,CellTypeKeywords);
	return ct;
}

// Cell definition parameters
const char *CellParameterKeywords[Cell::nCellParameters] = { "a", "b", "c", "alpha", "beta", "gamma", "ax", "ay", "az", "bx", "by", "bz", "cx", "cy", "cz" };
Cell::CellParameter Cell::cellParameter(const char *s)
{
	return (CellParameter) enumSearch("cell parameter",Cell::nCellParameters,CellParameterKeywords,s);
}

// Constructor
Cell::Cell()
{
	// Private variables
	type_ = Cell::NoCell;
	axes_.zero();
	transpose_.zero();
	itranspose_.zero();
	reciprocal_.zero();
	lengths_.zero();
	angles_.zero();
	centre_.zero();
	volume_ = 0.0;
	reciprocalVolume_ = 0.0;
	spacegroup_ = 0;
	spacegroupId_ = 0;
	parent_ = NULL;
}

// Assignment operator
void Cell::operator=(Cell &source)
{
	type_ = source.type_;
	axes_ = source.axes_;
	transpose_ = source.transpose_;
	reciprocal_ = source.reciprocal_;
	itranspose_ = source.itranspose_;
	centre_ = source.centre_;
	lengths_ = source.lengths_;
	angles_ = source.angles_;
	volume_ = source.volume_;
	reciprocalVolume_ = source.reciprocalVolume_;
	density_ = source.density_;
}

// Set parent model
void Cell::setParent(Model *m)
{
	parent_ = m;
}

// Return parent model
Model *Cell::parent()
{
	return parent_;
}

/*
// Set
*/

// Remove the cell definition (i.e. set 'type' to Cell::NoCell)
void Cell::reset()
{
	type_ = Cell::NoCell;
	centre_.zero();
}

// Set (by parameters)
void Cell::set(const Vec3<double> &newlengths, const Vec3<double> &newangles)
{
	msg.enter("Cell::set[vectors]");
	// Store cell lengths and angles (in degrees) in structure
	angles_ = newangles;
	lengths_ = newlengths;
	// Calculate new matrix
	calculateMatrix();
	// Update dependent quantities
	update();
	msg.exit("Cell::set[vectors]");
}

// Set (by matrix)
void Cell::set(const Mat3<double> &newaxes)
{
	msg.enter("Cell::set[matrix]");
	// Store the supplied matrix
	axes_ = newaxes;
	// Calculate new vectors
	calculateVectors();
	// Update dependent quantities
	update();
	msg.exit("Cell::set[matrix]");
}

// Set lengths and calculates matrix
void Cell::setLengths(const Vec3<double> &newlengths)
{
	// Store new cell lengths
	lengths_ = newlengths;
	// Calculate new matrix
	calculateMatrix();
	// Update dependent quantities
	update();
}

// Set individual length
void Cell::setLength(int i, double d)
{
	// Store new cell lengths
	lengths_.set(i,d);
	// Calculate new matrix
	calculateMatrix();
	// Update dependent quantities
	update();
}

// Set individual angle
void Cell::setAngle(int i, double d)
{
	// Store the supplied matrix
	angles_.set(i,d);
	// Calculate new vectors
	calculateVectors();
	// Update dependent quantities
	update();
}

// Set / adjust individual parameter
void Cell::setParameter(Cell::CellParameter cp, double value, bool adjust)
{
	switch (cp)
	{
		case (Cell::nCellParameters):
			printf("No cell parameter supplied to Cell::adjustParameter.\n");
			break;
		// Cell lengths
		case (Cell::CellA):
		case (Cell::CellB):
		case (Cell::CellC):
			adjust ? lengths_.add(cp - Cell::CellA, value) : lengths_.set(cp - Cell::CellA, value);
			// Calculate new matrix
			calculateMatrix();
			break;
		// Cell angles
		case (Cell::CellAlpha):
		case (Cell::CellBeta):
		case (Cell::CellGamma):
			adjust ? angles_.add(cp - Cell::CellAlpha, value) : angles_.set(cp - Cell::CellAlpha, value);
			// Calculate new matrix
			calculateMatrix();
			break;
		// Cell matrix elements
		default:
			// Adjust current matrix, then recalculate vectors
			int i = cp - Cell::CellAX;
			adjust ? axes_.add(i/3, i%3, value) : axes_.set(i/3, i%3, value);
			calculateVectors();
			break;
	}
	// Update dependent quantities
	update();
}

// Return the type of cell
Cell::CellType Cell::type() const
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
double *Cell::axesForGL()
{
	return axes_.forGL();
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

// Sets the spacegroup Id
void Cell::setSpacegroupId(int i)
{
	if ((i < 0) || (i > 230)) msg.print( "Warning - %i is not a valid spacegroup number. Spacegroup not set.\n", i);
	else spacegroupId_ = i;
}

// Return the spacegroup Id
int Cell::spacegroupId()
{
	return spacegroupId_;
}

// Sets the spacegroup by text name
void Cell::setSpacegroup(const char *s)
{
	printf("Oddly, setting spacegroups by name is not implemented yet.\n");
	// TGAY
}

// Return the spacegroup name
const char *Cell::spacegroup()
{
	return spacegroup_.get();
}

// Add manual generator
Generator *Cell::addGenerator()
{
	return generators_.add();
}

// Return number of manual generators defined
int Cell::nGenerators()
{
	return generators_.nItems();
}

// Return first manually-defined generator
Generator *Cell::generators()
{
	return generators_.first();
}

// Update dependent quantities
void Cell::update()
{
	// Calculate transpose of axes
	transpose_ = axes_.transpose();
	// Determine type of cell
	determineType();
	// Calculate the cell volume
	volume_ = axes_.determinant();
	// Calculate centre, inverse, and reciprocal
	calculateCentre();
	calculateInverse();
	calculateReciprocal();
}

// Determine Type
void Cell::determineType()
{
	msg.enter("Cell::determineType");
	// Compare cell angles....
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
		if (count == 2) type_ = Cell::CubicCell;
		else type_ = Cell::OrthorhombicCell;
		// While we're here, symmetrise the matrix for cubic and orthorhombic cells
		axes_.rows[0].y = axes_.rows[0].z = 0.0;
		axes_.rows[1].x = axes_.rows[1].z = 0.0;
		axes_.rows[2].x = axes_.rows[2].y = 0.0;
	}
	else type_ = Cell::ParallelepipedCell;
	msg.exit("Cell::determineType");
}

// Calculate cell lengths/angles from current matrix
void Cell::calculateVectors()
{
	msg.enter("Cell::calculateVectors");
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
	msg.exit("Cell::calculateVectors");
}

// Calculate cell matrix from current vectors
void Cell::calculateMatrix()
{
	msg.enter("Cell::calculateMatrix");
	double temp;
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
	msg.exit("Cell::calculateMatrix");
}

// Calculate reciprocal cell vectors
void Cell::calculateReciprocal()
{
	// Calculate the reciprocal cell of 'this->cell'
	msg.enter("Cell::calculateReciprocal");
	switch (type_)
	{
		case (Cell::NoCell):
			msg.print("Cell : Can't calculate reciprocal cell - no cell defined.\n");
			break;
		case (Cell::CubicCell):
		case (Cell::OrthorhombicCell):
			reciprocal_.rows[0].set(1.0 / axes_.rows[0].x, 0.0, 0.0);
			reciprocal_.rows[1].set(0.0, 1.0 / axes_.rows[1].y, 0.0);
			reciprocal_.rows[2].set(0.0, 0.0, 1.0 / axes_.rows[2].z);
			reciprocalVolume_ = 1.0 / (axes_.rows[0].x * axes_.rows[1].y * axes_.rows[2].z);
			break;
		case (Cell::ParallelepipedCell):
			// Reciprocal cell vectors are perpendicular to normal cell axes_t.
			// Calculate from cross products of normal cell vectors
			reciprocal_.rows[0] = axes_.rows[1] * axes_.rows[2];
			reciprocal_.rows[1] = axes_.rows[0] * axes_.rows[2];
			reciprocal_.rows[2] = axes_.rows[0] * axes_.rows[1];
			reciprocalVolume_ = fabs( axes_.rows[0].x*reciprocal_.rows[0].x + axes_.rows[1].y*reciprocal_.rows[1].y + axes_.rows[2].z*reciprocal_.rows[2].z);
			reciprocal_.rows[0] = reciprocal_.rows[0] / reciprocalVolume_;
			reciprocal_.rows[1] = reciprocal_.rows[1] / reciprocalVolume_;
			reciprocal_.rows[2] = reciprocal_.rows[2] / reciprocalVolume_;
			reciprocalVolume_ = 1.0 / reciprocalVolume_;
			break;
	}
	msg.exit("Cell::calculateReciprocal");
}

// Calculate centre coordinate of cell
void Cell::calculateCentre()
{
	msg.enter("Cell::calculateCentre");
	if (type_ != Cell::NoCell)
	{
		centre_.set(0.5,0.5,0.5);
		centre_ *= transpose_;
	}
	else centre_.set(0.0,0.0,0.0);
	msg.exit("Cell::calculateCentre");
}

// Calculate inverse transpose matrix
void Cell::calculateInverse()
{
	msg.enter("Cell::calculateInverse");
	itranspose_ = transpose_;
	itranspose_.invert();
	msg.exit("Cell::calculateInverse");
}

/*
// Minimum Image Routines
*/

// Minimum image position
Vec3<double> Cell::mim(const Vec3<double> &r1, const Vec3<double> &r2) const
{
	// Returns the minimum image coordinates of r1 with respect to r2.
	static Vec3<double> R;
	static double half;
	switch (type_)
	{
		// No cell - just return r1
		case (Cell::NoCell):
			R = r1;
			break;
		// Cubic
		case (Cell::CubicCell):
			R .set(r1.x,r1.y,r1.z);
			R -= r2;
			half = lengths_.x * 0.5;
			if (R.x < -half) R.x += lengths_.x;
			else if (R.x > half) R.x -= lengths_.x;
			if (R.y < -half) R.y += lengths_.x;
			else if (R.y > half) R.y -= lengths_.x;
			if (R.z < -half) R.z += lengths_.x;
			else if (R.z > half) R.z -= lengths_.x;
			R += r2;
			break;
		// Orthorhombic
		case (Cell::OrthorhombicCell):
			R .set(r1.x,r1.y,r1.z);
			R -= r2;
			half = lengths_.x * 0.5;
			if (R.x < -half) R.x += lengths_.x;
			else if (R.x > half) R.x -= lengths_.x;
			half = lengths_.y * 0.5;
			if (R.y < -half) R.y += lengths_.y;
			else if (R.y > half) R.y -= lengths_.y;
			half = lengths_.z * 0.5;
			if (R.z < -half) R.z += lengths_.z;
			else if (R.z > half) R.z -= lengths_.z;
			R += r2;
			break;
		// Parallelepiped 
		default:
			R .set(r1.x,r1.y,r1.z);
			R -= r2;
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
void Cell::fold(Vec3<double> &r, Atom *i, Model *parent) const
{
	// Folds the coordinates in 'r' into the defined unit cell
	msg.enter("Cell::fold");
	static Vec3<double> newr;
	switch (type_)
	{
		// No cell, so no image to fold into
		case (Cell::NoCell):
			break;
		// Cubic / Orthorhombic
		case (Cell::CubicCell):
		case (Cell::OrthorhombicCell):
			newr = r;
			if (newr.x < 0.0) newr.x -= int((newr.x-lengths_.x)/lengths_.x) * lengths_.x;
			else if (newr.x > lengths_.x) newr.x -= int(newr.x/lengths_.x)* lengths_.x;
			if (newr.y < 0.0) newr.y -= int((newr.y-lengths_.y)/lengths_.y)* lengths_.y;
			else if (newr.y > lengths_.y) newr.y -= int(newr.y/lengths_.y)* lengths_.y;
			if (newr.z < 0.0) newr.z -= int((newr.z-lengths_.z)/lengths_.z)* lengths_.z;
			else if (newr.z > lengths_.z) newr.z -= int(newr.z/lengths_.z)* lengths_.z;
/*			if (newr.x < 0.0) newr.x += lengths_.x;
			else if (newr.x > lengths_.x) newr.x -= lengths_.x;
			if (newr.y < 0.0) newr.y += lengths_.y;
			else if (newr.y > lengths_.y) newr.y -= lengths_.y;
			if (newr.z < 0.0) newr.z += lengths_.z;
			else if (newr.z > lengths_.z) newr.z -= lengths_.z;*/
			// Use model functions to store new position if we were given one
			if (parent != NULL) parent->positionAtom(i, newr);
			else r = newr;
			break;
		// Parallelepiped
		default:
			newr = r;
			// Convert these coordinates into fractional cell coordinates...
			newr *= itranspose_;
			if (newr.x < 0.0) newr.x -= int(newr.x-1.0);
			else if (newr.x > 1.0) newr.x -= int(newr.x);
			if (newr.y < 0.0) newr.y -= int(newr.y-1.0);
			else if (newr.y > 1.0) newr.y -= int(newr.y);
			if (newr.z < 0.0) newr.z -= int(newr.z-1.0);
			else if (newr.z > 1.0) newr.z -= int(newr.z);
// 			if (newr.x < 0.0) newr.x += 1.0;
// 			else if (newr.x >= 1.0) newr.x -= 1.0;
// 			if (newr.y < 0.0) newr.y += 1.0;
// 			else if (newr.y >= 1.0) newr.y -= 1.0;
// 			if (newr.z < 0.0) newr.z += 1.0;
// 			else if (newr.z >= 1.0) newr.z -= 1.0;
			// Convert back into world coordinates
			newr *= transpose_;
			// Use model functions to store new position if we were given one
			if (parent != NULL) parent->positionAtom(i, newr);
			else r = newr;
			break;
	}
	msg.exit("Cell::fold");
}

void Cell::fold(Atom *i, Model *parent) const
{
	fold(i->r(), i, parent);
}

/*
// Geometry Calculation
*/

double Cell::distance(const Vec3<double> &r1, const Vec3<double> &r2) const
{
	// Calculate the distance between atoms i and j
	msg.enter("Cell::distance");
	static Vec3<double> mimi;
	mimi = mimd(r1,r2);
	msg.exit("Cell::distance");
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
	msg.enter("Cell::angle");
	static Vec3<double> vecij, veckj;
	static double dp, a;
	vecij = mimd(r1,r2);
	veckj = mimd(r3,r2);
	// Normalise vectors and calculate dot product and angle.
	vecij.normalise();
	veckj.normalise();
	dp = vecij.dp(veckj);
	a = acos(dp);
	msg.exit("Cell::angle");
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
	msg.enter("Cell::torsion");
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
	if (dp < -1.0) dp = -1.0;
	else if (dp > 1.0) dp = 1.0;
	angle = acos(dp);
	// Calculate sign of torsion
	dp = xpj.dp(veckl);
	if (dp > 0) angle = -angle;
	msg.exit("Cell::torsion");
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
	return (transpose_ * v);
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
	msg.print("\t        x        y        z          l\n");
	msg.print("\t[ A <%8.4f %8.4f %8.4f > %8.4f [alpha=%8.3f]\n", axes_.rows[0].x, axes_.rows[0].y, axes_.rows[0].z, lengths_.x, angles_.x);
	msg.print("\t[ B <%8.4f %8.4f %8.4f > %8.4f [ beta=%8.3f]\n", axes_.rows[1].x, axes_.rows[1].y, axes_.rows[1].z, lengths_.y, angles_.y);
	msg.print("\t[ C <%8.4f %8.4f %8.4f > %8.4f [gamma=%8.3f]\n", axes_.rows[2].x, axes_.rows[2].y, axes_.rows[2].z, lengths_.z, angles_.z);
}
