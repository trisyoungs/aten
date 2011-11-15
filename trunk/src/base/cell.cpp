/*
	*** Periodic cell definition
	*** src/base/cell.cpp
	Copyright T. Youngs 2007-2011

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
#include "base/mathfunc.h"
#include "base/sysfunc.h"
#include "base/spacegroup.h"
using namespace std;

// Cell types
const char *CellTypeKeywords[UnitCell::nCellTypes] = { "None", "Cubic", "Orthorhombic", "Parallelepiped" };
const char *UnitCell::cellType(UnitCell::CellType i)
{
	return CellTypeKeywords[i];
}
UnitCell::CellType UnitCell::cellType(const char *s, bool reportError)
{
	UnitCell::CellType ct = (UnitCell::CellType) enumSearch("cell type",UnitCell::nCellTypes,CellTypeKeywords,s);
	if ((ct == UnitCell::nCellTypes) && reportError) enumPrintValid(UnitCell::nCellTypes,CellTypeKeywords);
	return ct;
}

// Cell definition parameters
const char *CellParameterKeywords[UnitCell::nCellParameters] = { "a", "b", "c", "alpha", "beta", "gamma", "ax", "ay", "az", "bx", "by", "bz", "cx", "cy", "cz" };
UnitCell::CellParameter UnitCell::cellParameter(const char *s)
{
	return (CellParameter) enumSearch("cell parameter",UnitCell::nCellParameters,CellParameterKeywords,s);
}

// Constructor
UnitCell::UnitCell()
{
	// Private variables
	type_ = UnitCell::NoCell;
	axes_.zero();
	inverse_.zero();
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
void UnitCell::operator=(UnitCell &source)
{
	type_ = source.type_;
	axes_ = source.axes_;
	reciprocal_ = source.reciprocal_;
	inverse_ = source.inverse_;
	centre_ = source.centre_;
	lengths_ = source.lengths_;
	angles_ = source.angles_;
	volume_ = source.volume_;
	reciprocalVolume_ = source.reciprocalVolume_;
	density_ = source.density_;
}

// Set parent model
void UnitCell::setParent(Model *m)
{
	parent_ = m;
}

// Return parent model
Model *UnitCell::parent()
{
	return parent_;
}

/*
// Set
*/

// Remove the cell definition (i.e. set 'type' to UnitCell::NoCell)
void UnitCell::reset()
{
	type_ = UnitCell::NoCell;
	centre_.zero();
}

// Set (by parameters)
void UnitCell::set(const Vec3<double> &newlengths, const Vec3<double> &newangles)
{
	msg.enter("UnitCell::set[vectors]");
	// Store cell lengths and angles (in degrees) in structure
	angles_ = newangles;
	lengths_ = newlengths;
	// Calculate new matrix
	calculateMatrix();
	// Update dependent quantities
	update();
	msg.exit("UnitCell::set[vectors]");
}

// Set (by matrix)
void UnitCell::set(const Matrix &newaxes)
{
	msg.enter("UnitCell::set[matrix]");
	// Store the supplied matrix, making sure that column 4 is correct
	axes_ = newaxes;
	axes_.setColumn(3, 0.0, 0.0, 0.0, 1.0);
	// Calculate new vectors
	calculateVectors();
	// Update dependent quantities
	update();
	msg.exit("UnitCell::set[matrix]");
}

// Set lengths and calculates matrix
void UnitCell::setLengths(const Vec3<double> &newlengths)
{
	// Store new cell lengths
	lengths_ = newlengths;
	// Calculate new matrix
	calculateMatrix();
	// Update dependent quantities
	update();
}

// Set individual length
void UnitCell::setLength(int i, double d)
{
	// Store new cell lengths
	lengths_.set(i,d);
	// Calculate new matrix
	calculateMatrix();
	// Update dependent quantities
	update();
}

// Set individual angle
void UnitCell::setAngle(int i, double d)
{
	// Store the supplied matrix
	angles_.set(i,d);
	// Calculate new vectors
	calculateVectors();
	// Update dependent quantities
	update();
}

// Set angles and calculates matrix
void UnitCell::setAngles(const Vec3<double> &newangles)
{
	// Store new cell lengths
	angles_ = newangles;
	// Calculate new matrix
	calculateMatrix();
	// Update dependent quantities
	update();
}

// Set / adjust individual parameter
void UnitCell::setParameter(UnitCell::CellParameter cp, double value, bool adjust)
{
	int i;
	switch (cp)
	{
		case (UnitCell::nCellParameters):
			printf("No cell parameter supplied to UnitCell::adjustParameter.\n");
			break;
		// Cell matrix elements
		case (UnitCell::CellAX):
		case (UnitCell::CellAY):
		case (UnitCell::CellAZ):
		case (UnitCell::CellBX):
		case (UnitCell::CellBY):
		case (UnitCell::CellBZ):			
		case (UnitCell::CellCX):
		case (UnitCell::CellCY):
		case (UnitCell::CellCZ):
			i = cp - UnitCell::CellAX;
			if (adjust) axes_[(i/3)*4+i%3] += value;
			else axes_[(i/3)*4+i%3] = value;
			calculateVectors();
			break;
		// Cell lengths
		case (UnitCell::CellA):
		case (UnitCell::CellB):
		case (UnitCell::CellC):
			adjust ? lengths_.add(cp - UnitCell::CellA, value) : lengths_.set(cp - UnitCell::CellA, value);
			// Calculate new matrix
			calculateMatrix();
			break;
		// Cell angles
		case (UnitCell::CellAlpha):
		case (UnitCell::CellBeta):
		case (UnitCell::CellGamma):
			adjust ? angles_.add(cp - UnitCell::CellAlpha, value) : angles_.set(cp - UnitCell::CellAlpha, value);
			// Calculate new matrix
			calculateMatrix();
			break;
		// Cell matrix elements
		default:
			break;
	}
	// Update dependent quantities
	update();
}

// Return the type of cell
UnitCell::CellType UnitCell::type() const
{
	return type_;
}

// Return the transpose of the cell vector matrix (giving individual axis vectors in rows[])
Matrix UnitCell::axes() const
{
	return axes_;
}

// Return a matrix of the reciprocal cell vectors
Matrix UnitCell::reciprocal() const
{
	return reciprocal_;
}

// Return inverse of axes matrix
Matrix UnitCell::inverse() const
{
	return inverse_;
}

// Return the axis lengths of the cell
Vec3<double> UnitCell::lengths() const
{
	return lengths_;
}

// Return the angles the cell
Vec3<double> UnitCell::angles() const
{
	return angles_;
}

// Return the origin the cell
Vec3<double> UnitCell::centre() const
{
	return centre_;
}

// Return the volume of the cell
double UnitCell::volume() const
{
	return volume_;
}

// Return the volume of the reciprocal cell
double UnitCell::reciprocalVolume() const
{
	return reciprocalVolume_;
}

// Return the density of the cell
double UnitCell::density() const
{
	return density_;
}

// Sets the spacegroup Id
void UnitCell::setSpacegroupId(int i)
{
	if ((i < 0) || (i > 230)) msg.print( "Warning - %i is not a valid spacegroup number. Spacegroup not set.\n", i);
	else spacegroupId_ = i;
}

// Return the spacegroup Id
int UnitCell::spacegroupId() const
{
	return spacegroupId_;
}

// Return the spacegroup name
const char *UnitCell::spacegroup() const
{
	return Spacegroups[spacegroupId_].name;
}

// Add manual generator
Generator *UnitCell::addGenerator()
{
	return generators_.add();
}

// Return number of manual generators defined
int UnitCell::nGenerators() const
{
	return generators_.nItems();
}

// Return first manually-defined generator
Generator *UnitCell::generators()
{
	return generators_.first();
}

// Update dependent quantities
void UnitCell::update()
{
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
void UnitCell::determineType()
{
	msg.enter("UnitCell::determineType");
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
		if (count == 2) type_ = UnitCell::CubicCell;
		else type_ = UnitCell::OrthorhombicCell;
		// While we're here, symmetrise the matrix for cubic and orthorhombic cells
		axes_[1] = 0.0;
		axes_[2] = 0.0;
		axes_[4] = 0.0;
		axes_[6] = 0.0;
		axes_[8] = 0.0;
		axes_[9] = 0.0;
	}
	else type_ = UnitCell::ParallelepipedCell;
	msg.exit("UnitCell::determineType");
}

// Calculate cell lengths/angles from current matrix
void UnitCell::calculateVectors()
{
	msg.enter("UnitCell::calculateVectors");
	// Calculate cell lengths
	lengths_.x = axes_.columnMagnitude(0);
	lengths_.y = axes_.columnMagnitude(1);
	lengths_.z = axes_.columnMagnitude(2);
	// Calculate cell angles
	Vec3<double> vecx,vecy,vecz;
	vecx = axes_.columnAsVec3(0);
	vecy = axes_.columnAsVec3(1);
	vecz = axes_.columnAsVec3(2);
	vecx.normalise();
	vecy.normalise();
	vecz.normalise();
	angles_.x = acos(vecy.dp(vecz));
	angles_.y = acos(vecx.dp(vecz));
	angles_.z = acos(vecx.dp(vecy));
	angles_ *= DEGRAD;
	msg.exit("UnitCell::calculateVectors");
}

// Calculate cell matrix from current vectors
void UnitCell::calculateMatrix()
{
	msg.enter("UnitCell::calculateMatrix");
	double temp;
	// Work in unit vectors. Assume that A lays along x-axis
	axes_.setColumn(0,1.0,0.0,0.0,0.0);
	// Assume that B lays in the xy plane. Since A={1,0,0}, cos(gamma) equals 'x' of the B vector.
	temp = cos(angles_.z/DEGRAD);
	axes_.setColumn(1,temp,sqrt(1.0 - temp*temp),0.0,0.0);
	// The C vector can now be determined in parts.
	// It's x-component is equal to cos(beta) since {1,0,0}{x,y,z} = {1}{x} = cos(beta)
	axes_.setColumn(2,cos(angles_.y/DEGRAD),0.0,0.0,0.0);
	// The y-component can be determined by completing the dot product between the B and C vectors
	axes_[9] = ( cos(angles_.x/DEGRAD) - axes_[4]*axes_[8] ) / axes_[5];
	// The z-component is simply the remainder of the unit vector...
	axes_[10] = sqrt(1.0 - axes_[8]*axes_[8] - axes_[9]*axes_[9]);
	// Lastly, adjust these unit vectors to give the proper cell lengths
	axes_.columnMultiply(0,lengths_.x);
	axes_.columnMultiply(1,lengths_.y);
	axes_.columnMultiply(2,lengths_.z);
	axes_.setColumn(3, 0.0, 0.0, 0.0, 1.0);
	msg.exit("UnitCell::calculateMatrix");
}

// Calculate reciprocal cell vectors
void UnitCell::calculateReciprocal()
{
	// Calculate the reciprocal cell of 'this->cell'
	msg.enter("UnitCell::calculateReciprocal");
	switch (type_)
	{
		case (UnitCell::NoCell):
			msg.print("Cell : Can't calculate reciprocal cell - no cell defined.\n");
			break;
		case (UnitCell::CubicCell):
		case (UnitCell::OrthorhombicCell):
			reciprocal_.setColumn(0,1.0 / axes_[0], 0.0, 0.0, 0.0);
			reciprocal_.setColumn(1,0.0, 1.0 / axes_[5], 0.0, 0.0);
			reciprocal_.setColumn(2,0.0, 0.0, 1.0 / axes_[10], 0.0);
			reciprocalVolume_ = 1.0 / (axes_[0] * axes_[5] * axes_[10]);
			break;
		case (UnitCell::ParallelepipedCell):
			// Reciprocal cell vectors are perpendicular to normal cell axes_t.
			// Calculate from cross products of normal cell vectors
			reciprocal_.setColumn(0, axes_.columnAsVec3(1) * axes_.columnAsVec3(2), 0.0);
			reciprocal_.setColumn(1, axes_.columnAsVec3(0) * axes_.columnAsVec3(2), 0.0);
			reciprocal_.setColumn(2, axes_.columnAsVec3(0) * axes_.columnAsVec3(1), 0.0);
			reciprocalVolume_ = fabs( axes_[0]*reciprocal_[0] + axes_[5]*reciprocal_[5] + axes_[10]*reciprocal_[10]);
			reciprocal_.columnMultiply(0, 1.0 / reciprocalVolume_);
			reciprocal_.columnMultiply(1, 1.0 / reciprocalVolume_);
			reciprocal_.columnMultiply(2, 1.0 / reciprocalVolume_);;
			reciprocalVolume_ = 1.0 / reciprocalVolume_;
			break;
		default:
			break;
	}
	msg.exit("UnitCell::calculateReciprocal");
}

// Calculate centre coordinate of cell
void UnitCell::calculateCentre()
{
	msg.enter("UnitCell::calculateCentre");
	if (type_ != UnitCell::NoCell) centre_ = axes_.transform(0.5,0.5,0.5);
	else centre_.set(0.0,0.0,0.0);
	msg.exit("UnitCell::calculateCentre");
}

// Calculate inverse matrix
void UnitCell::calculateInverse()
{
	msg.enter("UnitCell::calculateInverse");
	inverse_ = axes_;
	inverse_.invert();
	msg.exit("UnitCell::calculateInverse");
}

/*
// Minimum Image Routines
*/

// Minimum image position
Vec3<double> UnitCell::mim(const Vec3<double> &r1, const Vec3<double> &r2) const
{
	return mimd(r1,r2)+r2;
}

// Minimum image vector of r1 with respect to reference point r2
Vec3<double> UnitCell::mimd(const Vec3<double> &r1, const Vec3<double> &r2) const
{
	static Vec3<double> R;
	static double half;
	switch (type_)
	{
		// No cell - just return r1
		case (UnitCell::NoCell):
			R = r1 - r2;
			break;
		// Cubic / Orthorhombic
		case (UnitCell::CubicCell):
		case (UnitCell::OrthorhombicCell):
			R = r1 - r2;
// 			half = lengths_.x * 0.5;
// 			if (R.x < -half) R.x += lengths_.x;
// 			else if (R.x > half) R.x -= lengths_.x;
// 			if (R.y < -half) R.y += lengths_.x;
// 			else if (R.y > half) R.y -= lengths_.x;
// 			if (R.z < -half) R.z += lengths_.x;
// 			else if (R.z > half) R.z -= lengths_.x;
			R.x -= floor(R.x/lengths_.x + 0.5)*lengths_.x;
			R.y -= floor(R.y/lengths_.y + 0.5)*lengths_.y;
			R.z -= floor(R.z/lengths_.z + 0.5)*lengths_.z;
			break;
		// Parallelepiped 
		default:
			R = inverse_.transform(r1-r2);
/*			if (R.x < -0.5) R.x += 1.0;
			if (R.y < -0.5) R.y += 1.0;
			if (R.z < -0.5) R.z += 1.0;
			if (R.x > 0.5) R.x -= 1.0;
			if (R.y > 0.5) R.y -= 1.0;
			if (R.z > 0.5) R.z -= 1.0;*/
			R.x -= floor(R.x + 0.5);
			R.y -= floor(R.y + 0.5);
			R.z -= floor(R.z + 0.5);
			R = axes_.transform(R);
	}
	return R;
}

// Minimimum image routines with atom and vector pointers
Vec3<double> UnitCell::mim(Atom *i, const Vec3<double> &r2) const
{
	return mim(i->r(),r2);
}

Vec3<double> UnitCell::mimd(Atom *i, const Vec3<double> &r2) const
{
	return mimd(i->r(),r2);
}

// Minimimum image routines with atom pointers
Vec3<double> UnitCell::mim(Atom *i, Atom *j) const
{
	return mim(i->r(),j->r());
}

Vec3<double> UnitCell::mimd(Atom *i, Atom *j) const
{
	return mimd(i->r(),j->r());
}

// Fold atom
void UnitCell::fold(Vec3<double> &r, Atom *i, Model *parent) const
{
	// Folds the coordinates in 'r' into the defined unit cell
	msg.enter("UnitCell::fold");
	static Vec3<double> newr;
	switch (type_)
	{
		// No cell, so no image to fold into
		case (UnitCell::NoCell):
			break;
		// Cubic / Orthorhombic
		case (UnitCell::CubicCell):
		case (UnitCell::OrthorhombicCell):
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
			// Convert these coordinates into fractional cell coordinates...
			newr = inverse_.transform(r);
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
			newr = axes_.transform(newr);
			// Use model functions to store new position if we were given one
			if (parent != NULL) parent->positionAtom(i, newr);
			else r = newr;
			break;
	}
	msg.exit("UnitCell::fold");
}

void UnitCell::fold(Atom *i, Model *parent) const
{
	fold(i->r(), i, parent);
}

/*
// Geometry Calculation
*/

double UnitCell::distance(const Vec3<double> &r1, const Vec3<double> &r2, bool useMim) const
{
	// Calculate the distance between atoms i and j
	static Vec3<double> mimi;
	mimi = (useMim ? mimd(r1,r2) : r1-r2);
	return mimi.magnitude();
}

double UnitCell::distance(Atom *i, Atom *j, bool useMim) const
{
	return distance(i->r(),j->r(),useMim);
}

double UnitCell::angle(const Vec3<double> &r1, const Vec3<double> &r2, const Vec3<double> &r3, bool useMim) const
{
	// Calculate the angle formed between atoms i, j, and k
	static Vec3<double> vecij, veckj;
	static double dp, a;
	vecij = (useMim ? mimd(r1,r2) : r1-r2);
	veckj = (useMim ? mimd(r3,r2) : r3-r2);
	// Normalise vectors and calculate dot product and angle.
	vecij.normalise();
	veckj.normalise();
	dp = vecij.dp(veckj);
	a = acos(dp);
	return a * DEGRAD;
}

double UnitCell::angle(Atom *i, Atom *j, Atom *k, bool useMim) const
{
	return angle(i->r(),j->r(),k->r(), useMim);
}

double UnitCell::torsion(const Vec3<double> &i, const Vec3<double> &j, const Vec3<double> &k, const Vec3<double> &l, bool useMim) const
{
	// Calculate the torsion angle formed between the atoms i, j, k, and l.
	static Vec3<double> vecji, veckl, vecjk, veckj, mim_k, xpj, xpk;
	static double dp, angle;
	// Vector j->i (minimum image of i w.r.t. j)
	vecji = (useMim ? mimd(i,j) : i-j);
	// Vectors j->k and k->j (minimum image of k w.r.t. j)
	mim_k = (useMim ? mim(k,j) : k);
	vecjk = mim_k - j;
	veckj = -vecjk;
	// Vector k->l (minimum image of l w.r.t. k (in turn w.r.t. j))
	veckl = (useMim ? mimd(mim_k,l) : mim_k-l);
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
	return angle * DEGRAD;
}

double UnitCell::torsion(Atom *i, Atom *j, Atom *k, Atom *l, bool useMim) const
{
	return torsion(i->r(),j->r(),k->r(),l->r(), useMim);
}

/*
// Coordinate Transforms
*/

// Return the fractional coordinates of the specified position
Vec3<double> UnitCell::realToFrac(const Vec3<double> &v) const
{
	// Convert the real coordinates supplied into fractional cell coordinates
	return inverse_.transform(v);
}

// Return the real coordinates of the specified fractional cell coordinate
Vec3<double> UnitCell::fracToReal(const Vec3<double> &v) const
{
	// Convert the fractional cell coordinates supplied into real cell coordinates
	return axes_.transform(v);
}

/*
// Misc
*/

// Generate a random position inside the unit cell
Vec3<double> UnitCell::randomPos() const
{
	return axes_.transform(AtenMath::random(), AtenMath::random(), AtenMath::random());
}

// Print
void UnitCell::print()
{
	msg.print("\t        x        y        z          l\n");
	msg.print("\t[ A <%8.4f %8.4f %8.4f > %8.4f [alpha=%8.3f]\n", axes_[0], axes_[1], axes_[2], lengths_.x, angles_.x);
	msg.print("\t[ B <%8.4f %8.4f %8.4f > %8.4f [ beta=%8.3f]\n", axes_[4], axes_[5], axes_[6], lengths_.y, angles_.y);
	msg.print("\t[ C <%8.4f %8.4f %8.4f > %8.4f [gamma=%8.3f]\n", axes_[8], axes_[9], axes_[10], lengths_.z, angles_.z);
}
