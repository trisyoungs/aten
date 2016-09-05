/*
	*** Periodic Cell Definition
	*** src/base/cell.cpp
	Copyright T. Youngs 2007-2016

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
#include "base/sysfunc.h"
#include "sg/spacegroup.h"
#include "model/model.h"
#include "math/mathfunc.h"

ATEN_USING_NAMESPACE

// Cell types
const char* CellTypeKeywords[UnitCell::nCellTypes] = { "None", "Cubic", "Orthorhombic", "Parallelepiped" };
const char* UnitCell::cellType(UnitCell::CellType i)
{
	return CellTypeKeywords[i];
}
UnitCell::CellType UnitCell::cellType(QString s, bool reportError)
{
	UnitCell::CellType ct = (UnitCell::CellType) enumSearch("cell type",UnitCell::nCellTypes,CellTypeKeywords,s);
	if ((ct == UnitCell::nCellTypes) && reportError) enumPrintValid(UnitCell::nCellTypes,CellTypeKeywords);
	return ct;
}

// Cell definition parameters
const char* CellParameterKeywords[UnitCell::nCellParameters] = { "a", "b", "c", "alpha", "beta", "gamma", "ax", "ay", "az", "bx", "by", "bz", "cx", "cy", "cz" };
UnitCell::CellParameter UnitCell::cellParameter(QString s)
{
	return (CellParameter) enumSearch("cell parameter",UnitCell::nCellParameters,CellParameterKeywords,s);
}

// Constructor
UnitCell::UnitCell()
{
	// Private variables
	type_ = UnitCell::NoCell;
	axes_.zero();
	axes_.setColumn(3, 0.0, 0.0, 0.0, 1.0);
	inverse_.zero();
	reciprocal_.zero();
	lengths_.zero();
	angles_.zero();
	centre_.zero();
	volume_ = 0.0;
	reciprocalVolume_ = 0.0;
	spacegroupId_ = 0;
	parent_ = NULL;
	
	// Allocate SGInfo Seitz matrix arrays
	spacegroup_.MaxList = 192;
	spacegroup_.ListSeitzMx = new T_RTMx[192];
	spacegroup_.ListRotMxInfo = new T_RotMxInfo[192];
}

// Destructor
UnitCell::~UnitCell()
{
	// Delete sginfo arrays
	delete[] spacegroup_.ListSeitzMx;
	delete[] spacegroup_.ListRotMxInfo;
}

// Copy Constructor
UnitCell::UnitCell(const UnitCell& source)
{
	(*this) = source;
}

// Assignment operator
UnitCell& UnitCell::operator=(const UnitCell& source)
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

	// Allocate SGInfo Seitz matrix arrays
	spacegroup_.MaxList = 192;
	spacegroup_.ListSeitzMx = new T_RTMx[192];
	spacegroup_.ListRotMxInfo = new T_RotMxInfo[192];

	// Copy SGinfo
	spacegroupId_ = source.spacegroupId_;
	for (int n=0; n<192; ++n)
	{
		spacegroup_.ListSeitzMx[n] = source.spacegroup_.ListSeitzMx[n];
		spacegroup_.ListRotMxInfo[n] = source.spacegroup_.ListRotMxInfo[n];
	}

	return *this;
}

// Set parent model
void UnitCell::setParent(Model* m)
{
	parent_ = m;
}

// Return parent model
Model* UnitCell::parent()
{
	return parent_;
}

// Copy data from specified cell
bool UnitCell::copy(UnitCell* source)
{
	if (source == NULL) return false;
	source->print();
	(*this) = (*source);
	return true;
}

/*
 * Cell Definition
 */

// Generate a random position inside the unit cell
Vec3<double> UnitCell::randomPos() const
{
	return axes_.transform(AtenMath::random(), AtenMath::random(), AtenMath::random());
}

// Print
void UnitCell::print()
{
	Messenger::print("\t        x        y        z          l");
	Messenger::print("\t[ A <%8.4f %8.4f %8.4f > %8.4f [alpha=%8.3f]", axes_[0], axes_[1], axes_[2], lengths_.x, angles_.x);
	Messenger::print("\t[ B <%8.4f %8.4f %8.4f > %8.4f [ beta=%8.3f]", axes_[4], axes_[5], axes_[6], lengths_.y, angles_.y);
	Messenger::print("\t[ C <%8.4f %8.4f %8.4f > %8.4f [gamma=%8.3f]", axes_[8], axes_[9], axes_[10], lengths_.z, angles_.z);
	printf("%14.10f %14.10f %14.10f\n", axes_[0], axes_[1], axes_[2]);
	printf("%14.10f %14.10f %14.10f\n", axes_[4], axes_[5], axes_[6]);
	printf("%14.10f %14.10f %14.10f\n", axes_[8], axes_[9], axes_[10]);
}

// Remove the cell definition (i.e. set 'type' to UnitCell::NoCell)
void UnitCell::reset()
{
	type_ = UnitCell::NoCell;
	centre_.zero();
}

// Set (by parameters)
void UnitCell::set(const Vec3<double>& newlengths, const Vec3<double>& newangles)
{
	Messenger::enter("UnitCell::set[vectors]");
	// Store cell lengths and angles (in degrees) in structure
	angles_ = newangles;
	lengths_ = newlengths;
	// Calculate new matrix
	calculateMatrix();
	// Update dependent quantities
	update();
	Messenger::exit("UnitCell::set[vectors]");
}

// Set (by matrix)
void UnitCell::set(const Matrix& newaxes)
{
	Messenger::enter("UnitCell::set[matrix]");
	// Store the supplied matrix, making sure that column 4 is correct
	axes_ = newaxes;
	axes_.setColumn(3, 0.0, 0.0, 0.0, 1.0);
	// Calculate new vectors
	calculateVectors();
	// Update dependent quantities
	update();
	Messenger::exit("UnitCell::set[matrix]");
}

// Set lengths and calculates matrix
void UnitCell::setLengths(const Vec3<double>& newlengths)
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
	calculateMatrix();
	// Update dependent quantities
	update();
}

// Set angles and calculates matrix
void UnitCell::setAngles(const Vec3<double>& newangles)
{
	// Store new cell lengths
	angles_ = newangles;
	// Calculate new matrix
	calculateMatrix();
	// Update dependent quantities
	update();
}

// Return individual parameter
double UnitCell::parameter(UnitCell::CellParameter cp)
{
	int i;
	switch (cp)
	{
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
			return axes_[(i/3)*4+i%3];
			break;
		// Cell lengths
		case (UnitCell::CellA):
		case (UnitCell::CellB):
		case (UnitCell::CellC):
			return lengths_.get(cp - UnitCell::CellA);
			break;
		// Cell angles
		case (UnitCell::CellAlpha):
		case (UnitCell::CellBeta):
		case (UnitCell::CellGamma):
			return angles_.get(cp - UnitCell::CellAlpha);
			break;
		// Default
		default:
			printf("Invalid cell parameter supplied to UnitCell::parameter.\n");
			break;
	}
	return 0.0;
}

// Set / adjust individual parameter
void UnitCell::setParameter(UnitCell::CellParameter cp, double value, bool adjust)
{
	int i;
	switch (cp)
	{
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
		// Default
		default:
			printf("Invalid cell parameter supplied to UnitCell::setParameter.\n");
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

/*
 * Spacegroup
 */

// Set spacegroup from supplied spacegroup name
bool UnitCell::setSpacegroup(QString name, bool forceRhombohedral)
{
	Messenger::enter("UnitCell::setSpacegroup");
	// This is basically a chunk of verbatim code from 'sgquick.c'

	// Do a table lookup of the sg text (assume volume is 'A')
	const T_TabSgName *tsgn = FindTabSgNameEntry(qPrintable(name), 'A');
	if (tsgn == NULL)
	{
		Messenger::print("Unable to find spacegroup '%s'.", qPrintable(name));
		Messenger::exit("UnitCell::setSpacegroup");
		return false;
	}
	// Check for hexagonal basis, and whether to force rhombohedral basis
	if (strcmp(tsgn->Extension, "H") == 0)
	{
		if (!forceRhombohedral) Messenger::print("Warning: Spacegroup has hexagonal basis.");
		else
		{
			QString newName = tsgn->SgLabels;
			newName += ":R";
			tsgn = FindTabSgNameEntry(qPrintable(newName), 'A');
			if (tsgn == NULL)
			{
				Messenger::print("Unable to find spacegroup '%s' in rhombohedral basis.", qPrintable(name));
				Messenger::exit("UnitCell::setSpacegroup");
				return false;
			}
			Messenger::print("Spacegroup %s forced into rhombohedral basis.", tsgn->SgLabels);
		}
	}
	spacegroupId_ = tsgn->SgNumber;

	// Initialize the SgInfo structure
	InitSgInfo(&spacegroup_);
	spacegroup_.TabSgName = tsgn;
	
	// Translate the Hall symbol and generate the whole group
	ParseHallSymbol(tsgn->HallSymbol, &spacegroup_);
	if (SgError != NULL) return false;
	
	// Do some book-keeping and derive crystal system, point group, and - if not already set - find the entry in the internal table of space group symbols
	CompleteSgInfo(&spacegroup_);

	Messenger::print(Messenger::Verbose, "Space group belongs to the %s crystal system.", XS_Name[spacegroup_.XtalSystem]);
	return true;
}

// Return SgInfo spacegroup structure (if it exists)
T_SgInfo *UnitCell::spacegroup()
{
	return &spacegroup_;
}

// Return the spacegroup Id
int UnitCell::spacegroupId() const
{
	return spacegroupId_;
}

// Return the spacegroup name
const char* UnitCell::spacegroupName() const
{
	return Spacegroups[spacegroupId_].name;
}

// Add manual generator
Generator* UnitCell::addGenerator()
{
	return generators_.add();
}

// Return number of manual generators defined
int UnitCell::nGenerators() const
{
	return generators_.nItems();
}

// Return first manually-defined generator
Generator* UnitCell::generators()
{
	return generators_.first();
}

/*
 * Internal Methods
 */

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
	Messenger::enter("UnitCell::determineType");
	// Compare cell angles....
	int count = 0;
	if (fabs(90.0 - angles_.x) < 1.0e-5) ++count;
	if (fabs(90.0 - angles_.y) < 1.0e-5) ++count;
	if (fabs(90.0 - angles_.z) < 1.0e-5) ++count;
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
		// Removed 24/04/12 since this breaks the ability to build up (set) the cell from individual cell parameters
// 		axes_[1] = 0.0;
// 		axes_[2] = 0.0;
// 		axes_[4] = 0.0;
// 		axes_[6] = 0.0;
// 		axes_[8] = 0.0;
// 		axes_[9] = 0.0;
	}
	else type_ = UnitCell::ParallelepipedCell;
	Messenger::exit("UnitCell::determineType");
}

// Calculate cell lengths/angles from current matrix
void UnitCell::calculateVectors()
{
	Messenger::enter("UnitCell::calculateVectors");
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
	Messenger::exit("UnitCell::calculateVectors");
}

// Calculate cell matrix from current vectors
void UnitCell::calculateMatrix()
{
	Messenger::enter("UnitCell::calculateMatrix");
	Vec3<double> temp;

	// Work in unit vectors. Assume that A lays along x-axis
	axes_.setColumn(0,1.0,0.0,0.0,0.0);

	// Calculate cosines
	temp.x = cos(angles_.x/DEGRAD);
	temp.y = cos(angles_.y/DEGRAD);
	temp.z = cos(angles_.z/DEGRAD);
	if (fabs(temp.x) < 1.0e-6) temp.x = 0.0;
	if (fabs(temp.y) < 1.0e-6) temp.y = 0.0;
	if (fabs(temp.z) < 1.0e-6) temp.z = 0.0;

	// Assume that B lays in the xy plane. Since A={1,0,0}, cos(gamma) equals 'x' of the B vector.
	axes_.setColumn(1, temp.z, sqrt(1.0 - temp.z*temp.z), 0.0, 0.0);

	// The C vector can now be determined in parts.
	// -- It's x-component is equal to cos(beta) since {1,0,0}{x,y,z} = {1}{x} = cos(beta)
	axes_.setColumn(2, temp.y, 0.0, 0.0, 0.0);
	// -- The y-component can be determined by completing the dot product between the B and C vectors
	axes_[9] = ( temp.x - axes_[4]*axes_[8] ) / axes_[5];
	// -- The z-component is simply the remainder of the unit vector...
	axes_[10] = sqrt(1.0 - axes_[8]*axes_[8] - axes_[9]*axes_[9]);

	// Lastly, adjust these unit vectors to give the proper cell lengths
	axes_.columnMultiply(0, lengths_.x);
	axes_.columnMultiply(1, lengths_.y);
	axes_.columnMultiply(2, lengths_.z);
	axes_.setColumn(3, 0.0, 0.0, 0.0, 1.0);

	Messenger::exit("UnitCell::calculateMatrix");
}

// Calculate reciprocal cell vectors
void UnitCell::calculateReciprocal()
{
	// Calculate the reciprocal cell of 'this->cell'
	Messenger::enter("UnitCell::calculateReciprocal");
	switch (type_)
	{
		case (UnitCell::NoCell):
			Messenger::print("Cell : Can't calculate reciprocal cell - no cell defined.");
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
	Messenger::exit("UnitCell::calculateReciprocal");
}

// Calculate centre coordinate of cell
void UnitCell::calculateCentre()
{
	Messenger::enter("UnitCell::calculateCentre");
	if (type_ != UnitCell::NoCell) centre_ = axes_.transform(0.5,0.5,0.5);
	else centre_.set(0.0,0.0,0.0);
	Messenger::exit("UnitCell::calculateCentre");
}

// Calculate inverse matrix
void UnitCell::calculateInverse()
{
	Messenger::enter("UnitCell::calculateInverse");
	inverse_ = axes_;
	inverse_.invert();
	Messenger::exit("UnitCell::calculateInverse");
}

/*
 * Minimum Image Routines
 */

// Minimum image vector from r1 to r2
Vec3<double> UnitCell::mimVector(const Vec3<double>& r1, const Vec3<double>& r2) const
{
	static Vec3<double> R;
	static double half;
	switch (type_)
	{
		// No cell - just return r1
		case (UnitCell::NoCell):
			R = r2 - r1;
			break;
		// Cubic / Orthorhombic
		case (UnitCell::CubicCell):
		case (UnitCell::OrthorhombicCell):
			R = r2 - r1;
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
			R = inverse_.transform(r2-r1);
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

// Minimum image vector from i to r2
Vec3<double> UnitCell::mimVector(Atom* i, const Vec3<double>& r2) const
{
	return mimVector(i->r(), r2);
}

// Minimum image vector from i to j
Vec3<double> UnitCell::mimVector(Atom* i, Atom* j) const
{
	return mimVector(i->r(), j->r());
}

// Minimum image position of r1 with respect to r2
Vec3<double> UnitCell::mim(const Vec3<double>& r1, const Vec3<double>& r2) const
{
	return mimVector(r2,r1) + r2;
}

// Minimum image position of i with respect to r2
Vec3<double> UnitCell::mim(Atom* i, const Vec3<double>& r2) const
{
	return mimVector(r2,i->r()) + r2;
}

// Minimum image position of i with respect to j
Vec3<double> UnitCell::mim(Atom* i, Atom* j) const
{
	return mimVector(j->r(),i->r()) + j->r();
}

// Fold atom
Vec3<double> UnitCell::fold(Vec3<double>& r) const
{
	// Folds the coordinates in 'r' into the defined unit cell
	Messenger::enter("UnitCell::fold");
	static Vec3<double> R;
	switch (type_)
	{
		// No cell, so no image to fold into
		case (UnitCell::NoCell):
			R = r;
			break;
		// Cubic / Orthorhombic
		case (UnitCell::CubicCell):
		case (UnitCell::OrthorhombicCell):
			R = r;
			R.x -= floor(R.x/lengths_.x)*lengths_.x;
			R.y -= floor(R.y/lengths_.y)*lengths_.y;
			R.z -= floor(R.z/lengths_.z)*lengths_.z;
			break;
		// Parallelepiped
		default:
			// Convert these coordinates into fractional cell coordinates...
			R = inverse_.transform(r);
			R.x -= floor(R.x);
			R.y -= floor(R.y);
			R.z -= floor(R.z);
			R = axes_.transform(R);
			break;
	}
	Messenger::exit("UnitCell::fold");
	return R;
}

// Fold provided coordinates into unit cell
Vec3<double> UnitCell::fold(Atom* i) const
{
	return fold(i->r());
}

// Fold fractional coordinates into cell
void UnitCell::foldFrac(Vec3<double>& r)
{
	r.x -= floor(r.x);
	r.y -= floor(r.y);
	r.z -= floor(r.z);
}

// Return whether specified coordinates are inside the current unit cell
bool UnitCell::isInsideCell(Vec3<double>& v) const
{
	switch (type_)
	{
		// No cell, so no image to fold into
		case (UnitCell::NoCell):
			return false;
			break;
		// Cubic / Orthorhombic
		case (UnitCell::CubicCell):
		case (UnitCell::OrthorhombicCell):
			if ((v.x < 0.0) || (v.x > lengths_.x)) return false;
			if ((v.y < 0.0) || (v.y > lengths_.y)) return false;
			if ((v.z < 0.0) || (v.z > lengths_.z)) return false;
			break;
		// Parallelepiped
		default:
			// Convert these coordinates into fractional cell coordinates...
			Vec3<double> frac = inverse_.transform(v);
			if ((frac.x < 0.0) || (frac.x > 1.0)) return false;
			if ((frac.y < 0.0) || (frac.y > 1.0)) return false;
			if ((frac.z < 0.0) || (frac.z > 1.0)) return false;
			break;
	}
	return true;
}

/*
 * Geometry Calculation
 */

// Calculate distance between supplied coordinates
double UnitCell::distance(const Vec3<double>& r1, const Vec3<double>& r2, bool useMim) const
{
	// Calculate the distance between atoms i and j
	static Vec3<double> mimi;
	mimi = (useMim ? mimVector(r1,r2) : r1-r2);
	return mimi.magnitude();
}

// Calculate distance between supplied atoms
double UnitCell::distance(Atom* i, Atom* j, bool useMim) const
{
	return distance(i->r(),j->r(),useMim);
}

// Calculate angle between supplied coordinates
double UnitCell::angle(const Vec3<double>& r1, const Vec3<double>& r2, const Vec3<double>& r3, bool useMim) const
{
	// Calculate the angle formed between atoms i, j, and k
	static Vec3<double> vecji, vecjk;
	static double dp, a;
	vecji = (useMim ? mimVector(r2,r1) : r1-r2);
	vecjk = (useMim ? mimVector(r2,r3) : r3-r2);
	// Normalise vectors and calculate dot product and angle.
	vecji.normalise();
	vecjk.normalise();
	dp = vecji.dp(vecjk);
	a = acos(dp);
	return a * DEGRAD;
}

// Calculate angle between supplied atoms
double UnitCell::angle(Atom* i, Atom* j, Atom* k, bool useMim) const
{
	return angle(i->r(),j->r(),k->r(), useMim);
}

// Calculate torsion angle between supplied coordinates
double UnitCell::torsion(const Vec3<double>& i, const Vec3<double>& j, const Vec3<double>& k, const Vec3<double>& l, bool useMim) const
{
	// Calculate the torsion angle formed between the atoms i, j, k, and l.
	static Vec3<double> vecji, veckl, vecjk, veckj, mim_k, xpj, xpk;
	static double dp, angle;
	// Vector j->i
	vecji = (useMim ? mimVector(j,i) : i-j);
	// Vectors j->k and k->j (minimum image of k w.r.t. j)
	mim_k = (useMim ? mim(k,j) : k);
	vecjk = mim_k - j;
	veckj = -vecjk;
	// Vector k->l (minimum image of l w.r.t. k (in turn w.r.t. j))
	veckl = (useMim ? mimVector(mim_k,l) : l-mim_k);
	// Calculate cross products
	xpj = vecji * vecjk;
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

// Calculate torsion angle between supplied atoms
double UnitCell::torsion(Atom* i, Atom* j, Atom* k, Atom* l, bool useMim) const
{
	return torsion(i->r(),j->r(),k->r(),l->r(), useMim);
}

/*
 * Coordinate Transforms
 */

// Return the fractional coordinates of the specified position
Vec3<double> UnitCell::realToFrac(const Vec3<double>& v) const
{
	// Convert the real coordinates supplied into fractional cell coordinates
	return inverse_.transform(v);
}

// Return the real coordinates of the specified fractional cell coordinate
Vec3<double> UnitCell::fracToReal(const Vec3<double>& v) const
{
	// Convert the fractional cell coordinates supplied into real cell coordinates
	return axes_.transform(v);
}

/*
 * Miller Plane Calculation
 */

// Calculate miller plane coordinates and normals for supplied hkl
void UnitCell::millerPlanes(int h, int k, int l, Plane& plane1, Plane& plane2)
{
	// Clear old plane data
	plane1.clear();
	plane2.clear();

	// Initialise some useful variables
	int hkl[3];
	int anindex = -1, notanindex = -1, ncoords = 0;
	Vec3<double> coords[4], oneMinusCoords[4], one(1.0,1.0,1.0);
	
	hkl[0] = h;
	hkl[1] = k;
	hkl[2] = l;
	
	// Plane Eq : hx + ky + lz = 1    (h, k, and l are reciprocals)
	for (int n=0; n<3; ++n)
	{
		if (hkl[n] != 0)
		{
			coords[ncoords++].set(n, 1.0 / hkl[n]);
			anindex = n;
		}
		else notanindex = n;
	}

	// Generate other coordinates as necessary
	if (ncoords == 1)
	{
		// {100}
		int i = (anindex+1)%3;
		int j = (i+1)%3;
		for (int n=1; n<4; ++n) coords[n] = coords[0];
		coords[1].set(i, 1.0);
		coords[2].set(i, 1.0);
		coords[2].set(j, 1.0);
		coords[3].set(j, 1.0);
		ncoords = 4;
	}
	else if (ncoords == 2)
	{
		// {110}
		coords[2] = coords[1];
		coords[2].set(notanindex, 1.0);
		coords[3] = coords[0];
		coords[3].set(notanindex, 1.0);
		ncoords = 4;
	}

	// Convert coords from fractional into cell coordinates
	for (int n=0; n<4; ++n)
	{
		oneMinusCoords[n] = axes_ * (one-coords[n]);
		coords[n] = axes_ * coords[n];
	}

	// Fold triangles outside of cell back into cell, and determine second set of vertices
	Vec3<double> centre = (coords[0] + coords[1] + coords[2]) / 3.0;
	Vec3<double> delta = centre - fold(centre);
	for (int n=0; n<4; ++n)
	{
		coords[n] -= delta;
		oneMinusCoords[n] += delta;
	}

	// Set vertices for planes
	if (ncoords == 3)
	{
		// Triangular planes
		plane1.setVertices(coords[0], coords[1], coords[2]);
		plane2.setVertices(oneMinusCoords[0], oneMinusCoords[1], oneMinusCoords[2]);
	}
	else
	{
		// Quadrilateral planes
		plane1.setVertices(coords[0], coords[1], coords[2], coords[3]);
		plane2.setVertices(oneMinusCoords[0], oneMinusCoords[1], oneMinusCoords[2], oneMinusCoords[3]);
	}

	// Calculate normals
	// For plane1 the normal always points towards (1,1,1). For plane 2, towards (0,0,0).
	Vec3<double> normal;
	// -- Plane1
	normal = (coords[1] - coords[0])*(coords[1] - coords[2]);
	normal.normalise();
	if (normal.dp(centre_) > 0.0) normal = -normal;
	plane1.setNormal(normal);
	// -- Plane2
	plane2.setNormal(-normal);
}
