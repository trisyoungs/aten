/*
	*** Unit cell definition
	*** src/base/cell.h
	Copyright T. Youngs 2007-2015

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

#ifndef ATEN_UNITCELL_H
#define ATEN_UNITCELL_H

#include "base/generator.h"
#include "base/dnchar.h"
#include "templates/vector3.h"
#include "templates/reflist.h"
#define SGCOREDEF__
#include "base/sginfo.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Atom;
class Model;

// Unit cell
class UnitCell
{
	public:
	// Constructor / Destructur
	UnitCell();
	~UnitCell();
	// Cell types
	enum CellType { NoCell, CubicCell, OrthorhombicCell, ParallelepipedCell, nCellTypes };
	static const char* cellType(CellType);
	static CellType cellType(const char* name, bool reportError = 0);
	// Cell definition parameters
	enum CellParameter { CellA, CellB, CellC, CellAlpha, CellBeta, CellGamma, CellAX, CellAY, CellAZ, CellBX, CellBY, CellBZ, CellCX, CellCY, CellCZ, nCellParameters };
	static CellParameter cellParameter(const char* );
	// Assignment operator
	void operator=(UnitCell &source);

	/*
	// Cell Definition
	*/
	private:
	// Parent model
	Model* parent_;
	// Cell type
	CellType type_;
	// Vectors of the principal cell axes (rows[0] = A, rows[1] = B, rows[2] = C)
	Matrix axes_;
	// Reciprocal vectors of the principal cell axes
	Matrix reciprocal_;
	// Inverse of axes transpose
	Matrix inverse_;
	// Coordinates at centre of cell
	Vec3<double> centre_;
	// Principal axis lengths
	Vec3<double> lengths_;
	// Angles between principal axes
	Vec3<double> angles_;
	// Cell / reciprocal cell volume
	double volume_, reciprocalVolume_;
	// Density of cell
	double density_;


	public:
	// Set parent model
	void setParent(Model* m);
	// Return parent model
	Model* parent();
	// Copy data from specified cell
	bool copy(UnitCell* source);
	// Print cell data
	void print();
	// Generate random position inside cell
	Vec3<double> randomPos() const;
	// Remove the cell definition (i.e. set 'type' to NoCell)
	void reset();
	// Set lengths and angles and calculates matrix
	void set(const Vec3<double> &lengths, const Vec3<double> &angles);
	// Set matrix and calculates lengths and angles
	void set(const Matrix& axes);
	// Set lengths and calculates matrix
	void setLengths(const Vec3<double> &lengths);
	// Set individual length
	void setLength(int i, double d);
	// Set individual angle
	void setAngle(int i, double d);
	// Set all angles simultaneously
	void setAngles(const Vec3<double> &angles);
	// Set / adjust individual parameter
	void setParameter(UnitCell::CellParameter cp, double value, bool adjust = FALSE);
	// Return the type of cell
	UnitCell::CellType type() const;
	// Return the cell vector matrix
	Matrix axes() const;
	// Return reciprocal cell matrix
	Matrix reciprocal() const;
	// Return inverse matrix of cell axes
	Matrix inverse() const;
	// Return the axis lengths of the cell
	Vec3<double> lengths() const;
	// Return the angles the cell
	Vec3<double> angles() const;
	// Return the centre the cell
	Vec3<double> centre() const;
	// Return the inverse of the cell vectors as a column-major matrix in a 1D array
	void inverseTransposeColumn(double* m);
	// Return the volume of the cell
	double volume() const;
	// Return the volume of the reciprocal cell
	double reciprocalVolume() const;
	// Return the density of the cell
	double density() const;


	/*
	// Spacegroup
	*/
	private:
	// SGInfo structure
	T_SgInfo spacegroup_;
	// Spacegroup ID (if any)
	int spacegroupId_;
	// Manual list of generators, if no spacegroup is set
	List<Generator> generators_;

	public:
	// Set spacegroup from supplied spacegroup name
	bool setSpacegroup(const char* name, bool forceRhombohedral);
	// Return SgInfo spacegroup structure (if it exists)
	T_SgInfo *spacegroup();
	// Return spacegroup name in defined SgInfo structure
	const char* spacegroupName() const;
	// Return the spacegroup of the model
	int spacegroupId() const;
	// Add manual generator
	Generator *addGenerator();
	// Return number of manual generators defined
	int nGenerators() const;
	// Return first in reflist of manually-defined generators
	Generator *generators();


	/*
	// Internal Methods
	*/
	private:
	// Calculate cell lengths/angles from current matrix
	void calculateVectors();
	// Calculate cell matrix from current vectors
	void calculateMatrix();
	// Update quantities that depend on the cell lengths/angles after they've changed
	void update();
	// Determine the cell type from its lengths / angles
	void determineType();
	// Calculate density of cell
	void calculateDensity();
	// Calculate cell reciprocal
	void calculateReciprocal();
	// Calculate inverse of axes transpose
	void calculateInverse();
	// Calculate coordinates at centre of cell
	void calculateCentre();


	/*
	// Atom Positioning
	*/
	public:
	// Calculate and return the fractional coordinates of the specified real position
	Vec3<double> realToFrac(const Vec3<double>&) const;
	// Calculate and return the real coordinates of the specified fractional cell coordinates
	Vec3<double> fracToReal(const Vec3<double>&) const;


	/*
	// Minimum image calculation
	*/
	public:
	// Minimum image vector from r1 to r2
	Vec3<double> mimVector(const Vec3<double> &r1, const Vec3<double> &r2) const;
	// Minimum image vector from i to r2
	Vec3<double> mimVector(Atom* i, const Vec3<double> &r2) const;
	// Minimum image vector from i to j
	Vec3<double> mimVector(Atom* i, Atom* j) const;
	// Minimum image position of r1 with respect to r2
	Vec3<double> mim(const Vec3<double> &r1, const Vec3<double> &r2) const;
	// Minimum image position of i with respect to r2
	Vec3<double> mim(Atom* i, const Vec3<double> &r2) const;
	// Minimum image position of i with respect to j
	Vec3<double> mim(Atom* i, Atom* j) const;
	// Fold specified atom coordinates into unit cell
	Vec3<double> fold(Atom* i) const;
	// Fold provided coordinates into unit cell
	Vec3<double> fold(Vec3<double> &r) const;
	// Fold fractional coordinates into cell
	static void foldFrac(Vec3<double> &r);
	// Return whether specified coordinates are inside the current unit cell
	bool isInsideCell(Vec3<double> &v) const;


	/*
	// Geometry calculation
	*/
	public:
	// Calculate distance between supplied coordinates
	double distance(const Vec3<double> &ri, const Vec3<double> &rj, bool useMim = TRUE) const;
	// Calculate distance between supplied atoms
	double distance(Atom* i, Atom* j, bool useMim = TRUE) const;
	// Calculate angle between supplied coordinates
	double angle(const Vec3<double> &ri, const Vec3<double> &rj, const Vec3<double> &rk, bool useMim = TRUE) const;
	// Calculate angle between supplied atoms
	double angle(Atom* i, Atom* j, Atom* k, bool useMim = TRUE) const;
	// Calculate torsion angle between supplied coordinates
	double torsion(const Vec3<double> &ri, const Vec3<double> &rj, const Vec3<double> &rk, const Vec3<double> &rl, bool useMim = TRUE) const;
	// Calculate torsion angle between supplied atoms
	double torsion(Atom* i, Atom* j, Atom* k, Atom* l, bool useMim = TRUE) const;
};

ATEN_END_NAMESPACE

#endif
