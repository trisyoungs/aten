/*
	*** ZMatrix Definition
	*** src/classes/zmatrix.h
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

#ifndef ATEN_ZMATRIX_H
#define ATEN_ZMATRIX_H

#include "parser/variablelist.h"

// Forward declarations
class Atom;
class Model;
class ZMatrix;

// Element of ZMatrix
class ZMatrixElement
{
	public:
	// Constructor / Destructor
	ZMatrixElement();
	~ZMatrixElement();
	// List pointers
	ZMatrixElement *prev, *next;

	/*
	// Data
	*/
	private:
	// Parent ZMatrix
	ZMatrix *parent_;
	// Atom pointers (first = target, second-fourth = distance, angle, torsion specifiers)
	Atom *atoms_[4];
	// Variables holding defined distance, angle, and torsion values
	Variable *values_[3];
	// Flags stating whether the negative of the variable value should be used instead
	bool negated_[3];

	public:
	// Set parent
	void setParent(ZMatrix *parent);
	// Set n'th atom datum
	void setAtom(int id, Atom *i);
	// Retrieve n'th atom datum
	Atom *atom(int id);
	// Set n'th negate flag
	void setNegated(int id, bool b);
	// Retrieve n'th negate flag
	bool negated(int id);
	// Set distance (geometry variable 0)
	void setDistanceVariable(Variable *v);
	// Retrieve distance variable (geometry variable 0)
	Variable *distanceVariable();
	// Set distance variable name (geometry variable 0)
	void setDistanceName(const char *name);
	// Retrieve distance variable name (geometry variable 0)
	const char *distanceName();
	// Set distance value
	void setDistance(double value);
	// Retrieve distance value (geometry variable 0)
	double distance();
	// Set angle (geometry variable 1)
	void setAngleVariable(Variable *v);
	// Retrieve angle variable (geometry variable 1)
	Variable *angleVariable();
	// Set angle variable name (geometry variable 1)
	void setAngleName(const char *name);
	// Retrieve angle variable name (geometry variable 1)
	const char *angleName();
	// Set angle value
	void setAngle(double value);
	// Retrieve angle value (geometry variable 1)
	double angle();
	// Set torsion (geometry variable 2)
	void setTorsionVariable(Variable *v);
	// Retrieve torsion variable (geometry variable 2)
	Variable *torsionVariable();
	// Set torsion variable name (geometry variable 2)
	void setTorsionName(const char *name);
	// Retrieve torsion variable name (geometry variable 2)
	const char *torsionName();
	// Set torsion value
	void setTorsion(double value);
	// Retrieve torsion value (geometry variable 2)
	double torsion();
};

// ZMatrix
class ZMatrix
{
	public:
	// Constructor / Destructor
	ZMatrix();
	~ZMatrix();

	/*
	// Data
	*/
	private:
	// Parent model for which the zmatrix has been created
	Model *parent_;
	// Coordinate origin of first atom
	Vec3<double> origin_;
	// List of ZMatrix elements, one per atom
	List<ZMatrixElement> elements_;
	// Variable lists of various data items
	VariableList distances_, angles_, torsions_;

	public:
	// Return parent model
	Model *parent();
	// Return coordinate origin
	Vec3<double> origin();
	// Return number of defined elements
	int nElements() const;
	// Return start of defined elements
	ZMatrixElement *elements() const;
	// Return specified element
	ZMatrixElement *element(int index);
	// Return total number of defined variables
	int nVariables();
	// Return number of defined angle variables
	int nAngles();
	// Return start of angles list
	Variable *angles();
	// Return specified angle variable
	Variable *angle(int index);
	// Return number of defined distance variables
	int nDistances();
	// Return start of distances list
	Variable *distances();
	// Return specified distance variable
	Variable *distance(int index);
	// Return number of defined torsion variables
	int nTorsions();
	// Return start of torsions list
	Variable *torsions();
	// Return specified torsion variable
	Variable *torsion(int index);


	/*
	// Construction and Retrieval
	*/
	private:
	// Add single definition to list
	ZMatrixElement *addElement(Reflist<Atom,int> &atomlist);
	// Create zmatrix recursively along bonds
	void createAlongBonds(Atom *target, Reflist<Atom,int> &atomlist);
	// Create path of bound atoms from current atom
	bool createBoundPath(Reflist<Atom,int> &atomlist, int size, Reflist<Atom,int> &bestlist);

	public:
	// Create for specified model
	void create(Model *source, bool usebonds);
	// Set variable value and recalculate atom positions in model
	void setVariable(Variable *v, double value);
	// Print zmatrix
	void print();
};

#endif

