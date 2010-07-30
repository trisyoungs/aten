/*
	*** ZMatrix Definition
	*** src/classes/zmatrix.h
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

#ifndef ATEN_ZMATRIX_H
#define ATEN_ZMATRIX_H

#include "parser/variablelist.h"

// Forward declarations
class Atom;
class Model;

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
	// Atom pointers (first = target, second-fourth = distance, angle, torsion specifiers)
	Atom *atoms_[4];
	// Variables holding defined distance, angle, and torsion values
	Variable *values_[3];
	// Flags stating whether the negative of the variable value should be used instead
	bool negate_[3];

	public:
	// Set n'th atom datum
	void setAtom(int id, Atom *i);
	// Retrieve n'th atom datum
	Atom *atom(int id);
	// Set distance (geometry variable 0)
	void setDistance(Variable *v);
	// Retrieve distance (geometry variable 0)
	Variable *distance();
	// Set angle (geometry variable 1)
	void setAngle(Variable *v);
	// Retrieve angle (geometry variable 1)
	Variable *angle();
	// Set torsion (geometry variable 2)
	void setTorsion(Variable *v);
	// Retrieve torsion (geometry variable 2)
	Variable *torsion();
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
	// List of ZMatrix elements, one per atom
	List<ZMatrixElement> elements_;
	// Variable lists of various data items
	VariableList distances_, angles_, torsions_;


	/*
	// Construction and Retrieval
	*/
	private:
	// Add single definition to list
	ZMatrixElement *addElement(Reflist<Atom,int> &atoms);

	public:
	// Create for specified model
	void create(Model *source, bool usebonds);
	// Return start of defined elements
	ZMatrixElement *elements();
	// Retrieve named variable
	Variable *data(const char *s);
	// Return variable list
	VariableList *data();
};

#endif

