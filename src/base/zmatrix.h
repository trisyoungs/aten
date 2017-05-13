/*
	*** ZMatrix Definition
	*** src/base/zmatrix.h
	Copyright T. Youngs 2007-2017

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

#include "base/zmatrixelement.h"
#include "parser/variablelist.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Atom;
class LineParser;
class Model;
class ZMatrix;

// ZMatrix
class ZMatrix
{
	public:
	// Constructor / Destructor
	ZMatrix();
	~ZMatrix();


	/*
	 * Data
	 */
	private:
	// Parent model for which the zmatrix has been created
	Model* parent_;
	// Coordinate origin of first atom
	Vec3<double> origin_;
	// List of ZMatrix elements, one per atom
	List<ZMatrixElement> elements_;
	// Variable lists of various data items
	VariableList distances_, angles_, torsions_;

	public:
	// Return parent model
	Model* parent();
	// Return coordinate origin
	Vec3<double> origin();
	// Return number of defined elements
	int nElements() const;
	// Return start of defined elements
	ZMatrixElement* elements() const;
	// Return specified element
	ZMatrixElement* element(int index);
	// Return total number of defined variables
	int nVariables();
	// Return number of defined angle variables
	int nAngles();
	// Return start of angles list
	TreeNode* angles();
	// Return specified angle variable
	Variable* angle(int index);
	// Return number of defined distance variables
	int nDistances();
	// Return start of distances list
	TreeNode* distances();
	// Return specified distance variable
	Variable* distance(int index);
	// Return number of defined torsion variables
	int nTorsions();
	// Return start of torsions list
	TreeNode* torsions();
	// Return specified torsion variable
	Variable* torsion(int index);


	/*
	 * Construction and Retrieval
	 */
	private:
	// Add single definition to list
	ZMatrixElement* addElement(RefList<Atom,int>& atomlist);
	// Create zmatrix recursively along bonds
	void createAlongBonds(Atom* target, RefList<Atom,int>& atomlist);
	// Create path of bound atoms from current atom
	bool createBoundPath(RefList<Atom,int>& atomlist, int size, RefList<Atom,int>& bestlist);

	public:
	// Create for specified model
	void create(Model* source, bool usebonds);
	// Set variable value and recalculate atom positions in model
	void setVariable(Variable* v, double value);
	// Print zmatrix
	void print();
	// Print zmatrix to specified LineParser
	void print(LineParser& parser);
};

ATEN_END_NAMESPACE

#endif

