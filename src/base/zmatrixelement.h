/*
	*** ZMatrix Element
	*** src/base/zmatrixelement.h
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

#ifndef ATEN_ZMATRIXELEMENT_H
#define ATEN_ZMATRIXELEMENT_H

#include "parser/variablelist.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Atom;
class Model;
class ZMatrix;

// Element of ZMatrix
class ZMatrixElement : public ListItem<ZMatrixElement>
{
	public:
	// Constructor / Destructor
	ZMatrixElement();
	~ZMatrixElement();


	/*
	 * Data
	 */
	private:
	// Parent ZMatrix
	ZMatrix* parent_;
	// Atom pointers (first = target, second-fourth = distance, angle, torsion specifiers)
	Atom* atoms_[4];
	// Variables holding defined distance, angle, and torsion values
	Variable* values_[3];
	// Flags stating whether the negative of the variable value should be used instead
	bool negated_[3];

	public:
	// Set parent
	void setParent(ZMatrix* parent);
	// Set n'th atom datum
	void setAtom(int id, Atom* i);
	// Retrieve n'th atom datum
	Atom* atom(int id);
	// Set n'th negate flag
	void setNegated(int id, bool b);
	// Retrieve n'th negate flag
	bool negated(int id);
	// Set distance (geometry variable 0)
	void setDistanceVariable(Variable* v);
	// Retrieve distance variable (geometry variable 0)
	Variable* distanceVariable();
	// Set distance variable name (geometry variable 0)
	void setDistanceName(QString name);
	// Retrieve distance variable name (geometry variable 0)
	QString distanceName();
	// Set distance value
	void setDistance(double value);
	// Retrieve distance value (geometry variable 0)
	double distance();
	// Set angle (geometry variable 1)
	void setAngleVariable(Variable* v);
	// Retrieve angle variable (geometry variable 1)
	Variable* angleVariable();
	// Set angle variable name (geometry variable 1)
	void setAngleName(QString name);
	// Retrieve angle variable name (geometry variable 1)
	QString angleName();
	// Set angle value
	void setAngle(double value);
	// Retrieve angle value (geometry variable 1)
	double angle();
	// Set torsion (geometry variable 2)
	void setTorsionVariable(Variable* v);
	// Retrieve torsion variable (geometry variable 2)
	Variable* torsionVariable();
	// Set torsion variable name (geometry variable 2)
	void setTorsionName(QString name);
	// Retrieve torsion variable name (geometry variable 2)
	QString torsionName();
	// Set torsion value
	void setTorsion(double value);
	// Retrieve torsion value (geometry variable 2)
	double torsion();
};

ATEN_END_NAMESPACE

#endif

