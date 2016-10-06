/*
	*** ZMatrix Element
	*** src/base/zmatrixelement.cpp
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

#include "base/zmatrixelement.h"
#include "base/zmatrix.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
ZMatrixElement::ZMatrixElement() : ListItem<ZMatrixElement>()
{
	// Private variables
	parent_ = NULL;
	for (int i=0; i<4; ++i)
	{
		atoms_[i] = NULL;
		if (i<3)
		{
			values_[i] = NULL;
			negated_[i] = false;
		}
	}
}

// Destructor
ZMatrixElement::~ZMatrixElement()
{
}

// Set parent
void ZMatrixElement::setParent(ZMatrix* parent)
{
	parent_ = parent;
}

// Set n'th atom datum
void ZMatrixElement::setAtom(int id, Atom* i)
{
	if ((id < 0) || (id > 3)) printf("Internal Error: Id for ZMatrixElement::setAtom is out of range (%i)\n", id);
	else atoms_[id] = i;
}

// Retrieve n'th atom datum
Atom* ZMatrixElement::atom(int id)
{
	if ((id < 0) || (id > 3)) printf("Internal Error: Id for ZMatrixElement::atom is out of range (%i)\n", id);
	else return atoms_[id];
	return NULL;
}

// Set n'th negate flag
void ZMatrixElement::setNegated(int id, bool b)
{
	if ((id < 0) || (id > 2)) printf("Internal Error: Id for ZMatrixElement::setNegate is out of range (%i)\n", id);
	else
	{
		negated_[id] = b;
		Model* m = parent_->parent();
		if (m != NULL) m->recalculateFromZMatrix();
	}
}

// Retrieve n'th negate flag
bool ZMatrixElement::negated(int id)
{
	if ((id < 0) || (id > 2)) printf("Internal Error: Id for ZMatrixElement::negate is out of range (%i)\n", id);
	else return negated_[id];
	return false;
}
// Set distance (geometry variable 0)
void ZMatrixElement::setDistanceVariable(Variable* v)
{
	values_[0] = v;
}

// Retrieve distance variable (geometry variable 0)
Variable* ZMatrixElement::distanceVariable()
{
	return values_[0];
}

// Set distance variable name (geometry variable 0)
void ZMatrixElement::setDistanceName(QString name)
{
	if (values_[0] == NULL) Messenger::print("Warning: No distance variable exists in ZMatrixElement, so can't set its name.");
	else values_[0]->setName(name);
}

// Retrieve distance variable name (geometry variable 0)
QString ZMatrixElement::distanceName()
{
	if (values_[0] == NULL)
	{
		Messenger::print("Warning: No distance variable exists in ZMatrixElement from which to return a value.");
		return QString();
	}
	if (negated_[0]) return QString("-" + values_[0]->name());
	else return values_[0]->name();
}

// Set distance value
void ZMatrixElement::setDistance(double value)
{
	// Set variable value for distance, and recalculate model
	if (values_[0] == NULL) Messenger::print("Warning: No distance variable exists in ZMatrixElement to set.");
	else parent_->setVariable(values_[0], value);
}

// Retrieve distance (geometry variable 0)
double ZMatrixElement::distance()
{
	static ReturnValue rv;
	if (values_[0] == NULL) Messenger::print("Warning: No distance variable exists in ZMatrixElement from which to return a value.");
	else
	{
		values_[0]->execute(rv);
		return (negated_[0] ? -rv.asDouble() : rv.asDouble());
	}
	return 0.0;
}

// Return atom associated with distance
Atom* ZMatrixElement::distanceAtom()
{
	return atoms_[1];
}

// Set angle (geometry variable 1)
void ZMatrixElement::setAngleVariable(Variable* v)
{
	values_[1] = v;
}

// Retrieve angle variable (geometry variable 1)
Variable* ZMatrixElement::angleVariable()
{
	return values_[1];
}

// Set angle variable name (geometry variable 1)
void ZMatrixElement::setAngleName(QString name)
{
	if (values_[1] == NULL) Messenger::print("Warning: No angle variable exists in ZMatrixElement, so can't set its name.");
	else values_[1]->setName(name);
}

// Retrieve angle variable name (geometry variable 0)
QString ZMatrixElement::angleName()
{
	if (values_[1] == NULL)
	{
		Messenger::print("Warning: No angle variable exists in ZMatrixElement from which to return a value.");
		return QString();
	}
	if (negated_[1]) return QString("-" + values_[1]->name());
	else return values_[1]->name();
}

// Set angle value
void ZMatrixElement::setAngle(double value)
{
	// Set variable value for angle, and recalculate model
	if (values_[1] == NULL) Messenger::print("Warning: No angle variable exists in ZMatrixElement to set.");
	else parent_->setVariable(values_[1], value);
}

// Retrieve angle (geometry variable 1)
double ZMatrixElement::angle()
{
	static ReturnValue rv;
	if (values_[1] == NULL) Messenger::print("Warning: No angle variable exists in ZMatrixElement from which to return a value.");
	else
	{
		values_[1]->execute(rv);
		return (negated_[1] ? -rv.asDouble() : rv.asDouble());
	}
	return 0.0;
}

// Return atom associated with angle
Atom* ZMatrixElement::angleAtom()
{
	return atoms_[2];
}

// Set torsion (geometry variable 2)
void ZMatrixElement::setTorsionVariable(Variable* v)
{
	values_[2] = v;
}

// Retrieve torsion variable (geometry variable 2)
Variable* ZMatrixElement::torsionVariable()
{
	return values_[2];
}

// Set torsion variable name (geometry variable 2)
void ZMatrixElement::setTorsionName(QString name)
{
	if (values_[2] == NULL) Messenger::print("Warning: No torsion variable exists in ZMatrixElement, so can't set its name.");
	else values_[2]->setName(name);
}

// Retrieve torsion variable name (geometry variable 0)
QString ZMatrixElement::torsionName()
{
	if (values_[2] == NULL)
	{
		Messenger::print("Warning: No torsion variable exists in ZMatrixElement from which to return a value.");
		return QString();
	}
	if (negated_[2]) return QString("-" + values_[2]->name());
	else return values_[2]->name();
}

// Set torsion value
void ZMatrixElement::setTorsion(double value)
{
	// Set variable value for torsion, and recalculate model
	if (values_[2] == NULL) Messenger::print("Warning: No torsion variable exists in ZMatrixElement to set.");
	else parent_->setVariable(values_[2], value);
}

// Retrieve torsion (geometry variable 2)
double ZMatrixElement::torsion()
{
	static ReturnValue rv;
	if (values_[2] == NULL) Messenger::print("Warning: No torsion variable exists in ZMatrixElement from which to return a value.");
	else
	{
		values_[2]->execute(rv);
		return (negated_[2] ? -rv.asDouble() : rv.asDouble());
	}
	return 0.0;
}

// Return atom associated with torsion
Atom* ZMatrixElement::torsionAtom()
{
	return atoms_[3];
}
