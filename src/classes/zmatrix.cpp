/*
	*** ZMatrix Definition
	*** src/classes/zmatrix.cpp
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

// 'Prevent Windows collapsing in on itself' caveat
#define NOMINMAX

#include "classes/zmatrix.h"
#include "parser/double.h"
#include "model/model.h"
#include "base/sysfunc.h"

/*
// ZMatrix Element
*/

// Constructor
ZMatrixElement::ZMatrixElement()
{
	// Public variables
	prev = NULL;
	next = NULL;

	// Private variables
	for (int i=0; i<4; ++i)
	{
		atoms_[i] = NULL;
		if (i<3)
		{
			values_[i] = NULL;
			negate_[i] = FALSE;
		}
	}
}

// Destructor
ZMatrixElement::~ZMatrixElement()
{
}

// Set n'th atom datum
void ZMatrixElement::setAtom(int id, Atom *i)
{
	if ((id < 0) || (id > 3)) printf("Internal Error: Id for ZMatrixElement::setAtom is out of range (%i)\n", id);
	else atoms_[id] = i;
}

// Retrieve n'th atom datum
Atom *ZMatrixElement::atom(int id)
{
	if ((id < 0) || (id > 3)) printf("Internal Error: Id for ZMatrixElement::atom is out of range (%i)\n", id);
	else return atoms_[id];
	return NULL;
}

// Set distance (geometry variable 0)
void ZMatrixElement::setDistance(Variable *v)
{
	values_[0] = v;
}

// Retrieve distance (geometry variable 0)
Variable *ZMatrixElement::distance()
{
	return values_[0];
}

// Set angle (geometry variable 1)
void ZMatrixElement::setAngle(Variable *v)
{
	values_[1] = v;
}

// Retrieve angle (geometry variable 1)
Variable *ZMatrixElement::angle()
{
	return values_[1];
}

// Set torsion (geometry variable 2)
void ZMatrixElement::setTorsion(Variable *v)
{
	values_[2] = v;
}

// Retrieve torsion (geometry variable 2)
Variable *ZMatrixElement::torsion()
{
	return values_[2];
}

/*
// ZMatrix
*/

// Constructor
ZMatrix::ZMatrix()
{
	// Private variables
	parent_ = NULL;
}

// Destructor
ZMatrix::~ZMatrix()
{
}

// Add single definition to list
ZMatrixElement *ZMatrix::addElement(Reflist<Atom,int> &atoms)
{
	msg.enter("ZMatrix::addElement");
	int i;
	char name[32];
	DoubleVariable *v;
	// Create a new element structure, and store atoms from list in the element's array
	ZMatrixElement *zel = elements_.add();
	i = 0;
	for (Refitem<Atom,int> *ri = atoms.first(); ri != NULL; ri = ri->next) zel->setAtom(++i, ri->item);
	// Variable 'i' now contains the number of atoms we have in this element
	if ((i < 1) || (i > 4))
	{
		printf("Internal Error: Attempted to create a ZMatrixElement with %i atoms\n", i);
		msg.exit("ZMatrix::addElement");
		return NULL;
	}
	// Set geometric values
	if (i > 1)
	{
		v = new DoubleVariable(parent_->distance(zel->atom(0), zel->atom(1)), FALSE);
		distances_.take(v);
		strcpy(name,"d");
		strcat(name, itoa(distances_.nVariables()));
		v->setName(name);
		zel->setDistance(v);
	}
	if (i > 2)
	{
		v = new DoubleVariable(parent_->angle(zel->atom(0), zel->atom(1), zel->atom(2)), FALSE);
		angles_.take(v);
		strcpy(name,"d");
		strcat(name, itoa(distances_.nVariables()));
		v->setName(name);
	}

	msg.exit("ZMatrix::addElement");
	return zel;
}

// Create from specified model
void ZMatrix::create(Model *source, bool usebonds)
{
	msg.enter("ZMatrix::create");
	// Clear old data and set new target
	elements_.clear();
	distances_.clear();
	angles_.clear();
	torsions_.clear();
	parent_ = source;
	// List of previous atoms
	Reflist<Atom,int> atomlist;
	ZMatrixElement *zel;
	// Step through atoms in order, creating elements as we go...
	for (Atom *i = parent_->atoms(); i != NULL; i = i->next)
	{
		// Check current size of atomlist
		if (atomlist.nItems() == 4) atomlist.removeFirst();
		// Add current atom to the reflist
		zel = addElement(atomlist);
	}
	msg.exit("ZMatrix::create");
}

