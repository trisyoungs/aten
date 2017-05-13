/*
	*** ZMatrix Definition
	*** src/base/zmatrix.cpp
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

#include "base/zmatrix.h"
#include "parser/double.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

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

// Return parent model
Model* ZMatrix::parent()
{
	return parent_;
}

// Return coordinate origin
Vec3<double> ZMatrix::origin()
{
	return origin_;
}

// Return total number of defined variables
int ZMatrix::nVariables()
{
	return (distances_.nVariables() + angles_.nVariables() + torsions_.nVariables());
}

// Add single definition to list
ZMatrixElement* ZMatrix::addElement(RefList<Atom,int>& atoms)
{
	Messenger::enter("ZMatrix::addElement");
	int i;
	QString name;
	DoubleVariable* v;
	// Create a new element structure, and store a maximum of 4 atoms from list in the element's array
	ZMatrixElement* zel = elements_.add();
	zel->setParent(this);
	i = 0;
	for (RefListItem<Atom,int>* ri = atoms.first(); ri != NULL; ri = ri->next)
	{
		zel->setAtom(i++, ri->item);
		if (i == 4) break;
	}
	// Variable 'i' now contains the number of atoms we have in this element
	if ((i < 1) || (i > 4))
	{
		printf("Internal Error: Attempted to create a ZMatrixElement with %i atoms\n", i);
		Messenger::exit("ZMatrix::addElement");
		return NULL;
	}
	// Set geometric values
	if (i > 1)
	{
		v = new DoubleVariable(parent_->distance(zel->atom(0), zel->atom(1)), false);
		distances_.take(v);
		name.sprintf("d%i", distances_.nVariables());
		v->setName(name);
		zel->setDistanceVariable(v);
	}
	if (i > 2)
	{
		v = new DoubleVariable(parent_->angle(zel->atom(0), zel->atom(1), zel->atom(2)), false);
		angles_.take(v);
		name.sprintf("a%i", angles_.nVariables());
		v->setName(name);
		zel->setAngleVariable(v);
	}
	if (i > 3)
	{
		v = new DoubleVariable(parent_->torsion(zel->atom(0), zel->atom(1), zel->atom(2), zel->atom(3)), false);
		torsions_.take(v);
		name.sprintf("t%i", torsions_.nVariables());
		v->setName(name);
		zel->setTorsionVariable(v);
	}
	Messenger::exit("ZMatrix::addElement");
	return zel;
}

// Create (recursively) along bonds in the model wherever possible
void ZMatrix::createAlongBonds(Atom* target, RefList<Atom,int>& atomlist)
{
	Messenger::enter("ZMatrix::createAlongBonds");
	// Add the current atom to the list and create an element for it
	atomlist.addStart(target);
	addElement(atomlist);
	// Mark this atom so it won't be added again
	parent_->selectAtom(target, true);
	// Cycle over bonds, progressing along each connected atom
	Atom* i;
	for (RefListItem<Bond,int>* ri = target->bonds(); ri != NULL; ri = ri->next)
	{
		i = ri->item->partner(target);
		if (i->isSelected(true)) continue;
		createAlongBonds(i, atomlist);
	}
	Messenger::exit("ZMatrix::createAlongBonds");
}

// Create path of bound atoms of the requested size, starting from last atom of the supplied list
bool ZMatrix::createBoundPath(RefList<Atom,int>& atomlist, int size, RefList<Atom,int>& bestlist)
{
	// Check for correct path size...
	if (atomlist.nItems() == size)
	{
		bestlist = atomlist;
		return true;
	}
	// From last atom in path, add bound neighbours to path list (if their ID is lower) and recurse if necessary
	Atom* i = atomlist.last()->item, *j;
	int maxid = atomlist.first()->item->id();
	for (RefListItem<Bond,int>* ri = i->bonds(); ri != NULL; ri = ri->next)
	{
		// Get bond neighbour and check that it has a lower ID *and* doesn't already exist in the list
		j = ri->item->partner(i);
		if (j->id() >= maxid) continue;
		if (atomlist.contains(j)) continue;
		// OK, so add to list and check for correct size
		atomlist.add(j);
		if (atomlist.nItems() > bestlist.nItems()) bestlist = atomlist;
		if (atomlist.nItems() == size) return true;
		// Not enough atoms yet, so recurse...
		if (createBoundPath(atomlist, size, bestlist)) return true;
		// Still not big enough, so remove this atom from the list tail and try another bound neighbour
		atomlist.removeLast();
	}
	return false;
}

// Create from specified model
void ZMatrix::create(Model* source, bool usebonds)
{
	Messenger::enter("ZMatrix::create");
	// Clear old data and set new target
	elements_.clear();
	distances_.clear();
	angles_.clear();
	torsions_.clear();
	parent_ = source;
	// Lists of previous atoms
	RefList<Atom,int> atomlist, boundpath, bestpath;
	ZMatrixElement* zel;
	if (parent_->nAtoms() == 0)
	{
		Messenger::exit("ZMatrix::create");
		return;
	}
	// Create the elements
	origin_ = parent_->atoms()->r();
	if (usebonds)
	{
		// Step through atoms in order, creating elements as we go using bonds wherever possible
		// We always maintain a list of the previous four atoms, just in case there are not enough bound connections to use
		for (Atom* i = parent_->atoms(); i != NULL; i = i->next)
		{
			// Check current size of atomlist
			if (atomlist.nItems() == 4) atomlist.removeLast();
			// Add current atom to the reflist
			atomlist.addStart(i);
			// Construct a path of bound atoms
			boundpath.clear();
			bestpath.clear();
			boundpath.add(i);
			if (createBoundPath(boundpath, atomlist.nItems(), bestpath)) zel = addElement(bestpath);
			else
			{
				// Take best path found, and add extra atoms to it
				for (RefListItem<Atom,int>* ri = atomlist.first(); ri != NULL; ri = ri->next)
				{
					if (!bestpath.contains(ri->item)) bestpath.add(ri->item);
					if (bestpath.nItems() == atomlist.nItems()) break;
				}
				if (bestpath.nItems() != atomlist.nItems())
				{
					Messenger::print("Internal Error: Failed to create ZMatrix using connectivity.");
					Messenger::exit("ZMatrix::create");
					return;
				}
				zel = addElement(bestpath);
			}
		}
	}
	else
	{
		// Step through atoms in order, creating elements as we go...
		for (Atom* i = parent_->atoms(); i != NULL; i = i->next)
		{
			// Check current size of atomlist
			if (atomlist.nItems() == 4) atomlist.removeLast();
			// Add current atom to the reflist
			atomlist.addStart(i);
			// Create element
			zel = addElement(atomlist);
		}

	}
	Messenger::exit("ZMatrix::create");
}

// Return number of defined elements
int ZMatrix::nElements() const
{
	return elements_.nItems();
}

// Return specified element
ZMatrixElement* ZMatrix::elements() const
{
	return elements_.first();
}

// Return specified element
ZMatrixElement* ZMatrix::element(int index)
{
	return elements_[index];
}

// Return number of defined angle variables
int ZMatrix::nAngles()
{
	return angles_.nVariables();
}

// Return start of angles list
TreeNode* ZMatrix::angles()
{
	return angles_.variables();
}

// Return specified angle variable
Variable* ZMatrix::angle(int index)
{
	if ((index < 0) || (index >= angles_.nVariables())) printf("Array index %i is out of bounds for ZMatrix::angles\n", index);
	else return (Variable*) angles_.variable(index);
	return NULL;
}

// Return number of defined distance variables
int ZMatrix::nDistances()
{
	return distances_.nVariables();
}

// Return start of distances list
TreeNode* ZMatrix::distances()
{
	return distances_.variables();
}

// Return specified distance variable
Variable* ZMatrix::distance(int index)
{
	if ((index < 0) || (index >= distances_.nVariables())) printf("Array index %i is out of bounds for ZMatrix::distances\n", index);
	else return (Variable*) distances_.variable(index);
	return NULL;
}

// Return number of defined torsion variables
int ZMatrix::nTorsions()
{
	return torsions_.nVariables();
}

// Return start of torsions list
TreeNode* ZMatrix::torsions()
{
	return torsions_.variables();
}

// Return specified torsion variable
Variable* ZMatrix::torsion(int index)
{
	if ((index < 0) || (index >= torsions_.nVariables())) printf("Array index %i is out of bounds for ZMatrix::torsions\n", index);
	else return (Variable*) torsions_.variable(index);
	return NULL;
}

// Set variable value and update
void ZMatrix::setVariable(Variable* v, double value)
{
	Messenger::enter("ZMatrix::setVariable");
	// Check for NULL pointer
	if (v == NULL)
	{
		printf("Internal Error: NULL variable pointer passed to ZMatrix::setVariable\n");
		Messenger::exit("ZMatrix::setVariable");
		return;
	}
	// Set the new value of the specified variable
	ReturnValue newvalue(value);
	v->set( newvalue );
	parent_->recalculateFromZMatrix();
	Messenger::exit("ZMatrix::setVariable");
}

// Print zmatrix
void ZMatrix::print()
{
	Atom* i, *j, *k, *l;
	for (ZMatrixElement* zel = elements_.first(); zel != NULL; zel = zel->next)
	{
		// First atom (the creation target)
		i = zel->atom(0);
		// Second atom (distance specifier)
		j = zel->atom(1);
		if (j != NULL)
		{
			// Third atom (angle specifier)
			k = zel->atom(2);
			if (k != NULL)
			{
				// Fourth atom (torsion specifier)
				l = zel->atom(3);
				if (l != NULL)
				{
					printf("%-4s %-4i %-6s %-4i %-6s %-4i %-6s\n", ElementMap::symbol(i), j->id()+1, qPrintable(zel->distanceVariable()->name()), k->id()+1, qPrintable(zel->angleVariable()->name()), l->id()+1, qPrintable(zel->torsionVariable()->name()));
				}
				else printf("%-4s %-4i %-6s %-4i %-6s\n", ElementMap::symbol(i), j->id()+1, qPrintable(zel->distanceVariable()->name()), k->id()+1, qPrintable(zel->angleVariable()->name()));
			}
			else printf("%-4s %-4i %-6s\n", ElementMap::symbol(i), j->id()+1, qPrintable(zel->distanceVariable()->name()));
		}
		else printf("%-4s\n", ElementMap::symbol(i));
	}
	printf("\n");

	// Variable list
	ReturnValue rv;
	for (int n=0; n<distances_.nVariables(); ++n)
	{
		Variable* var = distances_.variable(n);
		var->execute(rv);
		printf("  %s   %f\n", qPrintable(var->name()), rv.asDouble());
	}
	for (int n=0; n<angles_.nVariables(); ++n)
	{
		Variable* var = angles_.variable(n);
		var->execute(rv);
		printf("  %s   %f\n", qPrintable(var->name()), rv.asDouble());
	}
	for (int n=0; n<torsions_.nVariables(); ++n)
	{
		Variable* var = torsions_.variable(n);
		var->execute(rv);
		printf("  %s   %f\n", qPrintable(var->name()), rv.asDouble());
	}
}

// Print zmatrix to specified LineParser
void ZMatrix::print(LineParser& parser)
{
	Atom* i, *j, *k, *l;
	for (ZMatrixElement* zel = elements_.first(); zel != NULL; zel = zel->next)
	{
		// First atom (the creation target)
		i = zel->atom(0);
		// Second atom (distance specifier)
		j = zel->atom(1);
		if (j != NULL)
		{
			// Third atom (angle specifier)
			k = zel->atom(2);
			if (k != NULL)
			{
				// Fourth atom (torsion specifier)
				l = zel->atom(3);
				if (l != NULL)
				{
					parser.writeLineF("%-4s %-4i %-6s %-4i %-6s %-4i %-6s\n", ElementMap::symbol(i), j->id()+1, qPrintable(zel->distanceVariable()->name()), k->id()+1, qPrintable(zel->angleVariable()->name()), l->id()+1, qPrintable(zel->torsionVariable()->name()));
				}
				else parser.writeLineF("%-4s %-4i %-6s %-4i %-6s\n", ElementMap::symbol(i), j->id()+1, qPrintable(zel->distanceVariable()->name()), k->id()+1, qPrintable(zel->angleVariable()->name()));
			}
			else parser.writeLineF("%-4s %-4i %-6s\n", ElementMap::symbol(i), j->id()+1, qPrintable(zel->distanceVariable()->name()));
		}
		else parser.writeLineF("%-4s\n", ElementMap::symbol(i));
	}
	parser.writeLineF("\n");

	// Variable list
	ReturnValue rv;
	for (int n=0; n<distances_.nVariables(); ++n)
	{
		Variable* var = distances_.variable(n);
		var->execute(rv);
		parser.writeLineF("  %s   %f\n", qPrintable(var->name()), rv.asDouble());
	}
	for (int n=0; n<angles_.nVariables(); ++n)
	{
		Variable* var = angles_.variable(n);
		var->execute(rv);
		parser.writeLineF("  %s   %f\n", qPrintable(var->name()), rv.asDouble());
	}
	for (int n=0; n<torsions_.nVariables(); ++n)
	{
		Variable* var = torsions_.variable(n);
		var->execute(rv);
		parser.writeLineF("  %s   %f\n", qPrintable(var->name()), rv.asDouble());
	}
}
