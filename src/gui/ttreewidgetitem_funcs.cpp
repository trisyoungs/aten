/*
	*** Qt TTreeWidgetItem functions
	*** src/gui/ttreewidgetitem_funcs.cpp
	Copyright T. Youngs 2007,2008

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

// Prevent Windows macro idiocy
#define NOMINMAX

#include "base/elements.h"
#include "base/sysfunc.h"
#include "classes/forcefieldatom.h"
#include "base/atom.h"
#include "gui/ttreewidgetitem.h"

// Constructor
TTreeWidgetItem::TTreeWidgetItem(QTreeWidgetItem *parent) : QTreeWidgetItem(parent)
{
	// Private variables
	atom_ = NULL;
	pattern_ = NULL;
	forcefieldAtom_ = NULL;
}

// Constructor
TTreeWidgetItem::TTreeWidgetItem(QTreeWidget *parent) : QTreeWidgetItem(parent)
{
	// Private variables
	atom_ = NULL;
	pattern_ = NULL;
	forcefieldAtom_ = NULL;
}

// Set the atom pointer in the widget
void TTreeWidgetItem::setAtom(Atom *source)
{
	atom_ = source;
}

// Return the atom pointer in the widget
Atom *TTreeWidgetItem::atom()
{
	return atom_;
}

// Set the ForcefieldAtom pointer in the widget
void TTreeWidgetItem::setForcefieldAtom(ForcefieldAtom *source)
{
	forcefieldAtom_ = source;
}

// Return the ForcefieldAtom pointer in the widget
ForcefieldAtom *TTreeWidgetItem::forcefieldAtom()
{
	return forcefieldAtom_;
}

// Set the pattern pointer in the widget
void TTreeWidgetItem::setPattern(Pattern *source)
{
	pattern_ = source;
}

// Return the pattern pointer in the widget
Pattern *TTreeWidgetItem::pattern()
{
	return pattern_;
}

// Set column data in item
void TTreeWidgetItem::setAtomColumns()
{
	static Vec3<double> r;
	if (atom_ == NULL) printf("TTreeWidgetItem::setAtomColumns <<<< Pointer has not yet been set >>>>\n");
	else
	{
		setText(TW_A_ID, itoa(atom_->id()+1));
		setText(TW_A_ELEMENT, elements.symbol(atom_));
		r = atom_->r();
		setText(TW_A_RX, ftoa(r.x));
		setText(TW_A_RY, ftoa(r.y));
		setText(TW_A_RZ, ftoa(r.z));
	}
}

void TTreeWidgetItem::setForcefieldAtomColumns()
{
	if (forcefieldAtom_ == NULL) printf("TTreeWidgetItem::setForcefieldAtomColumns <<<< Pointer has not yet been set >>>>\n");
	else
	{
		setText(TW_FFA_NAME, forcefieldAtom_->name());
		setText(TW_FFA_DESCRIPTION, forcefieldAtom_->description());
	}
}
