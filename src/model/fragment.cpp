/*
	*** Fragment Model Data
	*** src/model/fragment.cpp
	Copyright T. Youngs 2007-2010

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUE ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "model/fragment.h"
#include "model/model.h"
#include "base/messenger.h"

/*
// Fragment
*/

// Constructor
Fragment::Fragment()
{
	// Private variables
	model_ = NULL;
	linkAtom_ = NULL;
	linkPartner_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set data from source model
bool Fragment::setModel(Model *m)
{
	msg.enter("Fragment::setModel");
	model_ = m;
	// Define link (anchor) atom
	if (model_->nUnknownAtoms() == 0)
	{
		msg.print(" ... Warning - Fragment model has no defined anchor point. Assuming first atom.\n");
		linkAtom_ = model_->atoms();
	}
	else if (model_->nUnknownAtoms() > 1)
	{
		msg.print(" ... Warning - Fragment model has multiple anchor points. Using lowest ID.\n");
		for (linkAtom_ = model_->atoms(); linkAtom_ != NULL; linkAtom_ = linkAtom_->next) if (linkAtom_->element() == 0) break;
	}
	else for (linkAtom_ = model_->atoms(); linkAtom_ != NULL; linkAtom_ = linkAtom_->next) if (linkAtom_->element() == 0) break;
	if (linkAtom_ == NULL)
	{
		msg.print(" ... Error: No link atom defined for fragment '%s'. Fragment will be removed from list.\n", model_->name());
		msg.exit("Fragment::setModel");
		return FALSE;
	}
	// Find link partner
	if (linkAtom_->nBonds() != 1) linkPartner_ = linkAtom_->bonds()->item->partner(linkAtom_);
	else if (linkAtom_ == model_->atoms()) linkPartner_ = linkAtom_->next;
	else linkPartner_ = model_->atoms();
	msg.exit("Fragment::setModel");
	return TRUE;
}

// Return model pointer
Model *Fragment::model()
{
	return model_;
}

// Return link atom
Atom *Fragment::linkAtom()
{
	return linkAtom_;
}

// Return link atom partner
Atom *Fragment::linkPartner()
{
	return linkPartner_;
}

// Return pixmap
QPixmap &Fragment::pixmap()
{
	return pixmap_;
}

/*
// Fragment Group
*/

// Constructor
FragmentGroup::FragmentGroup()
{
	// Private variables

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set name of group
void FragmentGroup::setName(const char *s)
{
	name_ = s;
}

// Return name of group
const char *FragmentGroup::name()
{
	return name_.get();
}

// Return number of fragments in group
int FragmentGroup::nFragments()
{
	return fragments_.nItems();
}

// Return first fragment in group
Fragment *FragmentGroup::fragments()
{
	return fragments_.first();
}
