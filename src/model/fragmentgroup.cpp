/*
	*** Fragment Group
	*** src/model/fragmentgroup.cpp
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

#include "model/fragmentgroup.h"

ATEN_USING_NAMESPACE

// Constructor
FragmentGroup::FragmentGroup() : ListItem<FragmentGroup>()
{
}

// Set name of group
void FragmentGroup::setName(QString name)
{
	name_ = name;
}

// Return name of group
QString FragmentGroup::name()
{
	return name_;
}

// Return number of fragments in group
int FragmentGroup::nFragments()
{
	return fragments_.nItems();
}

// Add new fragment
Fragment* FragmentGroup::addFragment()
{
	return fragments_.add();
}

// Remove existing fragment
void FragmentGroup::removeFragment(Fragment* frag)
{
	fragments_.remove(frag);
}

// Return first fragment in group
Fragment* FragmentGroup::fragments()
{
	return fragments_.first();
}

