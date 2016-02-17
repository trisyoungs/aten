/*
	*** FragmentGroup
	*** src/model/fragmentgroup.h
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

#ifndef ATEN_FRAGMENTGROUP_H
#define ATEN_FRAGMENTGROUP_H

#include "model/fragment.h"
#include "templates/list.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Atom;

// Fragment Library
class FragmentGroup : public ListItem<FragmentGroup>
{
	public:
	// Constructor
	FragmentGroup();

	/*
	 * Data
	 */
	private:
	// Name of the group
	QString name_;
	// List of fragments in this group
	List<Fragment> fragments_;

	public:
	// Set name of group
	void setName(QString name);
	// Return name of group
	QString name();
	// Return number of fragments in group
	int nFragments();
	// Add new fragment
	Fragment* addFragment();
	// Remove existing fragment
	void removeFragment(Fragment* frag);
	// Return first fragment in group
	Fragment* fragments();
	
};

ATEN_END_NAMESPACE

#endif
