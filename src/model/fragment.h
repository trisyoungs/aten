/*
	*** Fragment Model Data
	*** src/model/fragment.h
	Copyright T. Youngs 2007-2011

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

#ifndef ATEN_FRAGMENTDATA_H
#define ATEN_FRAGMENTDATA_H

#include "base/dnchar.h"
#include "model/model.h"
#include <QtGui/QIcon>
#include "templates/list.h"

// Forward Declarations
class Atom;

// Fragment Model data
class Fragment
{
	public:
	// Constructor
	Fragment();
	// List pointers
	Fragment *prev, *next;

	/*
	// Fragment Model Data
	*/
	private:
	// Pointer to master model, containing fragment in original rotation
	Model *masterModel_;
	// Link atom, which acts as connection point for fragment
	Atom *masterLinkAtom_;
	// Bond partner for link atom (if any)
	Atom *masterLinkPartner_;
	// Set link partner
	void setLinkPartner();

	public:
	// Set data from source model
	bool setMasterModel(Model *m);
	// Return model pointer
	Model *masterModel();
	// Return icon (from masterModel_)
	QIcon &icon();
	// Cycle link atom
	void cycleLinkAtom();


	/*
	// Manipulated Fragments
	*/
	private:
	// Fragment oriented by user
	Model orientedModel_;
	// Fragment oriented to anchor point
	Model anchoredModel_;

	public:
	// Reset oriented model
	void resetOrientedModel();
	// Rotate oriented model according to screen delta
	void rotateOrientedModel(double dx, double dy);
	// Return oriented model pointer
	Model *orientedModel();
	// Paste oriented model to target model
	void pasteOrientedModel(Vec3<double> origin, Model *model);
	// Adjust anchored model rotation (from mouse delta)
	void rotateAnchoredModel(double dx, double dy);
	// Return anchored model, oriented to attach to specified atom
	Model *anchoredModel(Atom *anchorpoint, bool replace, int &replacebond);
	// Paste anchored model to target model
	void pasteAnchoredModel(Atom* anchorpoint, bool replace, int& replacebond, Model* target, bool adjustbond);
};

// Fragment Library
class FragmentGroup
{
	public:
	// Constructor
	FragmentGroup();
	// List pointers
	FragmentGroup *prev, *next;

	/*
	// Data
	*/
	private:
	// Name of the group
	Dnchar name_;
	// List of fragments in this group
	List<Fragment> fragments_;

	public:
	// Set name of group
	void setName(const char *s);
	// Return name of group
	const char *name();
	// Return number of fragments in group
	int nFragments();
	// Add new fragment
	Fragment *addFragment();
	// Remove existing fragment
	void removeFragment(Fragment *frag);
	// Return first fragment in group
	Fragment *fragments();
	
};

#endif
