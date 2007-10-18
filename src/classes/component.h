/*
	*** Monte Carlo molecule component
	*** src/classes/component.h

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

#ifndef H_COMPONENT_H
#define H_COMPONENT_H

#include "methods/mc.h"
#include "classes/region.h"
#include "classes/dnchar.h"

// Forward declarations
class model;
class pattern;

// Component
class component
{
	public:
	// Constructor / Destructor
	component();
	~component();
	// List pointers
	component *prev, *next;

	/*
	// Component (for disordered builder)
	*/
	private:
	// Pointer to the component model
	model *compmodel;
	// Pointer to the components related pattern
	pattern *comppattern;
	// Number of requested and actual (filled) molecules of this type
	int nrequested, nfilled;
	// Lists which MC move types are allowed for this component
	bool allowed_moves[MT_NITEMS];
	// ID of the component in the list
	int id;
	// Name of the component
	dnchar name;

	public:
	// Type of region the component is limited to
	region area;
	// Return the ID of the component
	int get_id() { return id; }
	// Set the ID of the component
	void set_id(int i) { id = i; }
	// Decrease the id of the component by 1
	void decrease_id() { id --; }
	// Set the component's model
	void set_model(model *m) { compmodel = m; }
	// Return the component's model
	model *get_model() { return compmodel; }
	// Set the component's pattern
	void set_pattern(pattern *p) { comppattern = p; }
	// Return the component's pattern
	pattern *get_pattern() { return comppattern; }
	// Set the requested number of molecules
	void set_nrequested(int i) { nrequested = i; }
	// Return the requested number of molecules
	int get_nrequested() { return nrequested; }
	// Set the number of molecules filled
	void set_nfilled(int i) { nfilled = i; }
	// Return the number of molecules filled
	int get_nfilled() { return nrequested; }
	// Set a specific move type for the component
	void set_allowed(mc_move m, bool b) { allowed_moves[m] = b; }
	// Set whether the component may be translated
	bool get_allowed(mc_move m) { return allowed_moves[m]; }
	// Set name of component
	void set_name(const char *s) { name = s; }
	// Get name of component
	const char *get_name() { return name.get(); }
};

#endif
