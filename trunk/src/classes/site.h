/*
	*** Molecule site
	*** src/classes/site.h
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

#ifndef ATEN_SITE_H
#define ATEN_SITE_H

#include "templates/vector3.h"
#include "templates/list.h"
#include "classes/dnchar.h"

// Site types
enum site_type { ST_ATOMCOM, ST_ATOMCOG, ST_MOLCOM, ST_MOLCOG, ST_NITEMS };
const char *text_from_ST(site_type);
site_type ST_from_text(const char *);

// Forward declarations
class atomaddress;
class pattern;
class model;

// Site
class site
{
	public:
	// Constructor / Destructor
	site();
	~site();
	// List pointers
	site *prev, *next;

	/*
	// Site pattern, molecule and atoms
	*/
	private:
	// Pattern the site is related to
	pattern *sourcepattern;
	// Molecule mask definition
	// TODO Select a subset of molecules based on some kind of criteria
	// Name of site
	dnchar name;

	public:
	// Set the pattern pointer for the atom
	void set_pattern(pattern *p) { sourcepattern = p; }
	// Returns the current pattern for the atom
	pattern *get_pattern() { return sourcepattern; }
	// Set name of site
	void set_name(const char *s) { name = s; }
	// Get name of site
	const char *get_name() { return name.get(); }

	/*
	// Site Centre
	*/
	private:
	// Type of centre site
	site_type centretype;
	// Coordinates of site
	vec3<double> centre;

	public:
	// Set type of site centre
	void set_centre_type(site_type st) { centretype = st; }
	// List of relative atom ids that define the site
	list< listitem<int> > atoms;
	// Calculate centre from config and molecule ID supplied
	vec3<double> calculate_centre(model*, int);

	/*
	// Site Axes
	*/
	private:
	// Matrix defining local coordinate system
	mat3<double> axes;

	public:
	// List of atoms whose average defines the x axis (from site centre)
	list< listitem<int> > xaxisatoms;
	// List of atoms whose average defines the y axis (from site centre)
	list< listitem<int> > yaxisatoms;
	// Calculate local coordinate system from config and molecule ID supplied
	mat3<double> calculate_axes(model*, int);
};

#endif
