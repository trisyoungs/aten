/*
	*** Molecule site
	*** src/classes/site.h
	Copyright T. Youngs 2007-2009
	
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
#include "base/dnchar.h"

// Site types
enum SiteType { ST_ATOMCOM, ST_ATOMCOG, ST_MOLCOM, ST_MOLCOG, ST_NITEMS };
const char *text_from_ST(SiteType);
SiteType ST_from_text(const char *);

// Forward declarations
class Atomaddress;
class Pattern;
class Model;

// Site
class Site
{
	public:
	// Constructor
	Site();
	// List pointers
	Site *prev, *next;

	/*
	// Site pattern, molecule and atoms
	*/
	private:
	// Pattern the site is related to
	Pattern *pattern_;
	// Molecule mask definition
	// TODO Select a subset of molecules based on some kind of criteria
	// Name of site
	Dnchar name_;

	public:
	// Set the pattern pointer for the atom
	void setPattern(Pattern *p);
	// Returns the current pattern for the atom
	Pattern *pattern();
	// Set name of site
	void setName(const char *s);
	// Get name of site
	const char *name();

	/*
	// Site Centre
	*/
	private:
	// Type of centre site
	SiteType type_;
	// Coordinates of site
	Vec3<double> centre_;

	public:
	// Set type of site centre
	void setType(SiteType st);
	// Set centre of site
	void setCentre(Vec3<double>);
	// List of relative atom ids that define the site
	List< ListItem<int> > atoms;

	/*
	// Site Axes
	*/
	private:
	// Matrix defining local coordinate system
	Mat3<double> axes_;

	public:
	// Set axes
	void setAxes(Mat3<double>);
	// List of atoms whose average defines the x axis (from site centre)
	List< ListItem<int> > xAxisAtoms;
	// List of atoms whose average defines the y axis (from site centre)
	List< ListItem<int> > yAxisAtoms;
};

#endif
