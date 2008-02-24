/*
	*** Basic quantity definition
	*** src/methods/quantity.h
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

#ifndef H_CALCULABLE_H
#define H_CALCULABLE_H

#include "classes/dnchar.h"

// Forward Declarations
class model;

// Calculable Class
class calculable
{
	/*
	// Calculable Quantity
	*/
	public:
	// Constructor / Destructor
	calculable();
	~calculable();
	// List pointers
	calculable *prev, *next;

	/*
	// Identity
	*/
	protected:
	// Identifiable name of the quantity
	dnchar name;
	// Filename (of loaded file or target to be saved)
	dnchar filename;

	public:
	// Set identifiable name of the quantity
	void set_name(const char *s) { name = s; }
	// Return name of the quantity
	const char *get_name() { return name.get(); }
	// Set filename of the quantity
	void set_filename(const char *s) { filename = s; }
	// Return filename of the quantity
	const char *get_filename() { return filename.get(); }

	/*
	// Methods
	*/
	public:
	// Accumulate quantity data from supplied config
	virtual void accumulate(model*)=0;
	// Initialise - check sites, create arrays etc.
	virtual bool initialise()=0;
	// Finalise data
	virtual void finalise(model*)=0;
	// Save data
	virtual bool save()=0;
};

#endif
