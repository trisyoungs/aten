/*
	*** Line minimiser
	*** src/methods/line.h
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

#ifndef ATEN_LINEMIN_H
#define ATEN_LINEMIN_H

// Forward declarations
class model;

// Line Minimiser
class linemin
{
	public:
	// Constructor
	linemin();

	private:
	// Tolerance for root finding
	double tolerance;

	public:
	// Set the tolerance
	void set_tolerance(double t) { tolerance = t; }
	// Return current tolerance
	double get_tolerance() { return tolerance; }
	// Generate a new config following the supplied gradient vector
	void gradient_move(model *source, model *dest, double delta);
	// Minimise the specified model (srcmodel should already contain desired forces (i.e. gradient vector)) along which to minimise)
	double line_minimise(model *source);
};

#endif
