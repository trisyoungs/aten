/*
	*** Line minimiser
	*** src/methods/line.h
	Copyright T. Youngs 2007-2015

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

#include "model/model.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Line Minimiser
class LineMinimiser
{
	public:
	// Constructor
	LineMinimiser();

	/*
	 * Variables
	 */
	protected:
	// Tolerance for root finding
	double tolerance_;
	// Local, temporary model for storing coordinates after gradient moves
	Model tempModel_;

	public:
	// Set the tolerance
	void setTolerance(double t);
	// Return current tolerance
	double tolerance() const;
	// Initialise temporary model
	void initialise(Model* srcmodel);

	/*
	 * Minimisation Methods
	 */
	protected:
	// Generate a new config in tempModel_ following the supplied gradient vector
	void gradientMove(Model* source, double delta);
	// Perform Golden Search within specified bounds
	void goldenSearch(Model* source, double* bounds, double* energies);

	public:
	// Minimise the specified model (srcmodel should already contain desired forces (i.e. gradient vector)) along which to minimise)
	double lineMinimise(Model* source);
};

ATEN_END_NAMESPACE

#endif
