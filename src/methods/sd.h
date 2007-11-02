/*
	*** Steepest descent minimiser
	*** src/methods/sd.h
	Copyright T. Youngs 2007

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

#ifndef H_SD_H
#define H_SD_H

#include "model/model.h"

// Steepest Descent methods
class sd_methods
{
	public:
	// Constructor
	sd_methods();

	/*
	// Control Options
	*/
	private:
	// Stepsize to take in direction
	double stepsize;
	// Maximum cartesian displacement (in any vector)
	double maxstep;
	// Maximum number of iterations to take in one minimisation
	int maxcycles;
	// Maximum number of attempts at finding a new point along the gradient vector
	int maxlinetrials;

	public:
	// Set the stepsize
	void set_stepsize(double d) { stepsize = d; }
	// Get the stepsize
	double get_stepsize() { return stepsize; }
	// Set the maximum cartesian step
	void set_maxstep(double d) { maxstep = d; }
	// Get the maximum cartesian step size
	double get_maxstep() { return maxstep; }
	// Set the maximum iterations
	void set_maxcycles(int i) { maxcycles = i; }
	// Get the maximum iterations
	int get_maxcycles() { return maxcycles; }
	// Set the maximum line trials
	void set_maxlinetrials(int i) { maxlinetrials = i; }
	// Get the maximum line trials
	int get_maxlinetrials() { return maxlinetrials; }
	// Generate a new config following the gradient vector in workcfg
	void gradient_move(model *oldf, model *newr, double delta);
	// Minimise the specified model
	void minimise(model*, double, double);
};

extern sd_methods sd;

#endif
