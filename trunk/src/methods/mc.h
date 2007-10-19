/*
	*** Monte Carlo methods
	*** src/methods/mc.h
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

#ifndef H_MONTECARLO_H
#define H_MONTECARLO_H

#include "templates/vector3.h"
#include "templates/reflist.h"
#include "templates/list.h"

// Monte Carlo move types
enum mc_move { MT_TRANSLATE, MT_ROTATE, MT_ZMATRIX, MT_INSERT, MT_DELETE, MT_NITEMS };
const char *text_from_MT(mc_move);
mc_move MT_from_text(const char*);

// Forward declarations
class model;
class unitcell;
class component;

// Monte Carlo
class mc_methods
{
	/*
	// Main Routines
	*/
	public:
	// Constructor
	mc_methods();
	// Minimise the specified model
	bool minimise(model*, double, double);
	// Run disordered builder
	bool disorder(model*);

	/*
	// Subroutines
	*/
	public:
	// Create acceptance ratio array
	void create_ratioarray(int);

	/*
	// MC Move Probabilities, step sizes, trial numbers
	*/
	private:
	// Maximum size of allowed move (units depend on move type)
	double maxstep[MT_NITEMS];
	// Number of times to attempt move types
	int ntrials[MT_NITEMS];
	// Turn on/off move types
	bool allowed[MT_NITEMS];
	// Acceptance ratio counts per pattern
	double **acceptratio;
	// Size of acceptratio array
	int acceptratio_size;
	// Number of cycles to perform in MC method
	int ncycles;
	// Energy differences below which to accept moves
	double eaccept[MT_NITEMS];
	// Scaling factor for VDW radius in disorder method
	double vdw_radius_scale;

	public:
	// Set maximum stepsize for MC move
	void set_maxstep(mc_move m, double d) { maxstep[m] = d; }
	// Get maximum stepsize for MC move
	double get_maxstep(mc_move m) { return maxstep[m]; }
	// Set ntrials for MC move
	void set_ntrials(mc_move m, int i) { ntrials[m] = i; }
	// Get ntrials for MC move
	int get_ntrials(mc_move m) { return ntrials[m]; }
	// Set allowed flag for MC move
	void set_allowed(mc_move m, bool b) { allowed[m] = b; }
	// Get allowed flag for MC move
	bool get_allowed(mc_move m) { return allowed[m]; }
	// Set eaccept limit for MC move
	void set_eaccept(mc_move m, double d) { eaccept[m] = d; }
	// Get eaccept limit for MC move
	double get_eaccept(mc_move m) { return eaccept[m]; }
	// Set number of MC cycles to perform
	void set_ncycles(int i) { ncycles = i; }
	// Get ntrials for MC move
	int get_ncycles() { return ncycles; }
	// Sets the vDW radius scale
	void set_vdw_radius_scale(double d) { vdw_radius_scale = d; }

	/*
	// Component list (for disorder builder)
	*/
	private:
	// List of component models to use in MC insertion
	list<component> components;

	public:
	// Returns the list of components
	component *get_components() { return components.first(); }
	// Returns the number of components in the list
	int get_ncomponents() { return components.size(); }
	// Adds a component to the list
	component *add_component();
	// Removes a component from the list
	void remove_component(component*);
	// Returns the component with id specified
	component *get_component_by_name(const char*);
};

extern mc_methods mc;

#endif
