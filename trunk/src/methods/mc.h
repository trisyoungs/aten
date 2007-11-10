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
#include "templates/list.h"
#include "classes/region.h"
#include "classes/dnchar.h"

// Monte Carlo move types
enum mc_move { MT_TRANSLATE, MT_ROTATE, MT_ZMATRIX, MT_INSERT, MT_DELETE, MT_NITEMS };
const char *text_from_MT(mc_move);
mc_move MT_from_text(const char*);

// Forward declarations
class model;
class unitcell;
class pattern;

// Disorder Component
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
	// Name of the component
	dnchar name;

	public:
	// Type of region the component is limited to
	region area;
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

// Monte Carlo
class mc_method
{
	/*
	// Main Routines
	*/
	public:
	// Constructor
	mc_method();
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
	public:
	// List of component models to use in MC insertion
	list<component> components;
	// Return the component with name specified
	component *get_component_by_name(const char*);
};

#endif
