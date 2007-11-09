/*
	*** Steepest descent minimiser
	*** src/methods/sd.cpp
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

#include "methods/sd.h"
#include "model/model.h"
#include "classes/pattern.h"
#include "classes/energystore.h"
#include "base/master.h"
#include "base/prefs.h"
#include "gui/gui.h"

sd_methods sd;

// Constructor
sd_methods::sd_methods()
{
	// Set initial values
	stepsize = 1.0;
	maxstep = 0.5;
	maxcycles = 500;
	maxlinetrials = 50;
}

// Minimise Energy w.r.t. coordinates by Steepest Descent
void sd_methods::minimise(model* srcmodel, double econ, double fcon)
{
	// Line Search (Steepest Descent) energy minimisation.
	dbg_begin(DM_CALLS,"sd_methods::minimise");
	int cycle, m, i;
	double enew1, enew2, ecurrent, step, bound[3], energy[3], newmin1, newmin2, a, b, b10, b12;
	model destmodel;
	bool linedone, converged;
	atom **modelatoms = srcmodel->get_staticatoms();

	/*
	// Prepare the calculation
	*/
	// First, create expression for the current model and assign charges
	if (!srcmodel->create_expression())
	{
	        dbg_end(DM_CALLS,"sd_methods::minimise");
	        return;
	}
	
	// Calculate initial reference energy and copy srcmodel coordinates so the size of destmodel is correct
	destmodel.copy(srcmodel);
	ecurrent = srcmodel->total_energy(srcmodel);
	srcmodel->energy.print();

	/* TEST
	srcmodel->calculate_forces(srcmodel);
	srcmodel->normalise_forces(1.0);
	for (cycle=0; cycle<100; cycle++)
	{
		gradient_move(srcmodel, &destmodel, cycle*0.001);
		printf("Move %f, energy = %f\n",cycle*0.001,srcmodel->total_energy(&destmodel));
	}
		srcmodel->copy_atom_data(&destmodel, AD_R);
	return; */

	// Reset stepsize and maxlinetrials (from sd class)
	step = stepsize;
	converged = FALSE;
	linedone = FALSE;

	msg(DM_NONE,"%10i  %15.5f \n",0,ecurrent);
	gui.progress_create("Minimising (SD)", maxcycles);
	srcmodel->zero_forces();
	srcmodel->calculate_forces(srcmodel);
	srcmodel->zero_forces_fixed();
	srcmodel->normalise_forces(1.0);

	for (cycle=0; cycle<maxcycles; cycle++)
	{
		// Calculate gradient vector for the current model coordinates


		// We need to define some sort of length scale so we take sensible steps along the gradient vector.
		// Since calculated forces are stored internally in J/mol, convert these in to kJ/mol and use as a basis.
		//for (i=0; i<srcmodel->get_natoms(); i++) modelatoms[i]->f /= 10000.0;

		//srcmodel->print_forces();
		// Perform linesearch along the gradient vector
		if (!gui.progress_update(cycle)) linedone = TRUE;
		else
	
		if (linedone) break;
		// Print out the step data
		if (prefs.update_energy(cycle+1)) msg(DM_NONE,"%10i  %15.5f   %15.5f\n",cycle+1,ecurrent,0.0);
		// Check for convergence
		if (fabs(2983293) < econ)
		{
			converged = TRUE;
			break;
		}
	}
	gui.progress_terminate();

	msg(DM_NONE,"\nFinal step:\n");
	msg(DM_NONE,"%10i  %15.5f   \n",cycle+1,ecurrent);
	if (converged) msg(DM_NONE,"Steepest descent converged in %i steps.\n",cycle+1);
	else msg(DM_NONE,"Steepest descent did not converge within %i steps.\n",maxcycles);
	// Copy config data to model and delete working configurations
	srcmodel->calculate_forces(srcmodel);
	srcmodel->update_measurements();
	srcmodel->log_change(LOG_COORDS);
	dbg_end(DM_CALLS,"sd_methods::minimise");
}

