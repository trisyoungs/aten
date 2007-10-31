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
	maxiterations = 500;
	maxlinetrials = 50;
}

/*
// Line Search Subroutines
*/

void sd_methods::gradient_move(model *srcmodel)
{
	// Generate a new set of coordinates in workmodel following the normalised gradient vector present in srcmodel, with the stepsize given
	dbg_begin(DM_CALLS,"sd_methods::gradient_move");
	int i;
	atom **srcatoms = srcmodel->get_staticatoms();
	atom **destatoms = workmodel.get_staticatoms();
	for (i=0; i<srcmodel->get_natoms(); i++)
	{
		destatoms[i]->r.x = srcatoms[i]->r.x + srcatoms[i]->f.x * stepsize;
		destatoms[i]->r.y = srcatoms[i]->r.y + srcatoms[i]->f.y * stepsize;
		destatoms[i]->r.z = srcatoms[i]->r.z + srcatoms[i]->f.z * stepsize;
	}
	dbg_end(DM_CALLS,"sd_methods::gradient_move");
}

/*
// Line Search Method Routines
*/

void sd_methods::minimise(model* srcmodel, double econ, double fcon)
{
	// Line Search (Steepest Descent) energy minimisation.
	dbg_begin(DM_CALLS,"sd_methods::minimise");
	int cycle, m, i;
	double enew, ecurrent, edelta;
	bool linefailed, converged;

	/*
	// Prepare the calculation
	*/
	// First, create expression for the current model and assign charges
	if (!srcmodel->create_expression())
	{
	        dbg_end(DM_CALLS,"sd_methods::minimise");
	        return;
	}
	
	// Create a local working copy of the model
	workmodel.clear();
	workmodel.copy(srcmodel);
	// Calculate initial reference energy
	ecurrent = srcmodel->total_energy(srcmodel);
	srcmodel->energy.print();

	// Reset stepsize
	stepsize = 0.5;
	converged = FALSE;
	maxlinetrials = 1000;
	msg(DM_NONE,"%10i  %15.5f \n",0,ecurrent);
	gui.progress_create("Minimising (SD)", maxiterations*maxlinetrials);
	for (cycle=0; cycle<maxiterations; cycle++)
	{
		// Calculate gradient vector for the current model coordinates
		srcmodel->zero_forces();
		srcmodel->calculate_forces(srcmodel);
		srcmodel->zero_forces_fixed();
		srcmodel->normalise_forces();
		// Perform linesearch along the gradient vector until we decrease the energy
		linefailed = TRUE;
		stepsize = 0.5;
		for (m=0; m<maxlinetrials; m++)
		{
			if (!gui.progress_update(cycle*maxlinetrials + m))
			{
				linefailed = TRUE;
				break;
			}
			gradient_move(srcmodel);
			enew = srcmodel->total_energy(&workmodel);
			edelta = enew - ecurrent;
			if (edelta < 0.0)
			{
				// New point found, so copy workmodel coordinates to model, set new energy, and exit
				ecurrent = enew;
				srcmodel->copy_atom_data(&workmodel, AD_R);
				linefailed = FALSE;
				break;
			}
			else
			{
				// Energy increased, so change step size and repeat
				stepsize *= 0.1;
			}
		}
		if (linefailed)
		{
			msg(DM_NONE,"sd_methods::minimise - Failed to find lower point along gradient within 'maxlinetrials'.\n");
			break;
		}
		// Print out the step data
		if (prefs.update_energy(cycle+1)) msg(DM_NONE,"%10i  %15.5f   %15.5f\n",cycle+1,ecurrent,edelta);
		// Check for convergence
		if (fabs(edelta) < econ)
		{
			converged = TRUE;
			break;
		}
	}
	gui.progress_terminate();

	msg(DM_NONE,"\nFinal step:\n");
	msg(DM_NONE,"%10i  %15.5f   %15.5f\n",cycle+1,ecurrent,edelta);
	if (converged) msg(DM_NONE,"Steepest descent converged in %i steps.\n",cycle+1);
	else msg(DM_NONE,"Steepest descent did not converge within %i steps.\n",maxiterations);
	// Copy config data to model and delete working configurations
	srcmodel->calculate_forces(srcmodel);
	srcmodel->log_change(LOG_COORDS);
	dbg_end(DM_CALLS,"sd_methods::minimise");
}

