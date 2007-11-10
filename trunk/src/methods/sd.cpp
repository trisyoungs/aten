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

#include "base/elements.h"
#include "methods/sd.h"
#include "model/model.h"
#include "classes/pattern.h"
#include "classes/energystore.h"
#include "base/master.h"
#include "base/prefs.h"
#include "gui/gui.h"

// Constructor
sd_method::sd_method()
{
	ncycles = 100;
}

// Minimise Energy w.r.t. coordinates by Steepest Descent
void sd_method::minimise(model* srcmodel, double econ, double fcon)
{
	// Line Search (Steepest Descent) energy minimisation.
	dbg_begin(DM_CALLS,"sd_method::minimise");
	int cycle, m, i;
	double enew, ecurrent, edelta, rmscurrent, rmsnew, fdelta;
	atom **modelatoms;
	bool linedone, converged;

	/*
	// Prepare the calculation
	*/
	// First, create expression for the current model and assign charges
	if (!srcmodel->create_expression())
	{
	        dbg_end(DM_CALLS,"sd_method::minimise");
	        return;
	}
	
	// Calculate initial reference energy and RMS force
	modelatoms = srcmodel->get_staticatoms();
	ecurrent = srcmodel->total_energy(srcmodel);
	srcmodel->calculate_forces(srcmodel);
	rmscurrent = srcmodel->calculate_rms_force();
	srcmodel->energy.print();

	converged = FALSE;
	linedone = FALSE;

	msg(DM_NONE,"Step         Energy          DeltaE          RMS Force\n");
	msg(DM_NONE,"Init  %15.5e          ---      %15.5e\n",ecurrent,rmscurrent);
	gui.progress_create("Minimising (SD)", ncycles);

	for (cycle=0; cycle<ncycles; cycle++)
	{
		// Calculate current forces which will be our gradient vector
		srcmodel->zero_forces();
		srcmodel->calculate_forces(srcmodel);
		srcmodel->zero_forces_fixed();
		// We need to (do we?) define some sort of length scale so we take sensible steps along the gradient vector.
		//srcmodel->normalise_forces(1.0);
		for (i=0; i<srcmodel->get_natoms(); i++) modelatoms[i]->f /= elements.mass(modelatoms[i]);

		// Perform linesearch along the gradient vector
		if (!gui.progress_update(cycle)) linedone = TRUE;
		else
		{
			enew = line_minimise(srcmodel);
			edelta = enew - ecurrent;
			rmsnew = srcmodel->calculate_rms_force();
			fdelta = rmsnew - rmscurrent;
			// Check convergence criteria
			if ((fabs(edelta) < econ) && (fabs(fdelta) < fcon)) converged = TRUE;
			ecurrent = enew;
			rmscurrent = rmsnew;
		}

		// Print out the step data
		if (prefs.update_energy(cycle+1)) msg(DM_NONE,"%-5i %15.5e  %15.5e  %15.5e\n",cycle+1,ecurrent,edelta,rmscurrent);

		if (linedone || converged) break;
	}
	gui.progress_terminate();

	if (converged) msg(DM_NONE,"Steepest descent converged in %i steps.\n",cycle+1);
	else msg(DM_NONE,"Steepest descent did not converge within %i steps.\n",ncycles);
	msg(DM_NONE,"Final energy:\n");
	ecurrent = srcmodel->total_energy(srcmodel);
	srcmodel->energy.print();
	// Calculate fresh new forces for the model, log changes / update, and exit.
	srcmodel->calculate_forces(srcmodel);
	srcmodel->update_measurements();
	srcmodel->log_change(LOG_COORDS);
	dbg_end(DM_CALLS,"sd_method::minimise");
}

