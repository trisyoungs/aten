/*
	*** Conjugate gradient minimiser
	*** src/methods/cg.cpp
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
cg_method::cg_method()
{
	ncycles = 100;
}

// Minimise Energy w.r.t. coordinates by Conjugate Gradient
void cg_method::minimise(model* srcmodel, double econ, double fcon)
{
	// Line Search (Steepest Descent) energy minimisation.
	dbg_begin(DM_CALLS,"cg_method::minimise");
	int cycle, m, i;
	double enew, ecurrent, edelta, rmscurrent, rmsnew, fdelta, g_old_sq, gamma, g_current_sq;
	double *g_old;
	vec3<double> f;
	atom **modelatoms;
	bool linedone, converged;

	/*
	// Prepare the calculation
	*/
	// First, create expression for the current model and assign charges
	if (!srcmodel->create_expression())
	{
	        dbg_end(DM_CALLS,"cg_method::minimise");
	        return;
	}
	
	// Calculate initial reference energy and RMS force
	modelatoms = srcmodel->get_staticatoms();
	g_old = new double[srcmodel->get_natoms()*3];
	ecurrent = srcmodel->total_energy(srcmodel);
	srcmodel->calculate_forces(srcmodel);
	rmscurrent = srcmodel->calculate_rms_force();
	srcmodel->energy.print();

	converged = FALSE;
	linedone = FALSE;

	msg(DM_NONE,"Step         Energy          DeltaE          RMS Force\n");
	msg(DM_NONE,"Init  %15.5e          ---      %15.5e\n",ecurrent,rmscurrent);
	gui.progress_create("Minimising (CG)", ncycles);

	for (cycle=0; cycle<ncycles; cycle++)
	{
		// We need to (do we?) define some sort of length scale so we take sensible steps along the gradient vector.
		//srcmodel->normalise_forces(1.0);
		//for (i=0; i<srcmodel->get_natoms(); i++) modelatoms[i]->f /= elements.mass(modelatoms[i]);

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

		// Store old forces and calculate new forces at the new line-minimised position
		for (i=0; i<srcmodel->get_natoms()*3; i += 3)
		{
			f = modelatoms[i/3]->f();
			g_old[i] = -f.x;
			g_old[i+1] = -f.y;
			g_old[i+2] = -f.z;
		}
		srcmodel->calculate_forces(srcmodel);

		// Calculate new conjugate gradient vector, if this isn't the first cycle
		if (cycle != 0)
		{
			/* The next gradient vector is given by (Polak-Ribiere):
			g[new] = g[current] + gamma * g[old]     where  gamma = (g[current] - g[old]).g[current]  /  g[old].g[old]
			**or** (Fletcher-Reeves)
			g[new] = g[current] + gamma * g[old]     where  gamma = g[current].g[current]  /  g[old].g[old]
			*/
			// Calculate 'g[current].g[current]' and '(g[current] - g[old]).g[current]'
			g_old_sq = 0.0;
			g_current_sq = 0.0;
			for (i=0; i<srcmodel->get_natoms()*3; i++) g_old_sq += g_old[i] * g_old[i];
			for (i=0; i<srcmodel->get_natoms(); i++)
			{
				f = modelatoms[i]->f();
				g_current_sq += f.x * f.x;
				g_current_sq += f.y * f.y;
				g_current_sq += f.z * f.z;
			}
			// Calculate gamma
			gamma = g_current_sq / g_old_sq;
			// Calculate new gradient vector
			for (int i=0; i<srcmodel->get_natoms()*3; i += 3)
			{
				f = modelatoms[i/3]->f();
				f.x = g_old[i] + gamma * f.x;
				f.y = g_old[i+1] + gamma * f.y;
				f.z = g_old[i+2] + gamma * f.z;
				modelatoms[i/3]->f() = f;
			}
		}
	}
	gui.progress_terminate();

	if (converged) msg(DM_NONE,"Conjugate gradient converged in %i steps.\n",cycle+1);
	else msg(DM_NONE,"Conjugate gradient did not converge within %i steps.\n",ncycles);
	msg(DM_NONE,"Final energy:\n");
	ecurrent = srcmodel->total_energy(srcmodel);
	srcmodel->energy.print();
	// Calculate fresh new forces for the model, log changes / update, and exit.
	srcmodel->calculate_forces(srcmodel);
	srcmodel->update_measurements();
	srcmodel->log_change(LOG_COORDS);
	dbg_end(DM_CALLS,"cg_method::minimise");
}

