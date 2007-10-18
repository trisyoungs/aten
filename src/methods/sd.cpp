/*
	*** Steepest descent minimiser
	*** src/methods/sd.cpp

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
void sd_methods::normalise_forces()
{
	// 'Normalise' the forces in linecfg such that the largest force is equal to the maximum cartesian step size
	dbg_begin(DM_CALLS,"sd_methods::normalise_forces");
	double maxfrc;
	static vec3<double> f;
	atom **modelatoms = workmodel.get_staticatoms();
	int i;
	// Find the largest force
	maxfrc = 0.0;
	for (i=0; i<workmodel.get_natoms(); i++)
	{
		f = modelatoms[i]->f;
		if (fabs(f.x) > maxfrc) maxfrc = fabs(f.x);
		if (fabs(f.y) > maxfrc) maxfrc = fabs(f.y);
		if (fabs(f.z) > maxfrc) maxfrc = fabs(f.z);
	}
	// Normalise with respect to this force
	maxfrc = maxfrc / maxstep;
	for (i=0; i<workmodel.get_natoms(); i++) modelatoms[i]->f /= maxfrc;
	dbg_end(DM_CALLS,"sd_methods::normalise_forces");
}

void sd_methods::gradient_move(model *srcmodel)
{
	// Generate a new set of coordinates in workmodel following the normalised gradient vector present within it, with stepsize stepsize, and from the coordinates given in the source model provided.
	dbg_begin(DM_CALLS,"sd_methods::gradient_move");
	int i;
	atom **srcatoms = srcmodel->get_staticatoms();
	atom **destatoms = workmodel.get_staticatoms();
	for (i=0; i<srcmodel->get_natoms(); i++)
	{
		destatoms[i]->r.x = srcatoms[i]->r.x + destatoms[i]->f.x * stepsize;
		destatoms[i]->r.y = srcatoms[i]->r.y + destatoms[i]->f.y * stepsize;
		destatoms[i]->r.z = srcatoms[i]->r.z + destatoms[i]->f.z * stepsize;
	}
	dbg_end(DM_CALLS,"sd_methods::gradient_move");
}

/*
// Line Search Method Routines
*/

void sd_methods::minimise(model* srcmodel, double econ, double fcon)
{
	// Line Search (Steepest Descent) energy minimisation.
	dbg_begin(DM_CALLS,"linesearch::minimise");
	int cycle, m, i;
	double enew, ecurrent, edelta;
	bool linefailed, converged;

	/*
	// Prepare the calculation
	*/
	// First, create expression for the current model and assign charges
	if (!srcmodel->create_expression())
	{
	        dbg_end(DM_CALLS,"linesearch::minimise");
	        return;
	}
	srcmodel->assign_charges(prefs.get_chargesource());
	
	// Create a local working copy of the model
	workmodel.clear();
	workmodel.copy(srcmodel);
	// Calculate initial reference energy
	ecurrent = srcmodel->total_energy(srcmodel);
	// Reset stepsize
	stepsize = 1.0;
	converged = FALSE;
	
	msg(DM_NONE,"%10i  %15.5f \n",0,ecurrent);
	for (cycle=0; cycle<maxiterations; cycle++)
	{
		// Calculate gradient vector (forces) for the current model coordinates
		//linecfg->copy(cfg,CFG_R);
		srcmodel->calculate_forces(&workmodel);
		workmodel.zero_forces_fixed();
		normalise_forces();
		// Perform linesearch along the gradient vector until we decrease the energy
		linefailed = TRUE;
		for (m=0; m<maxlinetrials; m++)
		{
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
				stepsize *= 0.9;
			}
		}
		if (linefailed)
		{
			msg(DM_NONE,"linesearch::minimise - Failed to find lower point along gradient within 'maxlinetrials'.\n");
			break;
		}
		// Print out the step data
		if (prefs.update_energy(cycle+1)) msg(DM_NONE,"%10i  %15.5f   %15.5f\n",cycle+1,ecurrent,edelta);
		// Refresh the model and the main window
		
		//if ((cycle+1)%prefs.modelupdate == 0) xmodel->copy_workcfg_to_model(TRUE);
		gui.process_events();
		// Check for convergence
		if (fabs(edelta) < econ)
		{
			converged = TRUE;
			break;
		}
	}
	msg(DM_NONE,"\nFinal step:\n");
	msg(DM_NONE,"%10i  %15.5f   %15.5f\n",cycle+1,ecurrent,edelta);
	if (converged) msg(DM_NONE,"Steepest descent converged in %i steps.\n",cycle+1);
	else msg(DM_NONE,"Steepest descent did not converge within %i steps.\n",maxiterations);
	// Copy config data to model and delete working configurations
	srcmodel->copy_atom_data(&workmodel,AD_R);
	srcmodel->calculate_forces(srcmodel);
	srcmodel->render_from_self();
	srcmodel->log_change(LOG_COORDS);
	//xmodel->project_all();
	dbg_end(DM_CALLS,"linesearch::minimise");
}

