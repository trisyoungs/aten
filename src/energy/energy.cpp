/*
	*** Base energy functions
	*** src/energy/energy.cpp
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

#include "model/model.h"
#include "classes/pattern.h"
#include "classes/energystore.h"
#include "classes/forcefield.h"
#include "classes/fourier.h"
#include "base/prefs.h"
#include "base/elements.h"

// Calculate total energy of model (from supplied coordinates)
double model::total_energy(model *srcmodel, pattern *molpattern, int molecule)
{
	dbg_begin(DM_CALLS,"model::total_energy");
	// Check the expression validity
	if (!expression_is_valid())
	{
		msg(DM_NONE,"model::total_energy - No valid energy expression defined for model.\n");
		dbg_end(DM_CALLS,"model::total_energy");
		return 0.0;
	}
	// Clear the energy store
	energy.clear();
	// Cycle through patterns, calculating the contributions from each
	pattern *p, *p2;
	p = patterns.first();
	// Calculate VDW correction
	if (prefs.calc_vdw() && (cell.get_type() != CT_NONE) && (molecule == -1)) p->vdw_correct_energy(&cell, &energy);
	// Prepare Ewald (if necessary)
	elec_method emodel = prefs.get_electrostatics();
	if (prefs.calc_elec())
	{
		if (emodel == EM_EWALDAUTO) prefs.ewald_estimate_parameters(&srcmodel->cell);
		// Create the fourier space for use in the Ewald sum
		if (emodel != EM_COULOMB) fourier.prepare(srcmodel,prefs.get_ewald_kvec());
	}
	while (p != NULL)
	{
		// Intramolecular Interactions
		if (prefs.calc_intra())
		{
			p->bond_energy(srcmodel, &energy, (p == molpattern ? molecule : -1));
			p->angle_energy(srcmodel, &energy, (p == molpattern ? molecule : -1));
			p->torsion_energy(srcmodel, &energy, (p == molpattern ? molecule : -1));
		}
		// Van der Waals Interactions
		if (prefs.calc_vdw())
		{
			p->vdw_intrapattern_energy(srcmodel,&energy, (p == molpattern ? molecule : -1));
			for (pattern *p2 = p; p2 != NULL; p2 = p2->next)
				p->vdw_interpattern_energy(srcmodel,p2,&energy, (p2 == molpattern ? molecule : -1));
		}
		// Electrostatic Interactions
		if (prefs.calc_elec())
		{
			switch (emodel)
			{
				case (EM_OFF):
					msg(DM_NONE,"Electrostatics requested but no method of calculation chosen!\n");
					break;
				case (EM_COULOMB):
					p->coulomb_intrapattern_energy(srcmodel,&energy);
					p2 = p;
					while (p2 != NULL)
					{
						p->coulomb_interpattern_energy(srcmodel,p2,&energy);
						p2 = p2->next;
					}
					break;
				default: // Ewald
					p->ewald_real_intrapattern_energy(srcmodel,&energy);
					p->ewald_correct_energy(srcmodel,&energy);
					p2 = p;
					while (p2 != NULL)
					{
						p->ewald_real_interpattern_energy(srcmodel,p2,&energy);
						p2 = p2->next;
					}
					// Calculate reciprocal space part (called once from first pattern only)
					if (p == patterns.first())
						p->ewald_reciprocal_energy(srcmodel,p,patterns.size(),&energy);
					break;
			}
		}
		p = p->next;
	}
	energy.totalise();
	dbg_end(DM_CALLS,"model::total_energy");
	return energy.get_total();
}

// Calculate forces from specified config
void model::calculate_forces(model *srcmodel)
{
	// Calculate the forces for the atoms of 'srcmodel' from the expression defined in the *this model
	dbg_begin(DM_CALLS,"model::calculate_forces");
	// Check the expression validity
	if (!expression_is_valid())
	{
		msg(DM_NONE,"calculate_forces : No valid energy expression defined for model.\n");
		dbg_end(DM_CALLS,"model::calculate_forces");
		return;
	}
	srcmodel->zero_forces();
	// Cycle through patterns, calculate the intrapattern forces for each
	pattern *p, *p2;
	p = patterns.first();
	// Prepare Ewald (if necessary)
	elec_method emodel = prefs.get_electrostatics();
	if (prefs.calc_elec())
	{
		if (emodel == EM_EWALDAUTO) prefs.ewald_estimate_parameters(&srcmodel->cell);
		// Create the fourier space for use in the Ewald sum
		fourier.prepare(srcmodel,prefs.get_ewald_kvec());
	}
	while (p != NULL)
	{
		// Bonded Interactions
		if (prefs.calc_intra())
		{
			p->bond_forces(srcmodel);
			p->angle_forces(srcmodel);
			p->torsion_forces(srcmodel);
		}
		// VDW
		if (prefs.calc_vdw())
		{
			p->vdw_intrapattern_forces(srcmodel);
			pattern *p2 = p;
			while (p2 != NULL)
			{
				p->vdw_interpattern_forces(srcmodel,p2);
				p2 = p2->next;
			}
		}
		// Electrostatics
		if (prefs.calc_elec())
		{
			switch (emodel)
			{
				case (EM_OFF):
					msg(DM_NONE,"Electrostatics requested but no method of calculation chosen!\n");
					break;
				case (EM_COULOMB):
					//p->coulomb_interpattern_forces(xxcfg);
					//p2 = p;
					//while (p2 != NULL)
				//	{
				//		p->coulomb_intrapattern_energy(xcfg,p2);
				//		p2 = p2->next;
				//	}
					break;
				default: // Ewald
					p->ewald_real_intrapattern_forces(srcmodel);
					p->ewald_correct_forces(srcmodel);
					p2 = p;
					while (p2 != NULL)
					{
						p->ewald_real_interpattern_forces(srcmodel,p2);
						p2 = p2->next;
					}
					// Calculate reciprocal space part (called once from first pattern only)
					if (p == patterns.first()) p->ewald_reciprocal_forces(srcmodel);
					break;
			}
		}
		p = p->next;
	}
	dbg_end(DM_CALLS,"model::calculate_forces");
}

