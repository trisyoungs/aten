/*
	*** Coulomb energy / force calculation
	*** src/energy/coulomb.cpp
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

#include <math.h>
#include "classes/pattern.h"
#include "classes/energystore.h"
#include "classes/forcefield.h"
#include "model/model.h"
#include "templates/vector3.h"
#include "base/master.h"
#include "base/prefs.h"

// Calculate the internal coulomb energy of the pattern.
// Consider only the intrapattern interactions of individual molecules  within this pattern.
void pattern::coulomb_intrapattern_energy(model *srcmodel, energystore *estore)
{
	dbg_begin(DM_CALLS,"pattern::coulomb_intrapattern_energy");
	static int n,i,j,aoff,m1;
	static vec3<double> mim_i;
	static double rij, energy_inter, energy_intra, energy, cutoff;
	cutoff = prefs.get_elec_cutoff();
	patbound *pb;
	atom **modelatoms = srcmodel->get_atomarray();
	unitcell *cell = srcmodel->get_cell();
	energy_inter = 0.0;
	energy_intra = 0.0;
	aoff = startatom;
	for (m1=0; m1<nmols; m1++)
	{
		// Add on contributions for connectivities of 0 (unbound) or > 3
		for (i=0; i<natoms; i++)
		{
			for (j=i+1; j<natoms; j++)
			{
				if ((conmat[i][j] > 3) || (conmat[i][j] == 0))
				{
					mim_i = cell->mimd(modelatoms[i+aoff]->r(),modelatoms[j+aoff]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					energy  = (modelatoms[i+aoff]->get_charge() * modelatoms[j+aoff]->get_charge()) / (rij * rij);
					conmat[i][j] == 0 ? energy_inter += energy : energy_intra += energy;
				}
			}
		}
		// Add on contributions from torsions (which are scaled)
		for (pb = torsions.first(); pb != NULL; pb = pb->next)
		{
			i = pb->get_atomid(0) + aoff;
			j = pb->get_atomid(3) + aoff;
			mim_i = cell->mimd(modelatoms[i]->r(),modelatoms[j]->r());
			rij = mim_i.magnitude();
			if (rij > cutoff) continue;
			energy  = (modelatoms[i]->get_charge() * modelatoms[j]->get_charge()) / (rij * rij);
			energy *= pb->get_data()->get_params().data[TF_ESCALE];
			energy_intra += energy;
		}
		aoff += natoms;
	}
	energy_intra = energy_intra * prefs.elec_convert;
	energy_inter = energy_inter * prefs.elec_convert;
	estore->add(ET_COULOMBINTRA,energy_intra,id);
	estore->add(ET_COULOMBINTER,energy_inter,id,id);
	dbg_end(DM_CALLS,"pattern::coulomb_intrapattern_energy");
}

// Calculate the coulomb contribution to the energy from interactions between different molecules of this pattern and the one supplied
void pattern::coulomb_interpattern_energy(model *srcmodel, pattern *xpnode, energystore *estore)
{
	dbg_begin(DM_CALLS,"pattern::coulomb_interpattern_energy");
	static int n1,n2,i,j,aoff1,aoff2,m1,m2,start,finish,a1,a2;
	static vec3<double> mim_i;
	static double rij, energy_inter, energy, cutoff;
	cutoff = prefs.get_elec_cutoff();
	atom **modelatoms = srcmodel->get_atomarray();
	unitcell *cell = srcmodel->get_cell();
	energy_inter = 0.0;
	aoff1 = startatom;
	 // When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
        this == xpnode ? finish = nmols - 1 : finish = nmols;
	for (m1=0; m1<finish; m1++)
	{
		this == xpnode ? start = m1 + 1 : start = 0;
		aoff2 = xpnode->startatom + start*natoms;
		for (m2=start; m2<xpnode->nmols; m2++)
		{
			for (a1=0; a1<natoms; a1++)
			{
				i = a1 + aoff1;
				for (a2=0; a2<xpnode->natoms; a2++)
		  		{
					j = a2 + aoff2;
					mim_i = cell->mimd(modelatoms[i]->r(),modelatoms[j]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
	//printf("Coulomb ij %i %i %8.4f %8.4f %8.4f \n",i,j,xcfg->q[i],xcfg->q[j],rij);
					energy  = (modelatoms[i]->get_charge() * modelatoms[j]->get_charge()) / (rij * rij);
					energy_inter += energy;
				}
			}
			aoff2 += xpnode->natoms;
		}
		aoff1 += natoms;
	}
	energy_inter = energy_inter * prefs.elec_convert;
	estore->add(ET_COULOMBINTER,energy_inter,id,xpnode->id);
	dbg_end(DM_CALLS,"pattern::coulomb_interpattern_energy");
}

// Calculate the internal coulomb forces in the pattern.
// Consider only the intrapattern interactions of individual molecules  within this pattern.
void pattern::coulomb_intrapattern_forces(model *srcmodel)
{
	dbg_begin(DM_CALLS,"pattern::coulomb_intrapattern_forces");
	static int n, i, j, aoff, m1;
	static vec3<double> mim_i, f_i, tempf;
	static double rij, factor, cutoff;
	cutoff = prefs.get_elec_cutoff();
	patbound *pb;
	atom **modelatoms = srcmodel->get_atomarray();
	unitcell *cell = srcmodel->get_cell();
	aoff = startatom;
	for (m1=0; m1<nmols; m1++)
	{
		// Add on contributions for connectivities of 0 (unbound) or > 3
		for (i=0; i<natoms; i++)
		{
			// Copy forces for atom i
			f_i = modelatoms[i+aoff]->f();
			for (j=i+1; j<natoms; j++)
			{
				if ((conmat[i][j] > 3) || (conmat[i][j] == 0))
				{
					mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					factor = (modelatoms[i]->get_charge() * modelatoms[j]->get_charge()) / (rij*rij*rij);
					tempf = mim_i * factor;
					f_i += tempf;
					modelatoms[j+aoff]->f() -= tempf;
				}
			}
			// Re-load forces back into main array
			modelatoms[i+aoff]->f() = f_i;
		}
		// Add on contributions from torsions (which are scaled)
		for (pb = torsions.first(); pb != NULL; pb = pb->next)
		{
			i = pb->get_atomid(0) + aoff;
			j = pb->get_atomid(3) + aoff;
			mim_i = cell->mimd(modelatoms[i]->r(), modelatoms[j]->r());
			rij = mim_i.magnitude();
			if (rij > cutoff) continue;
			factor = (modelatoms[i]->get_charge() * modelatoms[j]->get_charge()) / (rij*rij*rij);
			factor *= pb->get_data()->get_params().data[TF_ESCALE];
			modelatoms[i+aoff]->f() += tempf;
			modelatoms[j+aoff]->f() -= tempf;
		}
		aoff += natoms;
	}
	dbg_end(DM_CALLS,"pattern::coulomb_intrapattern_forces");
}

// Calculate the coulomb forces from interactions between different molecules of this pattern and the one supplied
void pattern::coulomb_interpattern_forces(model *srcmodel, pattern *xpnode)
{
	dbg_begin(DM_CALLS,"pattern::coulomb_interpattern_forces");
	static int n1,n2,i,j,aoff1,aoff2,m1,m2,start,finish,a1,a2;
	static vec3<double> mim_i, f_i, tempf;
	static double rij, factor, cutoff;
	atom **modelatoms = srcmodel->get_atomarray();
	unitcell *cell = srcmodel->get_cell();
	cutoff = prefs.get_elec_cutoff();
	aoff1 = startatom;
	 // When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
        this == xpnode ? finish = nmols - 1 : finish = nmols;
	for (m1=0; m1<finish; m1++)
	{
		this == xpnode ? start = m1 + 1 : start = 0;
	       	aoff2 = xpnode->startatom + start*natoms;
		for (m2=start; m2<xpnode->nmols; m2++)
		{
			for (a1=0; a1<natoms; a1++)
			{
				i = a1 + aoff1;
				// Copy forces to temporary vector
				f_i = modelatoms[i]->f();
				for (a2=0; a2<xpnode->natoms; a2++)
		  		{
					j = a2 + aoff2;
					mim_i = cell->mimd(modelatoms[i]->r(), modelatoms[j]->r());
					rij = mim_i.magnitude();
					if (rij < cutoff)
					{
	//printf("Coulomb ij %i %i %8.4f %8.4f %8.4f \n",i,j,xcfg->q[i],xcfg->q[j],rij);
						factor = (modelatoms[i]->get_charge() * modelatoms[j]->get_charge()) / (rij*rij*rij);
						tempf = mim_i * factor;
						f_i += tempf;
						modelatoms[j]->f() -= tempf;
					}
				}
			}
			// Replace forces in main array
			modelatoms[i]->f() = f_i;
			aoff2 += xpnode->natoms;
		}
		aoff1 += natoms;
	}
	dbg_end(DM_CALLS,"pattern::coulomb_interpattern_forces");
}

