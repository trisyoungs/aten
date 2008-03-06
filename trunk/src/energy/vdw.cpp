/*
	*** van der Waals energy / force calculation
	*** src/energy/vdw.cpp
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

#include "classes/pattern.h"
#include "classes/forcefield.h"
#include "model/model.h"
#include "templates/vector3.h"
#include "classes/energystore.h"
#include "base/prefs.h"
#include <math.h>

// Intrapattern VDW energy
void pattern::vdw_intrapattern_energy(model *srcmodel, energystore *estore, int lonemolecule)
{
	// Calculate the internal VDW contributions with coordinates from *xcfg
	// Consider only the intrapattern interactions between atoms in individual molecules within the pattern.
	dbg_begin(DM_CALLS,"pattern::vdw_intrapattern_energy");
	static int n,aoff,m1,i,j;
	static vec3<double> mim_i;
	static double sigma, sigmar6, epsilon, rij, energy_inter, energy_intra, cutoff, vrs;
	static ffparams paramsi, paramsj;
	patatom *pai, *paj;
	patbound *pb;
	cutoff = prefs.get_vdw_cutoff();
	vrs = prefs.get_vdw_radius_scale();
	atom **modelatoms = srcmodel->get_atomarray();
	unitcell *cell = srcmodel->get_cell();
	energy_inter = 0.0;
	energy_intra = 0.0;
	aoff = startatom;
	for (m1=0; m1<nmols; m1++)
	{
		// Calculate energies of atom pairs that are unbound or separated by more than three bonds
		i = -1;
		for (pai = atoms.first(); pai != atoms.last(); pai = pai->next)
		{
			i++;
			paramsi = atoms[i]->get_data()->get_params();
			j = i;
			for (paj = pai->next; paj != NULL; paj = paj->next)
			{
				j++;
				if ((conmat[i][j] > 3) || (conmat[i][j] == 0))
				{
					mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					paramsj = atoms[j]->get_data()->get_params();
					// TODO Check for conflicting VDW types
					switch (atoms[j]->get_data()->get_funcform())
					{
						case (VF_UNSPECIFIED):
							printf("pattern::vdw_intrapattern_energy <<<< VDW function is UNSPECIFIED >>>>\n");
							break;
						case (VF_LJ): // U = 4 * eps * [ (s/r)**12 - (s/r)**6 ]
							epsilon = 4.0 * sqrt( paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS] );
							sigma = ( paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA] ) * 0.5 * vrs;
							sigmar6 = pow((sigma / rij),6);
							conmat[i][j] == 0 ? energy_inter += epsilon * (sigmar6*sigmar6 - sigmar6)
								: energy_intra += epsilon * (sigmar6*sigmar6 - sigmar6);
							break;
					}
				}
			}
		}
		// Add scaled contributions from torsions
		for (pb = torsions.first(); pb != NULL; pb = pb->next)
		{
			i = pb->get_atomid(0);
			j = pb->get_atomid(3);
			mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
			rij = mim_i.magnitude();
			if (rij > cutoff) continue;
			paramsi = atoms[i]->get_data()->get_params();
			paramsj = atoms[j]->get_data()->get_params();
			// TODO Check for conflicting VDW types
			switch (atoms[j]->get_data()->get_funcform())
			{
				case (VF_UNSPECIFIED):
					printf("pattern::vdw_intrapattern_energy <<<< VDW function is UNSPECIFIED >>>>\n");
					break;
				case (VF_LJ): // U = 4 * eps * [ (s/r)**12 - (s/r)**6 ]
					epsilon = 4.0 * sqrt( paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS] );
					sigma = ( paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA] ) * 0.5 * vrs;
					epsilon *= pb->get_data()->get_params().data[TF_VSCALE];
					sigmar6 = pow((sigma / rij),6);
					energy_intra += epsilon * (sigmar6*sigmar6 - sigmar6);
					break;
			}
		}
		aoff += natoms;
	}
	// Add totals into energystore
	estore->add(ET_VDWINTRA,energy_intra,id);
	estore->add(ET_VDWINTER,energy_inter,id,id);
	dbg_end(DM_CALLS,"pattern::vdw_intrapattern_energy");
}

// Interpattern VDW energy
void pattern::vdw_interpattern_energy(model *srcmodel, pattern *xpnode, energystore *estore, int molecule)
{
	// Calculate the VDW contribution to the energy from interactions between molecules of this pattern and the one supplied
	dbg_begin(DM_CALLS,"pattern::vdw_interpattern_energy");
	static int n1,n2,i,j,aoff1,aoff2,m1,m2,finish1,start2,finish2;
	static vec3<double> mim_i;
	static double sigma, sigmar6, epsilon, rij, energy_inter, cutoff, vrs;
	patatom *pai, *paj;
	static ffparams paramsi, paramsj;
	cutoff = prefs.get_vdw_cutoff();
	vrs = prefs.get_vdw_radius_scale();
	atom **modelatoms = srcmodel->get_atomarray();
	unitcell *cell = srcmodel->get_cell();
	energy_inter = 0.0;
	aoff1 = startatom;
	 // When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
	if ((this == xpnode) && (molecule == -1)) finish1 = nmols - 1;
	else finish1 = nmols;
	for (m1=0; m1<finish1; m1++)
	{
		if (molecule == -1)
		{
			start2 = (this == xpnode ? m1 + 1 : 0);
			finish2 = xpnode->nmols;
		}
		else
		{
			start2 = molecule;
			finish2 = molecule + 1;
			// If the patterns are the same we must exclude molecule == m1
			if ((this == xpnode) && (molecule == m1)) { aoff1 += natoms; continue; }
		}

		//if (m1 == 0) printf("IPE - finish1 = %i, start2 = %i, finish2 = %i\n",finish1,start2,finish2);
		aoff2 = xpnode->startatom + start2*xpnode->natoms;

		for (m2=start2; m2<finish2; m2++)
		{
			//printf("      m1/m2=%i/%i  aoff1/aoff2=%i/%i \n",m1,m2,aoff1,aoff2);
			i = -1;
			for (pai = atoms.first(); pai != NULL; pai = pai->next)
			{
				i++;
				paramsi = pai->get_data()->get_params();
				j = -1;
				for (paj = xpnode->atoms.first(); paj != NULL; paj = paj->next)
				{
					j++;

					mim_i = cell->mimd(modelatoms[i+aoff1]->r(), modelatoms[j+aoff2]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					paramsj = paj->get_data()->get_params();
					// TODO Check for conflicting VDW types
					switch (atoms[i]->get_data()->get_funcform())
					{
						case (VF_UNSPECIFIED):
							printf("pattern::vdw_interpattern_energy <<<< VDW function is UNSPECIFIED >>>>\n");
							break;
						case (VF_LJ): 
							epsilon = 4.0*sqrt(paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS]);
							sigma = 0.5 * (paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA]) * vrs;
							sigmar6 = pow((sigma / rij),6);
							energy_inter += epsilon * (sigmar6*sigmar6 - sigmar6);
							break;
					}
				}
			}
			aoff2 += xpnode->natoms;
		}
		aoff1 += natoms;
	}
	estore->add(ET_VDWINTER,energy_inter,id,xpnode->id);
	dbg_end(DM_CALLS,"pattern::vdw_interpattern_energy");
}

// Intrapattern VDW forces
void pattern::vdw_intrapattern_forces(model *srcmodel)
{
	// Calculate the internal VDW contributions with coordinates from *xcfg
	// Consider only the intrapattern interactions between atoms in individual molecules within the pattern.
	// 'aoff' stores the atom number offset (molecule offset) but is *only* used for lookups in the coordinate
	// arrays since assuming the pattern definition is correct then the sigmas/epsilons in molecule 0 represent
	// those of all molecules.
	dbg_begin(DM_CALLS,"pattern::vdw_intrapattern_forces");
	static int n,i,j,aoff,m1;
	static vec3<double> mim_i, f_i, tempf;
	static double sigma, sigmar6, epsilon, rij, factor, cutoff, vrs;
	static ffparams paramsi, paramsj;
	patatom *pai, *paj;
	patbound *pb;
	cutoff = prefs.get_vdw_cutoff();
	vrs = prefs.get_vdw_radius_scale();
	atom **modelatoms = srcmodel->get_atomarray();
	unitcell *cell = srcmodel->get_cell();
	aoff = startatom;
	for (m1=0; m1<nmols; m1++)
	{
		// Add contributions from atom pairs that are unbound or separated by more than three bonds
		i = -1;
		for (pai = atoms.first(); pai != atoms.last(); pai = pai->next)
		{
			i++;
			paramsi = pai->get_data()->get_params();
			// Store temporary forces to avoid unnecessary array lookups
			f_i = modelatoms[i+aoff]->f();
			j = i;
			for (paj = pai->next; paj != NULL; paj = paj->next)
			{
				j++;
				if ((conmat[i][j] > 3) || (conmat[i][j] == 0))
				{
					mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					paramsj = paj->get_data()->get_params();
					// TODO Check for conflicting VDW types
					switch (atoms[j]->get_data()->get_funcform())
					{
						case (VF_UNSPECIFIED):
							printf("pattern::vdw_intrapattern_forces <<<< VDW function is UNSPECIFIED >>>>\n");
							break;
						case (VF_LJ): 
							epsilon = 48.0 * sqrt( paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS] );
							sigma = ( paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA] ) * 0.5 * vrs;
							sigmar6 = pow((sigma / rij),6);
							factor = epsilon * sigmar6 * (sigmar6 - 0.5);
							factor = factor / (rij*rij);
							break;
					}
					// Add the forces (mim_i contains dx, dy, dz between i and j)
					tempf = mim_i * factor;
					f_i += tempf;
					modelatoms[j+aoff]->f() -= tempf;
				}
			}
			// Put the temporary forces back into the main array
			modelatoms[i+aoff]->f() = f_i;
		}
		// Add scaled contributions from torsions
		for (pb = torsions.first(); pb != NULL; pb = pb->next)
		{
			i = pb->get_atomid(0);
			j = pb->get_atomid(3);
			mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
			rij = mim_i.magnitude();
			if (rij > cutoff) continue;
			paramsi = atoms[i]->get_data()->get_params();
			paramsj = atoms[j]->get_data()->get_params();
			// TODO Check for conflicting VDW types
			switch (atoms[j]->get_data()->get_funcform())
			{
				case (VF_UNSPECIFIED):
					printf("pattern::vdw_intrapattern_energy <<<< VDW function is UNSPECIFIED >>>>\n");
					break;
				case (VF_LJ): // U = 4 * eps * [ (s/r)**12 - (s/r)**6 ]
					epsilon = 48.0 * sqrt( paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS] );
					epsilon *= pb->get_data()->get_params().data[TF_VSCALE];
					sigma = ( paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA] ) * 0.5 * vrs;
					sigmar6 = pow((sigma / rij),6);
					factor = epsilon * sigmar6 * (sigmar6 - 0.5);
					factor = factor / (rij*rij);
					break;
			}
			tempf = mim_i * factor;
			modelatoms[i+aoff]->f() += tempf;
			modelatoms[j+aoff]->f() -= tempf;
		}
		aoff += natoms;
	}
	dbg_end(DM_CALLS,"pattern::vdw_intrapattern_forces");
}

// Interpattern VDW forces
void pattern::vdw_interpattern_forces(model *srcmodel, pattern *xpnode)
{
	// Calculate the VDW forces from interactions between different molecules
	// of this pnode and the one supplied
	dbg_begin(DM_CALLS,"pattern::vdw_interpattern_forces");
	static int n1,n2,i,j,aoff1,aoff2,m1,m2,start,finish;
	static vec3<double> mim_i, f_i, tempf;
	static double sigma, sigmar6, epsilon, rij, factor, cutoff, vrs;
	patatom *pai, *paj;
	static ffparams paramsi, paramsj;
	cutoff = prefs.get_vdw_cutoff();
	vrs = prefs.get_vdw_radius_scale();
	atom **modelatoms = srcmodel->get_atomarray();
	unitcell *cell = srcmodel->get_cell();
	aoff1 = startatom;
	// TODO Move loops so that we can load temporary forces for i then calculate all other forces on it in one go.
	 // When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
        this == xpnode ? finish = nmols - 1 : finish = nmols;
	for (m1=0; m1<finish; m1++)
	{
		this == xpnode ? start = m1 + 1 : start = 0;
		aoff2 = xpnode->startatom + start*natoms;
		for (m2=start; m2<xpnode->nmols; m2++)
		{
			i = -1;
			for (pai = atoms.first(); pai != NULL; pai = pai->next)
			{
				i++;
				paramsi = pai->get_data()->get_params();
				f_i = modelatoms[i+aoff1]->f();
				j = -1;
				for (paj = xpnode->atoms.first(); paj != NULL; paj = paj->next)
				{
					j++;
					mim_i = cell->mimd(modelatoms[i+aoff1]->r(), modelatoms[j+aoff2]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					paramsj = paj->get_data()->get_params();
					// TODO Check for conflicting VDW types
					switch (atoms[j]->get_data()->get_funcform())
					{
						case (VF_UNSPECIFIED):
							printf("pattern::vdw_interpattern_forces <<<< VDW function is UNSPECIFIED >>>>\n");
							break;
						case (VF_LJ): 
							epsilon = 48.0*sqrt(paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS]);
							sigma = 0.5 * (paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA]) * vrs;
							sigmar6 = pow((sigma / rij),6);
							factor = epsilon * sigmar6 * (sigmar6 - 0.5);
							factor = factor / (rij*rij);
							break;
					}
					// Add the forces (mim_i contains dx, dy, dz between i and j)
					tempf = mim_i * factor;
					f_i += tempf;
					modelatoms[j+aoff2]->f() -= tempf;
				}
				// Store temporary force array back into main force array
				modelatoms[i+aoff1]->f() = f_i;
			}
			aoff2 += xpnode->natoms;
		}
		aoff1 += natoms;
	}
	dbg_end(DM_CALLS,"pattern::vdw_interpattern_forces");
}

//
// VDW Long Range Correction to Energy
// Frenkel and Smit, Academic Press, 1996, p32	    // Eq in this edition is wrong
//
//               /inf
// U(lr) = 0.5 * |     dr 4 * pi * r**2 * rho(r) * u(r)
//               /rcut
//
// Assume p(r) is equal to the (bulk) number density at r > rcut.
//
void pattern::vdw_correct_energy(unitcell *cell, energystore *estore)
{
	// Calculate the long-range correction to the VDW energy
	dbg_begin(DM_CALLS,"pattern::vdw_correct_energy");
	static int i, j;
	static pattern *p1, *p2;
	static double energy, rho, cutoff, dudr, sigma, epsilon, sigmar3, sigmar9, volume, vrs;
	patatom *pai, *paj;
	static ffparams paramsi, paramsj;
	cutoff = prefs.get_vdw_cutoff();
	vrs = prefs.get_vdw_radius_scale();
	// The way the patterns are stored does not give direct access to the number of different
	// atom types used *or* the number densities of each. So, assume each atom in the pattern 
	// molecule is a unique VDW type and that the number density is nmols/cellvolume
	volume = cell->get_volume();
	energy = 0.0;
	for (p1 = this; p1 != NULL; p1 = p1->next)
	{
		for (p2 = this; p2 != NULL; p2 = p2->next)
		{
			rho = (p1->nmols * p2->nmols) /volume;
			i = 0;
			for (pai = p1->atoms.first(); pai != NULL; pai = pai->next)
			{
				paramsi = pai->get_data()->get_params();
				j = 0;
				for (paj = p2->atoms.first(); paj != NULL; paj = paj->next)
				{
					paramsj = paj->get_data()->get_params();
					// TODO Check for conflicting VDW types
					switch (p2->atoms[j]->get_data()->get_funcform())
					{
						case (VF_UNSPECIFIED):
							printf("pattern::vdw_correct_energy <<<< VDW function is UNSPECIFIED >>>>\n");
							break;
						case (VF_LJ):	// U = 4/3 * eps * sigma**3 * ( 1/3 * (s/r)**9 - (s/r)**3
							epsilon = sqrt(paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS]);
							sigma = 0.5 * (paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA]) * vrs;
							sigmar9 = sigma / cutoff;
							sigmar3 = sigmar9 * sigmar9 * sigmar9;
							sigmar9 = sigmar3 * sigmar3 * sigmar3;
							dudr = (4.0/3.0) * epsilon * ( sigmar9/3.0 - sigmar3 );
							dudr *= (sigma * sigma * sigma);
							break;
						default:
							msg(DM_NONE,"VDW tail correction not implemented for LJ form %s.\n", text_from_VF(atoms[j]->get_data()->get_funcform()));
							break;
					}
					energy += 2.0 * PI * rho * dudr;
				}
			}
		}
	}
	estore->add(ET_VDWTAIL,energy,-1);
	dbg_end(DM_CALLS,"pattern::vdw_correct_energy");
}

