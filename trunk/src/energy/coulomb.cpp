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
void Pattern::coulombIntraPatternEnergy(Model *srcmodel, Energy *estore, int molecule)
{
	msg.enter("Pattern::coulombIntraPatternEnergy");
	static int n,i,j,aoff,m1,con;
	static Vec3<double> mim_i;
	static double rij, energy_inter, energy_intra, energy, cutoff;
	cutoff = prefs.elecCutoff();
	PatternBound *pb;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	energy_inter = 0.0;
	energy_intra = 0.0;
	aoff = (molecule == -1 ? startAtom_ : startAtom_ + molecule*nAtoms_);
	for (m1=(molecule == -1 ? 0 : molecule); m1<(molecule == -1 ? nMols_ : molecule+1); m1++)
	{
		// Add on contributions for connectivities of 0 (unbound) or > 2
		for (i=0; i<nAtoms_; i++)
		{
			for (j=i+1; j<nAtoms_; j++)
			{
				con = conMatrix_[i][j];
				if ((con > 2) || (con == 0))
				{
					mim_i = cell->mimd(modelatoms[i+aoff]->r(),modelatoms[j+aoff]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					energy  = (modelatoms[i+aoff]->charge() * modelatoms[j+aoff]->charge()) / (rij * rij);
					con == 0 ? energy_inter += energy : energy_intra += (con == 3 ? energy * elecScaleMatrix_[i][j] : energy);
				}
			}
		}
		aoff += nAtoms_;
	}
	energy_intra = energy_intra * prefs.elecConvert();
	energy_inter = energy_inter * prefs.elecConvert();
	estore->add(Energy::CoulombIntraEnergy,energy_intra,id_);
	estore->add(Energy::CoulombInterEnergy,energy_inter,id_,id_);
	msg.exit("Pattern::coulombIntraPatternEnergy");
}

// Calculate the coulomb contribution to the energy from interactions between different molecules of this pattern and the one supplied
void Pattern::coulombInterPatternEnergy(Model *srcmodel, Pattern *xpnode, Energy *estore, int molecule)
{
	msg.enter("Pattern::coulombInterPatternEnergy");
	static int n1, n2, i, j, aoff1, aoff2, m1, m2, finish1, start2, finish2, a1, a2;
	static Vec3<double> mim_i;
	static double rij, energy_inter, energy, cutoff;
	cutoff = prefs.elecCutoff();
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	energy_inter = 0.0;
	aoff1 = startAtom_;
	// When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
	if ((this == xpnode) && (molecule == -1)) finish1 = nMols_ - 1;
	else finish1 = nMols_;
	for (m1=0; m1<finish1; m1++)
	{
		if (molecule == -1)
		{
			start2 = (this == xpnode ? m1 + 1 : 0);
			finish2 = xpnode->nMols_;
		}
		else
		{
			start2 = molecule;
			finish2 = molecule + 1;
			// If the patterns are the same we must exclude molecule == m1
			if ((this == xpnode) && (molecule == m1)) { aoff1 += nAtoms_; continue; }
		}
		//this == xpnode ? start = m1 + 1 : start = 0;
		aoff2 = xpnode->startAtom_ + start2*xpnode->nAtoms_;
		for (m2=start2; m2<finish2; m2++)
		{
			for (a1=0; a1<nAtoms_; a1++)
			{
				i = a1 + aoff1;
				for (a2=0; a2<xpnode->nAtoms_; a2++)
		  		{
					j = a2 + aoff2;
					mim_i = cell->mimd(modelatoms[i]->r(),modelatoms[j]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
	//printf("Coulomb ij %i %i %8.4f %8.4f %8.4f \n",i,j,xcfg->q[i],xcfg->q[j],rij);
					energy  = (modelatoms[i]->charge() * modelatoms[j]->charge()) / (rij * rij);
					energy_inter += energy;
				}
			}
			aoff2 += xpnode->nAtoms_;
		}
		aoff1 += nAtoms_;
	}
	energy_inter = energy_inter * prefs.elecConvert();
	estore->add(Energy::CoulombInterEnergy,energy_inter,id_,xpnode->id_);
	msg.exit("Pattern::coulombInterPatternEnergy");
}

// Calculate the internal coulomb forces in the pattern.
// Consider only the intrapattern interactions of individual molecules  within this pattern.
void Pattern::coulombIntraPatternForces(Model *srcmodel)
{
	msg.enter("Pattern::coulombIntraPatternForces");
	static int n, i, j, aoff, m1, con;
	static Vec3<double> mim_i, f_i, tempf;
	static double rij, factor, cutoff;
	cutoff = prefs.elecCutoff();
	PatternBound *pb;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	aoff = startAtom_;
	for (m1=0; m1<nMols_; m1++)
	{
		// Add on contributions for connectivities of 0 (unbound) or > 3
		for (i=0; i<nAtoms_; i++)
		{
			// Copy forces for atom i
			f_i = modelatoms[i+aoff]->f();
			for (j=i+1; j<nAtoms_; j++)
			{
				con = conMatrix_[i][j];
				if ((con > 2) || (con == 0))
				{
					mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					factor = (modelatoms[i]->charge() * modelatoms[j]->charge()) / (rij*rij*rij);
					if (con == 3) factor *= elecScaleMatrix_[i][j];
					tempf = mim_i * factor;
					f_i += tempf;
					modelatoms[j+aoff]->f() -= tempf;
				}
			}
			// Re-load forces back into main array
			modelatoms[i+aoff]->f() = f_i;
		}
		aoff += nAtoms_;
	}
	msg.exit("Pattern::coulombIntraPatternForces");
}

// Calculate the coulomb forces from interactions between different molecules of this pattern and the one supplied
void Pattern::coulombInterPatternForces(Model *srcmodel, Pattern *xpnode)
{
	msg.enter("Pattern::coulombInterPatternForces");
	static int n1,n2,i,j,aoff1,aoff2,m1,m2,start,finish,a1,a2;
	static Vec3<double> mim_i, f_i, tempf;
	static double rij, factor, cutoff;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	cutoff = prefs.elecCutoff();
	aoff1 = startAtom_;
	 // When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
        this == xpnode ? finish = nMols_ - 1 : finish = nMols_;
	for (m1=0; m1<finish; m1++)
	{
		this == xpnode ? start = m1 + 1 : start = 0;
	       	aoff2 = xpnode->startAtom_ + start*nAtoms_;
		for (m2=start; m2<xpnode->nMols_; m2++)
		{
			for (a1=0; a1<nAtoms_; a1++)
			{
				i = a1 + aoff1;
				// Copy forces to temporary vector
				f_i = modelatoms[i]->f();
				for (a2=0; a2<xpnode->nAtoms_; a2++)
		  		{
					j = a2 + aoff2;
					mim_i = cell->mimd(modelatoms[i]->r(), modelatoms[j]->r());
					rij = mim_i.magnitude();
					if (rij < cutoff)
					{
	//printf("Coulomb ij %i %i %8.4f %8.4f %8.4f \n",i,j,xcfg->q[i],xcfg->q[j],rij);
						factor = (modelatoms[i]->charge() * modelatoms[j]->charge()) / (rij*rij*rij);
						tempf = mim_i * factor;
						f_i += tempf;
						modelatoms[j]->f() -= tempf;
					}
				}
			}
			// Replace forces in main array
			modelatoms[i]->f() = f_i;
			aoff2 += xpnode->nAtoms_;
		}
		aoff1 += nAtoms_;
	}
	msg.exit("Pattern::coulombInterPatternForces");
}

