/*
	*** Coulomb energy / force calculation
	*** src/ff/coulomb.cpp
	Copyright T. Youngs 2007-2011

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
#include "base/pattern.h"
#include "model/model.h"
#include "classes/prefs.h"

// Calculate the internal coulomb energy of the pattern.
// Consider only the intrapattern interactions of individual molecules  within this pattern.
void Pattern::coulombIntraPatternEnergy(Model *srcmodel, EnergyStore *estore, int lonemolecule)
{
	msg.enter("Pattern::coulombIntraPatternEnergy");
	static int i,j,aoff,m1,start1, finish1, con;;
	static Vec3<double> mim_i;
	static double rij, energy_inter, energy_intra, energy, cutoff;
	PatternAtom *pai, *paj;
	cutoff = prefs.elecCutoff();
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	energy_inter = 0.0;
	energy_intra = 0.0;

	start1 = (lonemolecule == -1 ? 0 : lonemolecule);
	finish1 = (lonemolecule == -1 ? nMolecules_ : lonemolecule+1);
	aoff = startAtom_ + start1*nAtoms_;
	for (m1=start1; m1<finish1; m1++)
	{
		// Calculate energies of atom pairs that are unbound or separated by more than two bonds
		// I.E. bound interactions up to and including angles are excluded. Torsions are scaled by the scale matrix.
		i = -1;
		for (pai = atoms_.first(); pai != atoms_.last(); pai = pai->next)
		{
			i++;
			j = i;
			for (paj = pai->next; paj != NULL; paj = paj->next)
			{
				j++;
				con = conMatrix_[i][j];
				if ((con > 2) || (con == 0))
				{
					mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					energy  = (modelatoms[i+aoff]->charge() * modelatoms[j+aoff]->charge()) / rij;
					con == 0 ? energy_inter += energy : energy_intra += (con == 3 ? energy * elecScaleMatrix_[i][j] : energy);
				}
			}
		}
		aoff += nAtoms_;
	}
// 	aoff = (molecule == -1 ? startAtom_ : startAtom_ + molecule*nAtoms_);
// 	for (m1=(molecule == -1 ? 0 : molecule); m1<(molecule == -1 ? nMolecules_ : molecule+1); m1++)
// 	{
// 		// Add on contributions for connectivities of 0 (unbound) or > 2
// 		for (i=0; i<nAtoms_; i++)
// 		{
// 			for (j=i+1; j<nAtoms_; j++)
// 			{
// 				con = conMatrix_[i][j];
// 				if ((con > 2) || (con == 0))
// 				{
// 					mim_i = cell->mimd(modelatoms[i+aoff]->r(),modelatoms[j+aoff]->r());
// 					rij = mim_i.magnitude();
// 					if (rij > cutoff) continue;
// 					energy  = (modelatoms[i+aoff]->charge() * modelatoms[j+aoff]->charge()) / (rij * rij);
// 					con == 0 ? energy_inter += energy : energy_intra += (con == 3 ? energy * elecScaleMatrix_[i][j] : energy);
// 				}
// 			}
// 		}
// 		aoff += nAtoms_;
// 	}

	energy_intra *= prefs.elecConvert();
	energy_inter *= prefs.elecConvert();
	estore->add(EnergyStore::CoulombIntraEnergy,energy_intra,id_);
	estore->add(EnergyStore::CoulombInterEnergy,energy_inter,id_,id_);
	msg.exit("Pattern::coulombIntraPatternEnergy");
}

// Calculate the coulomb contribution to the energy from interactions between different molecules of this pattern and the one supplied
void Pattern::coulombInterPatternEnergy(Model *srcmodel, Pattern *otherPattern, EnergyStore *estore, int molId)
{
	msg.enter("Pattern::coulombInterPatternEnergy");
	static int i,j,aoff1,aoff2,m1,m2,finish1,start1,start2,finish2;
	static Vec3<double> mim_i;
	static double rij, energy_inter, energy, cutoff;
	PatternAtom *pai, *paj;
	cutoff = prefs.elecCutoff();
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	energy_inter = 0.0;

	// Outer loop over molecules in *this* pattern
	// When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
	if (molId == -1)
	{
		start1 = 0;
		finish1 = (this == otherPattern ? nMolecules_ - 1 : nMolecules_);
	}
	else
	{
		start1 = molId;
		finish1 = molId + 1;
	}
	aoff1 = startAtom_ + start1 * nAtoms_;
	for (m1=start1; m1<finish1; m1++)
	{
		// Inner loop - over *all* molecules in 'otherPattern'
		if (this == otherPattern)
		{
			// Same pattern - if a specific molecule was given then we loop over all molecules.
			// If not, loop over m1+1 to nMolecules_.
			if (molId == -1)
			{
				start2 = m1 + 1;
				finish2 = nMolecules_;
			}
			else
			{
				start2 = 0;
				finish2 = nMolecules_;
			}
		}
		else
		{
			// Simple - go over all molecules in the dissimilar pattern
			start2 = 0;
			finish2 = otherPattern->nMolecules_;
		}

		//if (m1 == 0) printf("IPE - finish1 = %i, start2 = %i, finish2 = %i\n",finish1,start2,finish2);
		aoff2 = otherPattern->startAtom_ + start2*otherPattern->nAtoms_;
		//printf("  VDWINTER2 %i %i %i\n",start2,finish2,aoff2);
		for (m2=start2; m2<finish2; m2++)
		{
			if ((this == otherPattern) && (molId == m2)) { aoff2 += nAtoms_; continue; }
			//printf("      m1/m2=%i/%i  aoff1/aoff2=%i/%i \n",m1,m2,aoff1,aoff2);
			i = -1;
			for (pai = atoms_.first(); pai != NULL; pai = pai->next)
			{
				i++;
				j = -1;
				for (paj = otherPattern->atoms_.first(); paj != NULL; paj = paj->next)
				{
					j++;
					mim_i = cell->mimd(modelatoms[i+aoff1]->r(), modelatoms[j+aoff2]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					energy  = (modelatoms[i+aoff1]->charge() * modelatoms[j+aoff2]->charge()) / rij;
					energy_inter += energy;
				}
			}
			aoff2 += otherPattern->nAtoms_;
		}
		aoff1 += nAtoms_;
	}

// 	aoff1 = startAtom_;
// 	// When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
// 	if ((this == xpnode) && (molecule == -1)) finish1 = nMolecules_ - 1;
// 	else finish1 = nMolecules_;
// 	for (m1=0; m1<finish1; m1++)
// 	{
// 		if (molecule == -1)
// 		{
// 			start2 = (this == xpnode ? m1 + 1 : 0);
// 			finish2 = xpnode->nMolecules_;
// 		}
// 		else
// 		{
// 			start2 = molecule;
// 			finish2 = molecule + 1;
// 			// If the patterns are the same we must exclude molecule == m1
// 			if ((this == xpnode) && (molecule == m1)) { aoff1 += nAtoms_; continue; }
// 		}
// 		//this == xpnode ? start = m1 + 1 : start = 0;
// 		aoff2 = xpnode->startAtom_ + start2*xpnode->nAtoms_;
// 		for (m2=start2; m2<finish2; m2++)
// 		{
// 			for (a1=0; a1<nAtoms_; a1++)
// 			{
// 				i = a1 + aoff1;
// 				for (a2=0; a2<xpnode->nAtoms_; a2++)
// 		  		{
// 					j = a2 + aoff2;
// 					mim_i = cell->mimd(modelatoms[i]->r(),modelatoms[j]->r());
// 					rij = mim_i.magnitude();
// 					if (rij > cutoff) continue;
// 	//printf("Coulomb ij %i %i %8.4f %8.4f %8.4f \n",i,j,xcfg->q[i],xcfg->q[j],rij);
// 					energy  = (modelatoms[i]->charge() * modelatoms[j]->charge()) / (rij * rij);
// 					energy_inter += energy;
// 				}
// 			}
// 			aoff2 += xpnode->nAtoms_;
// 		}
// 		aoff1 += nAtoms_;
// 	}

	energy_inter *= prefs.elecConvert();
	estore->add(EnergyStore::CoulombInterEnergy,energy_inter,id_,otherPattern->id_);
	msg.exit("Pattern::coulombInterPatternEnergy");
}

// Calculate the internal coulomb forces in the pattern.
// Consider only the intrapattern interactions of individual molecules  within this pattern.
void Pattern::coulombIntraPatternForces(Model *srcmodel)
{
	msg.enter("Pattern::coulombIntraPatternForces");
	static int i, j, aoff, m1, con;
	static Vec3<double> mim_i, f_i, tempf;
	static double rij, factor, cutoff;
	PatternAtom *pai, *paj;
	cutoff = prefs.elecCutoff();
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	aoff = startAtom_;
	for (m1=0; m1<nMolecules_; m1++)
	{
		// Add contributions from atom pairs that are unbound or separated by more than three bonds
		i = -1;
		for (pai = atoms_.first(); pai != atoms_.last(); pai = pai->next)
		{
			i++;
			// Store temporary forces to avoid unnecessary array lookups
			f_i = modelatoms[i+aoff]->f();
			j = i;
			for (paj = pai->next; paj != NULL; paj = paj->next)
			{
				j++;
				con = conMatrix_[i][j];
				if ((con > 2) || (con == 0))
				{
					mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					// Calculate force contribution
					factor = (modelatoms[i+aoff]->charge() * modelatoms[j+aoff]->charge()) / (rij*rij);
					if (con == 3) factor *= elecScaleMatrix_[i][j];
					tempf = mim_i * factor;
					f_i -= tempf;
					modelatoms[j+aoff]->f() += tempf;
				}
			}
			// Put the temporary forces back into the main array
			modelatoms[i+aoff]->f() = f_i;
		}
		aoff += nAtoms_;
	}
/*
	aoff = startAtom_;
	for (m1=0; m1<nMolecules_; m1++)
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
	}*/
	msg.exit("Pattern::coulombIntraPatternForces");
}

// Calculate the coulomb forces from interactions between different molecules of this pattern and the one supplied
void Pattern::coulombInterPatternForces(Model *srcmodel, Pattern *otherPattern)
{
	msg.enter("Pattern::coulombInterPatternForces");
	int i,j,aoff1,aoff2,m1,m2,finish1,start1,start2,finish2;
	Vec3<double> mim_i, f_i, tempf;
	double rij, energy_inter, cutoff, factor;
	PatternAtom *pai, *paj;
	cutoff = prefs.elecCutoff();
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	energy_inter = 0.0;

	// Outer loop over molecules in *this* pattern
	// When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
	start1 = 0;
	finish1 = (this == otherPattern ? nMolecules_ - 1 : nMolecules_);
	aoff1 = startAtom_ + start1 * nAtoms_;
	for (m1=start1; m1<finish1; m1++)
	{
		// Inner loop - over *all* molecules in 'otherPattern'
		if (this == otherPattern)
		{
			// Same pattern - if a specific molecule was given then we loop over all molecules.
			start2 = m1 + 1;
			finish2 = nMolecules_;
		}
		else
		{
			// Simple - go over all molecules in the dissimilar pattern
			start2 = 0;
			finish2 = otherPattern->nMolecules_;
		}

		//if (m1 == 0) printf("IPE - finish1 = %i, start2 = %i, finish2 = %i\n",finish1,start2,finish2);
		aoff2 = otherPattern->startAtom_ + start2*otherPattern->nAtoms_;
		//printf("  VDWINTER2 %i %i %i\n",start2,finish2,aoff2);
		for (m2=start2; m2<finish2; m2++)
		{
			//printf("      m1/m2=%i/%i  aoff1/aoff2=%i/%i \n",m1,m2,aoff1,aoff2);
			i = -1;
			for (pai = atoms_.first(); pai != NULL; pai = pai->next)
			{
				i++;
				j = -1;
				for (paj = otherPattern->atoms_.first(); paj != NULL; paj = paj->next)
				{
					j++;
					mim_i = cell->mimd(modelatoms[i+aoff1]->r(), modelatoms[j+aoff2]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					// Calculate force contribution
					factor = (modelatoms[i+aoff1]->charge() * modelatoms[j+aoff2]->charge()) / (rij*rij);
					tempf = mim_i * factor;
					f_i -= tempf;
					modelatoms[j+aoff2]->f() += tempf;
				}
			}
			aoff2 += otherPattern->nAtoms_;
		}
		aoff1 += nAtoms_;
	}

// 	// TODO Move loops so that we can load temporary forces for i then calculate all other forces on it in one go.
// 	 // When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
//         this == otherPattern ? finish = nMolecules_ - 1 : finish = nMolecules_;
// 	for (m1=0; m1<finish; m1++)
// 	{
// 		this == otherPattern ? start = m1 + 1 : start = 0;
// 		aoff2 = otherPattern->startAtom_ + start*nAtoms_;
// 		for (m2=start; m2<otherPattern->nMolecules_; m2++)
// 		{
// 			i = -1;
// 			for (pai = atoms_.first(); pai != NULL; pai = pai->next)
// 			{
// 				i++;
// 				f_i = modelatoms[i+aoff1]->f();
// 				j = -1;
// 				for (paj = otherPattern->atoms_.first(); paj != NULL; paj = paj->next)
// 				{
// 					j++;
// 					mim_i = cell->mimd(modelatoms[i+aoff1]->r(), modelatoms[j+aoff2]->r());
// 					rij = mim_i.magnitude();
// 					if (rij > cutoff) continue;
// 					// Calculate force contribution
// 					factor = (modelatoms[i+aoff1]->charge() * modelatoms[j+aoff2]->charge()) / (rij*rij);
// 					tempf = mim_i * factor;
// 					f_i -= tempf;
// 					modelatoms[j+aoff2]->f() += tempf;
// 				}
// 				// Store temporary force array back into main force array
// 				modelatoms[i+aoff1]->f() = f_i;
// 			}
// 			aoff2 += otherPattern->nAtoms_;
// 		}
// 		aoff1 += nAtoms_;
// 	}

/*	aoff1 = startAtom_;
	 // When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
        this == xpnode ? finish = nMolecules_ - 1 : finish = nMolecules_;
	for (m1=0; m1<finish; m1++)
	{
		this == xpnode ? start = m1 + 1 : start = 0;
	       	aoff2 = xpnode->startAtom_ + start*nAtoms_;
		for (m2=start; m2<xpnode->nMolecules_; m2++)
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
	} */
	msg.exit("Pattern::coulombInterPatternForces");
}

