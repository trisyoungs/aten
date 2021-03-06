/*
	*** Coulomb energy / force calculation
	*** src/ff/coulomb.cpp
	Copyright T. Youngs 2007-2018

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
#include "base/prefs.h"

ATEN_USING_NAMESPACE

// Calculate the internal coulomb energy of the pattern.
// Consider only the intrapattern interactions of individual molecules  within this pattern.
void Pattern::coulombIntraPatternEnergy(Model* srcmodel, EnergyStore* estore, int lonemolecule)
{
	Messenger::enter("Pattern::coulombIntraPatternEnergy");
	static int i,j,aoff,m1,start1, finish1, con;;
	static Vec3<double> vec_ij;
	static double rij, energy_inter, energy_intra, energy, cutoff;
	PatternAtom* pai, *paj;
	cutoff = prefs.elecCutoff();
	Atom** modelatoms = srcmodel->atomArray();
	UnitCell& cell = srcmodel->cell();
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
					vec_ij = cell.mimVector(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = vec_ij.magnitude();
					if (rij > cutoff) continue;
					energy  = (modelatoms[i+aoff]->charge() * modelatoms[j+aoff]->charge()) / rij;
					con == 0 ? energy_inter += energy : energy_intra += (con == 3 ? energy * elecScaleMatrix_[i][j] : energy);
				}
			}
		}
		aoff += nAtoms_;
	}

	energy_intra *= prefs.elecConvert();
	energy_inter *= prefs.elecConvert();
	estore->add(EnergyStore::CoulombIntraEnergy,energy_intra,id_);
	estore->add(EnergyStore::CoulombInterEnergy,energy_inter,id_,id_);
	Messenger::exit("Pattern::coulombIntraPatternEnergy");
}

// Calculate the coulomb contribution to the energy from interactions between different molecules of this pattern and the one supplied
void Pattern::coulombInterPatternEnergy(Model* srcmodel, Pattern* otherPattern, EnergyStore* estore, int molId)
{
	Messenger::enter("Pattern::coulombInterPatternEnergy");
	static int i,j,aoff1,aoff2,m1,m2,finish1,start1,start2,finish2;
	static Vec3<double> vec_ij;
	static double rij, energy_inter, energy, cutoff;
	PatternAtom* pai, *paj;
	cutoff = prefs.elecCutoff();
	Atom** modelatoms = srcmodel->atomArray();
	UnitCell& cell = srcmodel->cell();
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
					vec_ij = cell.mimVector(modelatoms[i+aoff1]->r(), modelatoms[j+aoff2]->r());
					rij = vec_ij.magnitude();
					if (rij > cutoff) continue;
					energy  = (modelatoms[i+aoff1]->charge() * modelatoms[j+aoff2]->charge()) / rij;
					energy_inter += energy;
				}
			}
			aoff2 += otherPattern->nAtoms_;
		}
		aoff1 += nAtoms_;
	}

	energy_inter *= prefs.elecConvert();
	estore->add(EnergyStore::CoulombInterEnergy,energy_inter,id_,otherPattern->id_);
	Messenger::exit("Pattern::coulombInterPatternEnergy");
}

// Calculate the internal coulomb forces in the pattern.
// Consider only the intrapattern interactions of individual molecules within this pattern.
void Pattern::coulombIntraPatternForces(Model* srcmodel)
{
	Messenger::enter("Pattern::coulombIntraPatternForces");
	static int i, j, aoff, m1, con;
	static Vec3<double> vec_ij, f_i, tempf;
	static double rij, factor, cutoff;
	PatternAtom* pai, *paj;
	cutoff = prefs.elecCutoff();
	Atom** modelatoms = srcmodel->atomArray();
	UnitCell& cell = srcmodel->cell();
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
					vec_ij = cell.mimVector(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = vec_ij.magnitude();
					if (rij > cutoff) continue;
					// Calculate force contribution
					factor = (modelatoms[i+aoff]->charge() * modelatoms[j+aoff]->charge()) / (rij*rij);
					if (con == 3) factor *= elecScaleMatrix_[i][j];
					tempf = vec_ij * factor;
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
	Messenger::exit("Pattern::coulombIntraPatternForces");
}

// Calculate the coulomb forces from interactions between different molecules of this pattern and the one supplied
void Pattern::coulombInterPatternForces(Model* srcmodel, Pattern* otherPattern)
{
	Messenger::enter("Pattern::coulombInterPatternForces");
	int i,j,aoff1,aoff2,m1,m2,finish1,start1,start2,finish2;
	Vec3<double> vec_ij, f_i, tempf;
	double rij, energy_inter, cutoff, factor;
	PatternAtom* pai, *paj;
	cutoff = prefs.elecCutoff();
	Atom** modelatoms = srcmodel->atomArray();
	UnitCell& cell = srcmodel->cell();
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
				++i;
				j = -1;
				for (paj = otherPattern->atoms_.first(); paj != NULL; paj = paj->next)
				{
					++j;
					vec_ij = cell.mimVector(modelatoms[i+aoff1]->r(), modelatoms[j+aoff2]->r());
					rij = vec_ij.magnitude();
					if (rij > cutoff) continue;
					// Calculate force contribution
					factor = (modelatoms[i+aoff1]->charge() * modelatoms[j+aoff2]->charge()) / (rij*rij);
					tempf = vec_ij * factor;
					f_i -= tempf;
					modelatoms[j+aoff2]->f() += tempf;
				}
			}
			aoff2 += otherPattern->nAtoms_;
		}
		aoff1 += nAtoms_;
	}

	Messenger::exit("Pattern::coulombInterPatternForces");
}

