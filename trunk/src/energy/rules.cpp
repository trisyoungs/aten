/*
	*** Specification for rule-based forcefields
	*** src/energy/rules.cpp
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

#include "base/elements.h"
#include "classes/atom.h"
#include "classes/forcefield.h"
#include "energy/forms.h"
#include "base/constants.h"

// References
// UFF:
// DREIDING: A Generic Force Field for Molecular Simulations
//	Stephen L. Mayo, Barry D. Olafson, and William A. Goddard III
//	J. Phys. Chem. 1990, 94, 8897-8909

// Generate VDW params
void Forcefield::generateVdw(Atom *i)
{
	// Simplest of all generation routines - creates the params() data for VDW interactions.
	msg.enter("Forcefield::generateVdw");
	double sigma, epsilon, r0, d0;
	ForcefieldAtom *ffi = i->type();
	switch (rules_)
	{
		case (Rules::None):
			msg.print("Error - tried to generate VDW parameters for a forcefield that has no rules.\n");
			break;
		case (Rules::Uff):
			// UFF VDW types are just the third [2] and fourth [3] data (for simple LJ)
			epsilon = ffi->generator(3);
			sigma = ffi->generator(2);
			ffi->setVdwForm(VdwFunctions::Lj);
			ffi->params().data[VdwFunctions::LjEpsilon] = epsilon * 0.25;
			ffi->params().data[VdwFunctions::LjSigma] = sigma;
			ffi->params().data[VdwFunctions::LjN] = 2.0;
			msg.print(Messenger::Verbose,"UFF LJ    : sigma, epsilon, n = %8.4f %8.4f 2.0\n", sigma, epsilon);
			break;
		case (Rules::DreidingLJ):
			r0 = ffi->generator(2);
			d0 = ffi->generator(3);
			ffi->setVdwForm(VdwFunctions::LjAB);
			ffi->params().data[VdwFunctions::LjA] = d0 * pow(r0,12.0);
			ffi->params().data[VdwFunctions::LjB] = 2.0 * d0 * pow(r0,6.0);
			msg.print(Messenger::Verbose,"Dreiding LJ (ljab) : A, B, %8.4f %8.4f\n", ffi->params().data[VdwFunctions::LjA], ffi->params().data[VdwFunctions::LjB]);
			break;
		case (Rules::DreidingX6):
			break;
	}
	msg.exit("Forcefield::generateVdw");
}

// Generate bond params
ForcefieldBound *Forcefield::generateBond(Atom *i, Atom *j)
{
	// Creates bond forcefield data for the specified atom types.
	// No check is performed to see if similar data has already been generated.
	msg.enter("Forcefield::generateBond");
	static double k, ri, rj, sumr, chii, chij, rBO, chi, rEN, Zi, Zj;
	ForcefieldAtom *ffi = i->type();
	ForcefieldAtom *ffj = j->type();
	// Create new bond and set type to None for now...
	ForcefieldBound *newbond = addBond(BondFunctions::None);
	switch (rules_)
	{
		case (Rules::None):
			msg.print("Error - tried to generate bond parameters for a forcefield that has no rules.\n");
			break;
		case (Rules::Uff):
			// UFF Harmonic Bond Generator
			// rij : Equilibrium distance : = ri + rj + rBO - rEN
			// rBO : Bond-order correction = -0.1332 * (ri + rj) * ln(n)
			// rEN : Electronegativity correction : ri*rj * (sqrt(Xi)-sqrt(Xj))**2 / (Xi*ri + Xj*rj)
			// Note: In the original paper  rij = ri + rj + rBO + rEN, but Marcus Martin (MCCCS Towhee) notes that the last term should be subtracted
			ri = ffi->generator(0);
			rj = ffj->generator(0);
			sumr = ri + rj;
			chii = ffi->generator(6);
			chij = ffj->generator(6);
			rBO = -0.1332 * sumr * log(i->bondOrder(j));
			chi = (sqrt(chii) - sqrt(chij));
			rEN = ri * rj * chi * chi / (chii*ri + chij*rj);
			Zi = ffi->generator(5);
			Zj = ffj->generator(5);
			k = prefs.convertEnergy(664.12, Prefs::KiloCalories) * ( (Zi * Zj) / (sumr + sumr + sumr) );
			// Create new bond definition in the forcefield space and set its parameters
			newbond->setBondStyle(BondFunctions::Harmonic);
			newbond->params().data[BondFunctions::HarmonicEq] = sumr + rBO - rEN;
			newbond->params().data[BondFunctions::HarmonicK] = k;
			msg.print(Messenger::Verbose,"UFF Bond  : eq, k = %8.4f %8.4f\n", newbond->params().data[BondFunctions::HarmonicEq], newbond->params().data[BondFunctions::HarmonicK]);
			break;
		case (Rules::DreidingLJ):
		case (Rules::DreidingX6):
			// Harmonic Form
			// Force constant k = 700.0 kcal/mol * BO(ij)
			ri = ffi->generator(0);
			rj = ffj->generator(0);
			k = prefs.convertEnergy(700.0, Prefs::KiloCalories) * i->bondOrder(j);
			newbond->setBondStyle(BondFunctions::Harmonic);
			newbond->params().data[BondFunctions::HarmonicEq] = ri + rj - 0.01;
			newbond->params().data[BondFunctions::HarmonicK] = k;
			msg.print(Messenger::Verbose,"Dreiding Bond (harm) : eq, k = %8.4f %8.4f\n", newbond->params().data[BondFunctions::HarmonicEq], newbond->params().data[BondFunctions::HarmonicK]);
			break;
	}
	msg.exit("Forcefield::generateBond");
	return newbond;
}

// Generate angle params
ForcefieldBound *Forcefield::generateAngle(Atom *i, Atom *j, Atom *k)
{
	// Creates angle forcefield data for the specified atom types.
	// No check is performed to see if similar data has already been generated.
	msg.enter("Forcefield::generateAngle");
	double ri, rj, rk, rij, rjk, rBO, rEN, chii, chij, chik, chi, rik2, rik5, Zi, Zk, beta, forcek, eq;
	double c0, c1, c2;
	int n;
	ForcefieldAtom *ffi = i->type();
	ForcefieldAtom *ffj = j->type();
	ForcefieldAtom *ffk = k->type();
	// Create new angle and set type to None for now...
	ForcefieldBound *newangle = addAngle(AngleFunctions::None);
	switch (rules_)
	{
		case (Rules::None):
			msg.print("Error - tried to generate angle parameters for a forcefield that has no rules.\n");
			break;
		case (Rules::Uff):
			// UFF Cosine Angle Generator
			// U(theta) = (k / (n*n)) * (1 + s*cos(n*theta))
			// rik2 = rij**2 + rjk**2 - 2 * rij * rjk * cos(eq)
			// k = beta (Zi * Zk / rik**5) * rij * rjk * (rij * rjk * (1-cos**2(eq)) - rik**2 * cos(eq))
			ri = ffi->generator(0);
			rj = ffj->generator(0);
			rk = ffk->generator(0);
			chii = ffi->generator(6);
			chij = ffj->generator(6);
			chik = ffk->generator(6);
			Zi = ffi->generator(5);
			Zk = ffk->generator(5);
			// Determine rij and riK
			rBO = -0.1332 * (ri + rj) * log(i->bondOrder(j));
			chi = sqrt(chii) - sqrt(chij);
			rEN = ri * rj * chi * chi / (chii*ri + chij*rj);
			rij = ri + rj + rBO - rEN;
			//printf("UFF Angle : IJ rBO, chi, rEN, rij = %8.4f %8.4f %8.4f %8.4f \n",rBO,chi,rEN,rij);
			rBO = -0.1332 * (rj + rk) * log(j->bondOrder(k));
			chi = sqrt(chij) - sqrt(chik);
			rEN = rj * rk * chi * chi / (chij*rj + chik*rk);
			rjk = rj + rk + rBO - rEN;
			//printf("          : JK rBO, chi, rEN, rjk = %8.4f %8.4f %8.4f %8.4f\n",rBO,chi,rEN,rjk);
			// Determine rik2 and rik5
			eq = ffj->generator(1) / DEGRAD;
			rik2 = rij * rij + rjk * rjk - 2.0 * ( rij * rjk * cos(eq));
			rik5 = rik2 * rik2 * sqrt(rik2);
			// Determine k
			beta = 664.12 / (rij * rjk);
			forcek = beta * (Zi * Zk / rik5) * rij * rjk;
			forcek = forcek * (3.0 * rij * rjk * (1.0 - cos(eq)*cos(eq)) - rik2 * cos(eq));
			//printf("          : eq, rik2, rik5, beta, forcek = %8.4f %8.4f %8.4f %8.4f %8.4f\n",eq,rik2,rik5,beta,forcek);
			// Determine 'n' based on the geometry of the central atom 'j'
			if (ffj->generator(1) > 170.0) n = 1;
			else if (ffj->generator(1) > 115.0) n = 3;
			else if (ffj->generator(1) > 95.0) n = 2;
			else n = 4;
			// We always use the Cosine form, with 'eq' set to zero and 's' set to +1 for linear and tetrahedral90 cases or -1 otherwise
			newangle->setAngleStyle(AngleFunctions::Cosine);
			newangle->params().data[AngleFunctions::CosineN] = n;
			newangle->params().data[AngleFunctions::CosineK] = forcek / (n*n);
			newangle->params().data[AngleFunctions::CosineEq] = 0.0;
			if (n == 1) newangle->params().data[AngleFunctions::CosineS] = 1.0;
			else newangle->params().data[AngleFunctions::CosineS] = -1.0;
			msg.print(Messenger::Verbose,"UFF Angle (cosine) : %s-%s-%s - forcek = %8.4f, eq = 0.0, s = %f, n = %i\n", ffi->name(), ffj->name(), ffk->name(), forcek, newangle->params().data[AngleFunctions::CosineS], n);
			break;
		case (Rules::DreidingLJ):
		case (Rules::DreidingX6):
			// Harmonic cosine form, except for eq=180 (linear molecules) where we use UFFCosine1
			// Force constants are always 100.0 kcal/mol/rad**2
			forcek = prefs.convertEnergy(100.0, Prefs::KiloCalories);
			eq = ffj->generator(1);
			newangle = angles_.add();
			if (eq > 179.0)
			{
				newangle->setAngleStyle(AngleFunctions::Cosine);
				newangle->params().data[AngleFunctions::CosineK] = forcek;
				newangle->params().data[AngleFunctions::CosineEq] = 0.0;
				newangle->params().data[AngleFunctions::CosineN] = 1.0;
				msg.print(Messenger::Verbose,"Dreiding Angle (cosine) : %s-%s-%s - forcek = %8.4f, eq = %8.4f, n = 1\n", ffi->name(), ffj->name(), ffk->name(), forcek, eq);
			}
			else
			{
				newangle->setAngleStyle(AngleFunctions::HarmonicCosine);
				newangle->params().data[AngleFunctions::HarmonicCosineK] = forcek / (sin(eq) * sin(eq));
				newangle->params().data[AngleFunctions::HarmonicCosineEq] = eq;
				msg.print(Messenger::Verbose,"Dreiding Angle (harmcos) : %s-%s-%s - forcek = %8.4f, eq = %8.4f\n", ffi->name(), ffj->name(), ffk->name(), newangle->params().data[AngleFunctions::HarmonicCosineK], eq);
			}
			break;
	}
	msg.exit("Forcefield::generateAngle");
	return newangle;
}

// Generate torsion params
ForcefieldBound *Forcefield::generateTorsion(Atom *i, Atom *j, Atom *k, Atom *l)
{
	// Creates torsion forcefield data for the specified atom types.
	// No check is performed to see if similar data has already been generated.
	msg.enter("Forcefield::generateTorsion");
	int hyb1, hyb2, group1, group2;
	double forcek, vj, vk, n, eq;
	Atom *sp2, *sp3;
	ForcefieldAtom *ffi = i->type();
	ForcefieldAtom *ffj = j->type();
	ForcefieldAtom *ffk = k->type();
	ForcefieldAtom *ffl = l->type();
	ForcefieldBound *newtorsion = addTorsion(TorsionFunctions::None);
	switch (rules_)
	{
		case (Rules::None):
			msg.print("Error - tried to generate torsion parameters for a forcefield that has no rules.\n");
			break;
		case (Rules::Uff):
			// UFF Torsions
			// There are four possibilities:
			//   a) atoms j and k are both sp3 centres (but not group 16 sp3 centres) [Note: Paper refers to old group 6]
			//   b) atoms j and k are both sp2 centres
			//   c) one atom is sp2 and one is sp3
			//   d) atom j or k is a group 16 element
			hyb1 = (int) (ffj->generator(7) + 0.1);
			hyb2 = (int) (ffk->generator(7) + 0.1);
			group1 = elements.group(j);
			group2 = elements.group(k);
			newtorsion->setTorsionStyle(TorsionFunctions::CosCos);
			if ((group1 == 16) || (group2 == 16))
			{
				// If both elements are group 16 then use special V values for them
				if (group1 == group2)
				{
					vj = prefs.convertEnergy((j->element() == 8 ? 2.0 : 6.8), Prefs::KiloCalories);
					vk = prefs.convertEnergy((k->element() == 8 ? 2.0 : 6.8), Prefs::KiloCalories);
					forcek = sqrt(vj*vk);
				}
				else forcek = 5.0* sqrt(ffj->generator(9)*ffk->generator(9)) * (1.0 + 4.18*log(j->bondOrder(k)));
				n = 2.0;
				eq = 90.0;
			}
			else if ((hyb1 == 3) && (hyb2 == 3))
			{
				forcek = sqrt(ffj->generator(8)*ffk->generator(8));
				n = 3.0;
				eq = 180.0;
			}
			else if ((hyb1 == 2) && (hyb2 == 2))
			{
				forcek = 5.0* sqrt(ffj->generator(9)*ffk->generator(9)) * (1.0 + 4.18*log(j->bondOrder(k)));
				n = 2.0;
				eq = 180.0;
			}
			else if ((hyb1+hyb2) == 5)
			{
				// Find sp2 atom to check for a second sp2 atom
				sp2 = (hyb1 == 2 ? j : k);
				sp3 = (sp2 == j ? k : j);
				bool another = FALSE;
				for (Refitem<Bond,int> *rb = sp2->bonds(); rb != NULL; rb = rb->next)
				{
					if (rb->item->partner(sp2) == sp3) continue;
					if (rb->item->partner(sp2)->type()->generator(7) == 2) another = TRUE;
				}
				if (another)
				{
					forcek = prefs.convertEnergy(2.0, Prefs::KiloCalories);
					n = 3.0;
					eq = 180.0;
				}
				else
				{
					forcek = prefs.convertEnergy(1.0, Prefs::KiloCalories);
					n = 6.0;
					eq = 0.0;
				}
			}
			else
			{
				forcek = 0.0;
				n = 1.0;
				eq = 0.0;
			}
			// Set parameters
			newtorsion->params().data[TorsionFunctions::CosCosK] = forcek;
			newtorsion->params().data[TorsionFunctions::CosCosN] = n;
			newtorsion->params().data[TorsionFunctions::CosCosEq] = eq;
			msg.print(Messenger::Verbose,"UFF Torsion (coscos) : %s-%s-%s-%s - forcek = %8.4f, n = %8.4f, eq = %8.4f\n", ffi->name(), ffj->name(), ffk->name(), ffl->name(), forcek, n, eq);
			break;
		case (Rules::DreidingLJ):
		case (Rules::DreidingX6):
			// Dreiding Torsions
			// There are four possibilities:
			//   a) atoms j and k are both sp3 centres (but not group 16 sp3 centres) [Note: Paper refers to old group 6]
			//   b) atoms j and k are both sp2 centres
			//   c) one atom is sp2 and one is sp3
			//   d) atom j or k is a group 16 element
			hyb1 = (int) ffj->generator(6) + 0.1;
			hyb2 = (int) ffk->generator(6) + 0.1;
			group1 = elements.group(j);
			group2 = elements.group(k);
			newtorsion->setTorsionStyle(TorsionFunctions::Dreiding);
			if ((group1 == 16) || (group2 == 16))
			{
				// If both elements are group 16 then use special V values for them
				if (group1 == group2)	// Rule (h) in paper
				{
					forcek = prefs.convertEnergy(2.0, Prefs::KiloCalories);
					eq = 90.0;
					n = 2.0;
				}
				else			// Rule (i) in paper
				{
					forcek = prefs.convertEnergy(2.0, Prefs::KiloCalories);
					eq = 180.0;
					n = 2.0;
				}
			}
			else if ((hyb1 == 3) && (hyb2 == 3))	// Rule (a) in paper
			{
				forcek = prefs.convertEnergy(2.0, Prefs::KiloCalories);
				n = 3.0;
				eq = 180.0;
			}
			else if ((hyb1 == 15) && (hyb2 == 15))	// Rule (d) in paper
			{
				forcek = prefs.convertEnergy(25.0, Prefs::KiloCalories);
				n = 2.0;
				eq = 180.0;
			}
			else if ((hyb1 + hyb2 == 17) || ((hyb1 == 2) && (hyb2 == 2)))	// Rule (e) in paper
			{
				forcek = prefs.convertEnergy(25.0, Prefs::KiloCalories);
				n = 2.0;
				eq = 180.0;
			}
			else if (hyb1+hyb2 == 5)	// Rules (b) and (j)
			{
				// Find sp2 atom to check for a second sp2 atom
				sp2 = (hyb1 == 2 ? j : k);
				sp3 = (sp2 == j ? k : j);
				bool another = FALSE;
				for (Refitem<Bond,int> *rb = sp2->bonds(); rb != NULL; rb = rb->next)
				{
					if (rb->item->partner(sp2) == sp3) continue;
					if (rb->item->partner(sp2)->type()->generator(6) == 2) another = TRUE;
				}
				if (another)	// Rule (j) in paper
				{
					forcek = prefs.convertEnergy(2.0, Prefs::KiloCalories);
					n = 3.0;
					eq = 180.0;
				}
				else		// Rule (b) in paper
				{
					forcek = prefs.convertEnergy(1.0, Prefs::KiloCalories);
					n = 6.0;
					eq = 0.0;
				}
			}
			else
			{
				forcek = 0.0;
				n = 1.0;
				eq = 0.0;
			}
			// Set parameters
			newtorsion->params().data[TorsionFunctions::DreidingK] = forcek;
			newtorsion->params().data[TorsionFunctions::DreidingN] = n;
			newtorsion->params().data[TorsionFunctions::DreidingEq] = eq;
			newtorsion->params().data[TorsionFunctions::DreidingEScale] = 1.0;
			newtorsion->params().data[TorsionFunctions::DreidingVScale] = 1.0;
			msg.print(Messenger::Verbose,"Dreiding Torsion (dreiding) : %s-%s-%s-%s - forcek = %8.4f, n = %8.4f, eq = %8.4f\n", ffi->name(), ffj->name(), ffk->name(), ffl->name(), forcek, n, eq);
			break;
	}
	msg.exit("Forcefield::generateTorsion");
	return newtorsion;
}
