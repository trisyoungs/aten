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

#include "classes/atom.h"
#include "classes/forcefield.h"
#include "energy/forms.h"
#include "base/constants.h"

// Generate VDW params
void Forcefield::generateVdw(Atom *i)
{
	// Simplest of all generation routines - creates the params() data for VDW interactions.
	dbgBegin(Debug::Calls,"Forcefield::generateVdw");
	double sigma, epsilon;
	ForcefieldAtom *ffi = i->type();
	switch (rules_)
	{
		case (Forms::NoRules):
			msg(Debug::None,"Forcefield::generateVdw <<<< Tried to generate parameters for a NORULES FF >>>>\n");
			break;
		case (Forms::UffRules):
			// UFF VDW types are just the third [2] and fourth [3] data (for simple LJ)
			epsilon = ffi->generator(3);
			sigma = ffi->generator(2);
			ffi->params().data[Forms::LjVdwEpsilon] = epsilon;
			ffi->params().data[Forms::LjVdwSigma] = sigma;
			msg(Debug::Verbose,"UFF LJ    : sigma, epsilon = %8.4f %8.4f\n", sigma, epsilon);
			ffi->setVdwForm(Forms::LjVdw);
			break;
	}
	dbgEnd(Debug::Calls,"Forcefield::generateVdw");
}

// Generate bond params
ForcefieldBound *Forcefield::generateBond(Atom *i, Atom *j)
{
	// Creates bond forcefield data for the specified atom types.
	// No check is performed to see if similar data has already been generated.
	dbgBegin(Debug::Calls,"Forcefield::generateBond");
	ForcefieldAtom *ffi = i->type();
	ForcefieldAtom *ffj = j->type();
	ForcefieldBound *newbond = NULL;
	switch (rules_)
	{
		case (Forms::NoRules):
			msg(Debug::None,"Forcefield::generateBond <<<< Tried to generate parameters for a NORULES FF >>>>\n");
			break;
		case (Forms::UffRules):
			// UFF Harmonic Bond Generator
			// rij : Equilibrium distance : = ri + rj + rBO - rEN
			// rBO : Bond-order correction = -0.1332 * (ri + rj) * ln(n)
			// rEN : Electronegativity correction : ri*rj * (sqrt(Xi)-sqrt(Xj))**2 / (Xi*ri + Xj*rj)
			double ri = ffi->generator(0);
			double rj = ffj->generator(0);
			double sumr = ri + rj;
			double chii = ffi->generator(6);
			double chij = ffj->generator(6);
			double rBO = -0.1332 * sumr * log(i->bondOrder(j));
			double chi = (sqrt(chii) - sqrt(chij));
			double rEN = ri * rj * chi * chi / (chii*ri + chij*rj);
			double Zi = ffi->generator(5);
			double Zj = ffj->generator(5);
			// Create new bond definition in the forcefield space and set its parameters
			newbond = bonds_.add();
			newbond->setBondStyle(Forms::HarmonicBond);
			newbond->params().data[Forms::HarmonicBondEq] = sumr + rBO - rEN;
			newbond->params().data[Forms::HarmonicBondK] = 664.12 * ( (Zi * Zj) / (sumr + sumr + sumr) );
			msg(Debug::Verbose,"UFF Bond  : eq, k = %8.4f %8.4f\n", newbond->params().data[Forms::HarmonicBondEq], newbond->params().data[Forms::HarmonicBondK]);
			break;
	}
	dbgEnd(Debug::Calls,"Forcefield::generateBond");
	return newbond;
}

// Generate angle params
ForcefieldBound *Forcefield::generateAngle(Atom *i, Atom *j, Atom *k)
{
	// Creates angle forcefield data for the specified atom types.
	// No check is performed to see if similar data has already been generated.
	dbgBegin(Debug::Calls,"Forcefield::generateAngle");
	ForcefieldAtom *ffi = i->type();
	ForcefieldAtom *ffj = j->type();
	ForcefieldAtom *ffk = k->type();
	ForcefieldBound *newangle = NULL;
	switch (rules_)
	{
		case (Forms::NoRules):
			msg(Debug::None,"Forcefield::generateAngle <<<< Tried to generate parameters for a NORULES FF >>>>\n");
			break;
		case (Forms::UffRules):
			// UFF Cosine Angle Generator
			// U(theta) = (k / (n*n)) * (1 + s*cos(n*theta))
			// rik2 = rij**2 + rjk**2 - 2 * rij * rjk * cos(eq)
			// k = beta (Zi * Zk / rik**5) * rij * rjk * (rij * rjk * (1-cos**2(eq)) - rik**2 * cos(eq))
			double ri, rj, rk, rij, rjk, rBO, rEN, chii, chij, chik, chi, rik2, rik5, Zi, Zk, beta, forcek;
			int n;
			newangle = angles_.add();
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
			double eq = ffj->generator(1) / DEGRAD;
			rik2 = rij * rij + rjk * rjk - 2.0 * ( rij * rjk * cos(eq));
			rik5 = rik2 * rik2 * sqrt(rik2);
			// Determine k
			beta = 664.12 / (rij * rjk);
			forcek = beta * (Zi * Zk / rik5) * rij * rjk;
			forcek = forcek * (3.0 * rij * rjk * (1.0 - cos(eq)*cos(eq)) - rik2 * cos(eq));
			//printf("          : eq, rik2, rik5, beta, forcek = %8.4f %8.4f %8.4f %8.4f %8.4f\n",eq,rik2,rik5,beta,forcek);
			// Store vars in forcefield node
			newangle->params().data[Forms::UffCosineAngleK] = forcek;
			newangle->params().data[Forms::UffCosineAngleEq] = ffj->generator(1);
			// Determine 'n' based on the geometry of the central atom 'j'
			if (ffj->generator(1) > 170.0) n = 1;
			else if (ffj->generator(1) > 115.0) n = 3;
			else if (ffj->generator(1) > 95.0) n = 2;
			else n = 4;
			newangle->params().data[Forms::UffCosineAngleN] = n;
			// Set function style
			if (n == 2) newangle->setAngleStyle(Forms::UffCosine2Angle);
			else newangle->setAngleStyle(Forms::UffCosine1Angle);
			msg(Debug::Verbose,"UFF Angle : %s-%s-%s - forcek = %8.4f, eq = %8.4f, n = %i\n", ffi->name(), ffj->name(), ffk->name(), forcek, eq, n);

			break;
	}
	dbgEnd(Debug::Calls,"Forcefield::generateAngle");
	return newangle;
}

// Generate torsion params
ForcefieldBound *Forcefield::generateTorsion(Atom *i, Atom *j, Atom *k, Atom *l)
{
	// Creates torsion forcefield data for the specified atom types.
	// No check is performed to see if similar data has already been generated.
	dbgBegin(Debug::Calls,"Forcefield::generateTorsion");
	ForcefieldBound *newtorsion = NULL;
	switch (rules_)
	{
		case (Forms::NoRules):
			msg(Debug::None,"Forcefield::generateTorsion <<<< Tried to generate parameters for a NORULES FF >>>>\n");
			break;
		case (Forms::UffRules):
			// UFF Torsions  TODO

			newtorsion = torsions_.add();
			break;
	}
	dbgEnd(Debug::Calls,"Forcefield::generateTorsion");
	return newtorsion;
}
