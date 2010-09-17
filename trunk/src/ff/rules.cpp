/*
	*** Specification for rule-based forcefields
	*** src/ff/rules.cpp
	Copyright T. Youngs 2007-2010

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

#include "ff/forcefield.h"
#include "ff/forms.h"
#include "classes/forcefieldatom.h"
#include "classes/forcefieldbound.h"

// References
// UFF:
// DREIDING: A Generic Force Field for Molecular Simulations
//	Stephen L. Mayo, Barry D. Olafson, and William A. Goddard III
//	J. Phys. Chem. 1990, 94, 8897-8909

// Set conversion flag for energetic generator data
void Forcefield::setEnergyGenerator(int n)
{
	if ((n < 0) || (n > MAXFFGENDATA)) msg.print("Index %i is out of range for generator data.\n", n);
	else energyGenerators_[n] = TRUE;
}

// Return energy generator array
bool *Forcefield::energyGenerators()
{
	return energyGenerators_;
}

// Return pointer to vdw generation function (if one is defined)
Tree *Forcefield::vdwGenerator()
{
	return vdwGenerator_;
}

// Return pointer to bond generation function (if one is defined)
Tree *Forcefield::bondGenerator()
{
	return bondGenerator_;
}

// Return pointer to angle generation function (if one is defined)
Tree *Forcefield::angleGenerator()
{
	return angleGenerator_;
}

// Return pointer to torsion generation function (if one is defined)
Tree *Forcefield::torsionGenerator()
{
	return torsionGenerator_;
}

// Generate VDW params
bool Forcefield::generateVdw(Atom *i)
{
	// Simplest of all generation routines - creates the params() data for VDW interactions.
	msg.enter("Forcefield::generateVdw");
	double sigma, epsilon, r0, d0;
	ForcefieldAtom *ffi = i->type();
	// Call the generator function.....
	if (vdwGenerator_ == NULL)
	{
		msg.print("Error - Tried to call VDW generator function in forcefield '%s', but it is not defined.\n", name_.get());
		msg.exit("Forcefield::generateVdw");
		return FALSE;
	}
// 	switch (rules_)
// 	{
// 		case (Rules::None):
// 			msg.print("Error - tried to generate VDW parameters for a forcefield that has no rules.\n");
// 			break;
// 		case (Rules::Uff):
// 			// UFF VDW types are just the third [2] and fourth [3] data (for simple LJ)
// 			epsilon = ffi->generator(3);
// 			sigma = ffi->generator(2);
// 			ffi->setVdwForm(VdwFunctions::Lj);
// 			ffi->setParameter(VdwFunctions::LjEpsilon, epsilon * 0.25);
// 			ffi->setParameter(VdwFunctions::LjSigma, sigma);
// 			ffi->setParameter(VdwFunctions::LjN, 2.0);
// 			msg.print(Messenger::Verbose,"UFF LJ    : sigma, epsilon, n = %8.4f %8.4f 2.0\n", sigma, epsilon);
// 			break;
// 		case (Rules::DreidingLJ):
// 			r0 = ffi->generator(2);
// 			d0 = ffi->generator(3);
// 			ffi->setVdwForm(VdwFunctions::LjAB);
// 			ffi->setParameter(VdwFunctions::LjA, d0 * pow(r0,12.0));
// 			ffi->setParameter(VdwFunctions::LjB, 2.0 * d0 * pow(r0,6.0));
// 			msg.print(Messenger::Verbose,"Dreiding LJ (ljab) : A, B, %8.4f %8.4f\n", ffi->parameter(VdwFunctions::LjA), ffi->parameter(VdwFunctions::LjB));
// 			break;
// 		case (Rules::DreidingX6):
// 			break;
// 	}
	msg.exit("Forcefield::generateVdw");
}

// Generate bond params
ForcefieldBound *Forcefield::generateBond(Atom *i, Atom *j)
{
	msg.enter("Forcefield::generateBond");
	// Check for presence of the generator function.....
	if (bondGenerator_ == NULL)
	{
		msg.print("Error - Tried to call bond generator function in forcefield '%s', but it is not defined.\n", name_.get());
		msg.exit("Forcefield::generateBond");
		return NULL;
	}
	// Create new bond and set type to None for now...
	ForcefieldBound *newbond = addBond(BondFunctions::None);
	// Call the generator function with the necessary 
	ReturnValue rv;
	if (!generatorFunctions_.executeGlobalFunction("generatebond", rv, "zaa", newbond, i, j))
	{
		msg.print("Error - Failed to generate function data for bond.\n");
		newbond = NULL;
	}
	msg.exit("Forcefield::generateBond");
	return newbond;
}

// Generate angle params
ForcefieldBound *Forcefield::generateAngle(Atom *i, Atom *j, Atom *k)
{
	msg.enter("Forcefield::generateAngle");
	// Check for presence of the generator function.....
	if (angleGenerator_ == NULL)
	{
		msg.print("Error - Tried to call angle generator function in forcefield '%s', but it is not defined.\n", name_.get());
		msg.exit("Forcefield::generateAngle");
		return NULL;
	}
	// Create new bond and set type to None for now...
	ForcefieldBound *newangle = addAngle(AngleFunctions::None);
	// Call the generator function with the necessary 
	ReturnValue rv;
	if (!generatorFunctions_.executeGlobalFunction("generateangle", rv, "zaaa", newangle, i, j, k))
	{
		msg.print("Error - Failed to generate function data for angle.\n");
		newangle = NULL;
	}
	msg.exit("Forcefield::generateAngle");
	return newangle;
}

// Generate torsion params
ForcefieldBound *Forcefield::generateTorsion(Atom *i, Atom *j, Atom *k, Atom *l)
{
	msg.enter("Forcefield::generateTorsion");
	// Check for presence of the generator function.....
	if (torsionGenerator_ == NULL)
	{
		msg.print("Error - Tried to call torsion generator function in forcefield '%s', but it is not defined.\n", name_.get());
		msg.exit("Forcefield::generateTorsion");
		return NULL;
	}
	// Create new bond and set type to None for now...
	ForcefieldBound *newtorsion = addTorsion(TorsionFunctions::None);
	// Call the generator function with the necessary 
	ReturnValue rv;
	if (!generatorFunctions_.executeGlobalFunction("generatetorsion", rv, "zaaa", newtorsion, i, j, k))
	{
		msg.print("Error - Failed to generate function data for torsion.\n");
		newtorsion = NULL;
	}
	msg.exit("Forcefield::generateTorsion");
	return newtorsion;
}
