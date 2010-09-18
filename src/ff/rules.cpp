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

// Add energy data value to list of those flagged as energies
void Forcefield::addEnergyData(const char *s)
{
	Dnchar *newdata = energyData_.add();
	newdata->set(s);
}

// Return list of energy data values
Dnchar *Forcefield::energyData()
{
	return energyData_.first();
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
	msg.enter("Forcefield::generateVdw");
	// Call the generator function.....
	if (vdwGenerator_ == NULL)
	{
		msg.print("Error - Tried to call VDW generator function in forcefield '%s', but it is not defined.\n", name_.get());
		msg.exit("Forcefield::generateVdw");
		return FALSE;
	}
	// Check atom pointers
	if (i == NULL)
	{
		msg.print("Internal Error - NULL atom passed to VDW generator function in forcefield '%s' (pointers: %p).\n", name_.get(), i);
		msg.exit("Forcefield::generateBond");
		return FALSE;
	}
	// Grab forcefieldatom pointer
	ForcefieldAtom *ffi = i->type();
	// Call the generator function with the necessary arguments
	ReturnValue rv;
	if (!generatorFunctions_.executeGlobalFunction("generatevdw", rv, "y", ffi))
	{
		msg.print("Error - Failed to generate Vdw function data for atom type.\n");
		ffi->setVdwForm(VdwFunctions::None);
		msg.exit("Forcefield::generateVdw");
		return FALSE;
	}
	msg.exit("Forcefield::generateVdw");
	return TRUE;
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
	// Check atom pointers
	if ((i == NULL) || (j == NULL))
	{
		msg.print("Internal Error - NULL atom(s) passed to bond generator function in forcefield '%s' (pointers: %p %p).\n", name_.get(), i, j);
		msg.exit("Forcefield::generateBond");
		return NULL;
	}
	// Create new bond and set atom equivalents, but set type to None for now...
	ForcefieldBound *newbond = addBond(BondFunctions::None);
	newbond->setTypeName(0, i->type()->equivalent());
	newbond->setTypeName(1, j->type()->equivalent());
	// Call the generator function with the necessary arguments
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
	// Check atom pointers
	if ((i == NULL) || (j == NULL) || (k == NULL))
	{
		msg.print("Internal Error - NULL atom(s) passed to angle generator function in forcefield '%s' (pointers: %p %p %p).\n", name_.get(), i, j, k);
		msg.exit("Forcefield::generateAngle");
		return NULL;
	}
	// Create new angle and set atom equivalents, but set type to None for now...
	ForcefieldBound *newangle = addAngle(AngleFunctions::None);
	newangle->setTypeName(0, i->type()->equivalent());
	newangle->setTypeName(1, j->type()->equivalent());
	newangle->setTypeName(2, k->type()->equivalent());
	// Call the generator function with the necessary arguments
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
	// Check atom pointers
	if ((i == NULL) || (j == NULL) || (k == NULL) || (l == NULL))
	{
		msg.print("Internal Error - NULL atom(s) passed to torsion generator function in forcefield '%s' (pointers: %p %p %p %p).\n", name_.get(), i, j, k, l);
		msg.exit("Forcefield::generateTorsion");
		return NULL;
	}
	// Create new torsion and set atom equivalents, but set type to None for now...
	ForcefieldBound *newtorsion = addTorsion(TorsionFunctions::None);
	newtorsion->setTypeName(0, i->type()->equivalent());
	newtorsion->setTypeName(1, j->type()->equivalent());
	newtorsion->setTypeName(2, k->type()->equivalent());
	newtorsion->setTypeName(3, l->type()->equivalent());
	// Call the generator function with the necessary arguments
	ReturnValue rv;
	if (!generatorFunctions_.executeGlobalFunction("generatetorsion", rv, "zaaaa", newtorsion, i, j, k, l))
	{
		msg.print("Error - Failed to generate function data for torsion.\n");
		newtorsion = NULL;
	}
	msg.exit("Forcefield::generateTorsion");
	return newtorsion;
}
