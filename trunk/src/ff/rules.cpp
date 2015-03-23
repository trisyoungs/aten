/*
	*** Specification for rule-based forcefields
	*** src/ff/rules.cpp
	Copyright T. Youngs 2007-2015

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
#include "base/forcefieldatom.h"
#include "base/forcefieldbound.h"

ATEN_USING_NAMESPACE

// Add energy data value to list of those flagged as energies
void Forcefield::addEnergyData(QString s)
{
	energyData_ << s;
}

// Return list of energy data values
QStringList Forcefield::energyData()
{
	return energyData_;
}

// Return pointer to vdw generation function (if one is defined)
Tree* Forcefield::vdwGenerator()
{
	return vdwGenerator_;
}

// Return pointer to bond generation function (if one is defined)
Tree* Forcefield::bondGenerator()
{
	return bondGenerator_;
}

// Return pointer to angle generation function (if one is defined)
Tree* Forcefield::angleGenerator()
{
	return angleGenerator_;
}

// Return pointer to torsion generation function (if one is defined)
Tree* Forcefield::torsionGenerator()
{
	return torsionGenerator_;
}

// Generate VDW params
bool Forcefield::generateVdw(Atom* i)
{
	Messenger::enter("Forcefield::generateVdw");
	// Call the generator function.....
	if (vdwGenerator_ == NULL)
	{
		Messenger::print("Error - Tried to call VDW generator function in forcefield '%s', but it is not defined.", qPrintable(name_));
		Messenger::exit("Forcefield::generateVdw");
		return FALSE;
	}
	// Check atom pointers
	if (i == NULL)
	{
		Messenger::print("Internal Error - NULL atom passed to VDW generator function in forcefield '%s' (pointers: %p).", qPrintable(name_), i);
		Messenger::exit("Forcefield::generateVdw");
		return FALSE;
	}
	// Grab forcefieldatom pointer
	ForcefieldAtom* ffi = i->type();
	// Call the generator function with the necessary arguments
	ReturnValue rv;
	if (!generatorFunctions_.executeFunction("vdwgenerator", rv, "y", ffi))
	{
		Messenger::print("Error - Failed to generate Vdw function data for atom type.");
		ffi->setVdwForm(VdwFunctions::None);
		Messenger::exit("Forcefield::generateVdw");
		return FALSE;
	}
	Messenger::exit("Forcefield::generateVdw");
	return TRUE;
}

// Generate bond params
ForcefieldBound* Forcefield::generateBond(Atom* i, Atom* j)
{
	Messenger::enter("Forcefield::generateBond");
	// Check for presence of the generator function.....
	if (bondGenerator_ == NULL)
	{
		Messenger::print("Error - Tried to call bond generator function in forcefield '%s', but it is not defined.", qPrintable(name_));
		Messenger::exit("Forcefield::generateBond");
		return NULL;
	}
	// Check atom pointers
	if ((i == NULL) || (j == NULL))
	{
		Messenger::print("Internal Error - NULL atom(s) passed to bond generator function in forcefield '%s' (pointers: %p %p).", qPrintable(name_), i, j);
		Messenger::exit("Forcefield::generateBond");
		return NULL;
	}
	// Create new bond and set atom equivalents, but set type to None for now...
	ForcefieldBound* newbond = addBond(BondFunctions::None);
	newbond->setTypeName(0, i->type()->equivalent());
	newbond->setTypeName(1, j->type()->equivalent());
	// Call the generator function with the necessary arguments
	ReturnValue rv;
	if (!generatorFunctions_.executeFunction("bondgenerator", rv, "zaa", newbond, i, j))
	{
		Messenger::print("Error - Failed to generate function data for bond.");
		newbond = NULL;
	}
	Messenger::exit("Forcefield::generateBond");
	return newbond;
}

// Generate angle params
ForcefieldBound* Forcefield::generateAngle(Atom* i, Atom* j, Atom* k)
{
	Messenger::enter("Forcefield::generateAngle");
	// Check for presence of the generator function.....
	if (angleGenerator_ == NULL)
	{
		Messenger::print("Error - Tried to call angle generator function in forcefield '%s', but it is not defined.", qPrintable(name_));
		Messenger::exit("Forcefield::generateAngle");
		return NULL;
	}
	// Check atom pointers
	if ((i == NULL) || (j == NULL) || (k == NULL))
	{
		Messenger::print("Internal Error - NULL atom(s) passed to angle generator function in forcefield '%s' (pointers: %p %p %p).", qPrintable(name_), i, j, k);
		Messenger::exit("Forcefield::generateAngle");
		return NULL;
	}
	// Create new angle and set atom equivalents, but set type to None for now...
	ForcefieldBound* newangle = addAngle(AngleFunctions::None);
	newangle->setTypeName(0, i->type()->equivalent());
	newangle->setTypeName(1, j->type()->equivalent());
	newangle->setTypeName(2, k->type()->equivalent());
	// Call the generator function with the necessary arguments
	ReturnValue rv;
	if (!generatorFunctions_.executeFunction("anglegenerator", rv, "zaaa", newangle, i, j, k))
	{
		Messenger::print("Error - Failed to generate function data for angle.");
		newangle = NULL;
	}
	Messenger::exit("Forcefield::generateAngle");
	return newangle;
}

// Generate torsion params
ForcefieldBound* Forcefield::generateTorsion(Atom* i, Atom* j, Atom* k, Atom* l)
{
	Messenger::enter("Forcefield::generateTorsion");
	// Check for presence of the generator function.....
	if (torsionGenerator_ == NULL)
	{
		Messenger::print("Error - Tried to call torsion generator function in forcefield '%s', but it is not defined.", qPrintable(name_));
		Messenger::exit("Forcefield::generateTorsion");
		return NULL;
	}
	// Check atom pointers
	if ((i == NULL) || (j == NULL) || (k == NULL) || (l == NULL))
	{
		Messenger::print("Internal Error - NULL atom(s) passed to torsion generator function in forcefield '%s' (pointers: %p %p %p %p).", qPrintable(name_), i, j, k, l);
		Messenger::exit("Forcefield::generateTorsion");
		return NULL;
	}
	// Create new torsion and set atom equivalents, but set type to None for now...
	ForcefieldBound* newtorsion = addTorsion(TorsionFunctions::None);
	newtorsion->setTypeName(0, i->type()->equivalent());
	newtorsion->setTypeName(1, j->type()->equivalent());
	newtorsion->setTypeName(2, k->type()->equivalent());
	newtorsion->setTypeName(3, l->type()->equivalent());
	// Call the generator function with the necessary arguments
	ReturnValue rv;
	if (!generatorFunctions_.executeFunction("torsiongenerator", rv, "zaaaa", newtorsion, i, j, k, l))
	{
		Messenger::print("Error - Failed to generate function data for torsion.");
		newtorsion = NULL;
	}
	Messenger::exit("Forcefield::generateTorsion");
	return newtorsion;
}
