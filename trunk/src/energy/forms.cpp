/*
	*** Forcefield term functional forms
	*** src/energy/forms.cpp
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

#include <string.h>
#include "energy/forms.h"

// Electrostatic model
const char *ElecMethodKeywords[Electrostatics::nElectrostatics] = { "none", "coulomb", "ewald", "ewaldauto" };
const char *Electrostatics::elecMethod(Electrostatics::ElecMethod i)
{
	return ElecMethodKeywords[i];
}
Electrostatics::ElecMethod Electrostatics::elecMethod(const char *s)
{
	return (Electrostatics::ElecMethod) enumSearch("electrostatics method", Electrostatics::nElectrostatics, ElecMethodKeywords,s);
}

// VDW potential forms
FunctionData VdwFunctions::VdwFunctions[VdwFunctions::nVdwFunctions] = {
	{ "None", "none",
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null" } },
	{ "Lennard-Jones 12-6", "lj",
		{ "Epsilon", "Sigma" },
		{ "epsilon", "sigma" } },
	{ "Buckingham", "buck",
		{ "A", "B", "C" },
		{ "a", "b", "c" } }
};
VdwFunctions::VdwFunction VdwFunctions::vdwFunction(const char *s)
{
	int i;
	for (i=0; i < VdwFunctions::nVdwFunctions; i++)
		if (strcmp(VdwFunctions::VdwFunctions[i].keyword,s) == 0) break;
	return (VdwFunctions::VdwFunction) i;
}

// Bond potential forms
FunctionData BondFunctions::BondFunctions[BondFunctions::nBondFunctions] = {
	{ "None", "none",
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null" } },
	{ "Harmonic", "harmonic",
		{ "Force K", "Eq. Distance" },
		{ "k", "eq" } },
	{ "Morse", "morse",
		{ "A", "B", "C" },
		{ "a", "b", "c" } }
};
BondFunctions::BondFunction BondFunctions::bondFunction(const char *s)
{
	int i;
	for (i=0; i < BondFunctions::nBondFunctions; i++)
		if (strcmp(BondFunctions::BondFunctions[i].keyword,s) == 0) break;
	return (BondFunctions::BondFunction) i;
}

// Angle potential forms
FunctionData AngleFunctions::AngleFunctions[AngleFunctions::nAngleFunctions] = {
	{ "None", "none",
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null" } },
	{ "Harmonic", "harmonic",
		{ "Force K", "Eq. Distance" },
		{ "k", "eq" } },
	{ "Cosine", "cos",
		{ "Force K", "Periodicity", "Eq. Angle" },
		{ "k", "n", "eq" } },
	{ "UFF Cosine 1", "uffcos1",
		{ "Force K", "Periodicity", "Eq. Angle" },
		{ "k", "n", "eq" } },
	{ "UFF Cosine 2", "uffcos2",
		{ "Force K", "Periodicity", "Eq. Angle" },
		{ "k", "n", "eq" } }
};
AngleFunctions::AngleFunction AngleFunctions::angleFunction(const char *s)
{
	int i;
	for (i=0; i < AngleFunctions::nAngleFunctions; i++)
		if (strcmp(AngleFunctions::AngleFunctions[i].keyword,s) == 0) break;
	return (AngleFunctions::AngleFunction) i;
}

// Torsion potential forms
FunctionData TorsionFunctions::TorsionFunctions[TorsionFunctions::nTorsionFunctions] = {
	{ "None", "none",
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null" } },
	{ "Cosine", "cos",
		{ "EScale", "VScale", "Force K", "Periodicity", "Eq. Angle" },
		{ "escale", "vscale", "k", "n", "eq" } },
	{ "Triple Cosine", "cos3",
		{ "EScale", "VScale", "Force K1", "Force K2", "Force K3" },
		{ "escale", "vscale", "k1", "k2", "k3" } },
	{ "Quadruple Cosine", "cos4",
		{ "EScale", "VScale", "Force K1", "Force K2", "Force K3", "Force K4" },
		{ "escale", "vscale", "k1", "k2", "k3", "k4" } },
	{ "Triple Cosine + Constant", "cos3c",
		{ "EScale", "VScale", "Force K0", "Force K1", "Force K2", "Force K3" },
		{ "escale", "vscale", "k0", "k1", "k2", "k3" } }
};
TorsionFunctions::TorsionFunction TorsionFunctions::torsionFunction(const char *s)
{
	int i;
	for (i=0; i < TorsionFunctions::nTorsionFunctions; i++)
		if (strcmp(TorsionFunctions::TorsionFunctions[i].keyword,s) == 0) break;
	return (TorsionFunctions::TorsionFunction) i;
}
