/*
	*** Energy command functions
	*** src/command/energy.cpp
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

#include "command/commands.h"
#include "base/prefs.h"
#include "base/messenger.h"
#include "model/model.h"

// Calculate energy of current trajectory frame ('frameenergy')
int CommandData::function_CA_FRAMEENERGY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	double energy;
	if (obj.m->createExpression()) energy = obj.m->totalEnergy(obj.rs);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Calculate energy of current model contents ('modelenergy')
int CommandData::function_CA_MODELENERGY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	double energy;
	if (obj.m->createExpression()) energy = obj.m->totalEnergy(obj.m);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Print out electrostatic decomposition matrix ('printelec')
int CommandData::function_CA_PRINTELEC(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->energy.printElecMatrix(obj.rs);
	return CR_SUCCESS;
}

// Print long energy decomposition of model ('printenergy')
int CommandData::function_CA_PRINTENERGY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->energy.print();
	return CR_SUCCESS;
}

// Print out Ewald energy decomposition of model ('printewald')
int CommandData::function_CA_PRINTEWALD(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->energy.printEwald();
	return CR_SUCCESS;
}

// Print out interpattern decomposition matrix ('printinter')
int CommandData::function_CA_PRINTINTER(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->energy.printInterMatrix(obj.rs);
	return CR_SUCCESS;
}

// Print out intramolecular decomposition matrix ('printintra')
int CommandData::function_CA_PRINTINTRA(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->energy.printIntraMatrix(obj.rs);
	return CR_SUCCESS;
}

// Print short energy decomposition of model ('printsummary')
int CommandData::function_CA_PRINTSUMMARY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->energy.printSummary();
	return CR_SUCCESS;
}

// Print out VDW decomposition matrix ('printvdw')
int CommandData::function_CA_PRINTVDW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->energy.printVdwMatrix(obj.rs);
	return CR_SUCCESS;
}
