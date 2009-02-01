/*
	*** Energy command functions
	*** src/command/energy.cpp
	Copyright T. Youngs 2007-2009

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
#include "model/model.h"

// Calculate energy of current trajectory frame ('frameenergy')
int Command::function_CA_FRAMEENERGY(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	double energy;
	if (obj.m->createExpression()) energy = obj.m->totalEnergy(obj.rs);
	else return Command::Fail;
	return Command::Success;
}

// Calculate energy of current model contents ('modelenergy')
int Command::function_CA_MODELENERGY(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	double energy;
	if (obj.m->createExpression()) energy = obj.m->totalEnergy(obj.m);
	else return Command::Fail;
	return Command::Success;
}

// Print out electrostatic decomposition matrix ('printelec')
int Command::function_CA_PRINTELEC(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->energy.printElecMatrix(obj.rs);
	return Command::Success;
}

// Print long energy decomposition of model ('printenergy')
int Command::function_CA_PRINTENERGY(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->energy.print();
	return Command::Success;
}

// Print out Ewald energy decomposition of model ('printewald')
int Command::function_CA_PRINTEWALD(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->energy.printEwald();
	return Command::Success;
}

// Print out interpattern decomposition matrix ('printinter')
int Command::function_CA_PRINTINTER(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->energy.printInterMatrix(obj.rs);
	return Command::Success;
}

// Print out intramolecular decomposition matrix ('printintra')
int Command::function_CA_PRINTINTRA(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->energy.printIntraMatrix(obj.rs);
	return Command::Success;
}

// Print short energy decomposition of model ('printsummary')
int Command::function_CA_PRINTSUMMARY(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->energy.printSummary();
	return Command::Success;
}

// Print out VDW decomposition matrix ('printvdw')
int Command::function_CA_PRINTVDW(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.m->energy.printVdwMatrix(obj.rs);
	return Command::Success;
}
