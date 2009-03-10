/*
	*** Energy functions
	*** src/parser/energy.cpp
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
bool NuCommand::function_Frameenergy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	double energy;
	if (obj.m->createExpression()) energy = obj.m->totalEnergy(obj.rs);
	else return FALSE;
	return TRUE;
}

// Calculate energy of current model contents ('modelenergy')
bool NuCommand::function_Modelenergy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	double energy;
	if (obj.m->createExpression()) energy = obj.m->totalEnergy(obj.m);
	else return FALSE;
	return TRUE;
}

// Print out electrostatic decomposition matrix ('printelec')
bool NuCommand::function_Printelec(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printElecMatrix(obj.rs);
	return TRUE;
}

// Print long energy decomposition of model ('printenergy')
bool NuCommand::function_Printenergy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.print();
	return TRUE;
}

// Print out Ewald energy decomposition of model ('printewald')
bool NuCommand::function_Printewald(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printEwald();
	return TRUE;
}

// Print out interpattern decomposition matrix ('printinter')
bool NuCommand::function_Printinter(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printInterMatrix(obj.rs);
	return TRUE;
}

// Print out intramolecular decomposition matrix ('printintra')
bool NuCommand::function_Printintra(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printIntraMatrix(obj.rs);
	return TRUE;
}

// Print short energy decomposition of model ('printsummary')
bool NuCommand::function_Printsummary(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printSummary();
	return TRUE;
}

// Print out VDW decomposition matrix ('printvdw')
bool NuCommand::function_Printvdw(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printVdwMatrix(obj.rs);
	return TRUE;
}
