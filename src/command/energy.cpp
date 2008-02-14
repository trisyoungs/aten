/*
	*** Energy command functions
	*** src/command/energy.cpp
	Copyright T. Youngs 2007

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
#include "command/functions.h"
#include "base/prefs.h"
#include "base/debug.h"
#include "model/model.h"

// Calculate energy of current trajectory frame ('frameenergy')
int commanddata::function_CA_FRAMEENERGY(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	double energy;
	model *frame = obj.m->get_currentframe();
	if (obj.m->create_expression()) energy = obj.m->total_energy(frame);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Calculate energy of current model contents ('modelenergy')
int commanddata::function_CA_MODELENERGY(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	double energy;
	if (obj.m->create_expression()) energy = obj.m->total_energy(obj.m);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Print out electrostatic decomposition matrix ('printelec')
int commanddata::function_CA_PRINTELEC(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->energy.print_elecmatrix(obj.m);
	return CR_SUCCESS;
}

// Print long energy decomposition of model ('printenergy')
int commanddata::function_CA_PRINTENERGY(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->energy.print();
	return CR_SUCCESS;
}

// Print out Ewald energy decomposition of model ('printewald')
int commanddata::function_CA_PRINTEWALD(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->energy.print_ewald();
	return CR_SUCCESS;
}

// Print out interpattern decomposition matrix ('printinter')
int commanddata::function_CA_PRINTINTER(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->energy.print_intermatrix(obj.m);
	return CR_SUCCESS;
}

// Print out intramolecular decomposition matrix ('printintra')
int commanddata::function_CA_PRINTINTRA(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->energy.print_intramatrix(obj.m);
	return CR_SUCCESS;
}

// Print short energy decomposition of model ('printsummary')
int commanddata::function_CA_PRINTSUMMARY(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->energy.print_summary();
	return CR_SUCCESS;
}

// Print out VDW decomposition matrix ('printvdw')
int commanddata::function_CA_PRINTVDW(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->energy.print_vdwmatrix(obj.m);
	return CR_SUCCESS;
}
