/*
	*** Energy Commands
	*** src/command/energy.cpp
	Copyright T. Youngs 2007-2017

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
#include "parser/commandnode.h"
#include "model/bundle.h"
#include "model/model.h"
#include "main/aten.h"

ATEN_USING_NAMESPACE

// Set electrostatic method to use ('elec none|coulomb|ewald|ewaldauto')
bool Commands::function_Electrostatics(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.reset();
	Electrostatics::ElecMethod em = Electrostatics::elecMethod(c->argc(0));
	if (em == Electrostatics::nElectrostatics) return false;
	switch (em)
	{
		// Set ewald sum params ('elec ewald <alpha> <kx ky kz>')
		case (Electrostatics::Ewald):
			if (!c->hasArg(4))
			{
				Messenger::print("Must supply the alpha parameter and kmax vectors to used this electrostatics option.");
				return false;
			}
			prefs.setEwaldAlpha(c->argd(1));
			prefs.setEwaldKMax(c->arg3i(2));
			break;
		// Set ewald precision
		case (Electrostatics::EwaldAuto):
			if (!c->hasArg(1))
			{
				Messenger::print("Must supply the Ewald precision parameter to used this electrostatics option.");
				return false;
			}
			prefs.ewaldPrecision().set(c->argd(1));
			break;
		default:
			break;
	}
	// Set method
	prefs.setElectrostaticsMethod(em);
	return true;
}

// Calculate energy of current trajectory frame ('frameenergy')
bool Commands::function_FrameEnergy(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	double energy;
	bool success;

	if (!obj.m->createExpression(Choice(), Choice(), Choice(), aten_.currentForcefield())) return false;
	energy = obj.m->totalEnergy(obj.rs(), success);
	rv.set(energy);

	return success;
}

// Calculate energy of current model contents ('modelenergy')
bool Commands::function_ModelEnergy(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	double energy;
	bool success;
	
	if (!obj.m->createExpression(Choice(), Choice(), Choice(), aten_.currentForcefield())) return false;
	energy = obj.m->totalEnergy(obj.m, success);
	rv.set(energy);

	return success;
}

// Print out electrostatic decomposition matrix ('printelec')
bool Commands::function_PrintElec(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->energy.printElecMatrix(obj.rs());
	rv.reset();
	return true;
}

// Print long energy decomposition of model ('printenergy')
bool Commands::function_PrintEnergy(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->energy.print();
	rv.reset();
	return true;
}

// Print out Ewald energy decomposition of model ('printewald')
bool Commands::function_PrintEwald(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->energy.printEwald();
	rv.reset();
	return true;
}

// Print out interpattern decomposition matrix ('printinter')
bool Commands::function_PrintInter(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->energy.printInterMatrix(obj.rs());
	rv.reset();
	return true;
}

// Print out intramolecular decomposition matrix ('printintra')
bool Commands::function_PrintIntra(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->energy.printIntraMatrix(obj.rs());
	rv.reset();
	return true;
}

// Print short energy decomposition of model ('printsummary')
bool Commands::function_PrintSummary(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->energy.printSummary();
	rv.reset();
	return true;
}

// Print out VDW decomposition matrix ('printvdw')
bool Commands::function_PrintVdw(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.m->energy.printVdwMatrix(obj.rs());
	rv.reset();
	return true;
}
