/*
	*** Energy Commands
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
#include "parser/commandnode.h"
#include "classes/prefs.h"
#include "model/model.h"

// Set electrostatic cutoff radius
bool Command::function_ECut(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setElecCutoff(c->argd(0));
	rv.set(prefs.elecCutoff());
	return TRUE;
}

// Set electrostatic method to use ('elec none|coulomb|ewald|ewaldauto')
bool Command::function_Electrostatics(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.reset();
	Electrostatics::ElecMethod em = Electrostatics::elecMethod(c->argc(0));
	if (em == Electrostatics::nElectrostatics) return FALSE;
	switch (em)
	{
		// Set ewald sum params ('elec ewald <alpha> <kx ky kz>')
		case (Electrostatics::Ewald):
			if (!c->hasArg(4))
			{
				msg.print("Must supply the alpha parameter and kmax vectors to used this electrostatics option.\n");
				return FALSE;
			}
			prefs.setEwaldAlpha(c->argd(1));
			prefs.setEwaldKMax(c->arg3i(2));
			break;
		// Set ewald precision
		case (Electrostatics::EwaldAuto):
			if (!c->hasArg(1))
			{
				msg.print("Must supply the Ewald precision parameter to used this electrostatics option.\n");
				return FALSE;
			}
			prefs.setEwaldPrecision(c->argd(1));
			break;
	}
	// Set method
	prefs.setElectrostaticsMethod(em);
	prefs.setCalculateElec(em == Electrostatics::None ? FALSE : TRUE);
	return TRUE;
}

// Calculate energy of current trajectory frame ('frameenergy')
bool Command::function_FrameEnergy(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	double energy;
	if (obj.m->createExpression()) return FALSE;
	energy = obj.m->totalEnergy(obj.rs);
	rv.set(energy);
	return TRUE;
}

// Calculate energy of current model contents ('modelenergy')
bool Command::function_ModelEnergy(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	double energy;
	if (!obj.m->createExpression()) return FALSE;
	energy = obj.m->totalEnergy(obj.m);
	rv.set(energy);
	return TRUE;
}

// Print out electrostatic decomposition matrix ('printelec')
bool Command::function_PrintElec(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printElecMatrix(obj.rs);
	rv.reset();
	return TRUE;
}

// Print long energy decomposition of model ('printenergy')
bool Command::function_PrintEnergy(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.print();
	rv.reset();
	return TRUE;
}

// Print out Ewald energy decomposition of model ('printewald')
bool Command::function_PrintEwald(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printEwald();
	rv.reset();
	return TRUE;
}

// Print out interpattern decomposition matrix ('printinter')
bool Command::function_PrintInter(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printInterMatrix(obj.rs);
	rv.reset();
	return TRUE;
}

// Print out intramolecular decomposition matrix ('printintra')
bool Command::function_PrintIntra(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printIntraMatrix(obj.rs);
	rv.reset();
	return TRUE;
}

// Print short energy decomposition of model ('printsummary')
bool Command::function_PrintSummary(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printSummary();
	rv.reset();
	return TRUE;
}

// Print out VDW decomposition matrix ('printvdw')
bool Command::function_PrintVdw(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.m->energy.printVdwMatrix(obj.rs);
	rv.reset();
	return TRUE;
}

// Set VDW cutoff radius
bool Command::function_VCut(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setVdwCutoff(c->argd(0));
	rv.set(prefs.vdwCutoff());
	return TRUE;
}

