/*
	*** Monte Carlo Commands
	*** src/command/mc.cpp
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

#include "command/commands.h"
#include "parser/commandnode.h"
#include "methods/mc.h"
#include "base/messenger.h"

// Prints the current MC params ('printmc')
bool Command::function_PrintMC(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	msg.print("Current Monte Carlo Parameters are:\n");
	msg.print("Move        Allowed  NTrials  MaxStep   EAccept :\n");
	MonteCarlo::MoveType mt;
	for (int n=0; n<MonteCarlo::nMoveTypes; n++)
	{
		mt = (MonteCarlo::MoveType) n;
		msg.print("%11s   %3s   %4i   %8.3f   %8.2e\n", MonteCarlo::moveTypeKeyword(mt), (mc.isMoveAllowed(mt) ? "Yes" : "No"), mc.nTrials(mt), mc.maxStep(mt), mc.acceptanceEnergy(mt));
	}
	rv.reset();
	return TRUE;
}

