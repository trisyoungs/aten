/*
	*** Monte Carlo command functions
	*** src/command/mc.cpp
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

#include "command/commandlist.h"
#include "methods/mc.h"
#include "base/messenger.h"

// Sets acceptance energy for moves ('mc accept <move> <energy>')
int Command::function_CA_MCACCEPT(CommandNode *&c, Bundle &obj)
{
	MonteCarlo::MoveType mt = MonteCarlo::moveType(c->argc(0));
	if (mt == MonteCarlo::nMoveTypes) return Command::Fail;
	mc.setAcceptanceEnergy(mt, c->argd(1));
	return Command::Success;
}

// Sets allowances for moves ('mc allow <move> <on|off>')
int Command::function_CA_MCALLOW(CommandNode *&c, Bundle &obj)
{
	MonteCarlo::MoveType mt = MonteCarlo::moveType(c->argc(0));
	if (mt == MonteCarlo::nMoveTypes) return Command::Fail;
	mc.setMoveAllowed(mt, c->argb(1));
	return Command::Success;
}

// Sets maximum stepsizes for moves ('mc maxstep <move> <stepsize>')
int Command::function_CA_MCMAXSTEP(CommandNode *&c, Bundle &obj)
{
	MonteCarlo::MoveType mt = MonteCarlo::moveType(c->argc(0));
	if (mt == MonteCarlo::nMoveTypes) return Command::Fail;
	mc.setMaxStep(mt, c->argd(1));
	return Command::Success;
}

// Sets ntrials for moves ('mc ntrials <move> <ntrials>')
int Command::function_CA_MCNTRIALS(CommandNode *&c, Bundle &obj)
{
	MonteCarlo::MoveType mt = MonteCarlo::moveType(c->argc(0));
	if (mt == MonteCarlo::nMoveTypes) return Command::Fail;
	mc.setNTrials(mt, c->argi(1));
	return Command::Success;
}

// Prints the current MC params ('printmc')
int Command::function_CA_PRINTMC(CommandNode *&c, Bundle &obj)
{
	msg.print("Current Monte Carlo Parameters are:\n");
	msg.print("Move        Allowed  NTrials  MaxStep   EAccept :\n");
	MonteCarlo::MoveType mt;
	for (int n=0; n<MonteCarlo::nMoveTypes; n++)
	{
		mt = (MonteCarlo::MoveType) n;
		msg.print("%11s   %3s   %4i   %8.3f   %8.2e\n", MonteCarlo::moveTypeKeyword(mt), (mc.isMoveAllowed(mt) ? "Yes" : "No"), mc.nTrials(mt), mc.maxStep(mt), mc.acceptanceEnergy(mt));
	}
	return Command::Success;
}
