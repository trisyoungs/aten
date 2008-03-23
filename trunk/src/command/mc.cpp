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
//#include "base/master.h"
#include "base/debug.h"

// Sets acceptance energy for moves ('mc accept <move> <energy>')
int CommandData::function_CA_MCACCEPT(Command *&c, Bundle &obj)
{
	MonteCarloMove mt = MT_from_text(c->argc(0));
	if (mt == MT_NITEMS) return CR_FAIL;
	mc.setAcceptanceEnergy(mt, c->argd(1));
	return CR_SUCCESS;
}

// Sets allowances for moves ('mc allow <move> <on|off>')
int CommandData::function_CA_MCALLOW(Command *&c, Bundle &obj)
{
	MonteCarloMove mt = MT_from_text(c->argc(0));
	if (mt == MT_NITEMS) return CR_FAIL;
	mc.setMoveAllowed(mt, c->argb(1));
	return CR_SUCCESS;
}

// Sets maximum stepsizes for moves ('mc maxstep <move> <stepsize>')
int CommandData::function_CA_MCMAXSTEP(Command *&c, Bundle &obj)
{
	MonteCarloMove mt = MT_from_text(c->argc(0));
	if (mt == MT_NITEMS) return CR_FAIL;
	mc.setMaxStep(mt, c->argd(1));
	return CR_SUCCESS;
}

// Sets ntrials for moves ('mc ntrials <move> <ntrials>')
int CommandData::function_CA_MCNTRIALS(Command *&c, Bundle &obj)
{
	MonteCarloMove mt = MT_from_text(c->argc(0));
	if (mt == MT_NITEMS) return CR_FAIL;
	mc.setNTrials(mt, c->argi(1));
	return CR_SUCCESS;
}

// Prints the current MC params ('printmc')
int CommandData::function_CA_PRINTMC(Command *&c, Bundle &obj)
{
	msg(DM_NONE,"Current Monte Carlo Parameters are:\n");
	msg(DM_NONE,"Move        Allowed  NTrials  MaxStep   EAccept :\n");
	MonteCarloMove mt;
	for (int n=0; n<MT_NITEMS; n++)
	{
		mt = (MonteCarloMove) n;
		msg(DM_NONE,"%11s   %3s   %4i   %8.3f   %8.2e\n", text_from_MT(mt), (mc.isMoveAllowed(mt) ? "Yes" : "No"), mc.nTrials(mt), mc.maxStep(mt), mc.acceptanceEnergy(mt));
	}
	return CR_SUCCESS;
}
