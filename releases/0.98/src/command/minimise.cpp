/*
	*** Minimiser command functions
	*** src/command/minimise.cpp
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
#include "methods/sd.h"
#include "methods/mc.h"

// Local variables
double econverge = 0.001, fconverge = 0.01, linetolerance = 0.0001;

// Minimise with conjugate gradient
int CommandData::function_CA_CGMINIMISE(Command *&c, Bundle &obj)
{
	return CR_FAIL;
}

// Set convergence criteria
int CommandData::function_CA_CONVERGE(Command *&c, Bundle &obj)
{
	econverge = c->argd(0);
	fconverge = c->argd(1);
	return CR_SUCCESS;
}

// Set line minimiser tolerance
int CommandData::function_CA_LINETOL(Command *&c, Bundle &obj)
{
	linetolerance = c->argd(0);
	return CR_SUCCESS;
}

// Minimise current model with Monte-Carlo method ('mcminimise <maxsteps>')
int CommandData::function_CA_MCMINIMISE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	mc.setNCycles(c->argi(0));
	mc.minimise(obj.m, econverge, fconverge);
	return CR_SUCCESS;
}

// Minimise current model with Steepest Descent method ('sdminimise <maxsteps>')
int CommandData::function_CA_SDMINIMISE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	sd.setTolerance(linetolerance);
	sd.setNCycles(c->argi(0));
	sd.minimise(obj.m, econverge, fconverge);
	return CR_SUCCESS;
}

int CommandData::function_CA_SIMPLEXMINIMISE(Command *&c, Bundle &obj)
{
	return CR_FAIL;
}
