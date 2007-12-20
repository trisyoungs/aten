/*
	*** Minimiser command functions
	*** src/command/minimise.cpp
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

#include "base/master.h"
#include "command/commands.h"
#include "base/debug.h"
#include "methods/mc.h"
#include "classes/cell.h"

// Local variables
double econverge = 0.001, fconverge = 0.01, linetolerance = 0.0001;


int command_functions::function_CA_CGMINIMISE(command *&c, bundle &obj)
{
}

// Set convergence criteria
int command_functions::function_CA_CONVERGE(command *&c, bundle &obj)
{
	econverge = c->argd(0);
	fconverge = c->argd(1);
	return CR_SUCCESS;
}

// Set line minimiser tolerance
int command_functions::function_CA_LINETOL(command *&c, bundle &obj)
{
	linetolerance = c->argd(0);
	return CR_SUCCESS;
}

// Minimise current model with Monte-Carlo method ('mcminimise <maxsteps>')
int command_functions::function_CA_MCMINIMISE(command *&c, bundle &obj)
{
	master.mc.set_ncycles(c->argi(0));
	master.mc.minimise(obj.m, econverge, fconverge);
	return CR_SUCCESS;
}

// Minimise current model with Steepest Descent method ('sdminimise <maxsteps>')
int command_functions::function_CA_SDMINIMISE(command *&c, bundle &obj)
{
	master.sd.set_tolerance(linetolerance);
	master.sd.set_ncycles(c->argi(0));
	master.sd.minimise(obj.m, econverge, fconverge);
	return CR_SUCCESS;
}

int command_functions::function_CA_SIMPLEXMINIMISE(command *&c, bundle &obj)
{
}
