/*
	*** Variable Commands
	*** src/nucommand/variables.cpp
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

#include "nucommand/commands.h"
#include "parser/commandnode.h"
#include "base/sysfunc.h"

// Get part of string before specified character
bool NuCommand::function_AfterChar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	rv.set( afterChar(c->argc(0), c->argc(1)[0]) );
	return TRUE;
}

// Get part of string before specified character
bool NuCommand::function_BeforeChar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	rv.set( beforeChar(c->argc(0), c->argc(1)[0]) );
	return TRUE;
}

// Normalise vector, returning magnitude
bool NuCommand::function_Normalise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Vec3<double> v = c->arg3d(0);
	double mag = v.magAndNormalise();
	rv.set(v);
	c->setArg(0, rv);
	rv.set(mag);
	return TRUE;
}

// Strip characters from supplied string
bool NuCommand::function_StripChars(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	rv.set( stripChars(c->argc(0), c->argc(1)) );
	return TRUE;
}
