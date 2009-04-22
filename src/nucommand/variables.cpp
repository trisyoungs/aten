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
bool Command::function_AfterChar(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( afterChar(c->argc(0), c->argc(1)[0]) );
	return TRUE;
}

// Convert string to floating point number
bool Command::function_AToF(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( atof(c->argc(0)) );
	return TRUE;
}

// Convert string to integer number
bool Command::function_AToI(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( atoi(c->argc(0)) );
	return TRUE;
}

// Get part of string before specified character
bool Command::function_BeforeChar(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( beforeChar(c->argc(0), c->argc(1)[0]) );
	return TRUE;
}

// Return number of occurrences of string in another string
bool Command::function_Contains(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	int count = 0;
	const char *ch, *s = c->argc(0);
	while (*s != '\0')
	{
		ch = strstr(s, c->argc(1));
		if (*ch != '\0') count++;
		s = ch;
	}
	rv.set( count );
	return TRUE;
}

// Convert string to integer number
bool Command::function_FToA(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( ftoa(c->argd(0)) );
	return TRUE;
}

// Convert string to integer number
bool Command::function_IToA(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( itoa(c->argi(0)) );
	return TRUE;
}

// Round real value to nearest integer
bool Command::function_Nint(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( floor(c->argd(0) + 0.5) );
	return TRUE;
}

// Normalise vector, returning magnitude
bool Command::function_Normalise(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Vec3<double> v = c->arg3d(0);
	double mag = v.magAndNormalise();
	rv.set(v);
	c->setArg(0, rv);
	rv.set(mag);
	return TRUE;
}

// Strip characters from supplied string
bool Command::function_StripChars(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( stripChars(c->argc(0), c->argc(1)) );
	printf("Original/Stripped strings are '%s'/'%s'\n", c->argc(0), rv.asString());
	return TRUE;
}
