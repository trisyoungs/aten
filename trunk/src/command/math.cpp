/*
	*** Math Commands
	*** src/command/math.cpp
	Copyright T. Youngs 2007-2013

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
#include "base/constants.h"
#include <math.h>

// Return absolute of argument
bool Command::function_Abs(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( fabs(c->argd(0)) );
	return TRUE;
}

// Return invserse cosine of argument
bool Command::function_ACos(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( acos(c->argd(0)) * DEGRAD );
	return TRUE;
}

// Return invserse sine of argument
bool Command::function_ASin(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( asin(c->argd(0)) * DEGRAD );
	return TRUE;
}

// Return invserse tangent of argument
bool Command::function_ATan(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( atan(c->argd(0)) * DEGRAD );
	return TRUE;
}

// Return cosine of argument (supplied in degrees)
bool Command::function_Cos(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( cos(c->argd(0) / DEGRAD) );
	return TRUE;
}

// Calculate vector dot product
bool Command::function_DotProduct(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Vec3<double> v = c->argv(0);
	rv.set(v.dp(c->argv(1)));
	return TRUE;
}

// Return exponential of of argument
bool Command::function_Exp(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( exp(c->argd(0)) );
	return TRUE;
}

// Return natural logarithm of argument
bool Command::function_Ln(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( log(c->argd(0)) );
	return TRUE;
}

// Return base-10 logarithm of argument
bool Command::function_Log(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( log10(c->argd(0)) );
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
	Vec3<double> v = c->argv(0);
	double mag = v.magAndNormalise();
	rv.set(v);
	c->setArg(0, rv);
	rv.set(mag);
	return TRUE;
}

// Return random real number between 0.0 and 1.0, exclusive of 1.0
bool Command::function_Random(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( AtenMath::random() );
	return TRUE;
}

// Return random integer number between 0 and MAXINT-1 or the maximum value supplied
bool Command::function_Randomi(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) rv.set( AtenMath::randomi( c->argi(0)) );
	else rv.set( AtenMath::randomimax() );
	return TRUE;
}

// Return sine of argument (supplied in degrees)
bool Command::function_Sin(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( sin(c->argd(0) / DEGRAD) );
	return TRUE;
}

// Return square root of argument
bool Command::function_Sqrt(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( sqrt(c->argd(0)) );
	return TRUE;
}

// Return tangent of argument (supplied in degrees)
bool Command::function_Tan(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	rv.set( tan(c->argd(0) / DEGRAD) );
	return TRUE;
}
