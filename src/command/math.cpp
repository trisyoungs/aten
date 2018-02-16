/*
	*** Math Commands
	*** src/command/math.cpp
	Copyright T. Youngs 2007-2018

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

ATEN_USING_NAMESPACE

// Return absolute of argument
bool Commands::function_Abs(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( fabs(c->argd(0)) );
	return true;
}

// Return invserse cosine of argument
bool Commands::function_ACos(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( acos(c->argd(0)) * DEGRAD );
	return true;
}

// Return invserse sine of argument
bool Commands::function_ASin(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( asin(c->argd(0)) * DEGRAD );
	return true;
}

// Return invserse tangent of argument
bool Commands::function_ATan(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( atan(c->argd(0)) * DEGRAD );
	return true;
}

// Return cosine of argument (supplied in degrees)
bool Commands::function_Cos(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( cos(c->argd(0) / DEGRAD) );
	return true;
}

// Calculate vector dot product
bool Commands::function_DotProduct(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Vec3<double> v = c->argv(0);
	rv.set(v.dp(c->argv(1)));
	return true;
}

// Return exponential of of argument
bool Commands::function_Exp(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( exp(c->argd(0)) );
	return true;
}

// Return natural logarithm of argument
bool Commands::function_Ln(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( log(c->argd(0)) );
	return true;
}

// Return base-10 logarithm of argument
bool Commands::function_Log(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( log10(c->argd(0)) );
	return true;
}

// Round real value to nearest integer
bool Commands::function_Nint(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( floor(c->argd(0) + 0.5) );
	return true;
}

// Normalise vector, returning magnitude
bool Commands::function_Normalise(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Vec3<double> v = c->argv(0);
	double mag = v.magAndNormalise();
	rv.set(v);
	c->setArg(0, rv);
	rv.set(mag);
	return true;
}

// Return random real number between 0.0 and 1.0, exclusive of 1.0
bool Commands::function_Random(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( AtenMath::random() );
	return true;
}

// Return random integer number between 0 and MAXINT-1 or the maximum value supplied
bool Commands::function_Randomi(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (c->hasArg(0)) rv.set( AtenMath::randomi( c->argi(0)) );
	else rv.set( AtenMath::randomimax() );
	return true;
}

// Return sine of argument (supplied in degrees)
bool Commands::function_Sin(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( sin(c->argd(0) / DEGRAD) );
	return true;
}

// Return square root of argument
bool Commands::function_Sqrt(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( sqrt(c->argd(0)) );
	return true;
}

// Return tangent of argument (supplied in degrees)
bool Commands::function_Tan(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( tan(c->argd(0) / DEGRAD) );
	return true;
}
