/*
	*** String Commands
	*** src/command/string.cpp
	Copyright T. Youngs 2007-2016

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
#include <QRegularExpression>

ATEN_USING_NAMESPACE

// Get part of string before specified character/string
bool Commands::function_AfterStr(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set(c->argc(0).section(c->argc(1), 1));	// ATEN2 TODO Test this
	if (c->hasArg(2) && c->argb(2) && (rv.asString()[0] == '\0')) rv.set(c->argc(0));
	return true;
}

// Convert string to floating point number
bool Commands::function_AToF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	static QRegularExpression re("[\\d.\\-eE]+");
	QRegularExpressionMatch match = re.match(c->argc(0));
	if (match.hasMatch()) rv.set( match.captured(0).toDouble() );
	else
	{
		Messenger::warn("Couldn't convert '%s' to a floating point number.\n", qPrintable(c->argc(0)));
		rv.reset();
	}
	return true;
}

// Convert string to integer number
bool Commands::function_AToI(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	static QRegularExpression re("[\\d.\\-eE]+");
	QRegularExpressionMatch match = re.match(c->argc(0));
	if (match.hasMatch()) rv.set( match.captured(0).toInt() );
	else
	{
		Messenger::warn("Couldn't convert '%s' to an integer number.\n", qPrintable(c->argc(0)));
		rv.reset();
	}
	return true;
}

// Get part of string before specified character
bool Commands::function_BeforeStr(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( c->argc(0).section(c->argc(1), 0, 0) ); // ATEN2 TODO Test this
	if (c->hasArg(2) && c->argb(2) && (rv.asString()[0] == '\0')) rv.set(c->argc(0));
	return true;
}

// Return number of occurrences of string in another string
bool Commands::function_Contains(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set(c->argc(0).count(c->argc(1))); // ATEN2 TODO Test this
	return true;
}

// Convert string to integer number
bool Commands::function_FToA(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( QString::number(c->argd(0)) );
	return true;
}

// Convert string to integer number
bool Commands::function_IToA(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( QString::number(c->argi(0)) );
	return true;
}

// Return lowercase converted string
bool Commands::function_Lowercase(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( c->argc(0).toLower() );
	return true;
}

// Replace characters in supplied string
bool Commands::function_ReplaceChars(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	QString result = c->argc(0);
	for (int n=0; n<c->argc(1).length(); ++n) result = result.replace(c->argc(1).at(n), c->argc(2));
	rv.set(result);
	return true;
}

// Replace substring in supplied string
bool Commands::function_ReplaceStr(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set(c->argc(0).replace(c->argc(1), c->argc(2)));
	return true;
}

// Remove substring from supplied string
bool Commands::function_RemoveStr(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( c->argc(0).remove(c->argc(1)) );
	return true;
}

// Print to string
bool Commands::function_SPrintF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	return function_WriteVariableFormatted(c, obj, rv);
}

// Strip characters from supplied string
bool Commands::function_StripChars(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	QString result = c->argc(0);
	for (int n=0; n<c->argc(1).length(); ++n) result.remove(c->argc(1).at(n));
	return true;
}

// Return substring of supplied string
bool Commands::function_SubStr(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set(c->argc(0).mid(c->argi(1)-1, c->argi(2)));
	return true;
}

// Return string based on supplied format and arguments
bool Commands::function_ToA(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* fmt = c->createFormat(0,1);
	if (fmt == NULL)
	{
		printf("Error - No format defined in 'toa' command.\n");
		return false;
	}
	bool result = fmt->writeToString();
	if (result) rv.set(fmt->string());
	else rv.reset();
	return result;
}

// Return uppercase converted string
bool Commands::function_Uppercase(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( c->argc(0).toUpper() );
	return true;
}
