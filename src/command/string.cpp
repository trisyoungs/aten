/*
	*** String Commands
	*** src/command/string.cpp
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
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Get part of string before specified character/string
bool Commands::function_AfterStr(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( afterStr(c->argc(0), c->argc(1)) );
	if (c->hasArg(2) && c->argb(2) && (rv.asString()[0] == '\0')) rv.set(c->argc(0));
	return TRUE;
}

// Convert string to floating point number
bool Commands::function_AToF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( atof(c->argc(0)) );
	return TRUE;
}

// Convert string to integer number
bool Commands::function_AToI(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( atoi(c->argc(0)) );
	return TRUE;
}

// Get part of string before specified character
bool Commands::function_BeforeStr(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( beforeStr(c->argc(0), c->argc(1)) );
	if (c->hasArg(2) && c->argb(2) && (rv.asString()[0] == '\0')) rv.set(c->argc(0));
	return TRUE;
}

// Return number of occurrences of string in another string
bool Commands::function_Contains(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	int count = 0, n;
	int length = strlen(c->argc(1));
	const char* s = c->argc(0), *ch;
	while (*s != '\0')
	{
		ch = strstr(s, c->argc(1));
		if (ch == NULL) break;
		if (*ch != '\0')
		{
			count++;
			for (n=0; n<length; ++n) ++ch;
		}
		s = ch;
	}
	rv.set( count );
	return TRUE;
}

// Convert string to integer number
bool Commands::function_FToA(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( ftoa(c->argd(0)) );
	return TRUE;
}

// Convert string to integer number
bool Commands::function_IToA(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( itoa(c->argi(0)) );
	return TRUE;
}

// Return lowercase converted string
bool Commands::function_Lowercase(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( lowerCase(c->argc(0)) );
	return TRUE;
}

// Replace characters in supplied string
bool Commands::function_ReplaceChars(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( replaceChars(c->argc(0), c->argc(1), c->argc(2)[0]) );
	return TRUE;
}

// Replace substring in supplied string
bool Commands::function_ReplaceStr(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Dnchar newstr(1024);
	const char* s = c->argc(0), *srch;
//	int replacelen = strlen(c->argc(2)), 
	int searchlen = strlen(c->argc(1));
// 	printf("Original [%s], search [%s], replace [%s]\n", s, c->argc(1), c->argc(2));
// 	printf("Strlen = %i\n", replacelen);
	while (s != NULL)
	{
		srch = strstr(s, c->argc(1));
		if (srch == NULL)
		{
// 			printf("No substring match\n");
			newstr.strcat(s);
			s = NULL;
		}
		else
		{
// 			printf("Match at %p, offset is %i\n", srch, srch-s);
			newstr.strcat(s, srch-s);
			newstr.strcat(c->argc(2));
			s = srch+searchlen;
		}
	}
	rv.set( newstr.get() );
	return TRUE;
}

// Remove substring from supplied string
bool Commands::function_RemoveStr(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Dnchar newstr(strlen(c->argc(0)+1));
	const char* s = c->argc(0), *srch;
	int searchlen = strlen(c->argc(1));
// 	printf("Strlen = %i\n", replacelen);
	while (s != NULL)
	{
		srch = strstr(s, c->argc(1));
		if (srch == NULL)
		{
// 			printf("No substring match\n");
			newstr.strcat(s);
			s = NULL;
		}
		else
		{
// 			printf("Match at %p, offset is %i\n", srch, srch-s);
			newstr.strcat(s, srch-s);
			s = srch+searchlen;
// 			if (s != NULL) s++;
		}
	}
	rv.set( newstr.get() );
	return TRUE;
}

// Print to string
bool Commands::function_SPrintF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	return function_WriteVariableFormatted(c, obj, rv);
}

// Strip characters from supplied string
bool Commands::function_StripChars(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( stripChars(c->argc(0), c->argc(1)) );
	return TRUE;
}

// Return substring of supplied string
bool Commands::function_SubStr(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Dnchar result;
	result.substr(c->argc(0), c->argi(1)-1, c->argi(2));
	rv.set( result.get() );
	return TRUE;
}

// Return string based on supplied format and arguments
bool Commands::function_ToA(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* fmt = c->createFormat(0,1);
	if (fmt == NULL)
	{
		printf("Error - No format defined in 'toa' command.\n");
		return FALSE;
	}
	bool result = fmt->writeToString();
	if (result) rv.set(fmt->string());
	else rv.reset();
	return result;
}

// Return uppercase converted string
bool Commands::function_Uppercase(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	rv.set( upperCase(c->argc(0)) );
	return TRUE;
}
