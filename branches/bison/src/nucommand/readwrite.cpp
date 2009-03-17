/*
	*** Read/write Commands
	*** src/nucommand/readwrite.cpp
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

#include "parser/commandnode.h"
#include "parser/tree.h"
// #include <fstream>
#include <cstring>

// Add file read option
bool NuCommand::function_AddReadOption(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Get parse option from variable
	LineParser::ParseOption po = LineParser::parseOption(c->argc(0));
	if (po != LineParser::nParseOptions) c->parent()->addReadOption(po);
	return TRUE;
}

// Search for line containing specified string
bool NuCommand::function_Find(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check that we are in a filter.
	if (!c->parent()->isFilter())
	{
		msg.print("The 'find' command can only be used from within a Filter.\n");
		return FALSE;
	}
	int result;
	rv.set(0);
	do
	{
		// Get line from file
		int result = c->parent()->parser.readLine();
		if (result != 0) break;
		// Check for string
		if (strstr(c->parent()->parser.line(), c->argc(0)) != '\0')
		{
			rv.set(1);
			// Store the line if a second argument was given
			if (c->hasArg(1))
			{
				NuReturnValue val(c->parent()->parser.line());
				c->setArg(1, val);
			}
		}
	} while (1);
	return TRUE;
}

// Read line from file and return it as result
bool NuCommand::function_GetLine(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check that we are in a filter.
	if (!c->parent()->isFilter())
	{
		msg.print("The 'getline' command can only be used from within a Filter.\n");
		return FALSE;
	}
	int result = c->parent()->parser.readLine();
	NuReturnValue val;
	if (result == 0) val.set(c->parent()->parser.line());
	else val.set("");
	c->setArg(0, val);
	rv.set(result);
	return TRUE;
}

// Read N characters from unformatted file
bool NuCommand::function_ReadChars(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check that we are in a filter.
	if (!c->parent()->isFilter())
	{
		msg.print("The 'readchars' command can only be used from within a Filter.\n");
		return FALSE;
	}
	rv.set( c->parent()->parser.getChars(c->argi(0)) );
	msg.print(Messenger::Commands,"Unformatted char read got '%s'\n", rv.asString());
	return TRUE;
}

// Read integer from unformatted file
bool NuCommand::function_ReadInteger(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check that we are in a filter.
	if (!c->parent()->isFilter())
	{
		msg.print("The 'readinteger' command can only be used from within a Filter.\n");
		return FALSE;
	}
	rv.set( c->parent()->parser.getInteger( c->hasArg(0) ? c->argi(0) : 0 ) );
	msg.print(Messenger::Commands,"Unformatted integer read got '%s'\n", rv.asInteger());
	return TRUE;
}

// Read line and parse with format
bool NuCommand::function_ReadLine(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check that we are in a filter.
	if (!c->parent()->isFilter())
	{
		msg.print("The 'readline' command can only be used from within a Filter.\n");
		return FALSE;
	}
	NuFormat *format = c->format();
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readline'.\n");
		return FALSE;
	}
	rv.set( format->readFormatted( c->parent()->parser, c->parent()->readOptions() ) );
	return TRUE;
}

// Get next whitespace-delimited argument from file
bool NuCommand::function_ReadNext(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check that we are in a filter.
	if (!c->parent()->isFilter())
	{
		msg.print("The 'readnext' command can only be used from within a Filter.\n");
		return FALSE;
	}
	rv.set(c->parent()->parser.getArgDelim(c->parent()->readOptions()));
	return TRUE;
}

// Read real value from unformatted file
bool NuCommand::function_ReadReal(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check that we are in a filter.
	if (!c->parent()->isFilter())
	{
		msg.print("The 'readchars' command can only be used from within a Filter.\n");
		return FALSE;
	}
	rv.set( c->parent()->parser.getReal(c->hasArg(0) ? c->argi(0) : 0) );
	msg.print(Messenger::Commands,"Unformatted real read got '%s'\n", rv.asReal());
	return TRUE;
}

// Parse given variable with format
bool NuCommand::function_ReadVar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	NuFormat *format = c->format();
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readline'.\n");
		return FALSE;
	}
	rv.set( format->readFormatted( c->argc(0), c->parent()->readOptions() ) );
	return TRUE;
}

// Remove file read option
bool NuCommand::function_RemoveReadOption(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Get parse option from variable
	LineParser::ParseOption po = LineParser::parseOption(c->argc(0));
	if (po != LineParser::nParseOptions) c->parent()->removeReadOption(po);
	return TRUE;
}

// Go to start of current file
bool NuCommand::function_Rewind(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check that we are in a filter.
	if (!c->parent()->isFilter())
	{
		msg.print("The 'readchars' command can only be used from within a Filter.\n");
		return FALSE;
	}
	c->parent()->parser.rewind();
	return TRUE;
}

// Discard N characters from unformatted file
bool NuCommand::function_SkipChars(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check that we are in a filter.
	if (!c->parent()->isFilter())
	{
		msg.print("The 'readchars' command can only be used from within a Filter.\n");
		return FALSE;
	}
	c->parent()->parser.getChars(c->argi(0));
	return TRUE;
}

// Skip line(s) of file
bool NuCommand::function_SkipLine(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check that we are in a filter.
	if (!c->parent()->isFilter())
	{
		msg.print("The 'skipline' command can only be used from within a Filter.\n");
		return FALSE;
	}
	c->parent()->parser.skipLines( c->hasArg(0) ? c->argi(0) : 1 );
	return TRUE;
}

// Write line with format
bool NuCommand::function_WriteLine(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// Check that we are in a filter.
	if (!c->parent()->isFilter())
	{
		msg.print("The 'writeline' command can only be used from within a Filter.\n");
		return FALSE;
	}
	// TGAY WRiteline
	return FALSE;
}

// Write line to variable
bool NuCommand::function_WriteVar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
// TGAY	// If the format node in the command is empty create a new (temporary) one
// 	if (c->format() == NULL)
// 	{
// 		// Create format from character variable arg(1)
// 		if (!c->createFormat(c->argc(1), FALSE)) return Command::Fail;
// 		if (c->format()->createString())
// 		{
// 			c->arg(0)->set(c->format()->createdString());
// 			c->deleteFormat();
// 			return TRUE;
// 		}
// 		else return Command::Fail;
// 	}
// 	else
// 	{
// 		if (c->format()->createString())
// 		{
// 			c->arg(0)->set(c->format()->createdString());
// 			return TRUE;
// 		}
// 		else return Command::Fail;
// 	}
	return FALSE;
}
