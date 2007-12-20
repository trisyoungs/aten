/*
	*** Messaging command functions
	*** src/command/transform.cpp
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

#include "command/commandlist.h"
#include "model/model.h"
#include "classes/atom.h"

// Print formatted string
int command_functions::function_CA_PRINT(command *&c, bundle &obj)
{
	// Go through supplied string, converting variables as we go
	printstr.create_empty(512);
	strcpy(srcstr, fn->argc(0));
	for (c = srcstr; *c != '\0'; c++)
	{
		// If the character is not '$', just add it to printstr
		if (*c != '$')
		{
			printstr += *c;
			continue;
		}
		// This is the start of a variable format
		// Clear the variable string and skip past the '$'
		var.create_empty(64);
		var += '$';
		c++;
		// Add characters to 'var' until we find the end of the format
		// Demand that vars are formatted as ${name[@format]}, or terminated by a space
		vardone = FALSE;
		while (*c != ' ')
		{
			switch (*c)
			{
				case ('{'):
					c++;
					break;
				case ('\0'):
					c--;
				case ('}'):
					vardone = TRUE;
					break;
				case (' '):
					vardone = TRUE;
					c--;
					break;
				default:
					var += *c;
					c++;
					break;
			}
			if (vardone) break;
		}
		// Now have variable (and format) in 'var'.
		// Create a quick format and add this to the printstr.
		varformat.create(var.get(), variables);
		//printf("Variable part = [%s]\n",var.get());
		//printf("Current PRINTSTR = [%s]\n",printstr.get());
		printstr.cat(varformat.create_string());
	}
	// Final string to print is now in printstr...
	msg(DM_NONE,"%s\n",printstr.get());
	return CR_SUCCESS;
}
