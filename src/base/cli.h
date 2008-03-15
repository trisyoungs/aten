/*
	*** Command-line option parsing
	*** src/base/cli.h
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

#ifndef ATEN_CLI_H
#define ATEN_CLI_H

#include <iostream>
#include "base/master.h"
#include <getopt.h>

// Command line options
enum cli_option {
	CO_BOHR, CO_BOND, CO_CACHE, CO_CENTRE, CO_COMMAND, CO_FF, CO_FOLD, CO_GRID, CO_HELP, CO_INTERACTIVE,
	CO_MAP, CO_NOBOND, CO_NOCENTRE, CO_NOFOLD, CO_NOPACK, CO_PACK, CO_SCRIPT, CO_UNDO, CO_ZMAP,
	CO_DEBUG, CO_DEBUGALL, CO_DEBUGFILE, CO_DEBUGMORE, CO_DEBUGPARSE, CO_DEBUGTYPING, CO_VERBOSE,
	CO_NITEMS };

// Command option data
class optiondata {
	/*
	// Description of command line option
	*/
	public:
	// Identifier
	cli_option clioption;
	// Short option character
	char shortopt;
	// Long option keyword
	const char *longopt;
	// Argument type
	int argument;
	// Argument text (for description)
	const char *argtext;
	// Description of option
	const char *description;
};

#endif
