/*
	*** Command-line option parsing
	*** src/base/cli.h
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

#ifndef H_CLI_H
#define H_CLI_H

#include <iostream>
#include "base/master.h"
#include <getopt.h>

// Command line options
enum cli_option { CO_A, CO_BOHR, CO_COMMAND, CO_DEBUG, CO_E,
	CO_FF, CO_G, CO_HELP, CO_INTERACTIVE, CO_J,
	CO_K, CO_L, CO_M, CO_N, CO_O,
	CO_P, CO_Q, CO_R, CO_SCRIPT, CO_T,
	CO_UNDO, CO_VERBOSE, CO_W, CO_X, CO_Y, CO_ZMAP,
	CO_FOLD, CO_NOFOLD, CO_BOND, CO_NOBOND, CO_CENTRE, CO_NOCENTRE, CO_PACK, CO_NOPACK,
	CO_DEBUGTYPING, CO_DEBUGPARSE, CO_DEBUGMORE, CO_DEBUGALL, CO_DEBUGFILE, CO_GRID, CO_CACHE, CO_NITEMS };

// Command option data
class optiondata {
	/*
	// Description of command line option
	*/
	public:
	// Identifier
	cli_option clioption;
	// Long option keyword
	const char *keyword;
	// Argument type
	int argument;
	// Argument text (for description)
	const char *argtext;
	// Description of option
	const char *description;

	/*
	// Get
	*/
	public:
	// Return identifier
	cli_option get_id() { return clioption; }
	// Return long option keyword
	const char *get_keyword() { return keyword; }
	// Return argument type
	int get_argument() { return argument; }
	// Return argument text
	const char *get_argtext() { return argtext; }
	// Return option description
	const char *get_description() { return description; }
};

#endif
