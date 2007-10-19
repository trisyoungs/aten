/*
	*** File parsing routines
	*** src/file/parse.h
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

#ifndef H_PARSE_H
#define H_PARSE_H

using namespace std;
#include <fstream>
#include <iostream>
#include "base/constants.h"
#include "classes/dnchar.h"

#define MAXARGS 50
#define MAXARGLENGTH 128
#define MAXLINELENGTH 1024

// Parse Options
enum parse_options { PO_DEFAULTS=0, PO_USEQUOTES=1, PO_SKIPBLANKS=2, PO_NOPARENTHESES=4, PO_NOPARCONTENTS=8, PO_STRIPBRACKETS=16, PO_SPLITLINES = 32 };

// Forward declarations
class format;

// Parser
class line_parser
{
	public:
	// Constructor
	line_parser();

	/*
	// Set Source Line
	*/
	private:
	// Temporary string variable
	dnchar temparg;
	// Line to parse
	dnchar line;
	// Parsed arguments
	dnchar arguments[MAXARGS];
	// Whether the end of the string has been found in get_next_arg()
	bool end_of_line;
	// Number of arguments grabbed from last parse
	int nargs;
	// Option bitmask (set by get_args() calls)
	int optmask;
	// Gets next delimited arg from internal 'line'
	bool get_next_arg(int);
	// Gets next n chars from internal 'line'
	bool get_next_n(int);
	// Gets all delimited args from supplied string
	void get_all_args_delim(dnchar&);
	// Gets all arguments from string by format
	void get_all_args_formatted(dnchar&, format*);

	public:
	// Cut and return next argument from string
	const char *get_next_delim(dnchar&, int);
	// Set line and parse using delimiters
	void get_args_delim(const char*, int);
	// Set line and parse into separate lines using ';' and '\n' as delimiters
	void get_lines_delim(const char*);
	// Set (read) line from file and parse using delimiters
	int get_args_delim(ifstream*, int);
	// Skip 'n' lines from file
	int skip_lines(ifstream*, int);
	// Parse file with format
	int get_args_formatted(ifstream*, int, format*);
	// Parse file with format
	void get_args_formatted(const char*, int, format*);

	/*
	// Argument Access
	*/
	public:
	// Returns number of arguments grabbed from last parse
	int get_nargs() { return nargs; }
	// Returns the specified argument as a character string
	const char *argc(int i) { return arguments[i].get(); }
	// Returns the specified argument as an integer
	int argi(int i) { return arguments[i].as_integer(); }
	// Returns the specified argument as a double
	double argd(int i) { return arguments[i].as_double(); }
	// Returns the specified argument as a bool
	bool argb(int i) { return arguments[i].as_bool(); }
	// Returns the specified argument as a float
	float argf(int i) { return (float) argd(i); }
	// Returns whether the specified argument is empty
	bool is_blank(int i) { return (arguments[i][0] == '\0' ? TRUE : FALSE); }

	/*
	// Atom type parsing
	*/
	public:
	// Remove atomtype description from string and return it
	const char *parse_atstring(dnchar&);
	// Remove keyword from string and return it
	const char *trim_atkeyword(dnchar&);
};

extern line_parser parser;

#endif
