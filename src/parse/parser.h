/*
	*** File parsing routines
	*** src/parse/parse.h
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

#ifndef H_PARSE_H
#define H_PARSE_H

using namespace std;
#include <fstream>
#include <iostream>
#include "base/constants.h"
#include "classes/dnchar.h"

#define MAXARGS 50
#define MAXARGLENGTH 256
#define MAXLINELENGTH 1024

// Parse Options
enum parse_option { PO_DEFAULTS=1, PO_USEQUOTES=2, PO_SKIPBLANKS=4, PO_STRIPBRACKETS=8, PO_DUMMY=16, PO_NITEMS=5};
parse_option PO_from_text(const char*);

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
	char temparg[MAXARGLENGTH];
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
	// Get next arg delimited from file stream
	const char *get_arg_delim(ifstream*);
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
	// Set argument manually
	void set_arg(int i, const char *s) { arguments[i] = s; }

	/*
	// Atom type parsing
	*/
	public:
	// Remove atomtype description from string and return it
	const char *parse_atstring(dnchar&);
	// Remove keyword from string and return it
	const char *trim_atkeyword(dnchar&);

	/*
	// Numerical Expression Parsing
	*/
	public:
	bool get_args_expression(const char *);
	bool is_operator(int, char);
};

extern line_parser parser;

#endif
