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

#ifndef ATEN_PARSE_H
#define ATEN_PARSE_H

#include "base/constants.h"
#include "classes/dnchar.h"
#include <fstream>
#include <iostream>
using namespace std;

#define MAXARGS 50
#define MAXARGLENGTH 256
#define MAXLINELENGTH 1024

// Forward declarations
class Format;

// Parser
class Parser
{
	public:
	// Constructor
	Parser();
	// Parse Options
	enum ParseOption { Defaults=1, UseQuotes=2, SkipBlanks=4, StripBrackets=8, Dummy=16, nParseOptions=5};
	static ParseOption parseOption(const char*);

	/*
	// Set Source Line
	*/
	private:
	// Temporary string variable
	char tempArg_[MAXARGLENGTH];
	// Line to parse
	Dnchar line_;
	// Parsed arguments
	Dnchar arguments_[MAXARGS];
	// Whether the end of the string has been found in get_next_arg()
	bool endOfLine_;
	// Number of arguments grabbed from last parse
	int nArgs_;
	// Option bitmask (set by get_args() calls)
	int optionMask_;
	// Gets next delimited arg from internal 'line'
	bool getNextArg(int);
	// Gets next n chars from internal 'line'
	bool getNextN(int);
	// Gets all delimited args from supplied string
	void getAllArgsDelim(Dnchar&);
	// Gets all arguments from string by format
	void getAllArgsFormatted(Dnchar&, Format*);

	public:
	// Cut and return next argument from string
	const char *getNextDelim(Dnchar&, int);
	// Set line and parse using delimiters
	void getArgsDelim(const char*, int);
	// Set line and parse into separate lines using ';' and '\n' as delimiters
	void getLinesDelim(const char*);
	// Set (read) line from file and parse using delimiters
	int getArgsDelim(ifstream*, int);
	// Skip 'n' lines from file
	int skipLines(ifstream*, int);
	// Get next arg delimited from file stream
	const char *getArgDelim(ifstream*);
	// Parse file with format
	int getArgsFormatted(ifstream*, int, Format*);
	// Parse file with format
	void getArgsFormatted(const char*, int, Format*);

	/*
	// Argument Access
	*/
	public:
	// Returns number of arguments grabbed from last parse
	int nArgs() { return nArgs_; }
	// Returns the specified argument as a character string
	const char *argc(int i) { return arguments_[i].get(); }
	// Returns the specified argument as an integer
	int argi(int i) { return arguments_[i].asInteger(); }
	// Returns the specified argument as a double
	double argd(int i) { return arguments_[i].asDouble(); }
	// Returns the specified argument as a bool
	bool argb(int i) { return arguments_[i].asBool(); }
	// Returns the specified argument as a float
	float argf(int i) { return (float) argd(i); }
	// Returns whether the specified argument is empty
	bool isBlank(int i) { return (arguments_[i][0] == '\0' ? TRUE : FALSE); }
	// Set argument manually
	void setArg(int i, const char *s) { arguments_[i] = s; }

	/*
	// Atom type parsing
	*/
	public:
	// Remove atomtype description from string and return it
	const char *parseAtomtypeString(Dnchar&);
	// Remove keyword from string and return it
	const char *trimAtomtypeKeyword(Dnchar&);

	/*
	// Numerical Expression Parsing
	*/
	public:
	bool getArgsExpression(const char *);
	bool isOperator(int, char);
	bool isNumber(int);
};

extern Parser parser;

#endif
