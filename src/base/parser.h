/*
	*** File parsing routines
	*** src/base/parser.h
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

#ifndef ATEN_PARSER_H
#define ATEN_PARSER_H

//#include "base/constants.h"
#include "base/dnchar.h"
#include <fstream>
#include <iostream>
using namespace std;

#define MAXARGS 100
#define MAXARGLENGTH 256
#define MAXLINELENGTH 1024

// Parser
class Parser
{
	public:
	// Constructor
	Parser();
	// Parse Options
	enum ParseOption { Defaults=1, UseQuotes=2, SkipBlanks=4, StripBrackets=8, NoExpressions=16, NoEscapes=32, nParseOptions=6};
	static ParseOption parseOption(const char*);
	// Determine form of argument
	enum ArgumentForm { ConstantForm, VariableForm, ExpressionForm, ReferenceForm };
	ArgumentForm argumentForm(int i);

	/*
	// Source line, options, and argument data
	*/
	private:
	// Temporary string variable
	char tempArg_[MAXARGLENGTH];
	// Line to parse
	char line_[MAXLINELENGTH];
	// Length of line_
	int lineLength_;
	// Current reading position in line_
	int linePos_;
	// Source file (set by readLine);
	ifstream *sourceFile_;
	// Parsed arguments
	Dnchar arguments_[MAXARGS];
	// Quotation used around the argument
	int quoted_[MAXARGS];
	// Whether the end of the string has been found in get_next_arg()
	bool endOfLine_;
	// Number of arguments grabbed from last parse
	int nArgs_;
	// Option bitmask (set by get_args() calls)
	int optionMask_;
	// Gets next delimited arg from internal line_
	bool getNextArg(int);
	// Gets next n chars from internal line_
	bool getNextN(int);
	// Gets all delimited args from internal line_
	void getAllArgsDelim();

	public:
	// Return pointer to start of current line_
	const char *line();
	// Set line and parse using delimiters
	void getArgsDelim(const char*, int);
	// Set line and parse into separate lines using ';' and '\n' as delimiters
	void getLinesDelim(const char*);
	// Set (read) line from file and parse using delimiters
	int getArgsDelim(ifstream*, int);
	// Skip 'n' lines from file
	int skipLines(ifstream*, int);
	// Read next line from source file
	int readLine(ifstream *source);
	// Get next arg delimited from file stream
	const char *getArgDelim(ifstream*);
	// Shift all arguments up one position (leaving arg[0] blank)
	void shiftArgsUp();

	/*
	// Argument Access
	*/
	public:
	// Returns number of arguments grabbed from last parse
	int nArgs();
	// Returns the specified argument as a character string
	const char *argc(int i);
	// Returns the specified argument as an integer
	int argi(int i);
	// Returns the specified argument as a double
	double argd(int i);
	// Returns the specified argument as a bool
	bool argb(int i);
	// Returns the specified argument as a float
	float argf(int i);
	// Returns whether the specified argument is empty
	bool isBlank(int i);
	// Returns whether the argument was quoted in some way
	bool wasQuoted(int i);
	// Set argument manually
	void setArg(int i, const char *s);

	/*
	// Atom type parsing
	*/
	public:
	// Remove atomtype description from string and return it
	const char *parseAtomtypeString(Dnchar&);
	// Remove keyword from string and return it
	const char *trimAtomtypeKeyword(Dnchar&);
};

extern Parser parser;

#endif
