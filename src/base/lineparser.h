/*
	*** Line Parsing Routines
	*** src/base/lineparser.h
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

#ifndef ATEN_LINEPARSER_H
#define ATEN_LINEPARSER_H

#include "base/dnchar.h"
#include "templates/list.h"
#include <fstream>
#include <iostream>
using namespace std;

#define MAXLINELENGTH 1024

// Line Parser
class LineParser
{
	public:
	// Constructor
	LineParser();
	// Parse Options
	enum LineParserOption { Defaults=1, UseQuotes=2, SkipBlanks=4, StripBrackets=8, NoExpressions=16, NoEscapes=32, nLineParserOptions=6};
	static LineParserOption lineParserOption(const char*);

	/*
	// Source line, options, and argument data
	*/
	protected:
	// Temporary string variable
	char tempArg_[MAXLINELENGTH];
	// Line to parse
	char line_[MAXLINELENGTH];
	// Length of line_
	int lineLength_;
	// Current reading position in line
	int linePos_;
	// Integer line number of last read line
	int lastLine_;
	// Source file
	std::ifstream sourceFile_;
	// Parsed arguments
	List<Dnchar> arguments_;
	// Whether the end of the string has been found in get_next_arg()
	bool endOfLine_;
	// Option bitmask (set by get_args() calls)
	int optionMask_;
	// Gets next delimited arg from internal line
	bool getNextArg(Dnchar *destarg);
	// Gets next n chars from internal line
	bool getNextN(int length);
	// Gets all delimited args from internal line
	void getAllArgsDelim();
	// Read next line from internal source file
	int readLine();

	public:
	// Return pointer to start of current line
	const char *line();
	// Return integer line number of last read line
	int lastLine();
	// Open file for parsing
	bool openFile(const char *filename);
	// Close file 
	void closeFile();
	// Read line from file and do delimited parse
	int getArgsDelim(int flags);
	// Set line and parse using delimiters
	void getArgsDelim(const char *string, int flags);
	// Skip 'n' lines from internal file
	int skipLines(int nskip);
	// Get next delimited argument from internal file
	const char *getArgDelim();

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
	// Set argument manually
	//void setArg(int i, const char *s);

	/*
	// Atom type parsing
	*/
	public:
	// Remove atomtype description from string and return it
	const char *parseAtomtypeString(Dnchar&);
	// Remove keyword from string and return it
	const char *trimAtomtypeKeyword(Dnchar&);
};

#endif
