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
#include "base/constants.h"
#include "templates/list.h"
#include <fstream>
#include <iostream>
using namespace std;

#define MAXLINELENGTH 1024

// Forward Declarations
class Format;

// Line Parser
class LineParser
{
	public:
	// Constructor
	LineParser();
	LineParser(const char *ifilename, bool outputstream = FALSE);
	// Parse Options
	enum ParseOption { Defaults=1, UseQuotes=2, SkipBlanks=4, StripBrackets=8, NoExpressions=16, NoEscapes=32, nParseOptions=6};
	static ParseOption parseOption(const char*);


	/*
	// Source line/file and read options
	*/
	private:
	// Filename of current file target (if any)
	Dnchar filename_;
	// Line to parse
	char line_[MAXLINELENGTH];
	// Length of line_
	int lineLength_;
	// Current reading position in line
	int linePos_;
	// Integer line number of last read line
	int lastLine_;
	// Source file (for reading) 
	std::fstream file_;
	// Whether the file is for reading or writing
	bool readOnly_;
	// Option bitmask (set by get*() calls)
	int optionMask_;

	public:
	// Return filename of opened (or recently closed) file
	const char *filename();
	// Return pointer to start of current line
	const char *line();
	// Set line target
	void setLine(const char *s);
	// Return integer line number of last read line
	int lastLine();
	// Open new file for parsing or writing
	bool openFile(const char *filename, bool outputstream = FALSE);
	// Close file(s)
	void closeFile();
	// Return whether current file source is good for reading/writing
	bool isFileGood();
	// Tell current position of file stream
	streampos tellg();
	// Peek next character in file
	char peek();
	// Seek position in file
	void seekg(streampos pos);
	// Seek n bytes in specified direction
	void seekg(streamoff off, ios_base::seekdir dir);
	// Rewind file to start
	void rewind();
	// Return whether the end of the file has been reached (or only whitespace remains)
	bool eofOrBlank();

	/*
	// Read/Write Routines
	*/
	private:
	// Gets all delimited args from internal line
	void getAllArgsDelim();

	public:
	// Gets next delimited arg from internal line
	bool getNextArg(Dnchar *destarg, int flags = LineParser::Defaults);
	// Gets next n chars from internal line
	bool getNextN(int length, Dnchar *destarg = NULL);
	// Read line from file and do delimited parse
	int getArgsDelim(int flags = LineParser::Defaults);
	// Set line and parse using delimiters
	void getArgsDelim(const char *string, int flags = LineParser::Defaults);
	// Read next line from internal source file, setting as parsing source
	int readLine();
	// Read next line from source file, skipping blank lines and removing comments
	int getLine();
	// Skip 'n' lines from internal file
	int skipLines(int nskip);
	// Get next delimited argument from internal file
	int getArgDelim(Dnchar *destarg, int flags);
	// Return a number of characters from the input stream
	const char *getChars(int nchars);
	// Return an integer value from reading 'n' chars of an (unformatted) input file
	int getInteger(int nbytes = 0);
	// Return a double value from reading 'n' chars of an (unformatted) input file
	double getReal(int nbytes = 0);
	// Write line to file
	bool writeLine(const char *s);

	/*
	// Argument Data
	*/
	private:
	// Temporary string variable
	char tempArg_[MAXLINELENGTH];
	// Parsed arguments
	List<Dnchar> arguments_;
	// Whether the end of the string has been found in get_next_arg()
	bool endOfLine_;

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
