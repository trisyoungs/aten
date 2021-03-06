/*
	*** Line Parsing Routines
	*** src/base/lineparser.h
	Copyright T. Youngs 2007-2018

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

#include "math/constants.h"
#include "templates/list.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <QStringList>
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

#define MAXLINELENGTH 1024

// Forward Declarations (Aten)
class Format;
class ParseFormat;

// Parser Options and Flags
class Parser
{
	public:
	// Parse Options
	enum ParseOption { Defaults = 0, StripComments = 1, UseQuotes = 2, SkipBlanks = 4, StripBrackets = 8, NoEscapes = 16, UseCurlies = 32, NormalCommas = 64, nParseOptions = 7 };
	static ParseOption parseOption(QString s);
};

// Line Parser
class LineParser
{
	public:
	// Constructors / Destructor
	LineParser();
	~LineParser();


	/*
	 * Source line/file and read options
	 */
	private:
	// Current input filename (if any)
	QString inputFilename_;
	// CUrrent output filename (if any)
	QString outputFilename_;
	// Line to parse
	QString line_;
	// Length of line_
	int lineLength_;
	// Current reading position in line
	int linePos_;
	// Integer line number of last read line
	int lastLineNo_;
	// Source stream for reading
	std::ifstream* inputFile_;
	// Target stream for writing
	std::ofstream* outputFile_;
	// Target stream for cached writing
	std::stringstream* cachedFile_;

	public:
	// Reset data
	void reset();
	// Return filename of current inputFile (if any)
	QString inputFilename() const;
	// Return filename of current outputFile (if any)
	QString outputFilename() const;
	// Return pointer to start of current line
	QString line() const;
	// Set line target
	void setLine(QString line);
	// Return integer line number of last read line
	int lastLineNo() const;
	// Return read-only status of file
	bool isFileReadOnly() const;
	// Open new file for reading
	bool openInput(QString filename);
	// Open new stream for writing
	bool openOutput(QString filename, bool directOutput = true);
	// Close file(s)
	void closeFiles();
	// Return whether current file source is good for reading
	bool isFileGoodForReading() const;
	// Return whether current file source is good for writing
	bool isFileGoodForWriting() const;
	// Tell current position of input stream
	std::streampos tellg() const;
	// Peek next character in input stream
	char peek() const;
	// Seek position in input stream
	void seekg(std::streampos pos);
	// Seek n bytes in specified direction in input stream
	void seekg(std::streamoff off, std::ios_base::seekdir dir);
	// Rewind input stream to start
	void rewind();
	// Return whether the end of the input stream has been reached (or only whitespace remains)
	bool eofOrBlank() const;


	/*
	 * Read/Write Routines
	 */
	private:
	// Whether output is cached or direct
	bool directOutput_;
	// Gets all delimited args from internal line
	void getAllArgsDelim(int optionMask);

	public:
	// Gets next delimited arg from internal line
	bool getNextArg(int optionMask, QString& destArg);
	// Gets next n chars from internal line
	bool getNextN(int optionMask, int length, QString& destArg);
	// Read line from file and do delimited parse
	int getArgsDelim(int optionMask);
	// Get rest of line
	bool getRest(QString& destArg);
	// Get rest of line starting at next delimited part
	bool getRestDelim(QString& destArg);
	// Set line and parse using delimiters
	void getArgsDelim(int optionMask, QString line);
	// Get next line (if requested) and arguments according to specified format
	bool getArgsFormatted(ParseFormat& format, int optionMask, bool readLine = true);
	// Get next delimited chunk from file (not line)
	bool getCharsDelim(QString& destArg);
	// Get next delimited chunk from string, removing grabbed part
	bool getCharsDelim(int optionMask, QString& source, QString& destArg);
	// Read next line from internal source file, setting as parsing source
	int readNextLine(int optionMask);
	// Skip 'n' lines from internal file
	int skipLines(int nskip);
	// Get next delimited argument from internal line
	bool getArgDelim(int optionMask, QString& destArg);
	// Return a number of characters from the input stream
	QString getChars(int nchars, bool skipeol = true);
	// Skip a number of characters from the input stream
	void skipChars(int nchars);
	// Return an integer value from reading 'n' chars of an (unformatted) input file
	bool getInteger(int& value, int nbytes = 0);
	// Fill an array of integer values from reading of an (unformatted) input file
	int getIntegerArray(int* array, int count);
	// Return a double value from reading 'n' chars of an (unformatted) input file
	double getDouble(double& value, int nbytes = 0);
	// Fill an array of double values from reading of an (unformatted) input file
	int getDoubleArray(double* array, int count);
	// Write partial line to file
	bool write(QString line);
	// Write formatted partial line to file
	bool writeF(const char* fmt, ...);
	// Write empty line to file
	bool writeLine();
	// Write whole line to file (appending CR/LF automatically)
	bool writeLine(QString line);
	// Write formatted line to file (appending CR/LF automatically)
	bool writeLineF(const char* fmt, ...);
	// Commit cached output stream to actual output file
	bool commitCache();


	/*
	 * Argument Data
	 */
	private:
	// Parsed arguments
	QStringList arguments_;
	// Whether the end of the string has been found in get_next_arg()
	bool endOfLine_;

	public:
	// Returns number of arguments grabbed from last parse
	int nArgs() const;
	// Returns the specified argument as a character string
	QString argc( int i );
	// Returns the specified argument as an integer
	int argi(int i);
	// Returns the specified argument as a double
	double argd(int i);
	// Returns the specified argument as a bool
	bool argb(int i);
	// Returns the specified argument readNextLineas a float
	float argf(int i);
	// Returns whether the specified argument exists
	bool hasArg(int i) const;
	// Return whether we are at the end of the current line
	bool atEndOfLine() const;
};

ATEN_END_NAMESPACE

#endif
