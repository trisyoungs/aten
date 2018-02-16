/*
	*** File Parser
	*** src/base/fileparser.h
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

#ifndef ATEN_FILEPARSER_H
#define ATEN_FILEPARSER_H

#include "base/lineparser.h"
#include "base/elementmap.h"
#include "templates/vector3.h"
#include <templates/array.h>

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;

// File Parser
class FileParser
{
	public:
	// Constructors / Destructor
	FileParser(LineParser& parser) : parser_(parser)
	{
	}
	~FileParser()
	{
	}


	/*
	 * Associated LineParser
	 */
	private:
	// Associated LineParser object
	LineParser& parser_;

	public:
	// Return current read/write filename
	QString filename()
	{
		return (parser_.inputFilename().isEmpty() ? parser_.outputFilename() : parser_.inputFilename());
	}
	// Tell current position of input stream
	std::streampos tellg() const
	{
		return parser_.tellg();
	}
	// Seek position in input stream
	void seekg(std::streampos pos)
	{
		parser_.seekg(pos);
	}
	// Seek n bytes in specified direction in input stream
	void seekg(std::streamoff off, std::ios_base::seekdir dir)
	{
		parser_.seekg(off, dir);
	}
	// Rewind input stream to start
	void rewind()
	{
		parser_.rewind();
	}


	/*
	 * Formatted Read Functions
	 */
	public:
	// Read next line from file
	bool readLine(QString& variable)
	{
		int result = parser_.readNextLine(Parser::Defaults);
		variable = parser_.line();
		return (result == 0);
	}
	// Read next line from file into the variable supplied, and parse it as well
	bool readAndParseLine(QString& variable)
	{
		bool result = parseLine(Parser::Defaults);
		variable = parser_.line();
		return result;
	}
	// Read next line from file (converted to int)
	bool readLineAsInteger(int& variable)
	{
		int result = parser_.readNextLine(Parser::Defaults);
		variable = parser_.line().toInt();
		return (result == 0);
	}
	// Read next line from file (converted to double)
	bool readLineAsDouble(double& variable)
	{
		int result = parser_.readNextLine(Parser::Defaults);
		variable = parser_.line().toDouble();
		return (result == 0);
	}
	// Return whether the end of the input stream has been reached (or only whitespace remains)
	bool eofOrBlank() const
	{
		return parser_.eofOrBlank();
	}
	// Skip n lines from file
	bool skipLines(int nLines = 1)
	{
		return (parser_.skipLines(nLines) == 0);
	}
	// Find next line containing supplied string
	bool find(QString string)
	{
		// Store current stream position in case the string is not found
		std::streampos previouspos = tellg();

		QString line;
		bool found = false;
		while (!eofOrBlank())
		{
			// Get line from file
			if (!readLine(line)) break;

			// Check for string
			if (line.contains(string))
			{
				// Parse line before we go...
				parser_.getArgsDelim(Parser::Defaults, line);
				return true;
			}
		}

		// Rewind file to previous position if not found
		seekg(previouspos);

		return false;
	}


	/*
	 * Raw Read Functions
	 */
	public:
	// Read characters
	QString readChars(int nChars, bool skipEOL = true)
	{
		return parser_.getChars(nChars, skipEOL);
	}
	// Skip characters
	void skipChars(int nChars)
	{
		parser_.skipChars(nChars);
	}
	// Read integer (with specified byte size)
	bool readRawInteger(int& value, int nBytes = 0)
	{
		return parser_.getInteger(value, nBytes);
	}
	// Read double (with specified byte size
	bool readRawDouble(double& value, int nBytes = 0)
	{
		return parser_.getDouble(value, nBytes);
	}
	// Read integer array
	bool readRawIntegerArray(Array<int>& array, int nValues)
	{
		array.createEmpty(nValues);
		return (parser_.getIntegerArray(array.array(), nValues) == 0);
	}
	// Read double array
	bool readRawDoubleArray(Array<double>& array, int nValues)
	{
		array.createEmpty(nValues);
		return (parser_.getDoubleArray(array.array(), nValues) == 0);
	}
	// Read double array
	bool readRawDoubleArray(double* array, int nValues)
	{
		return (parser_.getDoubleArray(array, nValues) == 0);
	}


	/*
	 * Formatted Write Functions
	 */
	public:
	// Write partial line to file
	bool write(QString line)
	{
		return parser_.write(line);
	}
	// Write formatted partial line to file
	bool writeF(const char* fmt, ...)
	{
		// Construct line
		va_list arguments;
		static char s[8096];
		s[0] = '\0';

		// Parse the argument list (...) and internally write the output string into s[]
		va_start(arguments,fmt);
		vsprintf(s,fmt,arguments);
		va_end(arguments);
		return parser_.write(s);
	}
	// Write empty line to file
	bool writeLine()
	{
		return parser_.writeLine();
	}
	// Write whole line to file (appending CR/LF automatically)
	bool writeLine(QString line)
	{
		return parser_.writeLine(line);
	}
	// Write formatted line to file (appending CR/LF automatically)
	bool writeLineF(const char* fmt, ...)
	{
		// Construct line
		va_list arguments;
		static char s[8096];
		s[0] = '\0';

		// Parse the argument list (...) and internally write the output string into s[]
		va_start(arguments,fmt);
		vsprintf(s,fmt,arguments);
		va_end(arguments);
		return parser_.writeLine(s);
	}


	/*
	 * Argument Data
	 */
	public:
	// Read and parse next line into delimited arguments
	bool parseLine(int parseOptions = Parser::Defaults)
	{
		return (parser_.getArgsDelim(parseOptions) == 0);
	}
	// Read and parse next line according to specified format
	bool parseLine(ParseFormat& format, int parseOptions = Parser::Defaults)
	{
		return (parser_.getArgsFormatted(format, parseOptions) == 0);
	}
	// Parse string into delimited arguments
	int parseString(QString line, int parseOptions = Parser::Defaults)
	{
		parser_.getArgsDelim(parseOptions, line);
		return parser_.nArgs();
	}
	// Parse string according to specified format
	bool parseString(QString line, ParseFormat& format, int parseOptions = Parser::Defaults)
	{
		parser_.setLine(line);
		return (parser_.getArgsFormatted(format, parseOptions, false) == 0);
	}
	// Returns number of arguments grabbed from last parse
	int nArgs() const
	{
		return parser_.nArgs();
	}
	// Returns the specified argument as a character string
	QString argc(int i)
	{
		return parser_.argc(i);
	}
	// Returns the specified argument as an integer
	int argi(int i)
	{
		return parser_.argi(i);
	}
	// Returns the specified argument as a double
	double argd(int i)
	{
		return parser_.argd(i);
	}
	// Returns the specified argument as a bool
	bool argb(int i)
	{
		return parser_.argb(i);
	}
	// Returns the specified argument as a float
	float argf(int i)
	{
		return parser_.argf(i);
	}
	// Returns the specified argument (+1, and +2) as a Vec3<int>
	Vec3<int> arg3i(int i)
	{
		return Vec3<int>(parser_.argi(i), parser_.argi(i+1), parser_.argi(i+2));
	}
	// Returns the specified argument (+1, and +2) as a Vec3<double>
	Vec3<double> arg3d(int i)
	{
		return Vec3<double>(parser_.argd(i), parser_.argd(i+1), parser_.argd(i+2));
	}
	// Returns whether the specified argument exists
	bool hasArg(int i) const
	{
		return parser_.hasArg(i);
	}
};

ATEN_END_NAMESPACE

#endif
