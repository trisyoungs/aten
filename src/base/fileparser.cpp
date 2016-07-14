/*
	*** File Parser
	*** src/base/fileparser.cpp
	Copyright T. Youngs 2007-2016

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

#include "base/fileparser.h"
#include "base/messenger.h"
#include "parser/format.h"

ATEN_USING_NAMESPACE

// Constructors
FileParser::FileParser(LineParser& parser) : parser_(parser)
{
}

// Destructor
FileParser::~FileParser()
{
}

/*
 * LineParser Object
 */

// Return current read/write filename
QString FileParser::filename()
{
	return (parser_.inputFilename().isEmpty() ? parser_.outputFilename() : parser_.inputFilename());
}

// Tell current position of input stream
std::streampos FileParser::tellg() const
{
	return parser_.tellg();
}

// Seek position in input stream
void FileParser::seekg(std::streampos pos)
{
	parser_.seekg(pos);
}

// Seek n bytes in specified direction in input stream
void FileParser::seekg(std::streamoff off, std::ios_base::seekdir dir)
{
	parser_.seekg(off, dir);
}

// Rewind input stream to start
void FileParser::rewind()
{
	parser_.rewind();
}

/*
 * File Read Functions
 */

// Read next line from file
bool FileParser::readLine(QString& variable)
{
	int result = parser_.readNextLine(Parser::Defaults);
	variable = parser_.line();
	return (result == 0);
}

// Read next line from file into the variable supplied, and parse it as well
bool FileParser::readAndParseLine(QString& variable)
{
	int result = parser_.readNextLine(Parser::Defaults);
	variable = parser_.line();
	parseLine();
	return (result == 0);
}

// Read next line from file (converted to int)
bool FileParser::readLineAsInteger(int& variable)
{
	int result = parser_.readNextLine(Parser::Defaults);
	variable = parser_.line().toInt();
	return (result == 0);
}

// Read next line from file (converted to double)
bool FileParser::readLineAsDouble(double& variable)
{
	int result = parser_.readNextLine(Parser::Defaults);
	variable = parser_.line().toDouble();
	return (result == 0);
}

// Return whether the end of the input stream has been reached (or only whitespace remains)
bool FileParser::eofOrBlank() const
{
	return parser_.eofOrBlank();
}

// Skip n lines from file
bool FileParser::skipLines(int nLines)
{
	return (parser_.skipLines(nLines) == 0);
}

// Find next line containing supplied string
bool FileParser::find(QString string)
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
		if (line.contains(string)) return true;
	}

	// Rewind file to previous position if not found
	seekg(previouspos);

	return false;
}

/*
 * Write Functions
 */

// Write partial line to file
bool FileParser::write(QString line)
{
	return parser_.write(line);
}

// Write formatted partial line to file
bool FileParser::writeF(const char* fmt, ...)
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
bool FileParser::writeLine()
{
	return parser_.writeLine();
}

// Write whole line to file (appending CR/LF automatically)
bool FileParser::writeLine(QString line)
{
	return parser_.writeLine(line);
}

// Write formatted line to file (appending CR/LF automatically)
bool FileParser::writeLineF(const char* fmt, ...)
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
 * Line Parsing
 */

// Read and parse next line into delimited arguments
bool FileParser::parseLine(int parseOptions)
{
	return (parser_.getArgsDelim(parseOptions) == 0);
}

// Read and parse next line according to specified format
bool FileParser::parseLine(ParseFormat& format, int parseOptions)
{
	return (parser_.getArgsFormatted(format, parseOptions) == 0);
}

// Parse string into delimited arguments
int FileParser::parseString(QString line, int parseOptions)
{
	parser_.getArgsDelim(parseOptions, line);
	return parser_.nArgs();
}

// Parse string according to specified format
bool FileParser::parseString(QString line, ParseFormat& format, int parseOptions)
{
	parser_.setLine(line);
	return (parser_.getArgsFormatted(format, parseOptions, false) == 0);
}

/*
 * Arguments
 */

// Returns number of arguments grabbed from last parse
int FileParser::nArgs() const
{
	return parser_.nArgs();
}

// Returns the specified argument as a character string
QString FileParser::argc(int i)
{
	return parser_.argc(i);
}

// Returns the specified argument as an integer
int FileParser::argi(int i)
{
	return parser_.argi(i);
}

// Returns the specified argument as a double
double FileParser::argd(int i)
{
	return parser_.argd(i);
}

// Returns the specified argument as a bool
bool FileParser::argb(int i)
{
	return parser_.argb(i);
}

// Returns the specified argument as a float
float FileParser::argf(int i)
{
	return parser_.argf(i);
}

// Returns the specified argument (+1, and +2) as a Vec3<int>
Vec3<int> FileParser::arg3i(int i)
{
	return Vec3<int>(parser_.argi(i), parser_.argi(i+1), parser_.argi(i+2));
}

// Returns the specified argument (+1, and +2) as a Vec3<double>
Vec3<double> FileParser::arg3d(int i)
{
	return Vec3<double>(parser_.argd(i), parser_.argd(i+1), parser_.argd(i+2));
}

// Returns whether the specified argument exists
bool FileParser::hasArg(int i) const
{
	return parser_.hasArg(i);
}
