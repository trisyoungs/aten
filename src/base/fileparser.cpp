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
 * File Read Functions
 */

// Read next line from file
bool FileParser::readLine(QString& variable)
{
	int result = parser_.readNextLine(Parser::Defaults);
	variable = parser_.line();
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

/*
 * Write Functions
 */

// Write line to file
bool FileParser::writeLine(QString line)
{
	return parser_.writeLine(line);
}

// Write formatted line to file
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

// Returns whether the specified argument exists
bool FileParser::hasArg(int i) const
{
	return parser_.hasArg(i);
}
