/*
	*** File Parser
	*** src/base/fileparser.h
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

#ifndef ATEN_FILEPARSER_H
#define ATEN_FILEPARSER_H

#include "base/lineparser.h"
#include "base/elementmap.h"
#include "templates/vector3.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;

// File Parser
class FileParser
{
	public:
	// Constructors / Destructor
	FileParser(LineParser& parser);
	~FileParser();


	/*
	 * Associated LineParser
	 */
	private:
	// Associated LineParser object
	LineParser& parser_;

	public:
	// Return current read/write filename
	QString filename();
	// Tell current position of input stream
	std::streampos tellg() const;
	// Seek position in input stream
	void seekg(std::streampos pos);
	// Seek n bytes in specified direction in input stream
	void seekg(std::streamoff off, std::ios_base::seekdir dir);
	// Rewind input stream to start
	void rewind();


	/*
	 * Read Functions
	 */
	public:
	// Read next line from file
	bool readLine(QString& variable);
	// Read next line from file (converted to int)
	bool readLineAsInteger(int& variable);
	// Read next line from file (converted to double)
	bool readLineAsDouble(double& variable);
	// Return whether the end of the input stream has been reached (or only whitespace remains)
	bool eofOrBlank() const;
	// Skip n lines from file
	bool skipLines(int nLines = 1);
	

	/*
	 * Write Functions
	 */
	public:
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


	/*
	 * Argument Data
	 */
	public:
	// Read and parse next line into delimited arguments
	bool parseLine(int parseOptions = Parser::Defaults);
	// Returns number of arguments grabbed from last parse
	int nArgs() const;
	// Returns the specified argument as a character string
	QString argc(int i);
	// Returns the specified argument as an integer
	int argi(int i);
	// Returns the specified argument as a double
	double argd(int i);
	// Returns the specified argument as a bool
	bool argb(int i);
	// Returns the specified argument as a float
	float argf(int i);
	// Returns the specified argument (+1, and +2) as a Vec3<int>
	Vec3<int> arg3i(int i);
	// Returns the specified argument (+1, and +2) as a Vec3<double>
	Vec3<double> arg3d(int i);
	// Returns whether the specified argument exists
	bool hasArg(int i) const;
};

ATEN_END_NAMESPACE

#endif
