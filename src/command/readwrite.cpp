/*
	*** Read/write command functions
	*** src/command/readwrite.cpp
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

#include "command/commandlist.h"
#include "parse/format.h"
#include "parse/parser.h"
#include <fstream>
#include <cstring>

// Add file read option
int CommandData::function_CA_ADDREADOPTION(Command *&c, Bundle &obj)
{
	// Get parse option from variable
	Parser::ParseOption po = Parser::parseOption(c->argc(0));
	if (po != Parser::nParseOptions) c->parent()->addReadOption(po);
	return CR_SUCCESS;
}

// Search for line containing specified string
int CommandData::function_CA_FIND(Command *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg(Debug::None,"No input file active.\n");
		return CR_FAIL;
	}
	static char linefromfile[MAXLINELENGTH];
	int iresult = -1;
	do
	{
		// Get line from file
		inputfile->getline(linefromfile,MAXLINELENGTH-1);
		// Check for string
		if (strstr(linefromfile,c->argc(0)) != '\0')
		{
			iresult = 1;
			// Store the line if a third variable was given
			if (c->hasArg(2)) c->arg(2)->set(linefromfile);
		}
		else if (inputfile->eof() || inputfile->fail()) iresult = 0;
	} while (iresult == -1);
	c->arg(1)->set(iresult);
	return CR_SUCCESS;
}

// Read N characters from unformatted file
int CommandData::function_CA_READCHARS(Command *&c, Bundle &obj)
{
	static char readc[512];
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg(Debug::None,"No input file active.\n");
		return CR_FAIL;
	}
	inputfile->read((char*) &readc, c->argi(1));
	c->arg(0)->set(readc);
	msg(Debug::Commands,"Unformatted char read got '%s'\n",readc);
	return CR_SUCCESS;
}

// Read double from unformatted file
int CommandData::function_CA_READFLOAT(Command *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg(Debug::None,"No input file active.\n");
		return CR_FAIL;
	}
	double readd;
	inputfile->read((char*) &readd, 8);
	c->arg(0)->set(readd);
	msg(Debug::Commands,"Unformatted double read got '%f'\n",readd);
	return CR_SUCCESS;
}

// Read integer from unformatted file
int CommandData::function_CA_READINTEGER(Command *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg(Debug::None,"No input file active.\n");
		return CR_FAIL;
	}
	int readi;
	inputfile->read((char*) &readi, 4);
	c->arg(0)->set(readi);
	msg(Debug::Commands,"Unformatted int read got '%i'\n",readi);
	return CR_SUCCESS;
}

// Read line and parse with format
int CommandData::function_CA_READLINE(Command *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg(Debug::None,"No input file active.\n");
		return CR_FAIL;
	}
	parser.getArgsFormatted(inputfile,c->parent()->readOptions(),c->format());
	return CR_SUCCESS;
}

// Get next whitespace-delimited argument from file
int CommandData::function_CA_READNEXT(Command *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg(Debug::None,"No input file active.\n");
		return CR_FAIL;
	}
	c->arg(0)->set(parser.getArgDelim(inputfile));
	return CR_SUCCESS;
}

// Parse given variable with format
int CommandData::function_CA_READVAR(Command *&c, Bundle &obj)
{
	parser.getArgsFormatted(c->argc(0),c->parent()->readOptions(),c->format());
	return CR_SUCCESS;
}

// Remove file read option
int CommandData::function_CA_REMOVEREADOPTION(Command *&c, Bundle &obj)
{
	// Get parse option from variable
	Parser::ParseOption po = Parser::parseOption(c->argc(0));
	if (po != Parser::nParseOptions) c->parent()->removeReadOption(po);
	return CR_SUCCESS;
}

// Go to start of current file
int CommandData::function_CA_REWIND(Command *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg(Debug::None,"No input file active.\n");
		return CR_FAIL;
	}
	inputfile->seekg(0, ios::beg);
	return CR_SUCCESS;
}

// Discard N characters from unformatted file
int CommandData::function_CA_SKIPCHARS(Command *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg(Debug::None,"No input file active.\n");
		return CR_FAIL;
	}
	inputfile->ignore(c->argi(0));
	return CR_SUCCESS;
}

// Skip line(s) of file
int CommandData::function_CA_SKIPLINE(Command *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg(Debug::None,"No input file active.\n");
		return CR_FAIL;
	}
	if (c->hasArg(0)) parser.skipLines(inputfile,c->argi(0));
	else parser.skipLines(inputfile,1);
	return CR_SUCCESS;
}

// Write line with format
int CommandData::function_CA_WRITELINE(Command *&c, Bundle &obj)
{
	ofstream *outputfile = c->parent()->outputFile();
	if (outputfile == NULL)
	{
		msg(Debug::None,"No output file active.\n");
		return CR_FAIL;
	}
	*outputfile << c->format()->createString();
	*outputfile << "\n";
	return CR_SUCCESS;
}

// Write line to variable
int CommandData::function_CA_WRITEVAR(Command *&c, Bundle &obj)
{
	c->arg(0)->set(c->format()->createString());
	return CR_SUCCESS;
}
