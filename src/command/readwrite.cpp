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

#include "variables/accesspath.h"
#include "command/commandlist.h"
#include "command/format.h"
#include "base/parser.h"
#include <fstream>
#include <cstring>

// Add file read option
int Command::function_CA_ADDREADOPTION(CommandNode *&c, Bundle &obj)
{
	// Get parse option from variable
	Parser::ParseOption po = Parser::parseOption(c->argc(0));
	if (po != Parser::nParseOptions) c->parent()->addReadOption(po);
	return Command::Success;
}

// Search for line containing specified string
int Command::function_CA_FIND(CommandNode *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg.print("No input file active.\n");
		return Command::Fail;
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
	return Command::Success;
}

// Read while line from file and put in supplied variable
int Command::function_CA_GETLINE(CommandNode *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg.print("No input file active.\n");
		return Command::Fail;
	}
	parser.readLine(inputfile);
	c->arg(0)->set(parser.line());
	return Command::Success;
}

// Read N characters from unformatted file
int Command::function_CA_READCHARS(CommandNode *&c, Bundle &obj)
{
	static char readc[512];
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg.print("No input file active.\n");
		return Command::Fail;
	}
	inputfile->read((char*) &readc, c->argi(1));
	c->arg(0)->set(readc);
	msg.print(Messenger::Commands,"Unformatted char read got '%s'\n",readc);
	return Command::Success;
}

// Read double from unformatted file
int Command::function_CA_READFLOAT(CommandNode *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg.print("No input file active.\n");
		return Command::Fail;
	}
	double readd;
	inputfile->read((char*) &readd, 8);
	c->arg(0)->set(readd);
	msg.print(Messenger::Commands,"Unformatted double read got '%f'\n",readd);
	return Command::Success;
}

// Read integer from unformatted file
int Command::function_CA_READINTEGER(CommandNode *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg.print("No input file active.\n");
		return Command::Fail;
	}
	int readi;
	inputfile->read((char*) &readi, 4);
	c->arg(0)->set(readi);
	msg.print(Messenger::Commands,"Unformatted int read got '%i'\n",readi);
	return Command::Success;
}

// Read line and parse with format
int Command::function_CA_READLINE(CommandNode *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg.print("No input file active.\n");
		return Command::Fail;
	}
	c->format()->getArgsFormatted(inputfile,c->parent()->readOptions());
	return Command::Success;
}

// Get next whitespace-delimited argument from file
int Command::function_CA_READNEXT(CommandNode *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg.print("No input file active.\n");
		return Command::Fail;
	}
	c->arg(0)->set(parser.getArgDelim(inputfile));
	return Command::Success;
}

// Parse given variable with format
int Command::function_CA_READVAR(CommandNode *&c, Bundle &obj)
{
	// If the format node in the command is empty create a new (temporary) one
	if (c->format() == NULL)
	{
		// Create format from character variable arg(1)
		if (!c->createFormat(c->argc(1), TRUE)) return Command::Fail;
		c->format()->getArgsFormatted(c->argc(0), c->parent()->readOptions());
		c->deleteFormat();
	}
	else c->format()->getArgsFormatted(c->argc(0), c->parent()->readOptions());
	return Command::Success;
}

// Remove file read option
int Command::function_CA_REMOVEREADOPTION(CommandNode *&c, Bundle &obj)
{
	// Get parse option from variable
	Parser::ParseOption po = Parser::parseOption(c->argc(0));
	if (po != Parser::nParseOptions) c->parent()->removeReadOption(po);
	return Command::Success;
}

// Go to start of current file
int Command::function_CA_REWIND(CommandNode *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg.print("No input file active.\n");
		return Command::Fail;
	}
	inputfile->seekg(0, ios::beg);
	return Command::Success;
}

// Discard N characters from unformatted file
int Command::function_CA_SKIPCHARS(CommandNode *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg.print("No input file active.\n");
		return Command::Fail;
	}
	inputfile->ignore(c->argi(0));
	return Command::Success;
}

// Skip line(s) of file
int Command::function_CA_SKIPLINE(CommandNode *&c, Bundle &obj)
{
	ifstream *inputfile = c->parent()->inputFile();
	if (inputfile == NULL)
	{
		msg.print("No input file active.\n");
		return Command::Fail;
	}
	if (c->hasArg(0)) parser.skipLines(inputfile,c->argi(0));
	else parser.skipLines(inputfile,1);
	return Command::Success;
}

// Write line with format
int Command::function_CA_WRITELINE(CommandNode *&c, Bundle &obj)
{
	ofstream *outputfile = c->parent()->outputFile();
	if (outputfile == NULL)
	{
		msg.print("No output file active.\n");
		return Command::Fail;
	}
	*outputfile << c->format()->createString();
	*outputfile << "\n";
	return Command::Success;
}

// Write line to variable
int Command::function_CA_WRITEVAR(CommandNode *&c, Bundle &obj)
{
	// If the format node in the command is empty create a new (temporary) one
	if (c->format() == NULL)
	{
		// Create format from character variable arg(1)
		if (!c->createFormat(c->argc(1), FALSE)) return Command::Fail;
		c->arg(0)->set(c->format()->createString());
		c->deleteFormat();
	}
	else c->arg(0)->set(c->format()->createString());
	return Command::Success;
}
