/*
	*** Read/write command functions
	*** src/command/image.cpp
	Copyright T. Youngs 2007

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

// Add file read option
int command_functions::function_CA_ADDREADOPTION(command *&c, bundle &obj)
{
	// Get parse option from variable
	parse_option po = PO_from_text(c->argc(0));
	if (po != PO_NITEMS) c->parent->add_readoption(po);
	return CR_SUCCESS;
}

// Search for line containing specified string
int command_functions::function_CA_FIND(command *&c, bundle &obj)
{
	ifstream *inputfile = c->parent->get_infile();
	if (inputfile == NULL)
	{
		msg(DM_NONE,"No input file active.\n");
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
			if (c->has_arg(2)) c->arg(2)->set(linefromfile);
		}
		else if (inputfile->eof() || inputfile->fail()) iresult = 0;
	} while (iresult == -1);
	c->arg(1)->set(iresult);
	return CR_SUCCESS;
}

// Read N characters from unformatted file
int command_functions::function_CA_READCHARS(command *&c, bundle &obj)
{
	static char readc[512];
	ifstream *inputfile = c->parent->get_infile();
	if (inputfile == NULL)
	{
		msg(DM_NONE,"No input file active.\n");
		return CR_FAIL;
	}
	inputfile->read((char*) &readc, c->argi(1));
	c->arg(0)->set(readc);
	msg(DM_FILTERS,"Unformatted char read got '%s'\n",readc);
	return CR_SUCCESS;
}

// Read double from unformatted file
int command_functions::function_CA_READDOUBLE(command *&c, bundle &obj)
{
	ifstream *inputfile = c->parent->get_infile();
	if (inputfile == NULL)
	{
		msg(DM_NONE,"No input file active.\n");
		return CR_FAIL;
	}
	double readd;
	inputfile->read((char*) &readd,8);
	c->arg(0)->set(readd);
	msg(DM_FILTERS,"Unformatted double read got '%f'\n",readd);
	return CR_SUCCESS;
}

// Read integer from unformatted file
int command_functions::function_CA_READINTEGER(command *&c, bundle &obj)
{
	ifstream *inputfile = c->parent->get_infile();
	if (inputfile == NULL)
	{
		msg(DM_NONE,"No input file active.\n");
		return CR_FAIL;
	}
	int readi;
	inputfile->read((char*) &readi,4);
	c->arg(0)->set(readi);
	msg(DM_FILTERS,"Unformatted int read got '%i'\n",readi);
	return CR_SUCCESS;
}

// Read line and parse with format
int command_functions::function_CA_READLINE(command *&c, bundle &obj)
{
	ifstream *inputfile = c->parent->get_infile();
	if (inputfile == NULL)
	{
		msg(DM_NONE,"No input file active.\n");
		return CR_FAIL;
	}
	parser.get_args_formatted(inputfile,c->parent->get_readoptions(),c->get_format());
	return CR_SUCCESS;
}

// Get next whitespace-delimited argument from file
int command_functions::function_CA_READNEXT(command *&c, bundle &obj)
{
	ifstream *inputfile = c->parent->get_infile();
	if (inputfile == NULL)
	{
		msg(DM_NONE,"No input file active.\n");
		return CR_FAIL;
	}
	c->arg(0)->set(parser.get_arg_delim(inputfile));
	return CR_SUCCESS;
}

// Parse given variable with format
int command_functions::function_CA_READVAR(command *&c, bundle &obj)
{
	parser.get_args_formatted(c->argc(0),c->parent->get_readoptions(),c->get_format());
	return CR_SUCCESS;
}

// Remove file read option
int command_functions::function_CA_REMOVEREADOPTION(command *&c, bundle &obj)
{
	// Get parse option from variable
	parse_option po = PO_from_text(c->argc(0));
	if (po != PO_NITEMS) c->parent->remove_readoption(po);
	return CR_SUCCESS;
}

// Discard N characters from unformatted file
int command_functions::function_CA_SKIPCHARS(command *&c, bundle &obj)
{
	static char readc[512];
	ifstream *inputfile = c->parent->get_infile();
	if (inputfile == NULL)
	{
		msg(DM_NONE,"No input file active.\n");
		return CR_FAIL;
	}
	inputfile->read((char*) &readc, c->argi(0));
	return CR_SUCCESS;
}

// Skip line(s) of file
int command_functions::function_CA_SKIPLINE(command *&c, bundle &obj)
{
	ifstream *inputfile = c->parent->get_infile();
	if (inputfile == NULL)
	{
		msg(DM_NONE,"No input file active.\n");
		return CR_FAIL;
	}
	if (c->has_arg(0)) parser.skip_lines(inputfile,c->argi(0));
	else parser.skip_lines(inputfile,1);
	return CR_SUCCESS;
}

// Write line with format
int command_functions::function_CA_WRITELINE(command *&c, bundle &obj)
{
	ofstream *outputfile = c->parent->get_outfile();
	if (outputfile == NULL)
	{
		msg(DM_NONE,"No output file active.\n");
		return CR_FAIL;
	}
	*outputfile << c->get_format()->create_string();
	*outputfile << "\n";
	return CR_SUCCESS;
}
