/*
	*** Basic read / write filter functions
	*** src/file/readwrite.cpp
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

#include "file/filter.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include <fstream>

// Read commands
bool filter::do_readwrite(command_node<filter_command> *&fn)
{
	dbg_begin(DM_CALLS,"filter::do_readwrite");
	atom *i;
	int readi;
	double readd;
	static char readc[512];
	bool result = TRUE;
	switch (fn->get_command())
	{
		// Read line and parse with format
		case (FC_READLINE):
			if (inputfile == NULL) break;
			parser.get_args_formatted(inputfile,PO_DEFAULTS,fn->get_format());
			break;
		// Parse given variable with format
		case (FC_READVAR):
			if (inputfile == NULL) break;
			parser.get_args_formatted(fn->datavar[0]->get_as_char(),PO_DEFAULTS,fn->get_format());
			break;
		// Get next whitespace-delimited argument from file
		case (FC_READNEXT):
			if (inputfile == NULL) break;
			fn->datavar[0]->set(parser.get_arg_delim(inputfile));
			break;
		// Skip line(s) of file
		case (FC_SKIPLINE):
			if (inputfile == NULL) break;
			if (fn->datavar[0] == NULL) parser.skip_lines(inputfile,1);
			else parser.skip_lines(inputfile,fn->datavar[0]->get_as_int());
			break;
		// Read integer from unformatted file
		case (FC_READINTEGER):
			if (inputfile == NULL) break;
			inputfile->read((char*) &readi,4);
			fn->datavar[0]->set(readi);
			msg(DM_FILTERS,"Unformatted int read got '%i'\n",readi);
			break;
		// Read double from unformatted file
		case (FC_READDOUBLE):
			if (inputfile == NULL) break;
			inputfile->read((char*) &readd,8);
			fn->datavar[0]->set(readd);
			msg(DM_FILTERS,"Unformatted double read got '%f'\n",readd);
			break;
		// Read N characters from unformatted file
		case (FC_READCHARS):
			if (inputfile == NULL) break;
			inputfile->read((char*) &readc, fn->datavar[1]->get_as_int());
			fn->datavar[0]->set(readc);
			msg(DM_FILTERS,"Unformatted char read got '%s'\n",readc);
			break;
		// Discard N characters from unformatted file
		case (FC_SKIPCHARS):
			if (inputfile == NULL) break;
			inputfile->read((char*) &readc, fn->datavar[0]->get_as_int());
			break;
		// Write line with format
		case (FC_WRITELINE):
			if (outputfile == NULL) break;
			*outputfile << fn->get_format()->create_string();
			*outputfile << "\n";
			break;
		// Write line to msg output
		case (FC_WARN):
			msg(DM_NONE,"Filter Warning: %s\n",fn->datavar[0]->get_as_char());
			break;
		// Write line to msg output and stop
		case (FC_ERROR):
			msg(DM_NONE,"Filter Error: %s\n",fn->datavar[0]->get_as_char());
			result = FALSE;
			break;
		default:
			result = FALSE;
			break;
	}
	if (result) fn = fn->next;
	dbg_end(DM_CALLS,"filter::do_readwrite");
	return result;
}
