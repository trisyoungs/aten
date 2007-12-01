/*
	*** File filter definition
	*** src/file/filter.cpp
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
#include "base/sysfunc.h"
#include "model/model.h"
#include "classes/pattern.h"
#include <fstream>

// Filter types
const char *FT_strings[FT_NITEMS] = { "importmodel", "exportmodel", "importtrajectory", "exporttrajectory", "importfield", "exportfield", "importgrid", "exportgrid" };
const char *text_from_FT(filter_type ft)
	{ return FT_strings[ft]; }
filter_type FT_from_text(const char *s)
	{ return (filter_type) enum_search("filter section",FT_NITEMS,FT_strings,s); }

// Constructor
filter::filter()
{
	next = NULL;
	prev = NULL;
	type = FT_NITEMS;
	has_extension = FALSE;
	inputfile = NULL;
	outputfile = NULL;
	has_zmapping = FALSE;
	zmapping = ZM_ALPHA;
	name.set("unnamed");
	glob.set("*");
	id = -1;
	partner = NULL;
	activemodel = NULL;
	activecell = NULL;
	readopts = PO_DEFAULTS;
	#ifdef MEMDEBUG
		memdbg.create[MD_FILTER] ++;
	#endif
}

// Destructor
filter::~filter()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_FILTER] ++;
	#endif
}

// Load filter (from file)
bool filter::load(ifstream &filterfile)
{
	dbg_begin(DM_CALLS,"filter::load");
	command_node<filter_command> *fn;
	filter_command fc;
	char longname[256];
	zmap_type zm;
	int success, itemsleft;
	bool done, error;
	// First, we must add a command to the flowstack so we know when to return (or raise an error)
	commands.clear();
	// Read in commands
	while (!filterfile.eof())
	{
		success = parser.get_args_delim(&filterfile,PO_USEQUOTES+PO_SKIPBLANKS);
		if (success == 1)
		{
			msg(DM_NONE,"filter::load - Error reading filter file.\n");
			dbg_end(DM_CALLS,"filter::load");
			return FALSE;
		}
		else if (success == -1) break;
		// Check branchstack - if empty then we're done (all filters have  a final 'END' command so the BC_ROOTNODE will get terminated)
		if (commands.get_branchstack_size() == 0)
		{
			// Create long filefilter string
			sprintf(longname,"%s (%s)",name.get(),glob.get());
			description = longname;
			dbg_end(DM_CALLS,"filter::load");
			return TRUE;
		}
		// Check for basic commands (local to command_nodes) first.
		basic_command bc = BC_from_text(parser.argc(0));
		if (bc != BC_NITEMS)
		{
			// Add the command to the list
			if (commands.add_basic(bc)) continue;
			else
			{
				msg(DM_NONE,"filter::load <<< Error adding basic command '%s' >>>>\n", parser.argc(0));
				dbg_end(DM_CALLS,"filter::load");
				return FALSE;
			}
		}
		else
		{
			// Find the filter command and add this to the command list
			fc = FC_from_text(parser.argc(0));
			// Some commands do not require nodes in the list, but set properties in the filter itself
			switch (fc)
			{
				// Long name of filter
				case (FC_NAME):
					name = parser.argc(1);
					break;
				// Nickname for filter
				case (FC_NICKNAME):
					nickname = parser.argc(1);
					break;
				// File extension(s)
				case (FC_EXTENSION):
					extension = parser.argc(1);
					break;
				// Exact filename list
				case (FC_EXACT):
					exactnames = parser.argc(1);
					break;
				// Set file filter glob for GUI
				case (FC_GLOB):
					glob = parser.argc(1);
					break;
				// Set filter ID
				case (FC_ID):
					id = parser.argi(1);
					break;
				// Set element zmapping to use for import
				case (FC_ZMAP):
					zm = ZM_from_text(parser.argc(1));
					if (zm != ZM_NITEMS)
					{
						zmapping = zm;
						has_zmapping = TRUE;
					}
					break;
				case (FC_NITEMS):
					msg(DM_NONE,"Unrecognised command '%s' in filter.\n", parser.argc(0));
					dbg_end(DM_CALLS,"filter::load");
					return FALSE;
					break;
				default:
					
					// If add_other() returns NULL then we encountered an error
					if (!commands.add_other(fc, text_from_FC(fc), vars_from_FC(fc)))
					{
						msg(DM_NONE,"Error adding filter command '%s'.\n", text_from_FC(fc));
						dbg_end(DM_CALLS,"filter::load");
						return FALSE;
					}
			}
		}
	}
	// Create long filefilter string
	sprintf(longname,"%s (%s)",name.get(),glob.get());
	description = longname;
	// Check the flowstack - it should be empty...
	itemsleft = commands.get_branchstack_size();
	if (itemsleft != 0)
	{
		printf("filter::load <<<< %i block%s not been terminated >>>>\n", itemsleft, (itemsleft == 1 ? " has" : "s have"));
		dbg_end(DM_CALLS,"filter::load");
		return FALSE;
	}
	dbg_end(DM_CALLS,"filter::load");
	return TRUE;
}

// Print
void filter::print()
{
	dbg_begin(DM_CALLS,"filter::print");
	printf("Filter Name : '%s'\n",name.get());
	printf(" Shell glob : '%s'\n",glob.get());
	printf(" Extensions : '%s'\n",extension.get());
	printf("Exact Names : '%s'\n",exactnames.get());
	printf("       Type : %s\n",text_from_FT(type));
	dbg_end(DM_CALLS,"filter::print");
}

// Set targets (model)
void filter::set_target(model *m)
{
	dbg_begin(DM_CALLS,"filter::set_target[model]");
	if (m == NULL)
	{
		activemodel = NULL;
		activecell = NULL;
	}
	else
	{
		activemodel = m;
		activecell = m->get_cell();
	}
	dbg_end(DM_CALLS,"filter::set_target[model]");
}

// Set targets (grid)
void filter::set_target(grid *g)
{
	dbg_begin(DM_CALLS,"filter::set_target[grid]");
	if (g == NULL) activegrid = NULL;
	else activegrid = g;
	dbg_end(DM_CALLS,"filter::set_target[grid]");
}

// Reset targets
void filter::reset_targets()
{
	activemodel = NULL;
	activecell = NULL;
	activegrid = NULL;
}

// Set input file
bool filter::set_input(const char *sourcefile)
{
	dbg_begin(DM_CALLS,"filter::set_input[filename]");
	if (inputfile != NULL) printf("filter::set_input[filename] <<<< Inputfile already set >>>>\n");
	inputfile = new ifstream(sourcefile,ios::in);
	filename = sourcefile;
	dbg_end(DM_CALLS,"filter::set_input[filename]");
	if (!inputfile->good()) return FALSE;
	else return TRUE;
}

// Set input file (pointer)
bool filter::set_input(ifstream *ifs)
{
	dbg_begin(DM_CALLS,"filter::set_input[ifstream]");
	if (inputfile != NULL) printf("filter::set_input[pointer] <<<< Inputfile already set >>>>\n");
	inputfile = ifs;
	filename.clear();
	dbg_end(DM_CALLS,"filter::set_input[ifstream]");
	if (!inputfile->good()) return FALSE;
	else return TRUE;
}

// Set output file
bool filter::set_output(const char *destfile)
{
	dbg_begin(DM_CALLS,"filter::set_output");
	outputfile = new ofstream(destfile,ios::out);
	filename = destfile;
	dbg_end(DM_CALLS,"filter::set_output");
	if (!outputfile->good()) return FALSE;
	else return TRUE;
}

// Close files
void filter::close_files()
{
	dbg_begin(DM_CALLS,"filter::close_files");
	if (inputfile != NULL)
	{
		inputfile->close();
		delete inputfile;
	}
	if (outputfile != NULL)
	{
		outputfile->close();
		delete outputfile;
	}
	inputfile = NULL;
	outputfile = NULL;
	filename.clear();
	dbg_end(DM_CALLS,"filter::close_files");
}
