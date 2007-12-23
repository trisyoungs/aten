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
#include "file/parse.h"
#include "base/sysfunc.h"
#include "base/master.h"
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
	has_zmapping = FALSE;
	zmapping = ZM_ALPHA;
	name.set("unnamed");
	glob.set("*");
	id = -1;
	partner = NULL;
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
	command *c;
	command_action ca;
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
		// Check branchstack - if empty then we're done (all filters have  a final 'END' command so the CA_ROOTNODE will get terminated)
		if (commands.get_branchstack_size() == 0)
		{
			// Create long filefilter string
			sprintf(longname,"%s (%s)",name.get(),glob.get());
			description = longname;
			dbg_end(DM_CALLS,"filter::load");
			return TRUE;
		}
		// Check for filter specification commands
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
			default:
				// Check for commands first
				ca = CA_from_text(parser.argc(0));
				if (ca != CA_NITEMS)
				{
					// Add the command to the list
					if (commands.add_command(ca)) continue;
					else
					{
						msg(DM_NONE,"filter::load <<< Error adding command '%s' >>>>\n", parser.argc(0));
						dbg_end(DM_CALLS,"filter::load");
						return FALSE;
					}
				}
				else
				{
					msg(DM_NONE,"Unrecognised command '%s' in filter.\n", parser.argc(0));
					dbg_end(DM_CALLS,"filter::load");
					return FALSE;
				}
				break;
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

// Execute filter
bool filter::execute(const char *filename, ifstream *sourcefile, bool trajheader, model *framemodel)
{
	dbg_begin(DM_CALLS,"filter::execute");
	// Grab pointer bundle from master
	bundle &obj = master.current;
	// Set element mapping type to that specified in file
	zmap_type temp_zmap = prefs.get_zmapping();
	if (has_zmapping) prefs.set_zmapping(zmapping);
	// Setup based on filter type...
	switch (type)
	{
		case (FT_MODEL_IMPORT):
			msg(DM_NONE,"Load Model : %s (%s)\n",obj.m->get_filename(), name.get());
			// Reset reserved variables
			commands.variables.set("title","Unnamed");
			break;
		case (FT_MODEL_EXPORT):
			msg(DM_NONE,"Save Model : %s (%s)...", obj.m->get_filename(), name.get());
			// Open file and set target
			if (!commands.set_outfile(obj.m->get_filename()))
			{
				msg(DM_NONE,"Error opening output file '%s'.\n",obj.m->get_filename());
				dbg_end(DM_CALLS,"filter::execute");
				return FALSE;
			}
			// Set variables
			commands.variables.set_model_variables(obj.m);
			commands.variables.set_cell_variables(obj.m->get_cell());
			break;
		case (FT_FIELD_EXPORT):
			msg(DM_NONE,"Save Field : %s (%s)\n", filename, name.get());
			// Need a valid pattern and energy expression to export
			if (!obj.m->autocreate_patterns() || !obj.m->create_expression())
			{
				msg(DM_NONE,"filter::execute - Must have valid pattern and energy expression to export a field file\n.");
				dbg_end(DM_CALLS,"filter::execute");
				return FALSE;
			}
			// Set variables
			commands.variables.set("title",obj.m->get_name());
			commands.variables.set("npatterns",obj.m->get_npatterns());
			commands.variables.set("energyunit",text_from_EU(prefs.get_internal_units()));
			// Open file...
			if (!commands.set_outfile(filename))
			{
				msg(DM_NONE,"Error opening field file '%s'.\n", filename);
				dbg_end(DM_CALLS,"filter::execute");
				return FALSE;
			}
			break;
		case (FT_GRID_IMPORT):
			msg(DM_NONE,"Load Grid  : %s (%s)\n",filename,name.get());
			// Open file...
			if (!commands.set_infile(filename))
			{
				msg(DM_NONE,"Error opening grid file '%s'.\n", filename);
				dbg_end(DM_CALLS,"filter::execute");
				return FALSE;
			}
			break;
		case (FT_GRID_EXPORT):
			msg(DM_NONE,"Save Grid  : %s (%s)\n",filename,name.get());
			// Open file...
			if (!commands.set_outfile(filename))
			{
				msg(DM_NONE,"Error opening grid file '%s'.\n", filename);
				dbg_end(DM_CALLS,"filter::execute");
				return FALSE;
			}
			break;
		case (FT_TRAJECTORY_IMPORT):
			// Set variables
			commands.variables.set("header",(trajheader ? "true" : "false"));
			commands.variables.set("frame",(trajheader ? "false" : "true"));
			// Set model target (if reading a frame)
			if (!trajheader)
			{
				model *parent = framemodel->get_trajparent();
				if (parent == NULL)
				{
					msg(DM_NONE,"filter::read_trajectory <<<< Trajectory parent is not set in frame model >>>>\n");
					dbg_end(DM_CALLS,"filter::read_trajectory(frame)");
					return FALSE;	
				}
				commands.variables.set("natoms",parent->get_natoms());
				commands.variables.set("cell.type",lower_case(text_from_CT(parent->get_celltype())));
				framemodel->clear();
			}
			else
			{
				commands.variables.set("natoms",framemodel->get_natoms());
				commands.variables.set("cell.type",lower_case(text_from_CT(framemodel->get_celltype())));
			}

	}
	// Execute commandlist
	bool result = commands.execute(framemodel,sourcefile);
	// Perform post-filter operations
	switch (type)
	{
		case (FT_MODEL_IMPORT):
			// Reset element mapping style
			prefs.set_zmapping(temp_zmap);
			break;
	}
	msg(DM_NONE,"Done.\n");
	dbg_end(DM_CALLS,"filter::execute");
}

