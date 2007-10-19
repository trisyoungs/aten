/*
	*** File type probes
	*** src/file/probe.cpp
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

#include "base/master.h"
#include "file/parse.h"
#include "file/filter.h"
#include <fstream>
#include <iostream>

// Probe model
filter *master_data::probe_file(const char *filename, filter_type probetype)
{
	// From the supplied filename and file type, determine (as best we can) the format of the file
	dbg_begin(DM_CALLS,"master::probe_file");
	// Before we do the proper checking, make sure that the file exists and is readable
	ifstream probefile;
	probefile.open(filename,ios::in);
	if (!probefile.is_open())
	{
		msg(DM_NONE,"File '%s' does not exist.\n",filename);
		dbg_end(DM_CALLS,"master::probe_file");
		return NULL;
	}
	probefile.close();
	int n;
	char *dotpos;
	dnchar nameonly;
	filter *f, *result = NULL;
	dotpos = strrchr(filename,'.');
	if (dotpos != NULL) dotpos++;
	nameonly = remove_path(filename);
	// Go through list of filters and do checks...
	for (f = filters[probetype].first(); f != NULL; f = f->next)
	{
		// Try to match file extension
		if (dotpos != NULL)
		{
			// Parse file extension list into separate extensions
			parser.get_args_delim(f->get_extension(),PO_DEFAULTS);
			for (n=0; n<parser.get_nargs(); n++)
				if (strcmp(dotpos,parser.argc(n)) == 0)
				{
					result = f;
					break;
				}
		}
		if (result != NULL) break;
		// Try to match exact filename
		// Parse file extension list into separate extensions
		parser.get_args_delim(f->get_exactnames(),PO_DEFAULTS);
		for (n=0; n<parser.get_nargs(); n++)
		{
			//printf("Comparing '%s' with '%s'\n",nameonly.get(),parser.argc(n));
			if (nameonly == parser.argc(n))
			{
				result = f;
				break;
			}
		}
	}
	// Strings in file
	//ifstream modelfile(filename,ios::in);
	//if (parser.find_phrase(&modelfile,"GAMESS VERSION",10)) result = MF_GAMESSUS;
	//modelfile.close();
	if (result == NULL) msg(DM_NONE,"Couldn't determine format of file '%s'.\n",filename);
	else msg(DM_VERBOSE,"master::probe_file - Selected filter '%s'\n",result->get_name());
	dbg_end(DM_CALLS,"master::probe_file");
	return result;
}
