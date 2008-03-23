/*
	*** File type probes
	*** src/parse/probe.cpp
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

#include "base/master.h"
#include "parse/parser.h"
#include "parse/filter.h"
#include <fstream>
#include <iostream>

// Probe model
Filter *MasterData::probeFile(const char *filename, FilterType probetype)
{
	// From the supplied filename and file type, determine (as best we can) the format of the file
	dbgBegin(DM_CALLS,"master::probeFile");
	// Before we do the proper checking, make sure that the file exists and is readable
	ifstream probefile;
	probefile.open(filename,ios::in);
	if (!probefile.is_open())
	{
		msg(DM_NONE,"File '%s' does not exist.\n",filename);
		dbgEnd(DM_CALLS,"master::probeFile");
		return NULL;
	}
	probefile.close();
	int n;
	char *dotpos;
	Dnchar nameonly;
	Filter *f, *result = NULL;
	dotpos = strrchr(filename,'.');
	if (dotpos != NULL) dotpos++;
	nameonly = removePath(filename);
	// Go through list of filters and do checks...
	for (f = filters_[probetype].first(); f != NULL; f = f->next)
	{
		// Try to match file extension
		if (dotpos != NULL)
		{
			// Parse file extension list into separate extensions
			parser.getArgsDelim(f->extension(),PO_DEFAULTS);
			for (n=0; n<parser.nArgs(); n++)
				if (strcmp(dotpos,parser.argc(n)) == 0)
				{
					result = f;
					break;
				}
		}
		if (result != NULL) break;
		// Try to match exact filename
		// Parse file extension list into separate extensions
		parser.getArgsDelim(f->exactNames(),PO_DEFAULTS);
		for (n=0; n<parser.nArgs(); n++)
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
	else msg(DM_VERBOSE,"master::probeFile - Selected filter '%s'\n",result->name());
	dbgEnd(DM_CALLS,"master::probeFile");
	return result;
}
