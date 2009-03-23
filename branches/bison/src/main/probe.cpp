/*
	*** File type probes
	*** src/aten/probe.cpp
	Copyright T. Youngs 2007-2009

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

#include "main/aten.h"
#include "base/sysfunc.h"
#include <fstream>
#include <iostream>

// Probe model
Tree *Aten::probeFile(const char *filename, Tree::FilterType probetype)
{
	// From the supplied filename and file type, determine (as best we can) the format of the file
	msg.enter("Aten::probeFile");
	// Before we do the proper checking, make sure that the file exists and is readable
	ifstream probefile;
	probefile.open(filename,ios::in);
	if (!probefile.is_open())
	{
		msg.print("File '%s' does not exist.\n",filename);
		msg.exit("Aten::probeFile");
		return NULL;
	}
	if (filename[0] == '\0')
	{
		msg.print("Filename is empty.\n");
		msg.exit("Aten::probeFile");
		return NULL;
	}
	probefile.close();
	LineParser parser;
	int n, m;
	const char *dotpos;
	Dnchar nameonly;
	Refitem<Tree,int> *ri;
	Tree *filter, *result = NULL;
	dotpos = strrchr(filename,'.');
	if (dotpos != NULL) dotpos++;
	nameonly = removePath(filename);
	// Go through list of filters and do checks...
	for (ri = filters_[probetype].first(); ri != NULL; ri = ri->next)
	{
		filter = ri->item;
		// Try to match text within files
		if (filter->searchStrings() != NULL)
		{
			bool done = FALSE;
			parser.openFile(filename);
			for (Dnchar *ss = filter->searchStrings(); ss != NULL; ss = ss->next)
			{
				// Make sure file is completely rewound
				parser.rewind();
				for (n = 0; n<filter->nLinesToSearch(); n++)
				{
					m = parser.readLine();
					if (m == -1) break;
					if (m == 1)
					{
						msg.print("File error encountered while searching for identifying string.\n");
						done = TRUE;
						break;
					}
					if (strstr(parser.line(), ss->get()) != NULL)
					{
						result = filter;
						done = TRUE;
						break;
					}
				}
				if (done) break;
			}
			probefile.close();
		}
		if (result != NULL) break;
		// Try to match file extension
		if (dotpos != NULL)
		{
			for (Dnchar *d = filter->extensions(); d != NULL; d = d->next)
				if (*d == dotpos)
				{
					result = filter;
					break;
				}
		}
		if (result != NULL) break;
		// Try to match exact filename
		for (Dnchar *d = filter->exactNames(); d != NULL; d = d->next)
		{
			//printf("Comparing '%s' with '%s'\n",nameonly.get(),parser.argc(n));
			if (*d == nameonly)
			{
				result = filter;
				break;
			}
		}

	}
	if (result == NULL) msg.print("Couldn't determine format of file '%s'.\n",filename);
	else msg.print(Messenger::Verbose,"Aten::probeFile - Selected filter '%s'\n",result->name());
	msg.exit("Aten::probeFile");
	return result;
}
