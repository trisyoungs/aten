/*
	*** File type probes
	*** src/main/probe.cpp
	Copyright T. Youngs 2007-2011

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
Tree *Aten::probeFile(const char *filename, FilterData::FilterType probetype)
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
	Dnchar nameonly, *d;
	Refitem<Tree,int> *ri;
	Tree *t = NULL;

	// Get position of file extention and pure filename
	dotpos = strrchr(filename,'.');
	if (dotpos != NULL) dotpos++;
	nameonly = removePath(filename);

	// Go through list of filters and do checks...
	for (ri = filters_[probetype].first(); ri != NULL; ri = ri->next)
	{
		t = ri->item;

		// Check filename extensions *or* exact names (if either were provided)
		if (t->filter.extensions() != NULL)
		{
			// If a file extension is not present on the filename, then the filter is not a match
			if (dotpos == NULL) continue;
			// Otherwise, try to match extension - if no match, then the filter is not a match
			for (d = t->filter.extensions(); d != NULL; d = d->next) if (*d == dotpos) break;
			if (d == NULL) continue;
		}
		else if (t->filter.exactNames() != NULL)
		{
			for (d = t->filter.exactNames(); d != NULL; d = d->next) if (*d == nameonly) break;
			if (d == NULL) continue;
		}
		
		// Try to match text within files
		if (t->filter.searchStrings() != NULL)
		{
			bool found = FALSE;
			parser.openFile(filename);
			for (d = t->filter.searchStrings(); d != NULL; d = d->next)
			{
				// Make sure file is completely rewound
				parser.rewind();
				for (n = 0; n<t->filter.nLinesToSearch(); n++)
				{
					m = parser.readNextLine(0);
printf("Searching for string [%s] in line [%s]\n", d->get(), parser.line());
					if (m == -1) break;
					if (m == 1)
					{
						msg.print("File error encountered while searching for identifying string.\n");
						break;
					}
					else if (strstr(parser.line(), d->get()) == NULL)
					{
						found = TRUE;
						break;
					}
				}
				
				if (found) break;
			}
			parser.closeFile();
			if (!found) continue;
		}

		// If we reach this point, then the filter is a match
		break;
	}

	if (t == NULL) msg.print("Couldn't determine format of file '%s'.\n",filename);
	else msg.print(Messenger::Verbose,"Aten::probeFile - Selected filter '%s'\n", t->filter.name());
	msg.exit("Aten::probeFile");
	return t;
}
