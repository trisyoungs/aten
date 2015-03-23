/*
	*** File type probes
	*** src/main/probe.cpp
	Copyright T. Youngs 2007-2015

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
#include "parser/tree.h"
#include <fstream>
#include <iostream>

ATEN_USING_NAMESPACE

// Probe model
Tree* Aten::probeFile(QString filename, FilterData::FilterType filterType)
{
	Messenger::enter("Aten::probeFile");

	// Check for empty filename
	if (filename.isEmpty())
	{
		Messenger::print("Filename is empty.");
		Messenger::exit("Aten::probeFile");
		return NULL;
	}

	// Before we do the proper checking, make sure that the file exists and is readable
	QFileInfo fileInfo(filename);
	if ((!fileInfo.exists()) || (!fileInfo.isReadable()))
	{
		Messenger::print("File '%s' does not exist or is not readable.", qPrintable(filename));
		Messenger::exit("Aten::probeFile");
		return NULL;
	}

	LineParser parser;
	int n, m;
	Refitem<Tree,int>* ri;
	Tree* filter = NULL, *result = NULL;

	// Go through list of filters and do checks...
	for (ri = filters_[filterType].first(); ri != NULL; ri = ri->next)
	{
		filter = ri->item;

		// Check filename extensions *or* exact names (if either were provided)
		if (filter->filter.extensions().count() > 0)
		{
			// If a file extension is not present on the filename, then the filter is not a match
			if (fileInfo.suffix().isEmpty()) continue;

			// Otherwise, try to match extension - if no match, then the filter is not a match
			for (n=0; n<filter->filter.extensions().count(); ++n)
			{
				if (filter->filter.extensions().at(n) == fileInfo.suffix())
				{
					Messenger::print(Messenger::Verbose, "PROBE: Filter '%s' matches file extension (%s).", qPrintable(filter->name()), qPrintable(fileInfo.suffix()));
					break;
				}
			}
			if (n >= filter->filter.extensions().count()) continue;
		}
		else if (filter->filter.exactNames().count() > 0)
		{
			for (n=0; n<filter->filter.exactNames().count(); ++n) if (filter->filter.exactNames().at(n) == fileInfo.fileName()) break;
			if (n >= filter->filter.exactNames().count()) continue;
		}
		
		// Try to match text within files
		if (filter->filter.searchStrings().count() > 0)
		{
			bool found = FALSE;
			parser.openInput(filename);
			for (n=0; n<filter->filter.searchStrings().count(); ++n)
			{
				// Make sure file is completely rewound
				parser.rewind();
				for (int linesToSearch = 0; linesToSearch<filter->filter.nLinesToSearch(); ++linesToSearch)
				{
					m = parser.readNextLine(0);
					if (m == -1) break;
					if (m == 1)
					{
						Messenger::print("File error encountered while searching for identifying string.");
						break;
					}
					else if (parser.line().contains(filter->filter.searchStrings().at(n)))
					{
						found = TRUE;
						break;
					}
				}
				
				if (found) break;
			}
			parser.closeFiles();
			if (!found) continue;
		}

		// If we reach this point, then the filter is a match
		result = filter;
		break;
	}

	if (result == NULL) Messenger::print("Couldn't determine format of file '%s'.", qPrintable(filename));
	else Messenger::print(Messenger::Verbose, "Aten::probeFile - Selected filter '%s'", qPrintable(result->filter.name()));

	Messenger::exit("Aten::probeFile");
	return result;
}
