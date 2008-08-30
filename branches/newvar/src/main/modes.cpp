/*
	*** Aten Program Modes (other than GUI)
	*** src/base/modes.cpp
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

#include "aten/aten.h"
#include "model/model.h"

/*
// BatchExport
*/

// Set format to use in export
void Aten::setExportFilter(Filter *f)
{
	exportFilter_ = f;
}

// Export all currently loaded models in the referenced format
void Aten::exportModels()
{
	msg.enter("Aten::exportModels");
	char filename[1024], *c;
	int n;
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		aten.setCurrentModel(m);
		// Generate new filename for model.
		// Find rightmost '.'
		strcpy(filename, m->filename());
		c = &filename[0];
		for (n=0; c != '\0'; n++)
		{
			if (filename[n] == '.') break;
			c++;
		}
		if (n != -1) filename[n] = '\0';
		// Append new suffix
		strcat(filename,".");
		strcat(filename,exportFilter_->extensions()->get());
		// Make sure that the new filename is not the same as the old filename
		if (strcmp(filename, m->filename()) == 0)
		{
			msg.print("Export filename generated is identical to the original - not converted.\n");
			continue;
		}
		m->setFilter(exportFilter_);
		m->setFilename(filename);
		exportFilter_->execute(filename);
	}
	msg.exit("Aten::exportModels");
}
