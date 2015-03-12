/*
	*** Aten Program Modes (other than GUI)
	*** src/main/modes.cpp
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
#include "model/model.h"

ATEN_USING_NAMESPACE

/*
// BatchExport
*/

// Set format to use in export
void Aten::setExportFilter(Tree* filter)
{
	exportFilter_ = filter;
}

// Export all currently loaded models in the referenced format
void Aten::exportModels()
{
	Messenger::enter("Aten::exportModels");
	Dnchar filename;

	// Loop over loaded models
	for (Model* m = models_.first(); m != NULL; m = m->next)
	{
		// Set current model
		setCurrentModel(m, TRUE);

		// Generate new filename for model.
		filename = m->filename();
		int n = filename.rFind('.', '/', '\\');
		filename.eraseFrom(n);

		// Append new suffix
		filename += '.';
		filename.strcat(exportFilter_->filter.extensions()->get());

		// Make sure that the new filename is not the same as the old filename
		if (filename == m->filename())
		{
			Messenger::print("Export filename generated is identical to the original (%s) - not converted.", filename.get());
			continue;
		}
		m->setFilter(exportFilter_);
		m->setFilename(filename);
		
		// Temporarily disable undo/redo for the model, save, and re-enable
		m->disableUndoRedo();
		if (exportFilter_->executeWrite(filename)) Messenger::print("Model '%s' saved to file '%s' (%s)", m->name(), filename.get(), exportFilter_->filter.name());
		else Messenger::print("Failed to save model '%s'.", m->name());
		m->enableUndoRedo();
	}
	Messenger::exit("Aten::exportModels");
}

/*
// Batch Process
*/

// Add set of batch commands
Program  *Aten::addBatchCommand()
{
	return batchCommands_.add();
}

// Run all stored commands on all loaded models
void Aten::processModels()
{
	ReturnValue rv;
	for (Model* m = models_.first(); m != NULL; m = m->next)
	{
		for (Program* cmd = batchCommands_.first(); cmd != NULL; cmd = cmd->next)
		{
			// Set the current model
			setCurrentModel(m, TRUE);

			// Run the command list
			if (!cmd->execute(rv)) return;
		}
	}
}

// Save all models under their original names
void Aten::saveModels()
{
	for (Model* m = models_.first(); m != NULL; m = m->next)
	{
		setCurrentModel(m, TRUE);
		// Check model's filter - it will be the import filter, so try to get the partner
		Tree* t = m->filter();
		if (t == NULL)
		{
			Messenger::print("No export filter available for model '%s'. Not saved.", m->name());
			continue;
		}
		if (t->filter.type() != FilterData::ModelExport)
		{
			Messenger::print("No export filter for model '%s' (format '%s'). Not saved.", m->name(), t->filter.nickname());
			continue;
		}
		
		Dnchar filename;
		filename = m->filename();
		if (!filename.isEmpty()) t->executeWrite(m->filename());
	}
}
