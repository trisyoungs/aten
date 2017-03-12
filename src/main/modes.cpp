/*
	*** Aten Program Modes (other than GUI)
	*** src/main/modes.cpp
	Copyright T. Youngs 2007-2017

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

// Return the current program mode
Aten::ProgramMode Aten::programMode() const
{
	return programMode_;
}

/*
 * BatchExport
 */

// Set plugin to use in export
void Aten::setExportModelPlugin(const FilePluginInterface* plugin, KVMap pluginOptions)
{
	exportModelPlugin_ = plugin;
	for (KVPair* pair = pluginOptions.pairs(); pair != NULL; pair = pair->next) exportModelPluginOptions_.add(pair->key(), pair->value());
}

// Export all currently loaded models in the referenced format
void Aten::exportModels()
{
	Messenger::enter("Aten::exportModels");
	QFileInfo fileInfo;
	QString newFilename;

	// Loop over loaded models
	for (Model* m = models_.first(); m != NULL; m = m->next)
	{
		// Set current model
		setCurrentModel(m);

		// Generate new filename for model, with new suffix
		fileInfo.setFile(m->filename());
		newFilename = fileInfo.dir().absoluteFilePath(fileInfo.baseName() + "." + exportModelPlugin_->extensions().first());

		QFileInfo newFileInfo(newFilename);
		// Make sure that the new filename is not the same as the old filename
		if (fileInfo == newFileInfo)
		{
			Messenger::print("Exported file would overwrite the original (%s) - not converted.", qPrintable(m->filename()));
			continue;
		}

		if (exportModel(m, newFilename, exportModelPlugin_, FilePluginStandardImportOptions(), exportModelPluginOptions_)) Messenger::print("Model '%s' saved to file '%s' (%s)", qPrintable(m->name()), qPrintable(newFilename), qPrintable(exportModelPlugin_->name()));
		else Messenger::print("Failed to save model '%s'.", qPrintable(m->name()));
		m->enableUndoRedo();
	}
	Messenger::exit("Aten::exportModels");
}

// Add set of batch commands
Program* Aten::addBatchCommand()
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
			setCurrentModel(m);

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
		setCurrentModel(m);

		// Check model's filter - it will be the import filter, so try to get the partner
		FilePluginInterface* plugin = m->plugin();
		if (plugin == NULL)
		{
			Messenger::print("No plugin available for model '%s'. Not saved.", qPrintable(m->name()));
			continue;
		}
		if (! plugin->canExport())
		{
			Messenger::print("Plugin for model '%s' has no export capability (format '%s'). Not saved.", qPrintable(m->name()), qPrintable(plugin->name()));
			continue;
		}
		if (m->filename().isEmpty())
		{
			Messenger::print("Model '%s' has no filename set. Not saved.", qPrintable(m->name()));
			continue;
		}

		// Save the model
		exportModel(m, m->filename(), m->plugin());
	}
}

// Clear type export map
void Aten::clearTypeExportMap()
{
	typeExportMap_.clear();
}

// Add key/value to type export map
void Aten::addTypeExportMapping(QString key, QString value)
{
	typeExportMap_.add(key, value);
}

// Return number of defined type export mappings
int Aten::nTypeExportMappings()
{
	return typeExportMap_.nPairs();
}

// Set whether type export conversion is enabled
void Aten::setTypeExportMapping(bool b)
{
	typeExportMapping_ = b;
}

// Return whether type export conversion is enabled
bool Aten::typeExportMapping() const
{
	return typeExportMapping_;
}

// Convert supplied type name according to export type map
QString Aten::typeExportConvert(QString oldName) const
{
	if (!typeExportMapping_) return oldName;
	KVPair* kvp = typeExportMap_.search(oldName);
	return (kvp == NULL ? oldName : kvp->value());
}
