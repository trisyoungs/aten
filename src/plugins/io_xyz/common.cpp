/*
        *** Common functions for XYZ plugins
        *** src/plugins/io_xyz/common.cpp
        Copyright T. Youngs 2016-2016

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

#include "plugins/interfaces/fileplugin.h"
#include "plugins/io_xyz/common.h"
#include "model/model.h"
#include "base/fileparser.h"

ATEN_USING_NAMESPACE

// Read single XYZ model from file
bool XYZFilePluginCommon::readXYZModel(FilePluginInterface* plugin, FileParser& parser, Model* targetModel)
{
	int nAtoms, n;
	QString e, name;
	
	// Check target model
	if (targetModel == NULL)
	{
		Messenger::error("NULL Model pointer passed to XYZFilePluginCommon::readXYZModel.");
		return false;
	}

	// Read number of atoms from file
	if (!parser.readLineAsInteger(nAtoms)) return false;
	
	// Next line is name of model
	if (!parser.readLine(name)) return false;
	
	targetModel->setName(name);
	
	// Load atoms for model
	for (n=0; n<nAtoms; ++n)
	{
		if (!parser.parseLine()) break;
		
		// Create the new atom
		plugin->createAtom(targetModel, parser.argc(0), parser.arg3d(1));
	}
	
	// Rebond the model
	if (!plugin->standardOptions().preventRebonding()) targetModel->calculateBonding(true);
	
	return true;
}

// Skip single XYZ model in file
bool XYZFilePluginCommon::skipXYZModel(FilePluginInterface* plugin1, FileParser& parser)
{
	int nAtoms;

	// Read number of atoms from file
	if (!parser.readLineAsInteger(nAtoms)) return false;
	
	// Next line is name of model
	if (!parser.skipLines(1)) return false;
	
	// Now atoms...
	if (!parser.skipLines(nAtoms)) return false;

	return true;
}

// Write single XYZ model to file
bool XYZFilePluginCommon::writeXYZModel(FilePluginInterface* plugin, FileParser& parser, Model* sourceModel)
{
	// Write number atoms line
	if (!parser.writeLineF("%i", sourceModel->nAtoms())) return false;

	// Write title line
	if (!parser.writeLine(sourceModel->name())) return false;

	// Write atom information
	bool useTypeNames = plugin->pluginOptions().value("useTypeNames") == "true";
	for (Atom* i = sourceModel->atoms(); i != NULL; i = i->next)
	{
		if (useTypeNames && i->type())
		{
			if (!parser.writeLineF("%-8s  %12.6f %12.6f %12.6f %12.6f", qPrintable(i->type()->name()), i->r().x, i->r().y, i->r().z, i->charge())) return false;
		}
		else if (!parser.writeLineF("%-8s  %12.6f %12.6f %12.6f %12.6f", ElementMap().symbol(i->element()), i->r().x, i->r().y, i->r().z, i->charge())) return false;
	}

	return true;
}