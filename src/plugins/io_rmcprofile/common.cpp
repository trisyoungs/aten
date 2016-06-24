/*
        *** Common functions for RMCProfile plugins
        *** src/plugins/io_rmcprofile/common.cpp
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
#include "plugins/io_rmcprofile/common.h"
#include "model/model.h"
#include "base/fileparser.h"

ATEN_USING_NAMESPACE

// Read single RMCProfile model from file
bool RMCProfileFilePluginCommon::readModel(FilePluginInterface* plugin, FileParser& parser, Model* targetModel)
{
	int nAtoms, n;
	QString e, name;
	
	// Check target model
	if (targetModel == NULL)
	{
		Messenger::error("NULL Model pointer passed to RMCProfileFilePluginCommon::readModel.");
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

