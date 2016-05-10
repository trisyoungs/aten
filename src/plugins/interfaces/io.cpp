/*
        *** I/O Plugin Interface Definition
        *** src/plugins/interface/io.cpp
        Copyright T. Youngs 2007-2016

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

#include "plugins/interfaces/io.h"
#include "parser/commandnode.h"
#include <QFileInfo>

/*
 * Object Handling
 */

// Create new model (in Aten)
Model* IOPluginInterface::createModel()
{
	ReturnValue result = CommandNode::run(Commands::NewModel);
	Model* newModel = (Model*) result.asPointer(VTypes::ModelData);
	return newModel;
}

/*
 * File Handling
 */

// Perform secondary checks on whether this plugin can load the specified file
bool IOPluginInterface::secondaryProbe(QString filename)
{
	return false;
}

// Return whether this plugin can load the specified file
bool IOPluginInterface::probe(QString filename)
{
	// Get file information
	QFileInfo fileInfo(filename);
	if ((!fileInfo.exists()) || (!fileInfo.isReadable())) return false;

	// Check filename extensions (if the filename has an extension)
	if (!fileInfo.suffix().isEmpty()) for (int n=0; n<extensions().count(); ++n)
	{
		if (extensions().at(n) == fileInfo.suffix())
		{
			Messenger::print(Messenger::Verbose, "IOPluginInterface : Plugin '%s' matches file extension (%s).", qPrintable(name()), qPrintable(fileInfo.suffix()));
			return true;
		}
	}

	// Check for exact name matches
	for (int n=0; n<exactNames().count(); ++n)
	{
		if (exactNames().at(n) == fileInfo.fileName())
		{
			Messenger::print(Messenger::Verbose, "IOPluginInterface : Plugin '%s' matched exact name (%s).", qPrintable(name()), qPrintable(exactNames().at(n)));
			return true;
		}
	}

	// Perform secondary checks
	if (secondaryProbe(filename)) return true;

	return false;
}
