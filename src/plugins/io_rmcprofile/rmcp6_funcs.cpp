/*
        *** RMCProfile V6 Plugin Functions
        *** src/plugins/io_rmcprofile/rmcp6_funcs.cpp
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

#include "plugins/io_rmcprofile/rmcp6.hui"
#include "plugins/io_rmcprofile/common.h"
// #include "plugins/io_rmcprofile/rmcp6exportoptions.h"
// #include "plugins/io_rmcprofile/rmcp6importoptions.h"
#include "model/model.h"

// Constructor
RMCProfile6ModelPlugin::RMCProfile6ModelPlugin()
{
	// Setup plugin options
	pluginOptions_.add("myOption", "false");
}

// Destructor
RMCProfile6ModelPlugin::~RMCProfile6ModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* RMCProfile6ModelPlugin::makeCopy()
{
	return new RMCProfile6ModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory RMCProfile6ModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString RMCProfile6ModelPlugin::name() const
{
	return QString("RMCProfile v6 Coordinates");
}

// Nickname of plugin
QString RMCProfile6ModelPlugin::nickname() const
{
	return QString("rmc6f");
}

// Description (long name) of plugin
QString RMCProfile6ModelPlugin::description() const
{
	return QString("Import for RMCProfile v6 coordinate files");
}

// Related file extensions
QStringList RMCProfile6ModelPlugin::extensions() const
{
	return QStringList() << "rmc6f";
}

// Exact names
QStringList RMCProfile6ModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool RMCProfile6ModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool RMCProfile6ModelPlugin::importData()
{
	// Create new model to read into
	createModel();

	// Loop over file looking for colon lines (ewww...)
	int nExpectedAtoms = -1;
	QString line;
	do
	{
		// Read in another line
		if (!fileParser_.readLine(line)) return false;

		// Parse the line into arguments here, so we have both the original line and delimited data
		fileParser_.parseString(line);

		// Keywords are case-insensitive, to convert the line to lowercase
		line = line.toLower();
		if (line.startsWith("metadata title:")) targetModel()->setName(line.mid(16).trimmed());
		else if (line.startsWith("number of atoms:")) nExpectedAtoms = fileParser_.argi(3);
		else if (line.startsWith("cell (ang/deg):"))
		{
			// If the cell has already been set, don't do anything here (lattice vectors take precedence)
			if (targetModel()->isPeriodic()) continue;
			targetModel()->setCell(fileParser_.arg3d(2), fileParser_.arg3d(5));
		}
		else if (line.startsWith("lattice vectors (ang):"))
		{
			Matrix axes;
			if (!fileParser_.parseLine()) return false;
			axes.setColumn(0, fileParser_.arg3d(0), 0.0);
			if (!fileParser_.parseLine()) return false;
			axes.setColumn(1, fileParser_.arg3d(0), 0.0);
			if (!fileParser_.parseLine()) return false;
			axes.setColumn(2, fileParser_.arg3d(0), 0.0);

			targetModel()->setCell(axes);
		}
		else if (line.startsWith("atoms:"))
		{
			// Read in atom data until we have either read in nExpectedAtoms (or get to the end of the file)
			QString symbol;
			Vec3<double> r;
			do
			{
				if (!fileParser_.parseLine()) return false;

				// Lines may start with an optional integer index for the atom - try to get it as an int...
				if (fileParser_.argi(0) == 0)
				{
					// No starting integer on line. The second piece of data may be another optional integer in square brackets
					if (fileParser_.argc(1).contains('['))
					{
						symbol = fileParser_.argc(0);
						r = fileParser_.arg3d(2);
					}
					else
					{
						symbol = fileParser_.argc(0);
						r = fileParser_.arg3d(1);
					}
				}
				else
				{
					// Line starts with an integer. The third piece of data may be another optional integer in square brackets
					if (fileParser_.argc(2).contains('['))
					{
						symbol = fileParser_.argc(1);
						r = fileParser_.arg3d(3);
					}
					else
					{
						symbol = fileParser_.argc(1);
						r = fileParser_.arg3d(2);
					}
				}

				// Create the atom
				if (symbol == "D") symbol = "H";
				createAtom(targetModel(), symbol, r);

				if (--nExpectedAtoms == 0) break;
			} while (!fileParser_.eofOrBlank());
		}
		// Could read in all other metadata etc. here, and store in plugin options so that files could be saved out in the same format with the same content
	} while (!fileParser_.eofOrBlank());

	// Coordinates were given in fractional cell coords, so convert them now
	targetModel()->fracToReal();

	// Recalculate bonding
	if (!standardOptions_.isSetAndOn(FilePluginStandardImportOptions::PreventRebondingSwitch)) targetModel()->calculateBonding(true);

	return true;
}

// Return whether this plugin can export data
bool RMCProfile6ModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool RMCProfile6ModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool RMCProfile6ModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool RMCProfile6ModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool RMCProfile6ModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool RMCProfile6ModelPlugin::showImportOptionsDialog()
{
// 	RMCProfile6ImportOptionsDialog optionsDialog(pluginOptions_);

// 	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
	return false;
}

// Return whether the plugin has export options
bool RMCProfile6ModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool RMCProfile6ModelPlugin::showExportOptionsDialog()
{
// 	RMCProfile6ExportOptionsDialog optionsDialog(pluginOptions_);

// 	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
	return false;
}
