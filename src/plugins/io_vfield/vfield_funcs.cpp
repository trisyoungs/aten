/*
        *** VField Model Plugin Functions
        *** src/plugins/io_vfield/vfield_funcs.cpp
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

#include "plugins/io_vfield/vfield.hui"
#include "model/model.h"

// Constructor
VFieldModelPlugin::VFieldModelPlugin()
{
}

// Destructor
VFieldModelPlugin::~VFieldModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* VFieldModelPlugin::makeCopy()
{
	return new VFieldModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory VFieldModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString VFieldModelPlugin::name() const
{
	return QString("VField (dlputils) 3D probability density");
}

// Nickname of plugin
QString VFieldModelPlugin::nickname() const
{
	return QString("vfield");
}

// Description (long name) of plugin
QString VFieldModelPlugin::description() const
{
	return QString("Import/export for dlputils VField files");
}

// Related file extensions
QStringList VFieldModelPlugin::extensions() const
{
	return QStringList() << "vfield";
}

// Exact names
QStringList VFieldModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool VFieldModelPlugin::canImport()
{
	return true;
}

// Import data from the spevfieldied file
bool VFieldModelPlugin::importData()
{
//filter(type="importmodel", name="Vector Field", nickname="vf", extension="vf", glob="*.vf",id=10)
//{
//	# Variable declaration
//	int n,i,j,k,nx,ny,nz;
//	vector r, v;
//
//	newModel(filterFilename());
//
//	# Read vector data
//	while (!eof())
//	{
//		readLine(r.x,r.y,r.z,v.x,v.y,v.z);
//		newGlyph("vector");
//		glyphData(1,r.x,r.y,r.z);
//		glyphData(2,v.x,v.y,v.z);
//	}
//	finaliseModel();
//}
	return true;
}

// Return whether this plugin can export data
bool VFieldModelPlugin::canExport()
{
	return false;
}

// Export data to the spevfieldied file
bool VFieldModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool VFieldModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool VFieldModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool VFieldModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool VFieldModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool VFieldModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool VFieldModelPlugin::showExportOptionsDialog()
{
	return false;
}
 
