/*
        *** Vector Field Model Plugin Functions
        *** src/plugins/io_vfield/vfield_funcs.cpp
        Copyright T. Youngs 2016-2018

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
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* VFieldModelPlugin::makeCopy() const
{
	return new VFieldModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType VFieldModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int VFieldModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString VFieldModelPlugin::name() const
{
	return QString("Vector Field (6-column: x,y,z,vx,vy,vz)");
}

// Nickname of plugin
QString VFieldModelPlugin::nickname() const
{
	return QString("vfield");
}

// Return whether the plugin is enabled
bool VFieldModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString VFieldModelPlugin::description() const
{
	return QString("Import for vector fields");
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
bool VFieldModelPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool VFieldModelPlugin::importData()
{
	Model* model = createModel(fileParser_.filename());

	// Read vector data
	Glyph* glyph;
	while (fileParser_.parseLine())
	{
		glyph = model->addGlyph(Glyph::VectorGlyph);
		glyph->data(0)->setVector(fileParser_.arg3d(0));
		glyph->data(1)->setVector(fileParser_.arg3d(3));
	}

	return true;
}

// Return whether this plugin can export data
bool VFieldModelPlugin::canExport() const
{
	return false;
}

// Export data to the specified file
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
bool VFieldModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool VFieldModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool VFieldModelPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool VFieldModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}
 
