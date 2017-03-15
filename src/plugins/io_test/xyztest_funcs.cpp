/*
        *** Simple XYZ Plugin Functions
        *** src/plugins/io_test/xyztest_funcs.cpp
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

#include "plugins/io_test/xyztest.hui"
#include "model/model.h"

// Constructor
TestModelPlugin::TestModelPlugin()
{
}

// Destructor
TestModelPlugin::~TestModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* TestModelPlugin::makeCopy() const
{
	return new TestModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType TestModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int TestModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString TestModelPlugin::name() const
{
	return QString("XYZ Files (XMol Style) (Test)");
}

// Nickname of plugin
QString TestModelPlugin::nickname() const
{
	return QString("xyz");
}

// Return whether the plugin is enabled
bool TestModelPlugin::enabled() const
{
	return false;
}

// Description (long name) of plugin
QString TestModelPlugin::description() const
{
	return QString("Import/export for XMol-style XYZ coordinate files (Test)");
}

// Related file extensions
QStringList TestModelPlugin::extensions() const
{
	return QStringList() << "xyz";
}

// Exact names
QStringList TestModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool TestModelPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool TestModelPlugin::importData()
{
	int nAtoms, n;
	QString e, name;
	Vec3<double> r;

	// Read data
	while (!fileParser_.eofOrBlank())
	{
		// Read number of atoms from file
		if (!fileParser_.readLineAsInteger(nAtoms)) return false;

		// Next line is name of model
		if (!fileParser_.readLine(name)) return false;

		// Create a new model now....
		createModel();
		targetModel()->setName(name);

		// Load atoms for model
		for (n=0; n<nAtoms; ++n)
		{
			if (!fileParser_.parseLine()) break;

			// Create the new atom
			r.set(fileParser_.argd(1), fileParser_.argd(2), fileParser_.argd(3));
			targetModel()->addAtom(ElementMap().find(fileParser_.argc(0)), r);
		}

		// Rebond the model
		targetModel()->calculateBonding(true);
	}

	return true;
}

// Return whether this plugin can export data
bool TestModelPlugin::canExport() const
{
	return true;
}

// Export data to the specified file
bool TestModelPlugin::exportData()
{
	// Write number atoms line
	fileParser_.writeLineF("%i", targetModel()->nAtoms());

	// Write title line
	fileParser_.writeLine(targetModel()->name());

	// Write atom information
	for (Atom* i = targetModel()->atoms(); i != NULL; i = i->next)
	{
		fileParser_.writeLineF("%-8s  %12.6f %12.6f %12.6f %12.6f", ElementMap().symbol(i->element()), i->r().x, i->r().y, i->r().z, i->charge());
	}

	return true;
}

// Import next partial data chunk
bool TestModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool TestModelPlugin::skipNextPart()
{
	return false;
}


/*
 * Options
 */

// Return whether the plugin has import options
bool TestModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool TestModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool TestModelPlugin::hasExportOptions() const
{
	return false;
}

// Show export options dialog
bool TestModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}
