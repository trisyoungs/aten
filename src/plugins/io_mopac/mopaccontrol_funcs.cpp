/*
        *** MOPAC Control File Plugin Functions
        *** src/plugins/io_mopac/mopaccontrol_funcs.cpp
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

#include "plugins/io_mopac/mopaccontrol.hui"
#include "plugins/io_mopac/controlexportoptions.h"
#include "plugins/io_mopac/common.h"
#include "model/model.h"

// Constructor
MOPACControlModelPlugin::MOPACControlModelPlugin()
{
	// Set plugin options
	pluginOptions_.add("jobtype", "BFGS");
	pluginOptions_.add("hamiltonian", "PM6");
	pluginOptions_.add("scftype", "RHF");
	pluginOptions_.add("state", "SINGLET");
	pluginOptions_.add("charge", "0");
	pluginOptions_.add("precise", "false");
	pluginOptions_.add("bfgs", "true");
	pluginOptions_.add("campking", "false");
	pluginOptions_.add("mozyme", "false");
	pluginOptions_.add("extra", "");
}

// Destructor
MOPACControlModelPlugin::~MOPACControlModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* MOPACControlModelPlugin::makeCopy() const
{
	return new MOPACControlModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType MOPACControlModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int MOPACControlModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString MOPACControlModelPlugin::name() const
{
	return QString("MOPAC control file");
}

// Nickname of plugin
QString MOPACControlModelPlugin::nickname() const
{
	return QString("mopaccontrol");
}

// Description (long name) of plugin
QString MOPACControlModelPlugin::description() const
{
	return QString("Basic import/export for MOPAC control files");
}

// Related file extensions
QStringList MOPACControlModelPlugin::extensions() const
{
	return QStringList() << "mop";
}

// Exact names
QStringList MOPACControlModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool MOPACControlModelPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool MOPACControlModelPlugin::importData()
{
	if (!MOPACFilePluginCommon::readMOPACModel(this, fileParser_, standardOptions_, createModel())) return false;
}

// Return whether this plugin can export data
bool MOPACControlModelPlugin::canExport() const
{
	return true;
}

// Export data to the specified file
bool MOPACControlModelPlugin::exportData()
{
	// Generate control lines
	int charge = pluginOptions_.value("charge").toInt();
	QString control1 = pluginOptions_.value("jobtype") + " " + pluginOptions_.value("hamiltonian") + " " + pluginOptions_.value("scftype") + " ";
	if (charge != 0) control1 += QString::number(charge) + " ";
	if (pluginOptions_.value("state") != "SINGLET") control1 += pluginOptions_.value("state") + " ";
	if (toBool(pluginOptions_.value("precise"))) control1 += "PRECISE ";
	if (toBool(pluginOptions_.value("campking"))) control1 += "KING ";
	if (!pluginOptions_.value("extra").isEmpty()) control1 += "+";

	// Write control and title lines
	if (!fileParser_.writeLine(control1)) return false;
	if (pluginOptions_.value("extra") != "") if (!fileParser_.writeLine(pluginOptions_.value("control2"))) return false;
	if (!fileParser_.writeLine(targetModel()->name())) return false;
	if (!fileParser_.writeLine("Coordinates churned out by Aten.")) return false;

	// Write atoms
	int fixedFlag;
	for (Atom* i = targetModel()->atoms(); i != NULL; i = i->next)
	{
		fixedFlag = i->isPositionFixed() ? 0 : 1;
		if (!fileParser_.writeLineF("%3s %12.6f %1i %12.6f %1i %12.6f %1i", ElementMap::symbol(i), i->r().x, fixedFlag, i->r().y, fixedFlag, i->r().z, fixedFlag)) return false;
	}

	// Write translation vector for cell
	if (targetModel()->isPeriodic())
	{
		Matrix axes = targetModel()->cell().axes();
		if (!fileParser_.writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0\n", axes[0], axes[1], axes[2])) return false;
		if (!fileParser_.writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0\n", axes[4], axes[5], axes[6])) return false;
		if (!fileParser_.writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0\n", axes[8], axes[9], axes[10])) return false;
	}

	return true;
}

// Import next partial data chunk
bool MOPACControlModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool MOPACControlModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool MOPACControlModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool MOPACControlModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool MOPACControlModelPlugin::hasExportOptions() const
{
	return true;
}

// Show export options dialog
bool MOPACControlModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	MOPACControlExportOptionsDialog optionsDialog(targetOptions);

	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}
 
