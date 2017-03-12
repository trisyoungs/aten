/*
        *** MOPAC 7.1 Control File Plugin Functions
        *** src/plugins/method_mopac71/mopac71control_funcs.cpp
        Copyright T. Youngs 2016-2017

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

#include "plugins/method_mopac71/mopac71control.hui"
#include "plugins/method_mopac71/controlexportoptions.h"
#include "plugins/method_mopac71/common.h"
#include "model/model.h"

// Constructor
MOPAC71ControlModelPlugin::MOPAC71ControlModelPlugin()
{
	// Set plugin options
	MOPAC71Common::initialiseOptions(pluginOptions_);
}

// Destructor
MOPAC71ControlModelPlugin::~MOPAC71ControlModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* MOPAC71ControlModelPlugin::makeCopy() const
{
	return new MOPAC71ControlModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType MOPAC71ControlModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int MOPAC71ControlModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString MOPAC71ControlModelPlugin::name() const
{
	return QString("MOPAC 7.1 control file");
}

// Nickname of plugin
QString MOPAC71ControlModelPlugin::nickname() const
{
	return QString("mopac71control");
}

// Description (long name) of plugin
QString MOPAC71ControlModelPlugin::description() const
{
	return QString("Basic export for MOPAC 7.1 control files");
}

// Related file extensions
QStringList MOPAC71ControlModelPlugin::extensions() const
{
	return QStringList() << "mop";
}

// Exact names
QStringList MOPAC71ControlModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool MOPAC71ControlModelPlugin::canImport() const
{
	return false;
}

// Import data from the specified file
bool MOPAC71ControlModelPlugin::importData()
{
	return false;
}

// Return whether this plugin can export data
bool MOPAC71ControlModelPlugin::canExport() const
{
	return true;
}

// Export data to the specified file
bool MOPAC71ControlModelPlugin::exportData()
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
	// -- If there are only three atoms we *must* use a zmatrix
	if (toBool(pluginOptions_.value("internal")) ||( targetModel()->nAtoms() <= 3))
	{
		ZMatrix* zmat = targetModel()->zMatrix();
		double distance, angle, torsion;
		int kk, jj, ll, count = 0;
		for (ZMatrixElement* ii = zmat->elements(); ii != NULL; ii = ii->next)
		{
			// Reset vars
			jj = 0;
			kk = 0;
			ll = 0;
			distance = 0.0;
			angle = 0.0;
			torsion = 0.0;

			// Grab ZMatrix data for element
			if (ii->distanceAtom())
			{
				jj = ii->distanceAtom()->id() + 1;
				distance = ii->distance();
			}
			if (ii->angleAtom())
			{
				kk = ii->angleAtom()->id() + 1;
				angle = ii->angle();
			}
			if (ii->torsionAtom())
			{
				ll = ii->torsionAtom()->id() + 1;
				torsion= ii->torsion();
			}

			// Write atom info
			fixedFlag = ii->atom(0)->isPositionFixed() ? 0 : 1;
			if (!fileParser_.writeLineF("%-3s  %11.6f %i  %11.6f %i  %11.6f %i  %i  %i  %i", ElementMap::symbol(ii->atom(0)->element()), distance, fixedFlag, angle, fixedFlag, torsion, jj, kk, ll)) return false;
		}
	}
	else
	{
		for (Atom* i = targetModel()->atoms(); i != NULL; i = i->next)
		{
			fixedFlag = i->isPositionFixed() ? 0 : 1;
			if (!fileParser_.writeLineF("%3s %12.6f %1i %12.6f %1i %12.6f %1i", ElementMap::symbol(i), i->r().x, fixedFlag, i->r().y, fixedFlag, i->r().z, fixedFlag)) return false;
		}
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
bool MOPAC71ControlModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool MOPAC71ControlModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool MOPAC71ControlModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool MOPAC71ControlModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool MOPAC71ControlModelPlugin::hasExportOptions() const
{
	return true;
}

// Show export options dialog
bool MOPAC71ControlModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	MOPAC71ControlExportOptionsDialog optionsDialog(targetOptions);

	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}
 
