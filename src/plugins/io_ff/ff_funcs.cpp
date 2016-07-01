/*
        *** Aten Forcefield Plugin Functions
        *** src/plugins/io_ff/ff_funcs.cpp
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

#include "plugins/io_ff/ff.hui"
#include "model/model.h"
#include "base/forcefieldbound.h"

// Constructor
AtenExpressionPlugin::AtenExpressionPlugin()
{
}

// Destructor
AtenExpressionPlugin::~AtenExpressionPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* AtenExpressionPlugin::makeCopy()
{
	return new AtenExpressionPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType AtenExpressionPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int AtenExpressionPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString AtenExpressionPlugin::name() const
{
	return QString("Aten Forcefield Format");
}

// Nickname of plugin
QString AtenExpressionPlugin::nickname() const
{
	return QString("ff");
}

// Description (long name) of plugin
QString AtenExpressionPlugin::description() const
{
	return QString("Import for Aten format forcefield files");
}

// Related file extensions
QStringList AtenExpressionPlugin::extensions() const
{
	return QStringList() << "ff";
}

// Exact names
QStringList AtenExpressionPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool AtenExpressionPlugin::canImport()
{
	return false;
}

// Import data from the specified file
bool AtenExpressionPlugin::importData()
{
	return false;
}

// Return whether this plugin can export data
bool AtenExpressionPlugin::canExport()
{
	return true;
}

// Export data to the speffied file
bool AtenExpressionPlugin::exportData()
{
	// Variable declaration
// 	Pattern p;
// 	Atom i;
// 	FFBound b;
// 	FFAtom fi;
// 	double escale = 1.0, vscale = 1.0;
// 	int uselj = true, n, nconstraints, nfailed[3] = 0, nub, hasub;
// 	string forms, currentform;

	// Write header
	if (!fileParser_.writeLine("name \"" + targetModel()->name() + "\"")) return false;
	if (!fileParser_.writeLineF("units %s", Prefs::energyUnit(prefs.energyUnit()))) return false;
	if (!fileParser_.writeLine()) return false;

	// Write types information
	if (!fileParser_.writeLine("types")) return false;
	for (RefListItem<ForcefieldAtom,int>* ri = targetModel()->uniqueForcefieldTypes(); ri != NULL; ri = ri->next)
	{
		ForcefieldAtom* ffa = ri->item;
		if (!fileParser_.writeLineF("%5i\t%s\t%s\t\"%s\"\t\"%s\"\n", ffa->typeId(), qPrintable(ffa->name()), ElementMap::symbol(ffa->element()), qPrintable(ffa->netaString()), qPrintable(ffa->description()))) return false;
	}
	if (!fileParser_.writeLine("end")) return false;

	// Write short-range information
	// Loop over forms, writing those that match the current form
	for (int form = 0; form < VdwFunctions::nVdwFunctions; ++form)
	{
		int count = 0;
		for (RefListItem<ForcefieldAtom,int>* ri = targetModel()->uniqueForcefieldTypes(); ri != NULL; ri = ri->next)
		{
			ForcefieldAtom* ffa = ri->item;

			// Does this ffatom match the current form?
			if (ffa->vdwForm() != form) continue;
			if (count == 0)
			{
				// Write inter header
				if (!fileParser_.writeLine()) return false;
				if (!fileParser_.writeLineF("inter %s", VdwFunctions::functionData[form].keyword)) return false;
			}

			// Write type data
			if (!fileParser_.writeF("%5i\t%s\t%12.6f", ffa->typeId(), qPrintable(ffa->name()), ffa->charge())) return false;
			for (int n=0; n<VdwFunctions::functionData[form].nParameters; ++n) if (!fileParser_.writeF(" %12.6f", ffa->parameter(n))) return false;
			if (!fileParser_.writeLine()) return false;
			++count;
		}

		if (count != 0)
		{
			// Write end block line
			if (!fileParser_.writeLine("end")) return false;
		}
	}

	// Write bond potential information
	// Loop over forms, writing those that match the current form
	for (int form = 0; form < BondFunctions::nBondFunctions; ++form)
	{
		int count = 0;
		for (RefListItem<ForcefieldBound,int>* ri = targetModel()->forcefieldBonds(); ri != NULL; ri = ri->next)
		{
			ForcefieldBound* ffb = ri->item;

			// Does this ffatom match the current form?
			if (ffb->bondForm() != form) continue;
			if (count == 0)
			{
				// Write inter header
				if (!fileParser_.writeLine()) return false;
				if (!fileParser_.writeLineF("bonds %s", BondFunctions::functionData[form].keyword)) return false;
			}

			// Write type data
			if (!fileParser_.writeF("%s\t%s\t", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)))) return false;
			for (int n=0; n<BondFunctions::functionData[form].nParameters; ++n) if (!fileParser_.writeF(" %12.6f", ffb->parameter(n))) return false;
			if (!fileParser_.writeLine()) return false;
			++count;
		}

		if (count != 0)
		{
			// Write end block line
			if (!fileParser_.writeLine("end")) return false;
		}
	}

	// Write angle potential information
	// Loop over forms, writing those that match the current form
	for (int form = 0; form < AngleFunctions::nAngleFunctions; ++form)
	{
		int count = 0;
		for (RefListItem<ForcefieldBound,int>* ri = targetModel()->forcefieldAngles(); ri != NULL; ri = ri->next)
		{
			ForcefieldBound* ffb = ri->item;

			// Does this ffatom match the current form?
			if (ffb->angleForm() != form) continue;
			if (count == 0)
			{
				// Write inter header
				if (!fileParser_.writeLine()) return false;
				if (!fileParser_.writeLineF("angles %s", AngleFunctions::functionData[form].keyword)) return false;
			}

			// Write type data
			if (!fileParser_.writeF("%s\t%s\t", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)), qPrintable(ffb->typeName(2)))) return false;
			for (int n=0; n<AngleFunctions::functionData[form].nParameters; ++n) if (!fileParser_.writeF(" %12.6f", ffb->parameter(n))) return false;
			if (!fileParser_.writeLine()) return false;
			++count;
		}

		if (count != 0)
		{
			// Write end block line
			if (!fileParser_.writeLine("end")) return false;
		}
	}

	// Write torsion potential information
	// Loop over forms, writing those that match the current form
	for (int form = 0; form < TorsionFunctions::nTorsionFunctions; ++form)
	{
		int count = 0;
		for (RefListItem<ForcefieldBound,int>* ri = targetModel()->forcefieldTorsions(); ri != NULL; ri = ri->next)
		{
			ForcefieldBound* ffb = ri->item;

			// Does this ffatom match the current form?
			if (ffb->angleForm() != form) continue;
			if (count == 0)
			{
				// Write inter header
				if (!fileParser_.writeLine()) return false;
				if (!fileParser_.writeLineF("torsions %s", TorsionFunctions::functionData[form].keyword)) return false;
			}

			// Write type data
			if (!fileParser_.writeF("%s\t%s\t", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)), qPrintable(ffb->typeName(2)), qPrintable(ffb->typeName(3)))) return false;
			for (int n=0; n<TorsionFunctions::functionData[form].nParameters; ++n) if (!fileParser_.writeF(" %12.6f", ffb->parameter(n))) return false;
			if (!fileParser_.writeLine()) return false;
			++count;
		}

		if (count != 0)
		{
			// Write end block line
			if (!fileParser_.writeLine("end")) return false;
		}
	}

	return true;
}

// Import next partial data chunk
bool AtenExpressionPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool AtenExpressionPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool AtenExpressionPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool AtenExpressionPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool AtenExpressionPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool AtenExpressionPlugin::showExportOptionsDialog()
{
	return false;
}
 
