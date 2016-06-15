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

// Constructor
AtenForcefieldModelPlugin::AtenForcefieldModelPlugin()
{
}

// Destructor
AtenForcefieldModelPlugin::~AtenForcefieldModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* AtenForcefieldModelPlugin::makeCopy()
{
	return new AtenForcefieldModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory AtenForcefieldModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString AtenForcefieldModelPlugin::name() const
{
	return QString("AtenForcefield (dlputils) 3D probability density");
}

// Nickname of plugin
QString AtenForcefieldModelPlugin::nickname() const
{
	return QString("ff");
}

// Description (long name) of plugin
QString AtenForcefieldModelPlugin::description() const
{
	return QString("Import/export for dlputils AtenForcefield files");
}

// Related file extensions
QStringList AtenForcefieldModelPlugin::extensions() const
{
	return QStringList() << "ff";
}

// Exact names
QStringList AtenForcefieldModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool AtenForcefieldModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool AtenForcefieldModelPlugin::importData()
{
//filter(type="importexpression",name="Aten Forcefield Format", extension="ff", glob="*.ff", nickname="ff")
//{
//	loadFF(filterFilename());
//}
//
//filter(type="exportexpression",name="Aten Forcefield Format", extension="ff", glob="*.ff", nickname="ff")
//{
//	# Variable declaration
//        Pattern p;
//	Atom i;
//	FFBound b;
//	FFAtom fi;
//	double escale = 1.0, vscale = 1.0;
//	int uselj = TRUE, n, nconstraints, nfailed[3] = 0, nub, hasub;
//	string forms, currentform;
//	
//	# Get current model
//	Model m = aten.frame;
//
//	# Write header
//	writeLineF("name \"%s\"\n", m.name());
//	writeLineF("units %s\n\n", aten.prefs.energyUnit);
//
//	# Write types information
//	writeLine("types");
//	for (fi in m.ffTypes) writeLineF("%5i\t%s\t%s\t\"%s\"\t\"%s\"\n", fi.id, fi.name, aten.elements[fi.z].symbol, fi.neta, fi.description);
//	writeLine("end");
//
//	# Write short-range information
//	forms = "";
//	while (1)
//	{
//		# Search for next functional form type used
//		currentform = "";
//		for (fi in m.ffTypes) if (!contains(forms, "___"+fi.form+"___")) { currentform = fi.form; break; }
//		if (currentform == "") break;
//		
//		# Write out forms of this type
//		writeLineF("\ninter %s\n", currentform);
//		for (fi in m.ffTypes)
//		{
//			if (fi.form != currentform) continue;
//			writeLineF("%5i\t%s\t%12.6f", fi.id, fi.name, fi.charge);
//			for (n=1; n<=fi.nParams; ++n) writeLineF(" %12.6f", fi.data[n]);
//			writeLineF("\n");
//		}
//		writeLine("end");
//
//		# Add form to list
//		forms += " ___"+currentform+"___ ";
//	}
//
//	# Write bond information
//	forms = "";
//	while (1)
//	{
//		# Search for next functional form type used
//		currentform = "";
//		for (b in m.ffBonds) if (!contains(forms, "___"+b.form+"___")) { currentform = b.form; break; }
//		if (currentform == "") break;
//		
//		# Write out forms of this type
//		writeLineF("\nbonds %s\n", currentform);
//		for (b in m.ffBonds)
//		{
//			if (b.form != currentform) continue;
//			writeLineF("%s\t%s\t", b.typeNames[1], b.typeNames[2]);
//			for (n=1; n<=b.nParams; ++n) writeLineF(" %12.6f", b.data[n]);
//			writeLineF("\n");
//		}
//		writeLine("end");
//
//		# Add form to list
//		forms += " ___"+currentform+"___ ";
//	}
//
//	# Write angle information
//	forms = "";
//	while (1)
//	{
//		# Search for next functional form type used
//		currentform = "";
//		for (b in m.ffAngles) if (!contains(forms, "___"+b.form+"___")) { currentform = b.form; break; }
//		if (currentform == "") break;
//		
//		# Write out forms of this type
//		writeLineF("\nangles %s\n", currentform);
//		for (b in m.ffAngles)
//		{
//			if (b.form != currentform) continue;
//			writeLineF("%s\t%s\t%s\t", b.typeNames[1], b.typeNames[2], b.typeNames[3]);
//			for (n=1; n<=b.nParams; ++n) writeLineF(" %12.6f", b.data[n]);
//			writeLineF("\n");
//		}
//		writeLine("end");
//
//		# Add form to list
//		forms += " ___"+currentform+"___ ";
//	}
//
//	# Write torsion information
//	forms = "";
//	while (1)
//	{
//		# Search for next functional form type used
//		currentform = "";
//		for (b in m.ffTorsions) if (!contains(forms, "___"+b.form+"___")) { currentform = b.form; break; }
//		if (currentform == "") break;
//		
//		# Write out forms of this type
//		writeLineF("\ntorsions %s\n", currentform);
//		for (b in m.ffTorsions)
//		{
//			if (b.form != currentform) continue;
//			writeLineF("%s\t%s\t%s\t%s\t", b.typeNames[1], b.typeNames[2], b.typeNames[3], b.typeNames[4]);
//			for (n=1; n<=b.nParams; ++n) writeLineF(" %12.6f", b.data[n]);
//			writeLineF("\n");
//		}
//		writeLine("end");
//
//		# Add form to list
//		forms += " ___"+currentform+"___ ";
//	}
//}

	return true;
}

// Return whether this plugin can export data
bool AtenForcefieldModelPlugin::canExport()
{
	return false;
}

// Export data to the speffied file
bool AtenForcefieldModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool AtenForcefieldModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool AtenForcefieldModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool AtenForcefieldModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool AtenForcefieldModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool AtenForcefieldModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool AtenForcefieldModelPlugin::showExportOptionsDialog()
{
	return false;
}
 
