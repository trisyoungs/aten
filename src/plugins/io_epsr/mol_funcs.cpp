/*
        *** EPSRMol Model Plugin Functions
        *** src/plugins/io_epsr/mol_funcs.cpp
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

#include "plugins/io_epsr/mol.hui"
#include "model/model.h"

// Constructor
EPSRMolModelPlugin::EPSRMolModelPlugin()
{
}

// Destructor
EPSRMolModelPlugin::~EPSRMolModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* EPSRMolModelPlugin::makeCopy()
{
	return new EPSRMolModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType EPSRMolModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int EPSRMolModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString EPSRMolModelPlugin::name() const
{
	return QString("EPSR Molfiles");
}

// Nickname of plugin
QString EPSRMolModelPlugin::nickname() const
{
	return QString("epsrmol");
}

// Description (long name) of plugin
QString EPSRMolModelPlugin::description() const
{
	return QString("Import/export for Empirical Potential Structure Refinement (EPSR) mol models");
}

// Related file extensions
QStringList EPSRMolModelPlugin::extensions() const
{
	return QStringList() << "mol";
}

// Exact names
QStringList EPSRMolModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool EPSRMolModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool EPSRMolModelPlugin::importData()
{
//filter(type="exportmodel",name="EPSR Molfile", extension="mol", glob="*.mol", nickname="epsrmol")
//{
//	# Variable declaration
//	Pattern p;
//	int nvdw, n, nc, n2, nconstraints;
//	Model m = aten.frame;
//
//	# Main dialog creation function
//	void createDefaultDialog(Dialog ui)
//	{
//		Widget w;
//		ui.title = "EPSR Mol File Export Options";
//		ui.verticalFill = TRUE;
//
//		ui.addDoubleSpin("temperature", "Temperature", 0.0, 100000.0, 10.0, 300.0);
//		ui.addDoubleSpin("epsr_vtemp", "Vibrational Temperature", 0.0, 1500.0, 10.0, 65.0);
//		ui.addDoubleSpin("epsr_density", "Density (atoms/ang3)", 0.0, 10.0, 0.01, 0.1);
//		ui.addDoubleSpin("epsr_ecore", "Energy for core repulsion terms", 0.0, 10.0, 0.1, 1.0);
//		ui.addDoubleSpin("epsr_dcore", "Distance for core repulsion terms", 0.0, 10.0, 0.1, 1.0);
//	}
//	if (!showDefaultDialog()) error("Canceled through dialog.\n");
//	Dialog ui = defaultDialog();
//
//	# Create a temporary forcefield to store our terms in
//	Forcefield dummyFF = newFF("Dummy FF");
//
//	# Write distinguishing header line - we will use no atom ID offset
//	writeLineF(" .gmol           0\n");
//
//	# Write atom coordinates and bonds
//	for (Atom i = m.atoms; i; ++i)
//	{
//		writeLineF(" atom %i %s %f %f %f %i", i.id, i.type.name, i.rx, i.ry, i.rz, i.nBonds);
//		if (!dummyFF.findType(i.type.name))
//		{
//			int id = dummyFF.nAtomTypes+1;
//			FFAtom x = dummyFF.addType(id, i.type.name, i.type.name, i.z, "", "");
//			dummyFF.addInter("lj", id, i.type.charge, i.type.data[1], i.type.data[2]);
//		}
//		for (Bond b = i.bonds; b; ++b) writeLineF(" %i", b.partner(i).id);
//		writeLineF("\n");
//	}
//
//	# Loop over bonds and angles in the system, creating a 'dummy' forcefield for them 
//	for (Bond b = m.bonds; b; ++b)
//	{
//		if (dummyFF.findBond(b.i.type.name, b.j.type.name)) continue;
//		dummyFF.addBond("harmonic", b.i.type.name, b.j.type.name, 0.0, geometry(b.i, b.j));
//		writeLineF("bond %s %s %f\n", b.i.type.name, b.j.type.name, geometry(b.i, b.j));
//	}
//	for (Atom j = m.atoms; j; ++j)
//	{
//		for (Bond b1 = j.bonds; b1; ++b1)
//		{
//			for (Bond b2 = b1; b2; ++b2)
//			{
//				if (b1 == b2) continue;
//				Atom i = b1.partner(j);
//				Atom k = b2.partner(j);
//				if (dummyFF.findAngle(i.type.name, j.type.name, k.type.name)) continue;
//				dummyFF.addAngle("harmonic", i.type.name, j.type.name, k.type.name, 0.0, geometry(i, j, k));
//				writeLineF("angle %s %s %s %f\n", i.type.name, j.type.name, k.type.name, geometry(i, j, k));
//			}
//		}
//	}
//
//	# Write dihedral information
//	for (Bond b = m.bonds; b; ++b)
//	{
//		# Loop over other bonds at first terminus, excluding the central one 'b'
//		for (Bond bi = b.i.bonds; bi; ++bi)
//		{
//			if (bi == b) continue;
//
//			# Loop over other bonds at second terminus, excluding the central one 'b'
//			for (Bond bj = b.j.bonds; bj; ++bj)
//			{
//				if (bj == b) continue;
//				Atom i = bi.partner(b.i);
//				Atom j = b.i;
//				Atom k = b.j;
//				Atom l = bj.partner(b.j);
//				writeLineF("dihedral %3i %3i %3i %3i %f\n", i.id, j.id, k.id, l.id, geometry(i,j,k,l) );
//			}
//		}
//	}
//
//	# Write rotational headgroup specifications
//	for (Bond b = m.bonds; b; ++b)
//	{
//		if (b.i.fixed && b.j.fixed) continue;
//		if (b.i.nBonds == 1) continue;
//		if (b.j.nBonds == 1) continue;
//		writeLineF("rot  %i  %i\n", b.i.id, b.j.id);
//	}
//
//	# Potential parameters
//	for (n=2; n<=dummyFF.nAtomTypes; ++n)
//	{
//		FFAtom at = dummyFF.atomTypes[n];
//		writeLineF("potential %s %f %f %f %f %s\n", at.name, at.data[1], at.data[2], aten.elements[at.z].mass, at.charge, aten.elements[at.z].symbol);
//	}
//
//	# Other variables
//	writeLineF("temperature %f\n", ui.asDouble("temperature"));
//	writeLineF("vibtemp %f\n", ui.asDouble("epsr_vtemp"));
//	writeLineF("density %f\n", ui.asDouble("epsr_density"));
//	writeLineF("ecoredcore %f %f\n", ui.asDouble("epsr_ecore"), ui.asDouble("epsr_dcore"));
//
//	# Remove our temporary forcefield
//	deleteFF(dummyFF);
//}
//
	return true;
}

// Return whether this plugin can export data
bool EPSRMolModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool EPSRMolModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool EPSRMolModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool EPSRMolModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool EPSRMolModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool EPSRMolModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool EPSRMolModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool EPSRMolModelPlugin::showExportOptionsDialog()
{
	return false;
}
 
