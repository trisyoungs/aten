/*
        *** MOPAC Model Plugin Functions
        *** src/plugins/io_mopac/mopac_funcs.cpp
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

#include "plugins/io_mopac/mopac.hui"
#include "model/model.h"

// Constructor
MOPACModelPlugin::MOPACModelPlugin()
{
}

// Destructor
MOPACModelPlugin::~MOPACModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* MOPACModelPlugin::makeCopy()
{
	return new MOPACModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory MOPACModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString MOPACModelPlugin::name() const
{
	return QString("MOPAC (dlputils) 3D probability density");
}

// Nickname of plugin
QString MOPACModelPlugin::nickname() const
{
	return QString("mopac");
}

// Description (long name) of plugin
QString MOPACModelPlugin::description() const
{
	return QString("Import/export for dlputils MOPAC files");
}

// Related file extensions
QStringList MOPACModelPlugin::extensions() const
{
	return QStringList() << "mopac";
}

// Exact names
QStringList MOPACModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool MOPACModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool MOPACModelPlugin::importData()
{
//filter(type="importmodel", name="Mopac Control File", nickname="mopac", extension="mop", glob="*.mop", id=4)
//{
//	# Variable declaration
//	int natoms,n,cellcount = 0,fx,fy,fz,ntitle=2, morekeywords, lesstitle;
//	string e,line,title;
//	double rx,ry,rz;
//	vector v[3];
//
//	# Discard keyword lines.
//	# A '+' in a line indicates that another line of keywords will follow.
//	# A '&' in a line indicates that a title/description line is replaced by keywords.
//	while (getLine(line))
//	{
//		if (contains(line,"&") == 0) { ntitle--; continue; }
//		if (contains(line,"+") != 0) continue;	# Read another line of keywords
//		break;
//	}
//
//	# Description lines follow, unless replaced by keyword lines with '&' in previous lines
//	title = "Mopac Input";
//	if (ntitle > 0) getLine(title);
//	if (ntitle > 1)	getLine(line);
//	newModel(title);
//
//	# All remaining lines make up the geometry
//	while (!eof())
//	{
//		readLine(e,rx,fx,ry,fy,rz,fy);
//		# If the element is 'Tv' its part of the cell specification
//		if (e == "Tv")
//		{
//			++cellcount;
//			if (cellcount < 4)
//			{
//				v[cellcount].x = rx;
//				v[cellcount].y = ry;
//				v[cellcount].z = rz;
//			}
//			else printf("Extra translation vector data found - ignored...\n");
//		}
//		else newAtom(e,rx,ry,rz);
//	}
//
//	# Were we supplied with a full cell description?
//	if (cellcount >= 3) cellAxes(v[1].x,v[1].y,v[1].z,v[2].x,v[2].y,v[2].z,v[3].x,v[3].y,v[3].z);
//
//	rebond();
//	finaliseModel();
//}
//
//filter(type="exportmodel", name="Mopac Control File", nickname="mopac", extension="mop", glob="*.mop", id=4)
//{
//	# Variable declaration
//	int n,f;
//	string control1;
//	double rx,ry,rz;
//	Atom i;
//	Model m = aten.frame;
//	
//	# Main dialog creation function
//	void createDefaultDialog(Dialog ui)
//	{
//		ui.addCombo("jobtype", "Job Type", "'1SCF (Single SCF)','EF (Optimisation)','BFGS (Optimisation)','FORCE (Vibrations)','FORCETS (Vibrations)'", 3);
//		ui.addCombo("hamil", "Hamiltonian", "AM1,MNDO,MNDOD,PM3,PM6,PM6-DH2,RM1", 5);
//		ui.addCombo("scftyp", "SCF Type", "RHF,UHF", 1);
//		ui.addCombo("state", "State", "SINGLET,EXCITED,DOUBLET,TRIPLET,QUARTET,QUINTET,SEXTET,SEPTET,OCTET,NONET", 1);
//		ui.addIntegerSpin("charge", "Charge", -100, 100, 1, 0);
//		ui.addCheck("precise", "Precise", 0, 1, 2);
//		ui.addCheck("bfgs", "BFGS", 0);
//		ui.addCheck("campking", "Camp King", 0);
//		ui.addCheck("mozyme", "Mozyme", 0);
//		ui.addEdit("control2", "Extra Keywords", "", 1, 3);
//	}
//
//	# Execute dialog
//	if (!showDefaultDialog()) error("Options dialog canceled.\n");
//	Dialog ui = defaultDialog();
//
//	# Generate control lines
//	control1 = beforeStr(ui.asString("jobtype")," ",TRUE) + " " + ui.asString("hamil") + " " + ui.asString("scftyp") + " ";
//	if (ui.asInteger("charge") != 0) control1 += ui.asString("charge") + " ";
//	if (ui.asString("state") != "SINGLET") control1 += ui.asString("state") + " ";
//	if (ui.asInteger("precise")) control1 += "PRECISE ";
//	if (ui.asInteger("campking")) control1 += "KING ";
//	if (ui.asString("control2") != "") control1 += "+";
//	writeLine(control1);
//	if (ui.asString("control2") != "") writeLine(ui.asString("control2"));
//	writeLine(m.name);
//	writeLine("Coordinates churned out by Aten.");
//	for (i = m.atoms; i != 0; ++i)
//	{
//		f = 1 - i.fixed;
//		writeLineF("%3s %12.6f %1i %12.6f %1i %12.6f %1i\n", i.symbol, i.rx, f, i.ry, f, i.rz, f);
//	}
//
//	# Write translation vector for cell
//	if (m.cell.type <> "none")
//	{
//		writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0\n",m.cell.ax,m.cell.ay,m.cell.az);
//		writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0\n",m.cell.bx,m.cell.by,m.cell.bz);
//		writeLineF("Tv  %12.6f 0 %12.6f 0 %12.6f 0\n",m.cell.cx,m.cell.cy,m.cell.cz);
//	}
//}
//
//filter(type="importmodel", name="Mopac Archive File", nickname="mopacarc", extension="arc", glob="*.arc", id=16)
//{
//	# Variable declaration
//	int natoms,n,cellcount = 0,ntitle=2, morekeywords, lesstitle;
//	string e,line,title;
//	double rx,ry,rz,q;
//	atom i;
//	vector v[3];
//
//	# Search for geometry data in file...
//	if (find("FINAL GEOMETRY OBTAINED") == 0) error("No geometry found in file.");
//
//	# Discard keyword lines.
//	# A '+' in a line indicates that another line of keywords will follow.
//	# A '&' in a line indicates that a title/description line is replaced by keywords.
//	while (getLine(line))
//	{
//		if (contains(line,"&") == 0) { ntitle--; continue; }
//		if (contains(line,"+") != 0) continue;	# Read another line of keywords
//		break;
//	}
//
//	# Description lines follow, unless replaced by keyword lines with '&' in previous lines
//	title = "Mopac Archive";
//	if (ntitle > 0) getLine(title);
//	if (ntitle > 1)	getLine(line);
//	newModel(title);
//
//	# All remaining lines make up the geometry
//	while (!eof())
//	{
//		readLineF("%3s  %13f   %13f   %13f   %f",e,rx,ry,rz,q);
//		# If the element is 'Tv' its part of the cell specification
//		if (e == " Tv")
//		{
//			++cellcount;
//			if (cellcount < 4)
//			{
//				v[cellcount].x = rx;
//				v[cellcount].y = ry;
//				v[cellcount].z = rz;
//			}
//			else printf("Extra translation vector data found - ignored...\n");
//		}
//		else
//		{
//			i = newAtom(e,rx,ry,rz);
//			i.q = q;
//		}
//	}
//
//	# Were we supplied with a full cell description?
//	if (cellcount >= 3) cellAxes(v[1].x,v[1].y,v[1].z,v[2].x,v[2].y,v[2].z,v[3].x,v[3].y,v[3].z);
//
//	rebond();
//	finaliseModel();
//}
	return true;
}

// Return whether this plugin can export data
bool MOPACModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool MOPACModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool MOPACModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool MOPACModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool MOPACModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool MOPACModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool MOPACModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool MOPACModelPlugin::showExportOptionsDialog()
{
	return false;
}
 
