/*
        *** PDB Model Plugin Functions
        *** src/plugins/io_pdb/pdb_funcs.cpp
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

#include "plugins/io_pdb/pdb.hui"
#include "model/model.h"

// Constructor
PDBModelPlugin::PDBModelPlugin()
{
}

// Destructor
PDBModelPlugin::~PDBModelPlugin()
{
}

/*
 * Core
 */

// Return a copy of the plugin object
FilePluginInterface* PDBModelPlugin::makeCopy()
{
	return new PDBModelPlugin;
}

/*
 * Definition
 */

// Return category of plugin
PluginTypes::FilePluginCategory PDBModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString PDBModelPlugin::name() const
{
	return QString("PDB (dlputils) 3D probability density");
}

// Nickname of plugin
QString PDBModelPlugin::nickname() const
{
	return QString("pdb");
}

// Description (long name) of plugin
QString PDBModelPlugin::description() const
{
	return QString("Import/export for dlputils PDB files");
}

// Related file extensions
QStringList PDBModelPlugin::extensions() const
{
	return QStringList() << "pdb";
}

// Exact names
QStringList PDBModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool PDBModelPlugin::canImport()
{
	return true;
}

// Import data from the spepdbied file
bool PDBModelPlugin::importData()
{
//filter(type="importmodel", name="Protein Databank PDB", nickname="pdb", extension="pdb", glob="*.pdb", zmap="firstalpha", id=13)
//{
//	# Variable declarations
//	string keyword, line, e, name, discard;
//	element el;
//	double rx,ry,rz,ca,cb,cc,alpha,beta,gamma;
//	int n, i, id, bondatoms[5], scale123 = 0, origx123 = 0;
//	matrix scale, origx;
//	vector scale_u, origx_u;
//
//	# Main dialog creation function
//	void createDefaultDialog(Dialog ui)
//	{
//		ui.title = "PDB Import Options";
//		ui.addCheck("strictFormat", "Demand strict format", 1);
//	}
//	# Execute dialog
//	if (!showDefaultDialog()) error("Options dialog canceled.\n");
//
//	# Get dialog values
//	Dialog ui = defaultDialog();
//	int strictFormat = ui.asInteger("strictFormat");
//
//	# Create default model
//	Model m = newModel("PDB Model");
//	while (!eof())
//	{
//		readLineF("%-6s%100s",keyword,line);
//		if (keyword == "HEADER") printf("HEADER entry: %s\n", line);
//		else if (keyword == "TITLE") setName(line);
//		else if (keyword == "CRYST1")
//		{
//			if (strictFormat) readVarF(line, "%9f%9f%9f%7f%7f%7f", ca, cb, cc, alpha, beta, gamma);
//			else readVar(line, ca, cb, cc, alpha, beta, gamma);
//			cell(ca,cb,cc,alpha,beta,gamma);
//		}
//		else if ((keyword == "HETATM") || (keyword == "ATOM"))
//		{
//			#readVar $line "*@5 $e@4 *@1 *@1 *@3 *@1 *@1 *@4 *@1 *@3 $rx@8 $ry@8 $rz@8 *@6 *@6 *@6 *@4 *@2 *@2"
//			if (strictFormat) readVarF(line, "%5i %4s              %8f%8f%8f%22*%2s", id, name, rx, ry, rz, e);
//			else readVar(line, id, name, discard, discard, discard, discard, discard, discard, discard, discard, rx, ry, rz, e);
//			# Check 'e' - if not blank, try to convert it first before we resort to using 'name'
//			el = 0;
//			if (e != "") el = aten.findElement(e);
//			if (!el.z) el = aten.findElement(name);
//			newAtom(el, rx, ry, rz);
//		}
//		else if (keyword == "CONECT")
//		{
//			bondatoms = 0;
//			readVar(line,i,bondatoms[1],bondatoms[2],bondatoms[3],bondatoms[4],bondatoms[5]);
//			for (n=1; n<=5; ++n) if (bondatoms[n] > 0) newBond(i,bondatoms[n]);
//		}	
//		else if (keyword == "SCALE1")
//		{
//			if (strictFormat) readVarF(line, "    %10.6f%10.6f%10.6f     %10.5f", scale.xx, scale.xy, scale.xz, scale_u.x);
//			else readVar(line, scale.xx, scale.xy, scale.xz, scale_u.x);
//			++scale123;
//		}
//		else if (keyword == "SCALE2")
//		{
//			if (strictFormat) readVarF(line, "    %10.6f%10.6f%10.6f     %10.5f", scale.yx, scale.yy, scale.yz, scale_u.y);
//			else readVar(line, scale.yx, scale.yy, scale.yz, scale_u.y);
//			++scale123;
//		}
//		else if (keyword == "SCALE3")
//		{
//			if (strictFormat) readVarF(line, "    %10.6f%10.6f%10.6f     %10.5f", scale.zx, scale.zy, scale.zz, scale_u.z);
//			else readVar(line, scale.zx, scale.zy, scale.zz, scale_u.z);
//			++scale123;
//		}
//		else if (keyword == "ORIGX1")
//		{
//			if (strictFormat) readVarF(line, "    %10.6f%10.6f%10.6f     %10.5f", origx.xx, origx.xy, origx.xz, origx_u.x);
//			else readVar(line, origx.xx, origx.xy, origx.xz, origx_u.x);
//			++origx123;
//		}
//		else if (keyword == "ORIGX2")
//		{
//			if (strictFormat) readVarF(line, "    %10.6f%10.6f%10.6f     %10.5f", origx.yx, origx.yy, origx.yz, origx_u.y);
//			else readVar(line, origx.yx, origx.yy, origx.yz, origx_u.y);
//			++origx123;
//		}
//		else if (keyword == "ORIGX3")
//		{
//			if (strictFormat) readVarF(line, "    %10.6f%10.6f%10.6f     %10.5f", origx.zx, origx.zy, origx.zz, origx_u.z);
//			else readVar(line, origx.zx, origx.zy, origx.zz, origx_u.z);
//			++origx123;
//		}
//	}
//
//	# Did we find a full SCALE matrix?
//	if (scale123 == 999) for (Atom a = m.atoms(); a; ++a) a.r = a.r * scale + scale_u; 
//
//	# Did we find a full ORIGX matrix?
//	if (origx123 == 999) for (Atom a = m.atoms(); a; ++a) a.r = a.r * scale + scale_u; 
//
//	finaliseModel();
//}
//
//filter(type="exportmodel",name="Protein Databank PDB gromacs compatible", extension="pdb", glob="*.pdb", nickname="pdb", id=13)
//{
//	# Variable declaration
//	Model m = aten.frame;
//	Pattern p;
//	Atom i, part;
//	Bond b;
//	String s;
//	int k,j,l,n;
//
//	# Make sure patterns exist
//	createPatterns();
//
//	writeLine("REMARK   Aten-created PDB");
//	writeLineF("REMARK   Model %s\n",m.name);
//
//	# Write unit cell information
//	if (m.cell.type != "none")
//	{
//		writeLineF("CRYST1%9.4f%9.4f%9.4f%7.2f%7.2f%7.2f %s\n", m.cell.a,m.cell.b,m.cell.c,m.cell.alpha,m.cell.beta,m.cell.gamma,m.cell.sgName);
//	}
//
//	# Loop over patterns (molecule types) and write atom information
//	k=0;
//	i=m.atoms;
//	for (p = m.patterns; p; ++p)
//	{
//
//		++k;
//
//		for (j=1; j<= p.nMols; ++j)
//		{
//			for (l=1; l<=p.nMolAtoms; ++l)
//			{
//				s = i.symbol + itoa(l);
//				writeLineF("ATOM  %-5i %-4s MOL  %-4i    %8.3f%8.3f%8.3f%6.2f%6.2f          %2s\n",i.id,s,k,i.rx,i.ry,i.rz,1.0,1.0,i.symbol);
//				++i;
//			}
//		}	
//	
//	}
//
//	# Loop over atoms and write bond information
//	for (i=m.atoms; i; ++i)
//	{
//		k = 0;
//		
//		for (b=i.bonds; b; ++b)
//		{
//			part = b.partner(i);
//			if (part.id > i.id)
//			{
//				if (k == 0) writeLineF("CONECT%5i", i.id);
//				writeLineF("%5i", part.id);
//				++k;
//				if (k == 4)
//				{
//					writeLineF("\n");
//					k = 0;
//				}
//			}
//		}	
//		if (k != 0) writeLineF("\n");
//	}
//
//	writeLine("TER");
//}
//
//filter(type="importtrajectory",name="Multiple Protein Databank PDB", nickname="pdb", extension="pdb", glob="*.pdb", id=13)
//{
//	int readHeader()
//	{
//		return TRUE;
//	}
//
//	int readFrame()
//	{
//		# Variables
//		double rx, ry, rz, ca, cb, cc, alpha, beta, gamma;
//		element el;
//		int id, bondatoms[4], i, n;
//		string e, name, keyword, line;
//
//		while (1)
//		{
//			readLineF("%-6s%100s",keyword,line);
//			if ((keyword == "HETATM") || (keyword == "ATOM"))
//			{
//				#readVar $line "*@5 $e@4 *@1 *@1 *@3 *@1 *@1 *@4 *@1 *@3 $rx@8 $ry@8 $rz@8 *@6 *@6 *@6 *@4 *@2 *@2"
//				readVarF(line, "%5i %4s              %8f%8f%8f%22*%2s", id, name, rx, ry, rz, e);
//				# Check 'e' - if not blank, try to convert it first before we resort to using 'name'
//				el = 0;
//				if (e != "") el = aten.findElement(e);
//				if (!el.z) el = aten.findElement(name);
//				newAtom(el, rx, ry, rz);
//			}
//			else if (keyword == "REMARK") setName(line);
//			else if (keyword == "CRYST1")
//			{
//				readVarF(line, "%9f%9f%9f%7f%7f%7f", ca,cb,cc,alpha,beta,gamma);
//				cell(ca,cb,cc,alpha,beta,gamma);
//			}
//			else if (keyword == "CONECT")
//			{
//				bondatoms = 0;
//				readVar(line,i,bondatoms[1],bondatoms[2],bondatoms[3],bondatoms[4]);
//				for (n=1; n<=4; ++n) if (bondatoms[n] > 0) newBond(i,bondatoms[n]);
//			}
//			else if (keyword == "END") return TRUE;
//		}
//	}
//}
//
	return true;
}

// Return whether this plugin can export data
bool PDBModelPlugin::canExport()
{
	return false;
}

// Export data to the spepdbied file
bool PDBModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool PDBModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool PDBModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool PDBModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool PDBModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool PDBModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool PDBModelPlugin::showExportOptionsDialog()
{
	return false;
}
 
