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
#include "plugins/io_pdb/pdbimportoptions.h"
#include "model/model.h"
#include "base/pattern.h"
#include "sg/spacegroup.h"

// Constructor
PDBModelPlugin::PDBModelPlugin()
{
	// Adjust standard options
	standardOptions_.setZMappingType(ElementMap::FirstAlphaZMap);

	// Setup plugin options
	pluginOptions_.add("strictFormat", "true");
}

// Destructor
PDBModelPlugin::~PDBModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* PDBModelPlugin::makeCopy()
{
	return new PDBModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType PDBModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int PDBModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString PDBModelPlugin::name() const
{
	return QString("Protein Databank (PDB) model");
}

// Nickname of plugin
QString PDBModelPlugin::nickname() const
{
	return QString("pdb");
}

// Description (long name) of plugin
QString PDBModelPlugin::description() const
{
	return QString("Basic import/export for Protein Databank model files");
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

// Import data from the specified file
bool PDBModelPlugin::importData()
{
	// Create default model
	createModel("PDB Model");

	// Get strict flag, and define strict formats
	bool strict = pluginOptions_.value("strictFormat") == "true";
	ParseFormat generalFormat("%-6s%r");
	ParseFormat crystFormat("%9f%9f%9f%7f%7f%7f");
	ParseFormat atomFormat("%5i %4s              %8f%8f%8f%22*%2s");
	ParseFormat scaleOriginFormat("    %10.6f%10.6f%10.6f     %10.5f");

	QString keyword, data;
	int ii, n;
	Vec3<int> scaleFlags, originFlags;
	Matrix scaleMatrix, originMatrix;
	Vec3<double> scaleOffset, originOffset;

	while (!fileParser_.eofOrBlank())
	{
		// Read in line, split into keyword and 'rest of line'
		if (!fileParser_.parseLine(generalFormat, Parser::Defaults)) return false;
		keyword = fileParser_.argc(0);
		data = fileParser_.argc(1);
		if (keyword == "HEADER") Messenger::print("HEADER entry: " + data);
		else if (keyword == "TITLE") targetModel()->setName(data);
		else if (keyword == "CRYST1")
		{
			// Crystal information is contained in 'data'
			if (strict) fileParser_.parseString(data, crystFormat);
			else fileParser_.parseString(data);
			targetModel()->cell().setLengths(fileParser_.arg3d(0));
			targetModel()->cell().setAngles(fileParser_.arg3d(3));
		}
		else if ((keyword == "HETATM") || (keyword == "ATOM"))
		{
			// Try to convert element string (last two characters of line) if present, before resorting to the atom name
			if (strict)
			{
				fileParser_.parseString(data, atomFormat);  //"%5i %4s              %8f%8f%8f%22*%2s", id, name, rx, ry, rz, e);
				if (fileParser_.hasArg(5) && (!fileParser_.argc(5).isEmpty())) createAtom(targetModel(), fileParser_.argc(5), fileParser_.arg3d(2));
				else createAtom(targetModel(), fileParser_.argc(1), fileParser_.arg3d(2));
			}
			else
			{
				fileParser_.parseString(data); // readVar(line, id, name, discard, discard, discard, discard, discard, discard, discard, discard, rx, ry, rz, e);
				if (fileParser_.hasArg(13)) createAtom(targetModel(), fileParser_.argc(13), fileParser_.arg3d(10));
				else createAtom(targetModel(), fileParser_.argc(1), fileParser_.arg3d(10));
			}
		}
		else if (keyword == "CONECT")
		{
			fileParser_.parseString(data);
			ii = fileParser_.argi(0) - 1;
			for (n = 1; n<fileParser_.nArgs(); ++n) targetModel()->bondAtoms(ii, fileParser_.argi(n)-1, Bond::Single);
		}
		else if (keyword.startsWith("SCALE"))
		{
			if (strict) fileParser_.parseString(data, scaleOriginFormat);
			else fileParser_.parseString(data);
			int scaleIndex = keyword.right(1).toInt() - 1;
			scaleMatrix.setColumn(scaleIndex, fileParser_.arg3d(0), 0.0);
			scaleFlags.set(scaleIndex, 1);
		}
		else if (keyword.startsWith("ORIGX"))
		{
			if (strict) fileParser_.parseString(data, scaleOriginFormat);
			else fileParser_.parseString(data);
			int originIndex = keyword.right(1).toInt() - 1;
			originMatrix.setColumn(originIndex, fileParser_.arg3d(0), 0.0);
			originFlags.set(originIndex, 1);
		}
	}

	// Did we find a full SCALE matrix?
// 	if (scaleFlags.sum() == 3) 
//
//	# Did we find a full ORIGX matrix?
//	if (originFlags.sum() == 3)

	return true;
}


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
//			else if (fileParser_.argc(0) == "REMARK") setName(line);
//			else if (fileParser_.argc(0) == "CRYST1")
//			{
//				readVarF(line, "%9f%9f%9f%7f%7f%7f", ca,cb,cc,alpha,beta,gamma);
//				cell(ca,cb,cc,alpha,beta,gamma);
//			}
//			else if (fileParser_.argc(0) == "CONECT")
//			{
//				bondatoms = 0;
//				readVar(line,i,bondatoms[1],bondatoms[2],bondatoms[3],bondatoms[4]);
//				for (n=1; n<=4; ++n) if (bondatoms[n] > 0) newBond(i,bondatoms[n]);
//			}
//			else if (fileParser_.argc(0) == "END") return TRUE;
//		}
//	}
//}
//

// Return whether this plugin can export data
bool PDBModelPlugin::canExport()
{
	return true;
}

// Export data to the specified file
bool PDBModelPlugin::exportData()
{
	Atom* i, *j;
	int molId, n, m, bondCount;
	QString s;

	if (!fileParser_.writeLine("REMARK Aten-created PDB")) return false;
	if (!fileParser_.writeLine("TITLE  " + targetModel()->name())) return false;

	// Write unit cell information
	if (targetModel()->isPeriodic())
	{
		Vec3<double> lengths = targetModel()->cell().lengths();
		Vec3<double> angles = targetModel()->cell().angles();
		if (!fileParser_.writeLineF("CRYST1%9.3f%9.3f%9.3f%7.2f%7.2f%7.2f %11s%4i\n", lengths.x, lengths.y, lengths.z, angles.x, angles.y, angles.z, Spacegroups[targetModel()->cell().spacegroupId()].name, 1)) return false;
	}
	else if (!fileParser_.writeLineF("CRYST1%9.3f%9.3f%9.3f%7.2f%7.2f%7.2f %11s%4i\n", 1.0, 1.0, 1.0, 90.0, 90.0, 90.0, "P 1", 1)) return false;

	// Loop over patterns (molecule types) and write atom information
	targetModel()->createPatterns();
	molId = 0;
	i = targetModel()->atoms();
	for (Pattern* p = targetModel()->patterns(); p != NULL; p = p->next, ++molId)
	{
		for (m = 0; m<p->nMolecules(); ++m)
		{
			for (n = 0; n < p->nAtoms(); ++n)
			{
				s = QString("%1%2").arg(ElementMap::symbol(i)).arg(n);
				if (!fileParser_.writeLineF("ATOM  %-5i %-4s MOL  %-4i    %8.3f%8.3f%8.3f%6.2f%6.2f          %2s", i->id()+1, qPrintable(s), molId, i->r().x, i->r().y, i->r().z, 1.0, 1.0, ElementMap::symbol(i))) return false;
				i = i->next;
			}
		}
	}

	// Loop over atoms and write bond information
	for (i = targetModel()->atoms(); i != NULL; i = i->next)
	{
		bondCount = 0;
		
		for (RefListItem<Bond,int>* ri = i->bonds(); ri != NULL; ri = ri->next)
		{
			Bond* b = ri->item;

			j = b->partner(i);
			if (j->id() > i->id())
			{
				// Write entry line if necessary
				if (bondCount == 0)
				{
					if (!fileParser_.writeF("CONECT%5i", i->id())) return false;
				}

				// Write bond partner
				if (!fileParser_.writeF("%5i", j->id())) return false;
				++bondCount;

				// If bondCount == 4 write new line (only four bound partners per line)
				if (bondCount == 4)
				{
					if (!fileParser_.writeLine()) return false;
					bondCount = 0;
				}
			}
		}	
		if (bondCount != 0) if (!fileParser_.writeLine()) return false;
	}

	if (!fileParser_.writeLine("TER")) return false;

	if (!fileParser_.writeLine("END")) return false;

	return true;
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
	return true;
}

// Show import options dialog
bool PDBModelPlugin::showImportOptionsDialog()
{
	PDBImportOptionsDialog optionsDialog(pluginOptions_);

	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
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
 
