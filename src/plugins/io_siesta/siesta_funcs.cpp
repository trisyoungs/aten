/*
        *** Siesta Model Plugin Functions
        *** src/plugins/io_siesta/siesta_funcs.cpp
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

#include "plugins/io_siesta/siesta.hui"
#include "model/model.h"

// Constructor
SiestaModelPlugin::SiestaModelPlugin()
{
}

// Destructor
SiestaModelPlugin::~SiestaModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* SiestaModelPlugin::makeCopy()
{
	return new SiestaModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType SiestaModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int SiestaModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString SiestaModelPlugin::name() const
{
	return QString("Siesta (dlputils) 3D probability density");
}

// Nickname of plugin
QString SiestaModelPlugin::nickname() const
{
	return QString("siesta");
}

// Description (long name) of plugin
QString SiestaModelPlugin::description() const
{
	return QString("Import/export for dlputils Siesta files");
}

// Related file extensions
QStringList SiestaModelPlugin::extensions() const
{
	return QStringList() << "siesta";
}

// Exact names
QStringList SiestaModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool SiestaModelPlugin::canImport()
{
	return true;
}

// Import data from the specified file
bool SiestaModelPlugin::importData()
{
//filter(type="importmodel", name="Siesta FDF", nickname="siesta", extension="fdf", glob="*.fdf", id=9)
//{
//	# Variable declaration
//	int natoms,nspecies,n,elmap[aten.nElements],i,el;
//	string keywd,data,line,unit;
//	double x,y,z,a,b,c,d;
//	vector axes[3];
//	int coordsinbohr = FALSE;
//
//	elmap = 0;
//
//	model m = newModel("Siesta FDF");
//
//	# Search for keywords and '%block's in the file...
//	while (!eof())
//	{
//		getLine(line);
//		readVarF(line, "%s%r", keywd,data);
//		if (keywd == "SystemLabel") setName(data);
//		else if (keywd == "NumberOfAtoms") natoms = atoi(data);
//		else if (keywd == "NumberOfSpecies") nspecies = atoi(data);
//		else if (keywd == "LatticeConstant")
//		{
//			readVar(data,d,unit);
//			cell(d,d,d,90,90,90);
//			if (unit == "Bohr") bohr(m.cell);
//		}
//		else if (keywd == "AtomCoordinatesFormat")
//		{
//			if (data == "Bohr") coordsinbohr = TRUE;
//			else coordsinbohr = FALSE;
//		}
//		else if (keywd == "%block")
//		{
//			if (data == "ChemicalSpeciesLabel")
//			{
//				for (n=0; n<nspecies; ++n)
//				{
//					readLine(i,el,data);
//					elmap[i] = el;
//				}
//			}
//			else if (data == "AtomicCoordinatesAndAtomicSpecies")
//			{
//				for (n=0; n<natoms; ++n)
//				{
//					readLine(x,y,z,i);
//					newAtom(aten.elements[elmap[i]].symbol, x, y, z);
//				}
//			}
//			else if (data == "LatticeParameters")
//			{
//				readLine(x,y,z,a,b,c);
//				cell(x,y,z,a,b,c);
//			}
//			else if (data == "LatticeVectors")
//			{
//				readLine(axes[1].x,axes[1].y,axes[1].z);
//				readLine(axes[2].x,axes[2].y,axes[2].z);
//				readLine(axes[3].x,axes[3].y,axes[3].z);
//				cellAxes(axes[1].x,axes[1].y,axes[1].z,axes[2].x,axes[2].y,axes[2].z,axes[3].x,axes[3].y,axes[3].z);
//			}
//		}
//	}
//
//	if (coordsinbohr) bohr(m);
//	rebond();
//	finaliseModel();
//}
//
//filter(type="exportmodel", name="Siesta FDF", nickname="siesta", extension="fdf", glob="*.fdf", id=9)
//{
//	# Variable declaration
//	int n, el[aten.nElements], nspecies;
//	string e;
//	double rx,ry,rz;
//	atom i;
//
//	# Grab current model (or frame)
//	model m = aten.frame;
//
//	# Determine number of species (elements) used in model, and create element->species map
//	el = 0;
//	nspecies = 0;
//	for (i = m.atoms; i; ++i) if (el[i.z] == 0) el[i.z] = ++nspecies;
//	
//	# Write title information
//	writeLine("SystemName",m.name);
//	writeLine("SystemLabel", m.name);
//	writeLine("NumberOfAtoms", m.nAtoms);
//	writeLine("NumberOfSpecies", nspecies);
//	writeLine("");
//
//        # Determine total charge of system
//        selectAll();
//        double q = charge();
//        selectNone();
//
//        # Write 'standard' keyword data
//        writeLine("PAO.EnergyShift      25 meV");
//        writeLine("MeshCutoff          600 Ry");
//        writeLine("DM.MixingWeight      0.3");
//        writeLineF("NetCharge            %5.3f\n",q);
//	writeLine("");
//	writeLine("PAO.BasisSize       DZP");
//	writeLine("XC.functional       GGA");
//	writeLine("XC.authors          PBE");
//	writeLine("DM.NumberPulay      4");
//	writeLine("DM.Tolerance        1.d-6");
//	writeLine("SolutionMethod      diagon");
//	writeLine("MaxSCFIterations    100");
//	writeLine("ParallelOverK       true");
//	writeLine("MD.TypeOfRun        Verlet");
//	writeLine("MD.InitialTemperature  0.0  K");
//	writeLine("MD.InitialTimeStep    1");
//	writeLine("MD.FinalTimeStep    1");
//	writeLine("WriteCoorXmol       true");
//	writeLine("WriteForces         true");
//	writeLine("");
//
//	# Write chemical species data, and label species in ascending order
//	writeLine("%block ChemicalSpeciesLabel");
//	for (n=1; n<=aten.nElements; n++) if (el[n] != 0) writeLineF("%3i  %3i  %s-PBE\n",el[n],n,aten.elements[n].symbol);
//	writeLine("%endblock ChemicalSpeciesLabel");
//	writeLine("");
//
//	# Write unit cell (if present)
//	if (m.cell.type == "cubic") writeLine("LatticeConstant", m.cell.a, "Ang");
//	else if ((m.cell.type == "orthorhombic") || (m.cell.type == "parallelepiped"))
//	{
//		writeLine("LatticeConstant 1.0 Ang");
//		writeLine("%block LatticeParameters");
//		writeLine(m.cell.a, m.cell.b, m.cell.c, m.cell.alpha, m.cell.beta, m.cell.gamma);
//		writeLine("%endblock LatticeParameters");
//	}
//
//	# Write atomic coordinates
//	writeLine("AtomicCoordinatesFormat  Ang");
//	writeLine("%block AtomicCoordinatesAndAtomicSpecies");
//	for (i = m.atoms; i; ++i) writeLineF("%14.8f %14.8f %14.8f %3i\n",i.rx,i.ry,i.rz,el[i.z]);
//	writeLine("%endblock AtomicCoordinatesAndAtomicSpecies");
//	writeLine("");
//}
//
//filter(type="importmodel", name="Siesta Vectors File", nickname="siestavec", extension="vectors", glob="*.vectors")
//{
//	# Variable declaration
//	int result,nstructures,natoms,n,l,count;
//	Vibration vib;
//	string line,e,name,discard;
//	double freq;
//
//	# Everything depends on the current loaded model
//	model m = aten.frame;
//	natoms = m.nAtoms;
//	if (natoms == 0) error("No atoms in current model. Can't load in frequencies.");
//
//	count = 0;
//	while (find("Eigenvector"))
//	{
//		++count;
//		readLine(discard, discard, freq);
//		vib = m.newVibration();
//		vib.frequency = freq;
//		# SKip next line ("Eigenmode (real part)")
//		skipLine();
//		# Now read in atomic displacements
//		for (n=1; n<=natoms; ++n)
//		{
//			readLine(vib.displacements[n].x, vib.displacements[n].y, vib.displacements[n].z);
//			vib.displacements[n].x *= ANGBOHR;
//			vib.displacements[n].y *= ANGBOHR;
//			vib.displacements[n].z *= ANGBOHR;
//		}
//	}
//
//	# Summarise
//	printf("Added %i frequencies to model '%s'\n", count, m.name);
//	if (count != (3*natoms)) printf("Warning - Model '%s' contains %i atoms, but read in %i frequencies (!= 3N).\n", m.name, natoms, count);
//
//}
//
//filter(type="importmodel", name="Siesta XV Output", nickname="xv", extension="XV", glob="*.XV")
//{
//	# Variable declaration
//	int discard, el, natoms, nconfig = 1;
//	double x,y,z,fx,fy,fz;
//	vector v1,v2,v3;
//
//	model frame, m = newModel(filterFilename());
//	while (!eof())
//	{
//		nconfig++;
//		frame = addFrame(toa("Frame %i",nconfig));
//		# First three lines are cell specification
//		if (!readLine(v1.x,v1.y,v1.z)) error("Error reading cell parameters for configuration %i", nconfig);
//		if (!readLine(v2.x,v2.y,v2.z)) error("Error reading cell parameters for configuration %i", nconfig);
//		if (!readLine(v3.x,v3.y,v3.z)) error("Error reading cell parameters for configuration %i", nconfig);
//		v1 *= ANGBOHR;
//		v2 *= ANGBOHR;
//		v3 *= ANGBOHR;
//		cellAxes(v1.x,v1.y,v1.z,v2.x,v2.y,v2.z,v3.x,v3.y,v3.z);
//		# Next line contains number of atoms to follow
//		readLine(natoms);
//		for (int n=0; n<natoms; ++n)
//		{
//			if (!readLine(discard,el,x,y,z,fx,fy,fz)) error("Error reading atomic positions for configuration %i", nconfig);
//			newAtom(el,x*ANGBOHR,y*ANGBOHR,z*ANGBOHR,fx,fy,fz);
//		}
//		rebond();
//		finaliseFrame();
//		# Copy this frame, ready to paste into parent model at end
//		selectAll();
//		copy();
//		selectNone();
//	}
//	m.cell = frame.cell;
//	m.paste();
//	selectNone();
//	finaliseModel();
//}
	return true;
}

// Return whether this plugin can export data
bool SiestaModelPlugin::canExport()
{
	return false;
}

// Export data to the specified file
bool SiestaModelPlugin::exportData()
{
	return false;
}

// Import next partial data chunk
bool SiestaModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool SiestaModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool SiestaModelPlugin::hasImportOptions()
{
	return false;
}

// Show import options dialog
bool SiestaModelPlugin::showImportOptionsDialog()
{
	return false;
}

// Return whether the plugin has export options
bool SiestaModelPlugin::hasExportOptions()
{
	return false;
}

// Show export options dialog
bool SiestaModelPlugin::showExportOptionsDialog()
{
	return false;
}
 
