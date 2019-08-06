/*
        *** ChemShell Plugin Functions
        *** src/plugins/io_chemshell/chemshell_funcs.cpp
        Copyright T. Youngs 2016-2018
	Copyright T. W. Keal 2016

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

#include "plugins/io_chemshell/chemshell.hui"
#include "plugins/io_chemshell/chemshellexportoptions.h"
#include "model/model.h"

// Constructor
ChemShellModelPlugin::ChemShellModelPlugin()
{
	// Setup plugin options
	pluginOptions_.add("useTypeNames", "false");
	pluginOptions_.add("py-chemsh_input", "false");
    // YL 06/08/2019: strings for PY-ChemShell input script
	pluginOptions_.add("_frag"     , "");
	pluginOptions_.add("_qm_region", "");
	pluginOptions_.add("_theory"   , "");
	pluginOptions_.add("_task"     , "");
}

// Destructor
ChemShellModelPlugin::~ChemShellModelPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* ChemShellModelPlugin::makeCopy() const
{
	return new ChemShellModelPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType ChemShellModelPlugin::type() const
{
	return PluginTypes::FilePlugin;
}

// Return category of plugin
int ChemShellModelPlugin::category() const
{
	return PluginTypes::ModelFilePlugin;
}

// Name of plugin
QString ChemShellModelPlugin::name() const
{
	return QString("ChemShell fragment files");
}

// Nickname of plugin
QString ChemShellModelPlugin::nickname() const
{
	return QString("chemshell");
}

// Return whether the plugin is enabled
bool ChemShellModelPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString ChemShellModelPlugin::description() const
{
	return QString("Import/export for ChemShell fragment files");
}

// Related file extensions
QStringList ChemShellModelPlugin::extensions() const
{
	return QStringList() << "c" << "pun" << "coo" ;
}

// Exact names
QStringList ChemShellModelPlugin::exactNames() const
{
	return QStringList();
}

/*
 * Input / Output
 */

// Return whether this plugin can import data
bool ChemShellModelPlugin::canImport() const
{
	return true;
}

// Import data from the specified file
bool ChemShellModelPlugin::importData()
{
        bool isFragment;
        ChemShellModelPlugin::BlockName block;
	int nRecords;
	int i;
	Atom *atom;
	Matrix matrix;

	QString fragmentTitle;
	
	// Create a new Model to put our data in
	createModel();

	// Read in model data
	isFragment = false;
	while (!fileParser_.eofOrBlank()) {
	  
	  // Parse block description
	  if (!fileParser_.parseLine()) break;
	  
	  block = ChemShellModelPlugin::blockName(fileParser_.argc(2));
	  nRecords = fileParser_.argi(5); 

	  switch (block) {
	  case (ChemShellModelPlugin::FragmentBlock):
	    isFragment = true;
	    break;
	  case (ChemShellModelPlugin::TitleBlock):
	    if (!fileParser_.readLine(fragmentTitle)) return false;
	    targetModel()->setName(fragmentTitle);
	    break;
	  case (ChemShellModelPlugin::CoordinatesBlock):
	    for (i = 0; i < nRecords; ++i) {
	      if (!fileParser_.parseLine()) break;
	      atom = createAtom(targetModel(), fileParser_.argc(0), fileParser_.arg3d(1)*ANGBOHR);
          ForcefieldAtom* ffa = targetModel()->addAtomName(atom->element(), fileParser_.argc(0));
          atom->setType(ffa);
	    }
	    break;
	  case (ChemShellModelPlugin::AtomChargesBlock):
	    for (i = 0; i < nRecords; ++i) {
	      if (!fileParser_.parseLine()) break;
	      atom = targetModel()->atom(i);
	      atom->setCharge(fileParser_.argd(0));
	    }
	    break;	    
	  case (ChemShellModelPlugin::CellVectorsBlock):
	    for (i = 0; i < nRecords; ++i) {
	      if (!fileParser_.parseLine()) break;
	      matrix.setColumn(i, fileParser_.arg3d(0)*ANGBOHR, 0.0);
	    }
	    targetModel()->setCell(matrix);
	    break;	    
	  case (ChemShellModelPlugin::ConnectivityBlock):
	    for (i = 0; i < nRecords; ++i) {
	      if (!fileParser_.parseLine()) break;	      
	      targetModel()->bondAtoms(fileParser_.argi(0)-1, fileParser_.argi(1)-1, Bond::Single);
	    }
	    break;
	  case (ChemShellModelPlugin::nBlockNames):
	    if (isFragment) {
	      Messenger::print("Unrecognised block '" + fileParser_.argc(2) + \
			       "' in ChemShell fragment file '" + fileParser_.filename() + "'. Ignoring it...");
	      for (i = 0; i < nRecords; ++i) {
		if (!fileParser_.parseLine()) break;
	      }
	    } else {
	      /* The first block should be named fragment */
	      Messenger::print("Error: not a ChemShell fragment file!");
	      return false;
	    }
	    break;
	  }

	}

	return true;
}

// Return whether this plugin can export data
bool ChemShellModelPlugin::canExport() const
{
	return true;
}

// Export data to the specified file
bool ChemShellModelPlugin::exportData()
{
        bool hasCharges;
        double tiny;

  	// Get the current model pointer containing the data we are to export
	Model* sourceModel = targetModel();

    // YL 06/08/2019: we have to use io_chemshell to write out since tool_chemshell cannot access I/O
    if(pluginOptions_.value("py-chemsh_input") == "true") {

        if(!fileParser_.writeLine("# Py-ChemShell input script generated by Aten")) return false;
        if(!fileParser_.writeLine("# www.chemshell.org")) return false;
        if(!fileParser_.writeLine("# Cite: J. Chem. Theory Comput. 2019, 15, 1317-1328\n")) return false;
        if(!fileParser_.writeLine("from chemsh import *\n")) return false;
        if(!fileParser_.writeLine(pluginOptions_.value("_frag"))) return false;
        if(!fileParser_.writeLine(pluginOptions_.value("_pun_filename"))) return false;
        if(!fileParser_.writeLine(pluginOptions_.value("_qm_region"))) return false;
        if(!fileParser_.writeLine(pluginOptions_.value("_theory"))) return false;
        if(!fileParser_.writeLine(pluginOptions_.value("_task"))) return false;
        if(!fileParser_.writeLine("my_task.run()\n")) return false;
        if(!fileParser_.writeLine("energy = my_task.result.energy\n")) return false;

        Messenger::print("Py-ChemShell input script '%s' generated", qPrintable(fileParser_.filename()));
        return true;

    }

	// Header
	if (!fileParser_.writeLine("block = fragment records = 0")) return false;
	if (!fileParser_.writeLine("block = title records = 1")) return false;
	if (!fileParser_.writeLine(sourceModel->name())) return false;

	// Atom coordinates
	if (!fileParser_.writeLineF("block = coordinates records = %d", sourceModel->nAtoms())) return false;
        for ( Atom* i = sourceModel->atoms(); i != NULL; i = i->next ) {
        // YL 02/08/2019: changed to print ff types rather than element symbols
        if (!fileParser_.writeLineF("%s  %20.14e %20.14e %20.14e ", i->type()->name().toUtf8().constData(),
            i->r().x/ANGBOHR, i->r().y/ANGBOHR, i->r().z/ANGBOHR)) return false;
//	  if (!fileParser_.writeLineF("%s  %20.14e %20.14e %20.14e ", ElementMap::symbol(i->element()),
//				      i->r().x/ANGBOHR, i->r().y/ANGBOHR, i->r().z/ANGBOHR)) return false;
	}

    // total charge
	if (!fileParser_.writeLine("block = charge records = 1")) return false;
	if (!fileParser_.writeLineF("%20.5e", 0.00000)) return false;

	// Atom charges
        hasCharges = false;
        tiny = 1.0e-10;
  	// As far as I'm aware there is no flag to check that charges have been set. Add one later?
	for ( Atom* i = sourceModel->atoms(); i != NULL; i = i->next ) {
          if (abs(i->charge()) > tiny) {
            hasCharges = true;
            break;
          }
        }
        if (hasCharges) {
          if (!fileParser_.writeLineF("block = atom_charges records = %d", sourceModel->nAtoms())) return false;
          for ( Atom* i = sourceModel->atoms(); i != NULL; i = i->next ) {
            if (!fileParser_.writeLineF("%20.10f", i->charge())) return false;
          }
        }
	
	// Cell vectors
	if (sourceModel->isPeriodic()) {
	  Matrix axes = sourceModel->cell().axes();
	  // TODO: 2D case
	  if (!fileParser_.writeLineF("block = cell_vectors records = 3")) return false;
	  if (!fileParser_.writeLineF("%20.14e %20.14e %20.14e", axes[0]/ANGBOHR, axes[1]/ANGBOHR, axes[2]/ANGBOHR)) return false;
	  if (!fileParser_.writeLineF("%20.14e %20.14e %20.14e", axes[4]/ANGBOHR, axes[5]/ANGBOHR, axes[6]/ANGBOHR)) return false;
	  if (!fileParser_.writeLineF("%20.14e %20.14e %20.14e", axes[8]/ANGBOHR, axes[9]/ANGBOHR, axes[10]/ANGBOHR)) return false;
	}
	
	// Connectivity
	if (!fileParser_.writeLineF("block = connectivity records = %d", sourceModel->nBonds())) return false;
	for (Bond* b = sourceModel->bonds(); b != NULL; b = b->next) {
	  if (!fileParser_.writeLineF("%d %d", b->atomI()->id()+1, b->atomJ()->id()+1)) return false;
	}

	return true;
}

// Import next partial data chunk
bool ChemShellModelPlugin::importNextPart()
{
	return false;
}

// Skip next partial data chunk
bool ChemShellModelPlugin::skipNextPart()
{
	return false;
}

/*
 * Options
 */

// Return whether the plugin has import options
bool ChemShellModelPlugin::hasImportOptions() const
{
	return false;
}

// Show import options dialog
bool ChemShellModelPlugin::showImportOptionsDialog(KVMap& targetOptions) const
{
	return false;
}

// Return whether the plugin has export options
bool ChemShellModelPlugin::hasExportOptions() const
{
	return true;
}

// Show export options dialog
bool ChemShellModelPlugin::showExportOptionsDialog(KVMap& targetOptions) const
{
	ChemShellExportOptionsDialog optionsDialog(targetOptions);

	return (optionsDialog.updateAndExecute() == QDialog::Accepted);
}

/*
 * ChemShell functions
 */

// Block enum
ChemShellModelPlugin::BlockName ChemShellModelPlugin::blockName(QString s)
{
  static QStringList blockNames = QStringList() << "fragment" << "title" << "coordinates" << "atom_charges" << "cell_vectors" << "connectivity";
  
  for (int i = 0; i < ChemShellModelPlugin::nBlockNames; ++i) if (s == blockNames.at(i)) return (ChemShellModelPlugin::BlockName) i;
  return ChemShellModelPlugin::nBlockNames;
}
