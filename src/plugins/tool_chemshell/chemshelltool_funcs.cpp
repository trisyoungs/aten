/*
        *** ChemShell Tool Functions
        *** src/plugins/tool_chemshell/chemshelltool_funcs.cpp
        Copyright T. Youngs 2016-2018

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

#include "plugins/tool_chemshell/chemshelltool.hui"
#include "plugins/io_chemshell/chemshell.hui"
#include "plugins/tool_chemshell/chemshelltooldialog.h"
#include "gui/qcustomplot/qcustomplot.hui"
#include "model/model.h"
#include "model/clipboard.h"
#include "base/pattern.h"

#include "plugins/interfaces/fileplugin.h"

ATEN_USING_NAMESPACE

// Constructor
// YL: to be processed by Qt marcros this constructor should not contain any argments for initialising
ChemShellToolPlugin::ChemShellToolPlugin() {
	// Setup plugin options
    pluginOptions_.add("selected_as_qm", "true");
    pluginOptions_.add("replace_suffix", "true");


	// Create dialog if the tool has one
	if (hasDialog()) dialog_ = new ChemShellToolDialog(*this, pluginOptions_);
	else dialog_ = NULL;
}

// Destructor
ChemShellToolPlugin::~ChemShellToolPlugin()
{
}

/*
 * Instance Handling
 */

// Return a copy of the plugin object
BasePluginInterface* ChemShellToolPlugin::makeCopy() const
{
	return new ChemShellToolPlugin;
}

/*
 * Definition
 */

// Return type of plugin
PluginTypes::PluginType ChemShellToolPlugin::type() const
{
	return PluginTypes::ToolPlugin;
}

// Return category of plugin
int ChemShellToolPlugin::category() const
{
	return PluginTypes::GeneralToolPlugin;
}

// Name of plugin
QString ChemShellToolPlugin::name() const
{
	return QString("ChemShell Tool Plugin");
}

// Nickname of plugin
QString ChemShellToolPlugin::nickname() const
{
	return QString("chemshell");
}

// Return whether the plugin is enabled
bool ChemShellToolPlugin::enabled() const
{
	return true;
}

// Description (long name) of plugin
QString ChemShellToolPlugin::description() const
{
	return QString("Py-ChemShell tools");
}

/*
 * Tool Definition
 */

// Return button label to use in GUI
QString ChemShellToolPlugin::buttonLabel() const
{
	return QString("ChemShell");
}

// Return icon for button in GUI
QIcon ChemShellToolPlugin::buttonIcon() const
{
	return QIcon(":/chemshelltool_icons/icon.svg");
}

// Return group name for tool (used to group similar tools together)
QString ChemShellToolPlugin::groupName() const
{
	return QString("ChemShell");
}

// Return whether the tool is enabled (appears in the GUI)
bool ChemShellToolPlugin::isEnabled() const
{
	return true;
}

// Return whether the tool has a dialog
bool ChemShellToolPlugin::hasDialog() const
{
	return true;
}

// Show the dialog for the tool
bool ChemShellToolPlugin::showDialog() {

	// get the target models
	RefList<Model,bool> targets;
	if (pluginOptions_.value("applyToAll") == "true") {
        targets = allModels();
	} else { 
        targets.add(currentModelOrFrame());
    }
    // guess a new filename for the edited model to save
    for (RefListItem<Model,bool>* ri = targets.first(); ri != NULL; ri = ri->next) {
        Model* sourceModel = ri->item;
        QString model_filename = sourceModel->filename();
        QFileInfo fileInfo(sourceModel->filename());
        pluginOptions_.add("new_punch", fileInfo.baseName()+QString("_relabelled.pun"));
    }
	// Check if a dialog actually exists
	if (dialog_ == NULL)
	{
		Messenger::error("No dialog is associated to the tool '%s'\n", qPrintable(name()));
		return false;
	}

	// Cast the dialog_ pointer into our custom class
	ChemShellToolDialog* testToolDialog = (ChemShellToolDialog*) dialog_;
	if (!testToolDialog)
	{
		Messenger::error("Error casting tool dialog into custom class for the tool '%s'\n", qPrintable(name()));
		return false;
	}
	testToolDialog->applyPluginOptions();
	testToolDialog->exec();
	
	return true;
}


// Select by QM/MM region number ('selecttype <el> <typedesc>')
bool Commands::function_SelectRegion(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
    if (obj.notifyNull(Bundle::ModelPointer)) return false;
    int nselected = obj.rs()->nSelected();

    QRegExp re("*["+c->argc(0)+"]");
    re.setPatternSyntax(QRegExp::Wildcard);

    obj.rs()->beginUndoState("Select atoms by QM/MM region number");
    for (Atom* i = obj.rs()->atoms(); i != NULL; i = i->next)
    {
        if (!i->type()) continue;
        if (re.exactMatch(i->type()->name())) obj.rs()->selectAtom(i);
    }
    obj.rs()->endUndoState();

    rv.set(obj.rs()->nSelected() - nselected);
    return true;
}

void ChemShellToolPlugin::renameKeywords() {
    if(pluginOptions_.value("qm_theory") == "GAMESS-UK") {
        pluginOptions_.add("qm_theory", "GAMESS_UK");
    } else if(pluginOptions_.value("qm_theory") == "DFTB+") {
        pluginOptions_.add("qm_theory", "DFTBplus");
    }
    if(pluginOptions_.value("mm_theory") == "DL_POLY 4") {
        pluginOptions_.add("mm_theory", "DL_POLY");
    }
    if(pluginOptions_.value("mm_ff") == "All-atom CHARMM") {
        pluginOptions_.add("mm_ff", "CHARMM");
    } else if(pluginOptions_.value("mm_ff") == "CHARMM22 for proteins") {
        pluginOptions_.add("mm_ff", "CHARMM22_prot");
    } else if(pluginOptions_.value("mm_ff") == "CHARMM36 for proteins") {
        pluginOptions_.add("mm_ff", "CHARMM36_prot");
    } else if(pluginOptions_.value("mm_ff") == "CHARMM36 for lipids") {
        pluginOptions_.add("mm_ff", "CHARMM36_lipid");
    } else if(pluginOptions_.value("mm_ff") == "CHARMM36 for carbonhydrates") {
        pluginOptions_.add("mm_ff", "CHARMM36_carb");
    } else if(pluginOptions_.value("mm_ff") == "CHARMM general forcefield") {
        pluginOptions_.add("mm_ff", "CHARMM36_cgenff");
    } else if(pluginOptions_.value("mm_ff") == "United-atom for CHARMM") {
        pluginOptions_.add("mm_ff", "CHARMM19");
    } else if(pluginOptions_.value("mm_ff") == "All-atom Amber") {
        pluginOptions_.add("mm_ff", "AMBER");
    } else if(pluginOptions_.value("mm_ff") == "Amber general forcefield") {
        pluginOptions_.add("mm_ff", "AMBER16_gaff");
    } else if(pluginOptions_.value("mm_ff") == "OPLS-AA") {
        pluginOptions_.add("mm_ff", "OPLSAA");
    } else if(pluginOptions_.value("mm_ff") == "OPLS_2005") {
        pluginOptions_.add("mm_ff", "OPLS2005");
    } else if(pluginOptions_.value("mm_ff") == "OPLS AA/M for proteins") {
        pluginOptions_.add("mm_ff", "OPLS_AAM");
    } else if(pluginOptions_.value("mm_ff") == "DREIDING") {
        pluginOptions_.add("mm_ff", "DREIDING");
    } else if(pluginOptions_.value("mm_ff") == "Polymer consistent forcefield") {
        pluginOptions_.add("mm_ff", "PCFF");
    } else if(pluginOptions_.value("mm_ff") == "Consistent valence forcefield") {
        pluginOptions_.add("mm_ff", "CVFF");
    } else if(pluginOptions_.value("mm_ff") == "Gromos united atom G54A7") {
        pluginOptions_.add("mm_ff", "G54A7");
    } else if(pluginOptions_.value("mm_ff") == "Inorganic forcefield") {
        pluginOptions_.add("mm_ff", "INORGANIC");
    } else if(pluginOptions_.value("mm_ff") == "Inorganic forcefield for binary oxides") {
        pluginOptions_.add("mm_ff", "INORGANIC_binary_oxide");
    } else if(pluginOptions_.value("mm_ff") == "Inorganic forcefield for ternary oxides") {
        pluginOptions_.add("mm_ff", "INORGANIC_ternary_oxide");
    } else if(pluginOptions_.value("mm_ff") == "Inorganic forcefield for binary halides") {
        pluginOptions_.add("mm_ff", "INORGANIC_binary_halide");
    } else if(pluginOptions_.value("mm_ff") == "Inorganic forcefield for glass") {
        pluginOptions_.add("mm_ff", "IORGANIC_glass");
    } else if(pluginOptions_.value("mm_ff") == "Inorganic forcefield for clay") {
        pluginOptions_.add("mm_ff", "IORGANIC_glass");
    } else if(pluginOptions_.value("mm_ff") == "Multiple potential") {
        pluginOptions_.add("mm_ff", "multiple");
    }
    if(pluginOptions_.value("task") == "Single-Point") {
        pluginOptions_.add("task", "SP");
    } else if(pluginOptions_.value("task") == "Geometry Optimisation") {
        pluginOptions_.add("task", "Opt");
    } else if(pluginOptions_.value("task") == "Molecular Dynamics") {
        pluginOptions_.add("task", "MD");
    } else if(pluginOptions_.value("task") == "Charge Fitting") {
        pluginOptions_.add("task", "ChargeFitting");
    } else if(pluginOptions_.value("task") == "Scan") {
        pluginOptions_.add("task", "Scan");
    } else if(pluginOptions_.value("task") == "Parameterisation") {
        pluginOptions_.add("task", "Parameterise");
    }
    if(pluginOptions_.value("qmmm_embedding") == "Electrostatical") {
        pluginOptions_.add("qmmm_embedding", "electrostatic");
    } else if(pluginOptions_.value("qmmm_embedding") == "Mechanical") {
        pluginOptions_.add("qmmm_embedding", "mechanical");
    }
    if(pluginOptions_.value("qmmm_coupling") == "Covalent") {
        pluginOptions_.add("qmmm_coupling", "covalent");
    } else if(pluginOptions_.value("qmmm_coupling") == "Ionic") {
        pluginOptions_.add("qmmm_coupling", "ionic");
    }
    if(pluginOptions_.value("qmmm_scheme") == "Additive") {
        pluginOptions_.add("qmmm_scheme", "additive");
    } else if(pluginOptions_.value("qmmm_scheme") == "Subtractive") {
        pluginOptions_.add("qmmm_scheme", "subtractive");
    }
}


// Run the tool with the current settings
bool ChemShellToolPlugin::runTool() {

    bool ierror;
    LineParser parser;

	// Get the target models
	RefList<Model,bool> targets;
	if (pluginOptions_.value("applyToAll") == "true") {
        targets = allModels();
	} else { 
        targets.add(currentModelOrFrame());
    }

    // it seems all buttons should function via the runTool() function
    // rename the selected atoms by attaching the given character(s)
    bool relabel_ = pluginOptions_.value("_relabel") == "true";
    if(relabel_) {

	    for (RefListItem<Model,bool>* ri = targets.first(); ri != NULL; ri = ri->next) {
	    	Model* sourceModel = ri->item;
            const int nselected = sourceModel->nSelected();
            Atom* selected[nselected];
            if(nselected == 0) {
                Messenger::print("\nNo atoms yet selected\n");
            } else {
                if(sourceModel->selectedAtoms(nselected, selected)) {
                    for(int i=0; i<nselected; ++i) {
                        Atom* iatom = sourceModel->atom(selected[i]->id());
                        if(iatom->type() != NULL) {

                            // regular expression for existing labels
                            QRegularExpression re_qt5(pluginOptions_.value("regex"));
                            QRegExp            re_qt4(pluginOptions_.value("regex"));

                            QString newtype;
                            // if checkbox is ticked
                            if(pluginOptions_.value("replace_suffix") == "true") {
                                if(re_qt5.isValid()) {
                                    newtype = iatom->type()->name();
                                    newtype.replace(re_qt5, pluginOptions_.value("type_suffix"));
                                } else if(re_qt4.isValid()) {
                                    newtype = iatom->type()->name().replace(re_qt4, pluginOptions_.value("type_suffix"));
                                } else {
                                    Messenger::print(QString("Error: invalid regular expression: "+pluginOptions_.value("regex")));
                                }
                            } else {
                                newtype = iatom->type()->name()+pluginOptions_.value("type_suffix");
                            }
                            Messenger::print(QString("renamed atom %1's type \"%2\" to \"%3\"").arg(selected[i]->id()).arg(iatom->type()->name()).arg(newtype));
                            // everytime create a new FFA type since in Aten an FFA type is shared by multiple atoms (then associate unselected atoms' type are also changed)
                            ForcefieldAtom* ffa = sourceModel->addAtomName(iatom->element(), newtype);
                            sourceModel->setAtomType(iatom, ffa, false);
                        } else {
                            Messenger::print(QString("Cannot rename type of atom %1").arg(selected[i]->id()));
                        }
                    }
                } else {
                    Messenger::print("\nInvalid selection\n");
                }
            }

            // write to punch
            const FilePluginInterface* plugin = pluginStore_->findFilePluginByNickname(PluginTypes::ModelFilePlugin, PluginTypes::ExportPlugin, QString("chemshell"));
            if(plugin != NULL) {
                FilePluginInterface* pluginInterface = (FilePluginInterface*) plugin->duplicate();
                if(!pluginInterface->openOutput(pluginOptions_.value("new_punch"))) {
                    Messenger::print("\nChemShell cannot open file %s to write\n", pluginOptions_.value("new_punch").toUtf8().constData());
                    Messenger::exit("ChemShellToolPlugin::runTool");
                    return false;
                }
                KVMap IOPluginOptions;
                IOPluginOptions.add("py-chemsh_input", "false");
                pluginInterface->setParentModel(sourceModel);
                pluginInterface->setOptions(IOPluginOptions);
                if(!pluginInterface->exportData()) {
                    sourceModel->setFilename(pluginOptions_.value("new_punch"));
                    sourceModel->setPlugin(pluginInterface);
                    sourceModel->updateSavePoint();
                    pluginInterface->closeFiles();
                }
            }
        }
	    // Update the display
        emit(updateWidgets(0));
        return true;

    } else {

        bool selected_as_qm = pluginOptions_.value("selected_as_qm") == "true";
        bool labelled_as_qm = pluginOptions_.value("labelled_as_qm") == "true";
        bool radius_as_qm   = pluginOptions_.value("radius_as_qm") == "true";
    
        KVMap IOPluginOptions;
        IOPluginOptions.add("py-chemsh_input", "true");
        
    	// Loop over targets
    	for(RefListItem<Model,bool>* ri = targets.first(); ri != NULL; ri = ri->next) {

    		Model* sourceModel = ri->item;
            const int nselected = sourceModel->nSelected();
            Atom* selected[nselected];
            QString qm_region = "";
            // only for QM/MM and ticked option
            if(pluginOptions_.value("theory") == "QM/MM") {
                qm_region.append("qm_region = ");
                // selection
                if(pluginOptions_.value("selected_as_qm") == "true") {
                    if(nselected == 0) {
                        Messenger::print("\nNo atoms yet selected\n");
                        qm_region.append("[]\n");
                    } else {
                        if(sourceModel->selectedAtoms(nselected, selected)) {
                            qm_region.append("[ ");
                            for(int i=0; i<nselected-1; ++i) {
                                Atom* iatom = sourceModel->atom(selected[i]->id());
                                QString s = QString::number(selected[i]->id());
                                qm_region.append(s);
                                qm_region.append(", ");
                            }
                            // the last selected atom
                            QString s = QString::number(selected[nselected-1]->id());
                            qm_region.append(s);
                            qm_region.append(" ]\n\n");
                        } else {
                            qm_region.append("[]\n");
                        }
                    }
                }
                if(pluginOptions_.value("labelled_as_qm") == "true") {
                    qm_region.append(QString("my_frag.getRegion(%1)\n").arg(pluginOptions_.value("qm_label")));
                }
                if(pluginOptions_.value("radius_as_qm") == "true") {
                    qm_region.append(QString("my_frag.selectByRadius(%1, centre=None, unit='a.u.')\n").arg(pluginOptions_.value("qm_radius")));
                }
            }
            // pass to io_chemshell
            IOPluginOptions.add("_qm_region", qm_region);
    
            // rename items to Py-ChemShell names
            renameKeywords();
   
            // fragment
            QString frag_strbuff = "";
            frag_strbuff.append(QString("my_frag = Fragment(coords='%1')\n\n").arg(pluginOptions_.value("new_punch")));
            // pass to io_chemshell
            IOPluginOptions.add("_frag", frag_strbuff);
 
            // theory
            QString strbuff = "";
    
            // QM
            QString qm_method_strbuff = "";
            strbuff.append("qm_charge = int(my_frag.totalcharge)\n\n");
            // indentation
            QString qm_offset = QString(" ").repeated(pluginOptions_.value("qm_theory").length() + 9);
            if(pluginOptions_.value("qm_method") == "DFT") {
                qm_method_strbuff.append("'dft',\n");
                qm_method_strbuff.append(QString("%1functional='%2',\n").arg(qm_offset).arg(pluginOptions_.value("qm_functional")));
            } else if(pluginOptions_.value("qm_method") == "HF") {
                qm_method_strbuff.append("'hf',\n");
            }
            qm_method_strbuff.append(QString("%1basis='%2',\n").arg(qm_offset).arg(pluginOptions_.value("qm_basis")));
            qm_method_strbuff.append(QString("%1direct=True,\n").arg(qm_offset));
            qm_method_strbuff.append(QString("%1ecp='',\n").arg(qm_offset));
            qm_method_strbuff.append(QString("%1harmonic=False,\n").arg(qm_offset));
            qm_method_strbuff.append(QString("%1mult=1,\n").arg(qm_offset));
            qm_method_strbuff.append(QString("%1charge=qm_charge,\n").arg(qm_offset));
            qm_method_strbuff.append(QString("%1scftype='rhf',\n").arg(qm_offset));
            qm_method_strbuff.append(QString("%1maxiter=100)\n\n").arg(qm_offset));
    
            // MM
            QString mm_method_strbuff = "";
            if(pluginOptions_.value("mm_ff_custom") == "false" && pluginOptions_.value("theory") != "QM") {
                strbuff.append(QString("my_ff = DL_FIELD(ff='%1')\n\n").arg(pluginOptions_.value("mm_ff")));
                mm_method_strbuff.append(QString("my_ff"));
            } else {
                mm_method_strbuff.append(pluginOptions_.value("mm_ff"));
            }
   
            // 
            QString my_theory = "";
            if(pluginOptions_.value("theory") == "QM/MM") {
                my_theory.append("my_qmmm");
                strbuff.append(QString("my_qm = %1(method=%2").arg(pluginOptions_.value("qm_theory")).arg(qm_method_strbuff));
                strbuff.append(QString("my_mm = %1(ff='%2')\n\n").arg(pluginOptions_.value("mm_theory")).arg(pluginOptions_.value("mm_ff")));
                strbuff.append("my_qmmm = QMMM(frag=my_frag,\n");
                strbuff.append("               qm_regiom=qm_region,\n");
                strbuff.append("               qm=my_qm,\n");
                strbuff.append("               mm=my_mm,\n");
                strbuff.append("               separate_ecps=False,\n");
                strbuff.append(QString("               embeddig='%1',\n").arg(pluginOptions_.value("qmmm_embedding")));
                strbuff.append(QString("               coupling='%1',\n").arg(pluginOptions_.value("qmmm_coupling")));
                strbuff.append(QString("               scheme='%1')\n\n").arg(pluginOptions_.value("qmmm_scheme")));
                QString model_filename = sourceModel->filename();
                QFileInfo fileInfo(sourceModel->filename());
                strbuff.append(QString("my_qmmm.qm.frag.save('%1')\n\n").arg(fileInfo.baseName()+QString("_partitioned_qm_only.pun")));
            } else if(pluginOptions_.value("theory") == "QM") {
                my_theory.append("my_qm");
                strbuff.append(QString("my_qm = %1(method=%2").arg(pluginOptions_.value("qm_theory")).arg(qm_method_strbuff));
            } else if(pluginOptions_.value("theory") == "MM") {
                my_theory.append("my_mm");
                strbuff.append(QString("my_mm = %1(ff='%2')\n\n").arg(pluginOptions_.value("mm_theory")).arg(mm_method_strbuff));
            }
            // pass to io_chemshell
            IOPluginOptions.add("_theory", strbuff);
    
            // task
            QString task_strbuff = "";
            QString task_offset = QString(" ").repeated(pluginOptions_.value("task").length() + 11);
            QString opt_strbuff = "";
            if(pluginOptions_.value("task") == "Opt") {
                opt_strbuff.append(",\n");
                opt_strbuff.append(QString("%1active_region=my_frag.selectByRadius(%2, centre=None, unit='a.u.')").arg(task_offset).arg(pluginOptions_.value("active_radius")));
            }
            task_strbuff.append(QString("my_task = %1(theory=%2,\n").arg(pluginOptions_.value("task")).arg(my_theory));
            task_strbuff.append(QString("%1dump=5,\n").arg(task_offset));
            task_strbuff.append(QString("%1restart=False%2)\n\n").arg(task_offset).arg(opt_strbuff));
            // pass to io_chemshell
            IOPluginOptions.add("_task", task_strbuff);
    
            // get a handle of io_chemshell as the current tool plugin has no access to I/O
            const FilePluginInterface* plugin = pluginStore_->findFilePluginByNickname(PluginTypes::ModelFilePlugin, PluginTypes::ExportPlugin, QString("chemshell"));
    	    QString filename = pluginOptions_.value("filename");
            if(plugin != NULL) {
    
                // cast const pointer to pointer
                FilePluginInterface* pluginInterface = (FilePluginInterface*) plugin->duplicate();
                if(!pluginInterface->openOutput(filename)) {
                    Messenger::print("\nChemShell cannot open file %s to write\n", filename.toUtf8().constData());
                    Messenger::exit("ChemShellToolPlugin::runTool");
                    return false;
                }
    
                pluginInterface->setOptions(IOPluginOptions);
                if(pluginInterface->exportData()) {
                    ri->item->setFilename(filename);
                    ri->item->setPlugin(pluginInterface);
                    ri->item->updateSavePoint();
                    pluginInterface->closeFiles();
                }
    
            } else {
                printf("\n### plugin == NULL\n");
            }
    
    	}

    }
    
	// Update the display
	emit(updateWidgets(0));
	return true;
}

// Return interface as QObject
QObject* ChemShellToolPlugin::object() {
	return this;
}
