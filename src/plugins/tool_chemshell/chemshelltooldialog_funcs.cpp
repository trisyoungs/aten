/*
	*** Py-ChemShell Tool Dialog Functions
	*** src/gui/tool_chemshell/chemshelltooldialog_funcs.cpp
    Copyright T. Youngs 2007-2019
    Copyright Y. Lu 2019

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
/*
This plugin was written by You Lu, Aug. 2019
you.lu@stfc.ac.uk
*/

#include "plugins/tool_chemshell/chemshelltooldialog.h"
#include "gui/qcustomplot/qcustomplot.hui"

// Constructor
ChemShellToolDialog::ChemShellToolDialog(ToolPluginInterface& targetInterface,
                                         KVMap& pluginOptions
                                        ) : QDialog(NULL),
                                            targetInterface_(targetInterface),
                                            pluginOptions_(pluginOptions)
{
	ui.setupUi(this);

	// Initialise the icon resource
	Q_INIT_RESOURCE(chemshelltool_icons);

}

/*
 * Widget Functions
 */

void ChemShellToolDialog::on_CloseButton_clicked(bool checked) {
	// Close the dialog, storing UI options before we close
	setPluginOptions();
	accept(); 
    clearItems();
}

void ChemShellToolDialog::on_RunButton_clicked(bool checked) {
	// Set options before we call the run method
	setPluginOptions();
    pluginOptions_.add("_relabel", "false");
	targetInterface_.runTool();
}

void ChemShellToolDialog::on_RelabelButton_clicked(bool checked) {
    setPluginOptions();
    pluginOptions_.add("_relabel", "true");
    targetInterface_.runTool();
}


void ChemShellToolDialog::clearItems() {
    ui.theory->clear();
    ui.task->clear();
    clearQMMMItems();
    clearQMItems();
    clearMMItems();
}

void ChemShellToolDialog::addQMFunctionalItems() {
	ui.qm_functional->addItem("BLYP");
	ui.qm_functional->addItem("B3LYP");
	ui.qm_functional->addItem("LDA");
    ui.qm_functional->setEditable(true);;
}

void ChemShellToolDialog::clearQMFunctionalItems() {
    ui.qm_functional->clear();
}

void ChemShellToolDialog::addQMMMItems() {
    clearQMMMItems();
    ui.qmmm_embedding->addItem("Electrostatical");
    ui.qmmm_embedding->addItem("Mechanical");
    ui.qmmm_coupling->addItem("Covalent");
    ui.qmmm_coupling->addItem("Ionic");
    ui.qmmm_scheme->addItem("Additive");
    ui.qmmm_scheme->addItem("Subtractive");
    ui.qm_radius->setRange(0.0, 99.99);
    ui.qm_radius->setValue(15.0);
}

void ChemShellToolDialog::addQMItems() {
    clearQMItems();
    ui.qm_theory->addItem("NWChem");
    ui.qm_theory->addItem("GAMESS-UK");
    ui.qm_theory->addItem("ORCA");
    ui.qm_theory->addItem("LSDalton");
    ui.qm_theory->addItem("DFTB+");
    ui.qm_theory->addItem("CP2K");
    ui.qm_basis->addItem("STO-3G");
    ui.qm_basis->addItem("3-21G");
    ui.qm_basis->addItem("3-21G*");
    ui.qm_basis->addItem("6-31G*");
    ui.qm_basis->addItem("TZVP");
    ui.qm_basis->addItem("def2-SVP");
    ui.qm_basis->addItem("def2-TZVP");
    ui.qm_basis->setEditable(true);;
    ui.qm_method->addItem("HF");
    ui.qm_method->addItem("DFT");
    ui.qm_basis->setCurrentIndex(2);
    ui.qm_method->setCurrentIndex(0);
}

void ChemShellToolDialog::addMMItems() {
    clearMMItems();
	ui.mm_theory->addItem("DL_POLY 4");
	ui.mm_theory->addItem("GULP");
	ui.mm_ff->addItem("All-atom CHARMM");
	ui.mm_ff->addItem("CHARMM22 for proteins");
	ui.mm_ff->addItem("CHARMM36 for proteins");
	ui.mm_ff->addItem("CHARMM36 for lipids");
	ui.mm_ff->addItem("CHARMM36 for carbonhydrates");
	ui.mm_ff->addItem("CHARMM general forcefield");
	ui.mm_ff->addItem("United-atom for CHARMM");
	ui.mm_ff->addItem("All-atom Amber");
	ui.mm_ff->addItem("Amber general forcefield");
	ui.mm_ff->addItem("OPLS-AA");
	ui.mm_ff->addItem("OPLS_2005");
	ui.mm_ff->addItem("OPLS AA/M for proteins");
	ui.mm_ff->addItem("DREIDING");
	ui.mm_ff->addItem("Polymer consistent forcefield");
	ui.mm_ff->addItem("Consistent valence forcefield");
	ui.mm_ff->addItem("Gromos united atom G54A7");
	ui.mm_ff->addItem("Inorganic forcefield");
	ui.mm_ff->addItem("Inorganic forcefield for binary oxides");
	ui.mm_ff->addItem("Inorganic forcefield for ternary oxides");
	ui.mm_ff->addItem("Inorganic forcefield for binary halides");
	ui.mm_ff->addItem("Inorganic forcefield for glass");
	ui.mm_ff->addItem("Inorganic forcefield for clay");
	ui.mm_ff->addItem("Multiple potential");
    ui.mm_ff->setEditable(true);;
}

void ChemShellToolDialog::clearQMMMItems() {
    ui.qmmm_embedding->clear();
    ui.qmmm_coupling->clear();
    ui.qmmm_scheme->clear();
}

void ChemShellToolDialog::clearQMItems() {
    ui.qm_theory->clear();
    ui.qm_basis->clear();
    ui.qm_functional->clear();
    ui.qm_method->clear();
    ui.qm_basis->setEditable(false);;
    ui.qm_functional->setEditable(false);;
}

void ChemShellToolDialog::clearMMItems() {
    ui.mm_theory->clear();
    ui.mm_ff->clear();
    ui.mm_ff->setEditable(false);;
}

// Apply plugin options to UI controls
void ChemShellToolDialog::applyPluginOptions() {
    pluginOptions_.add("mm_ff_custom", "false");
	ui.selected_as_qm->setChecked(pluginOptions_.value("selected_as_qm") == "true");
	ui.replace_suffix->setChecked(pluginOptions_.value("replace_suffix") == "true");
	ui.theory->addItem("QM/MM");
	ui.theory->addItem("QM");
	ui.theory->addItem("MM");
    addQMMMItems();
    addQMItems();
    addMMItems();
	ui.task->addItem("Single-Point");
	ui.task->addItem("Geometry Optimisation");
	ui.task->addItem("Molecular Dynamics");
	ui.task->addItem("Charge Fitting");
	ui.task->addItem("Scan");
	ui.task->addItem("Parameterisation");
    ui.active_radius->setRange(0.0, 99.99);
    ui.active_radius->setValue(15.0);
    ui.new_punch->setText(pluginOptions_.value("new_punch"));
    chooseActiveRadius(ui.task->currentIndex());
    selectedQMRegion(true);

    connect(ui.theory   , SIGNAL(currentIndexChanged(int)), this, SLOT(switchTheories(int)));
    connect(ui.qm_method, SIGNAL(currentIndexChanged(int)), this, SLOT(switchQMMethods(int)));
    connect(ui.task     , SIGNAL(currentIndexChanged(int)), this, SLOT(chooseActiveRadius(int)));
    connect(ui.mm_ff    , SIGNAL(editTextChanged(const QString)), this, SLOT(setCustomFF()));
    connect(ui.mm_ff    , SIGNAL(currentIndexChanged(int)), this, SLOT(unsetCustomFF(int)));
    connect(ui.labelled_as_qm, SIGNAL(toggled(bool)), this, SLOT(labelledQMRegion(bool)));
    connect(ui.selected_as_qm, SIGNAL(toggled(bool)), this, SLOT(selectedQMRegion(bool)));
    connect(ui.radius_as_qm  , SIGNAL(toggled(bool)), this, SLOT(radiusQMRegion(bool)));
}


void ChemShellToolDialog::selectedQMRegion(bool toggled) {
    if(toggled) {
        ui.qm_label->setDisabled(true);
        ui.qm_radius->setDisabled(true);
    }
}

void ChemShellToolDialog::labelledQMRegion(bool toggled) {
    if(toggled) {
        ui.qm_label->setDisabled(false);
        ui.qm_radius->setDisabled(true);
    }
}

void ChemShellToolDialog::radiusQMRegion(bool toggled) {
    if(toggled) {
        ui.qm_label->setDisabled(true);
        ui.qm_radius->setDisabled(false);
    }
}

void ChemShellToolDialog::unsetCustomFF(int) {
	pluginOptions_.add("mm_ff_custom", "false");
}

void ChemShellToolDialog::setCustomFF() {
	pluginOptions_.add("mm_ff_custom", "true");
}

void ChemShellToolDialog::chooseActiveRadius(int index) {

    if(index == 1 || index == 2) {
        ui.active_radius->setRange(0.0, 99.99);
        ui.active_radius->setValue(15.0);
    } else {
        ui.active_radius->setRange(0.0, 0.0);
    }

}

void ChemShellToolDialog::switchQMMethods(int index) {
    clearQMFunctionalItems();
    // DFT
    if(index == 1) {
        addQMFunctionalItems();
    }
}

void ChemShellToolDialog::switchTheories(int index) {
    clearQMMMItems();
    clearQMItems();
    clearMMItems();
    // if QM selected
    if(index == 1) {
        addQMItems();
    // if MM selected
    } else if(index == 2) {
        addMMItems();
    // if QM/MM selected
    } else {
        addQMMMItems();
        addQMItems();
        addMMItems();
    }

 
//    QComboBox *combo1 = new QComboBox;
//    QComboBox *combo2 = new QComboBox;
//    if (sender() == combo1)
//    {
//    	combo2->blockSignals(true);
//    	combo2->setCurrentIndex(index);
//    	combo2->blockSignals(false);
//    }
//    else if (sender() == combo2)
//    {
//    	combo1->blockSignals(true);
//    	combo1->setCurrentIndex(index);
//    	combo1->blockSignals(false);
//    }
}

// Set plugin options from UI controls
void ChemShellToolDialog::setPluginOptions()
{
	pluginOptions_.add("selected_as_qm", ui.selected_as_qm->isChecked() ? "true" : "false");
	pluginOptions_.add("labelled_as_qm", ui.labelled_as_qm->isChecked() ? "true" : "false");
	pluginOptions_.add("radius_as_qm"  , ui.radius_as_qm->isChecked() ? "true" : "false");
    pluginOptions_.add("qm_label"      , ui.qm_label->text());
	pluginOptions_.add("qm_radius"     , QString::number(ui.qm_radius->value(), 'f', 2));
	pluginOptions_.add("replace_suffix", ui.replace_suffix->isChecked() ? "true" : "false");
	pluginOptions_.add("theory"        , ui.theory->currentText());
	pluginOptions_.add("qmmm_embedding", ui.qmmm_embedding->currentText());
	pluginOptions_.add("qmmm_coupling" , ui.qmmm_coupling->currentText());
	pluginOptions_.add("qmmm_scheme"   , ui.qmmm_scheme->currentText());
	pluginOptions_.add("qm_theory"     , ui.qm_theory->currentText());
	pluginOptions_.add("qm_method"     , ui.qm_method->currentText());
	pluginOptions_.add("qm_basis"      , ui.qm_basis->currentText());
	pluginOptions_.add("qm_functional" , ui.qm_functional->currentText());
	pluginOptions_.add("mm_theory"     , ui.mm_theory->currentText());
	pluginOptions_.add("mm_ff"         , ui.mm_ff->currentText());
	pluginOptions_.add("task"          , ui.task->currentText());
	pluginOptions_.add("active_radius" , QString::number(ui.active_radius->value(), 'f', 2));
    pluginOptions_.add("type_suffix"   , ui.type_suffix->text());
    pluginOptions_.add("new_punch"     , ui.new_punch->text());
    pluginOptions_.add("regex"         , ui.regex->text());
    pluginOptions_.add("filename"      , ui.filename->text());
}
