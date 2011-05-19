/*
	*** Minimiser Dock Widget
	*** src/gui/minimise_funcs.cpp
	Copyright T. Youngs 2007-2011

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

#include "main/aten.h"
#include "methods/mc.h"
#include "methods/sd.h"
#include "methods/cg.h"
#include "ff/forcefield.h"
#include "base/pattern.h"
#include "base/sysfunc.h"
#include "classes/forcefieldatom.h"
#include "gui/ffeditor.h"
#include "gui/selectpattern.h"
#include "gui/mainwindow.h"
#include "gui/toolbox.h"
#include "gui/gui.h"
#include "gui/forcefields.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
ForcefieldsWidget::ForcefieldsWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);

	// Private variables
	typelistElement_ = -1;
	refreshing_ = FALSE;
	
	// Create open forcefield dialog
	QStringList filters;
	openForcefieldDialog = new QFileDialog(this);
	openForcefieldDialog->setFileMode(QFileDialog::ExistingFile);
	openForcefieldDialog->setDirectory(aten.dataDir());
	openForcefieldDialog->setWindowTitle("Open Forcefield");
	filters.clear();
	filters << "All files (*)";
	filters << "Forcefield Files (*.ff)";
	openForcefieldDialog->setFilters(filters);
	
	// Create save forcefield dialog
	saveForcefieldDialog = new QFileDialog(this);
	saveForcefieldDialog->setWindowTitle("Save Forcefield");
	saveForcefieldDialog->setAcceptMode(QFileDialog::AcceptSave);
	saveForcefieldDialog->setDirectory(aten.workDir());
	saveForcefieldDialog->setFileMode(QFileDialog::AnyFile);
	filters.clear();
	filters << "All files (*)";
	filters << "Forcefield Files (*.ff)";
	saveForcefieldDialog->setFilters(filters);
}

void ForcefieldsWidget::showWidget()
{
	show();
	refresh();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.ForcefieldsButton->setChecked(TRUE);
}

void ForcefieldsWidget::refresh()
{
	msg.enter("ForcefieldsWidget::refresh");
	// If the window is not visible, don't do anything
	if (!gui.forcefieldsWidget->isVisible())
	{
		msg.exit("ForcefieldsWidget::refresh");
		return;
	}
	
	// Update list of forcefields in the combo box
	refreshing_ = TRUE;
	QStringList slist;
	int def = -1, n = 0;
	slist << "<No Forcefield>";
	for (Forcefield *ff = aten.forcefields(); ff != NULL; ff = ff->next)
	{
		n++;
		if (ff == aten.currentForcefield()) def = n;
		slist << ff->name();
	}
	ui.ForcefieldCombo->clear();
	ui.ForcefieldCombo->addItems(slist);
	ui.ForcefieldCombo->setEnabled( n == 0 ? FALSE : TRUE );
	
	// Select whichever forcefield is marked as the default
	if (def != -1) ui.ForcefieldCombo->setCurrentIndex(def);
	else ui.ForcefieldCombo->setCurrentIndex(0);
	
	// Is a valid current forcefield selected?
	bool ffselected = (aten.currentForcefield() != NULL);
	ui.CloseForcefieldButton->setEnabled(ffselected);
	ui.EditForcefieldButton->setEnabled(ffselected);
	ui.AssociateGroup->setEnabled(ffselected);
	ui.AutomaticTypingGroup->setEnabled(ffselected);
	ui.ManualTypingGroup->setEnabled(ffselected);
	if (ffselected) refreshTypes();
	refreshing_ = FALSE;
	msg.exit("ForcefieldsWidget::refresh");
}

// Update list of forcefield types in typelist
void ForcefieldsWidget::refreshTypes()
{
	ui.FFTypeTable->clear();
	QTableWidgetItem *item;
	int count = 0;
	Forcefield *ff = aten.currentForcefield();
	if (ff == NULL) return;
	// Reset header labels
	ui.FFTypeTable->setHorizontalHeaderLabels(QStringList() << "TypeID" << "Name" << "Description");
	for (ForcefieldAtom *ffa = ff->types(); ffa != NULL; ffa = ffa->next)
	{
		if (ffa->neta()->characterElement() != typelistElement_) continue;
		ui.FFTypeTable->setRowCount(count+1);
		item = new QTableWidgetItem(itoa(ffa->typeId()));
		ui.FFTypeTable->setItem(count, 0, item);
		item = new QTableWidgetItem(ffa->name());
		ui.FFTypeTable->setItem(count, 1, item);
		item = new QTableWidgetItem(ffa->description());
		ui.FFTypeTable->setItem(count, 2, item);
		count ++;
	}
	// Resize the columns
	ui.FFTypeTable->resizeColumnToContents(0);
	ui.FFTypeTable->resizeColumnToContents(1);
	ui.FFTypeTable->resizeColumnToContents(2);
}

// Load forcefield (public function)
void ForcefieldsWidget::loadForcefield()
{
	static QDir currentDirectory_(aten.dataDir());
	QString filename = QFileDialog::getOpenFileName(this, "Select Forcefield", currentDirectory_.path());
	if (!filename.isEmpty())
	{
		aten.loadForcefield(qPrintable(filename));
		refresh();
	}
}

// Save forcefield (public function)
void ForcefieldsWidget::saveForcefield()
{
// 	static QDir currentDirectory_(aten.dataDir());
// 	QString filename = QFileDialog::getSFileName(this, "Select Forcefield", currentDirectory_.path());
// 	if (!filename.isEmpty())
// 	{
// 		aten.loadForcefield(qPrintable(filename));
// 		refresh();
// 	}
}

/*
// Energy Tab
*/

void ForcefieldsWidget::on_CurrentEnergyButton_clicked(bool checked)
{
	bool result;
	if (aten.current.rs() == aten.current.m) result = CommandNode::run(Command::ModelEnergy, "");
	else result = CommandNode::run(Command::FrameEnergy, "");
	// Print energy
	if (result) aten.currentModel()->renderSourceModel()->energy.print();
}

void ForcefieldsWidget::on_CurrentForcesButton_clicked(bool checked)
{
	if (aten.current.rs() == aten.current.m) CommandNode::run(Command::ModelForces, "");
	else CommandNode::run(Command::FrameForces, "");
}

void ForcefieldsWidget::on_ForcefieldMinimiseButton_clicked(bool checked)
{
	// Set convergence criteria and get maxcycles data
	CommandNode::run(Command::Converge, "dd", pow(10.0,ui.EnergyConvergeSpin->value()), pow(10.0,ui.ForceConvergeSpin->value()));
	int maxcycles = ui.MinimiseCyclesSpin->value();
	Dnchar options;
	
	// Perform the minimisation
	switch (ui.MinimiserMethodCombo->currentIndex())
	{
		case (SimpleSteepestMethod):
			CommandNode::run(Command::LineTolerance, "d", 1.0e-4);
			CommandNode::run(Command::SDMinimise, "ii", maxcycles, 1);
			break;
		case (SteepestMethod):
			CommandNode::run(Command::LineTolerance, "d", 1.0e-4);
			CommandNode::run(Command::SDMinimise, "ii", maxcycles, 0);
			break;
		case (ConjugateMethod):
			CommandNode::run(Command::LineTolerance, "d", 1.0e-4);
			CommandNode::run(Command::CGMinimise, "i", maxcycles);
			break;
		case (MonteCarloMethod):
			CommandNode::run(Command::MCMinimise, "i", maxcycles);
			break;
	}
	// Update the view
	gui.update(GuiQt::AtomsTarget+GuiQt::CanvasTarget);
}

void ForcefieldsWidget::on_MopacMinimiseButton_clicked(bool checked)
{
	// Construct command string from GUI widget options
	Dnchar options(-1, "BFGS %s %s %s CHARGE=%i", qPrintable(ui.MopacHFCombo->currentText()), qPrintable(ui.MopacHamiltonianCombo->currentText()), 	qPrintable(ui.MopacSpinCombo->currentText()), ui.MopacChargeSpin->value());
	if (ui.MopacMozymeCheck->isChecked()) options.strcat(" MOZYME");
	CommandNode::run(Command::MopacMinimise, "c", options.get());
	gui.update(GuiQt::AtomsTarget+GuiQt::CanvasTarget);
}
			
/*
// Forcefields Tab
*/

void ForcefieldsWidget::on_ForcefieldCombo_currentIndexChanged(int index)
{
	if (refreshing_) return;
	// Set the new default forcefield in the master and refresh the forcefields page
	if (index == 0) aten.setCurrentForcefield( (Forcefield*) NULL);
	else aten.setCurrentForcefield(index-1);
	refreshTypes();
}

// Load forcefield 
void ForcefieldsWidget::on_OpenForcefieldButton_clicked(bool checked)
{
	loadForcefield();
}

// Save forcefield 
void ForcefieldsWidget::on_SaveForcefieldButton_clicked(bool checked)
{
	saveForcefield();
}

// Remove selected forcefield in list
void ForcefieldsWidget::on_CloseForcefieldButton_clicked(bool checked)
{
	aten.removeForcefield(aten.currentForcefield());
	refresh();
}

// Call forcefield editor
void ForcefieldsWidget::on_EditForcefieldButton_clicked(bool checked)
{
	gui.forcefieldEditorDialog->populate(aten.currentForcefield());
	gui.forcefieldEditorDialog->show();
}

// Assign current forcefield to model
void ForcefieldsWidget::on_AssignFFToCurrentButton_clicked(bool checked)
{
	aten.currentModelOrFrame()->setForcefield(aten.currentForcefield());
}

// Assign current forcefield to all models
void ForcefieldsWidget::on_AssignFFToAllButton_clicked(bool checked)
{
	for (Model *m = aten.models(); m != NULL; m = m->next) m->setForcefield(aten.currentForcefield());
}

// Assign current forcefield to pattern
void ForcefieldsWidget::on_AssignFFToPatternButton_clicked(bool checked)
{
	Pattern *p = gui.selectPatternDialog->selectPattern(aten.currentModelOrFrame());
	if (p != NULL) p->setForcefield(aten.currentForcefield());
}

// Perform automatic atom typing
void ForcefieldsWidget::on_TypeModelButton_clicked(bool checked)
{
	if (aten.currentModelOrFrame()->typeAll()) gui.update(GuiQt::CanvasTarget);
}

// Remove typing from model
void ForcefieldsWidget::on_UntypeModelButton_clicked(bool checked)
{
	aten.currentModelOrFrame()->removeTyping();
	gui.update(GuiQt::CanvasTarget);
}

void ForcefieldsWidget::on_CreateExpressionButton_clicked(bool clicked)
{
	aten.currentModelOrFrame()->createExpression(Choice::Default, Choice::Default, ui.AssignFFChargesCheck->isChecked());
}

/*
// Manual Typing Tab
*/

// Set the selected atoms to have the specified forcefield type
void ForcefieldsWidget::on_ManualTypeSetButton_clicked(bool checked)
{
	// Check selected forcefield against that assigned to the model
	Model *m = aten.currentModel();
	Forcefield *ff = aten.currentForcefield();
	if ((m == NULL) || (ff == NULL)) return;
	if (m->forcefield() != ff)
	{
		msg.print("The type you are trying to assign is in a different forcefield to that assigned to the model.\n");
		return;
	}
	// Get the selected row in the FFTypeList
	int row = ui.FFTypeTable->currentRow();
	if (row == -1) return;
	QTableWidgetItem *item = ui.FFTypeTable->item(row,0);
	ForcefieldAtom *ffa = ff->findType(atoi(qPrintable(item->text())));
	if (ffa != NULL)
	{
		m->selectionSetType(ffa, TRUE);
		msg.print("Manually set types of %i atoms.\n", aten.currentModel()->nSelected());
	}
	gui.update(GuiQt::CanvasTarget);
}

// Clear type definitions from the selected atoms
void ForcefieldsWidget::on_ManualTypeClearButton_clicked(bool checked)
{
	aten.currentModel()->selectionSetType(NULL, FALSE);
	msg.print("Cleared types of %i atoms.\n", aten.currentModel()->nSelected());
	gui.update(GuiQt::CanvasTarget);
}

// Test selected atom type on current atom selection
void ForcefieldsWidget::on_ManualTypeTestButton_clicked(bool checked)
{
	Forcefield *ff = aten.currentForcefield();
	int row = ui.FFTypeTable->currentRow();
	if (row == -1) return;
	QTableWidgetItem *item = ui.FFTypeTable->item(row,0);
	ForcefieldAtom *ffa = ff->findType(atoi(qPrintable(item->text())));
	if (ffa != NULL)
	{
		Model *m = aten.currentModel();
		Neta *at = ffa->neta();
		if (m->autocreatePatterns())
		{
			msg.print("Testing atom type '%s' (id = %i) from forcefield '%s' on current selection:\n", ffa->name(), ffa->typeId(), ff->name());
			// Prepare for typing
			m->describeAtoms();
			int matchscore;
			for (Refitem<Atom,int> *ri = m->selection(); ri != NULL; ri = ri->next)
			{
				// Get the pattern in which the atom exists
				Pattern *p = m->pattern(ri->item);
				if (ri->item->element() == at->characterElement())
				{
					matchscore = at->matchAtom(ri->item, p->ringList(), m);
					msg.print("Atom %i (%s) matched type with score %i.\n", ri->item->id()+1, elements().symbol(ri->item), matchscore);
				}
				else msg.print("Atom %i (%s) is the wrong element for this type.\n", ri->item->id()+1, elements().symbol(ri->item));
			}
		}
	}
}

// Change target element in type list
void ForcefieldsWidget::on_ManualTypeEdit_returnPressed()
{
	// Get the contents of the line edit and check that it is an element symbol
	int el = elements().find(qPrintable(ui.ManualTypeEdit->text()));
	if (el == -1)
	{
		msg.print("Unknown element '%s'\n",qPrintable(ui.ManualTypeEdit->text()));
		ui.ManualTypeEdit->setText("H");
		typelistElement_ = 1;
	}
	else typelistElement_ = el;
	refreshTypes();
}

void ForcefieldsWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.ForcefieldsButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainWidget()->postRedisplay();
	event->accept();
}
