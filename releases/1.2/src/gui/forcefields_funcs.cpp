/*
	*** Qt GUI: Forcefields window
	*** src/gui/forcefield_funcs.cpp
	Copyright T. Youngs 2007-2009

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
#include "gui/mainwindow.h"
#include "gui/forcefields.h"
#include "gui/ffeditor.h"
#include "gui/selectpattern.h"
#include "gui/tlistwidgetitem.h"
#include "gui/gui.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "base/pattern.h"
#include "base/sysfunc.h"

// Constructor
AtenForcefields::AtenForcefields(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);

	// Private variables
	typelistElement_ = -1;
	shouldRefresh_ = FALSE;
	checkedItem_ = NULL;

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

// Destructor
AtenForcefields::~AtenForcefields()
{
}

// Show window
void AtenForcefields::showWindow()
{
	if (shouldRefresh_) refresh();
	show();
}

// Update the list of loaded forcefields
void AtenForcefields::refresh()
{
	// Always refresh combo box in main window
	gui.mainWindow->refreshForcefieldCombo();
	// If the window is not visible, don't do anything
	if (!gui.forcefieldsWindow->isVisible())
	{
		shouldRefresh_ = TRUE;
		return;
	}
	ui.ForcefieldList->clear();
	TListWidgetItem *item;
	for (Forcefield *ff = aten.forcefields(); ff != NULL; ff = ff->next)
	{
		item = new TListWidgetItem(ui.ForcefieldList);
		item->setText(ff->name());
		if (ff == aten.defaultForcefield())
		{
			item->setCheckState(Qt::Checked);
			checkedItem_ = item;
		}
		else item->setCheckState(Qt::Unchecked);
		item->setPointer(ff);
	}
	// Select the current FF.
	if (aten.currentForcefield() == NULL)
	{
		ui.ForcefieldList->setCurrentRow(0);
		ui.RemoveForcefieldButton->setEnabled(FALSE);
		ui.EditForcefieldButton->setEnabled(FALSE);
		ui.AssociateGroup->setEnabled(FALSE);
		ui.AutomaticTypingGroup->setEnabled(FALSE);
		ui.ManualTypingGroup->setEnabled(FALSE);
	}
	else
	{
		ui.ForcefieldList->setCurrentRow(aten.currentForcefieldId());
		ui.RemoveForcefieldButton->setEnabled(TRUE);
		ui.EditForcefieldButton->setEnabled(TRUE);
		ui.AssociateGroup->setEnabled(TRUE);
		ui.AutomaticTypingGroup->setEnabled(TRUE);
		ui.ManualTypingGroup->setEnabled(TRUE);
	}
	refreshTypes();
	// Refresh list of forcefields on Forcefields toolbar
	gui.mainWindow->refreshForcefieldCombo();
	shouldRefresh_ = FALSE;
}

// Update list of forcefield types in typelist
void AtenForcefields::refreshTypes()
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
void AtenForcefields::loadForcefield()
{
	QString filename;
	if (openForcefieldDialog->exec() == 1)
	{
		// Get selected filter in file dialog
		QString filter = openForcefieldDialog->selectedFilter();
		filename = openForcefieldDialog->selectedFiles().first();
		aten.loadForcefield(qPrintable(filename));
		refresh();
	}
}

// Set the current forcefield in master to reflect the list change
void AtenForcefields::on_ForcefieldList_currentRowChanged(int row)
{
	if (row != -1) aten.setCurrentForcefield(row);
	// Update type list
	refreshTypes();
}

// Item in forcefield list has changed?
void AtenForcefields::on_ForcefieldList_itemClicked(QListWidgetItem *item)
{
	// Cast item to our own TListWidgetItem
	TListWidgetItem *titem = (TListWidgetItem*) item;
	// Get forcefield associated to item
	Forcefield *ff = (Forcefield*) titem->pointer();
	Forcefield *defaultff = aten.defaultForcefield();
	// Look at checked state
	if ((titem->checkState() == Qt::Checked) && (ff != defaultff))
	{
		aten.setDefaultForcefield(ff);
		refreshTypes();
		// Uncheck old item if necessary
		if (checkedItem_ != NULL) checkedItem_->setCheckState(Qt::Unchecked);
		checkedItem_ = titem;
	}
	else if ((titem->checkState() == Qt::Unchecked) && (ff == defaultff))
	{
		aten.setDefaultForcefield(NULL);
		refreshTypes();
		checkedItem_ = NULL;
	}
	// Refresh list of forcefields on Forcefields toolbar
	gui.mainWindow->refreshForcefieldCombo();
}

// Load forcefield 
void AtenForcefields::on_LoadForcefieldButton_clicked(bool checked)
{
	loadForcefield();
}

// Remove selected forcefield in list
void AtenForcefields::on_RemoveForcefieldButton_clicked(bool checked)
{
	aten.removeForcefield(aten.currentForcefield());
	refresh();
}

// Call forcefield editor
void AtenForcefields::on_EditForcefieldButton_clicked(bool checked)
{
	gui.forcefieldEditorDialog->populate(aten.currentForcefield());
	gui.forcefieldEditorDialog->show();
}

// Assign current forcefield to model
void AtenForcefields::on_AssignFFToCurrentButton_clicked(bool checked)
{
	aten.currentModel()->setForcefield(aten.currentForcefield());
}

// Assign current forcefield to all models
void AtenForcefields::on_AssignFFToAllButton_clicked(bool checked)
{
	for (Model *m = aten.models(); m != NULL; m = m->next) m->setForcefield(aten.currentForcefield());
}

// Assign current forcefield to pattern
void AtenForcefields::on_AssignFFToPatternButton_clicked(bool checked)
{
	Pattern *p = gui.selectPatternDialog->selectPattern(aten.currentModel());
	if (p != NULL) p->setForcefield(aten.currentForcefield());
}

// Perform automatic atom typing
void AtenForcefields::on_TypeModelButton_clicked(bool checked)
{
	if (aten.currentModel()->typeAll()) gui.modelChanged();
}

// Remove typing from model
void AtenForcefields::on_UntypeModelButton_clicked(bool checked)
{
	aten.currentModel()->removeTyping();
	gui.modelChanged();
}

// Set the selected atoms to have the specified forcefield type
void AtenForcefields::on_ManualTypeSetButton_clicked(bool checked)
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
	if (ffa != NULL) m->selectionSetType(ffa, TRUE);
	gui.modelChanged();
}

// Clear type definitions from the selected atoms
void AtenForcefields::on_ManualTypeClearButton_clicked(bool checked)
{
	aten.currentModel()->selectionSetType(NULL, FALSE);
	gui.modelChanged();
}

// Test selected atom type on current atom selection
void AtenForcefields::on_ManualTypeTestButton_clicked(bool checked)
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
			for (Atom *i = m->firstSelected(); i != NULL; i = i->nextSelected())
			{
				// Get the pattern in which the atom exists
				Pattern *p = m->pattern(i);
				if (i->element() == at->characterElement())
				{
					matchscore = at->matchAtom(i, p->ringList(), m, i);
					msg.print("Atom %i (%s) matched type with score %i.\n", i->id()+1, elements().symbol(i), matchscore);
				}
				else msg.print("Atom %i (%s) is the wrong element for this type.\n", i->id()+1, elements().symbol(i));
			}
		}
	}
}

// Change target element in type list
void AtenForcefields::on_ManualTypeEdit_returnPressed()
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

void AtenForcefields::dialogFinished(int result)
{
	gui.mainWindow->ui.actionForcefieldsWindow->setChecked(FALSE);
}

