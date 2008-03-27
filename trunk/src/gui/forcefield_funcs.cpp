/*
	*** Qt forcefield functions interface
	*** src/gui/forcefield_funcs.cpp
	Copyright T. Youngs 2007,2008

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

#include "base/master.h"
#include "base/elements.h"
#include "classes/pattern.h"
#include "gui/mainwindow.h"
#include "gui/ffeditor.h"
#include "gui/tlistwidgetitem.h"
#include "gui/gui.h"
#include "model/model.h"
#include <QtGui/QTableWidgetItem>

// Local variables
int typelist_element = 1;

// Update the list of loaded forcefields
void AtenForm::refreshForcefieldPage()
{
	// If the cell page is not visible, don't do anything
	if (!ui.ShowCellPageButton->isChecked())
	{
		dbgEnd(Debug::Calls,"AtenForm::refreshCellPage");
		return;
	}
	ui.ForcefieldList->clear();
	TListWidgetItem *item;
	for (Forcefield *ff = master.forcefields(); ff != NULL; ff = ff->next)
	{
		item = new TListWidgetItem(ui.ForcefieldList);
		item->setText(ff->name());
		item->setCheckState(ff == master.defaultForcefield() ? Qt::Checked : Qt::Unchecked);
		item->setForcefield(ff);
	}
	// Select the current FF.
	if (master.currentForcefield() == NULL)
	{
		ui.ForcefieldList->setCurrentRow(0);
		ui.RemoveForcefieldButton->setEnabled(FALSE);
		ui.EditForcefieldButton->setEnabled(FALSE);
	}
	else
	{
		ui.ForcefieldList->setCurrentRow(master.currentForcefieldId());
		ui.RemoveForcefieldButton->setEnabled(TRUE);
		ui.EditForcefieldButton->setEnabled(TRUE);
	}
}

// Update list of forcefield types in typelist
void AtenForm::refreshForcefieldTypeList()
{
	ui.FFTypeTable->clear();
	QTableWidgetItem *item;
	int count = 0;
	Forcefield *ff = master.currentForcefield();
	if (ff == NULL) return;
	// Reset header labels
	ui.FFTypeTable->setHorizontalHeaderLabels(QStringList() << "TypeID" << "Name" << "Description");
	for (ForcefieldAtom *ffa = ff->types(); ffa != NULL; ffa = ffa->next)
	{
		if (ffa->atomType()->characterElement() != typelist_element) continue;
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

// Set the current forcefield in master to reflect the list change
void AtenForm::on_ForcefieldList_currentRowChanged(int row)
{
	if (row != -1) master.setCurrentForcefield(row);
	// Update type list
	refreshForcefieldTypeList();
}

// Item in forcefield list has changed?
void AtenForm::on_ForcefieldList_itemClicked(QListWidgetItem *item)
{
	// Cast item to our own TListWidgetItem
	TListWidgetItem *titem = (TListWidgetItem*) item;
	// Get forcefield associated to item
	Forcefield *ff = titem->forcefield();
	Forcefield *defaultff = master.defaultForcefield();
	// Look at checked state
	if ((titem->checkState() == Qt::Checked) && (ff != defaultff))
	{
		master.setDefaultForcefield(ff);
		refreshForcefieldPage();
	}
	else if ((titem->checkState() == Qt::Unchecked) && (ff == defaultff))
	{
		master.setDefaultForcefield(NULL);
		refreshForcefieldPage();
	}
}

// Load forcefield 
void AtenForm::on_LoadForcefieldButton_clicked(bool checked)
{
	QString filename;
	if (openForcefieldDialog->exec() == 1)
	{
		// Get selected filter in file dialog
		QString filter = openForcefieldDialog->selectedFilter();
		filename = openForcefieldDialog->selectedFiles().first();
		master.loadForcefield(qPrintable(filename));
		refreshForcefieldPage();
	}
}

// Remove selected forcefield in list
void AtenForm::on_RemoveForcefieldButton_clicked(bool checked)
{
	master.removeForcefield(master.currentForcefield());
	refreshForcefieldPage();
}

// Call forcefield editor
void AtenForm::on_EditForcefieldButton_clicked(bool checked)
{
	gui.editDialog->populate(master.currentForcefield());
	gui.editDialog->show();
}

// Update the list of model patterns
void AtenForm::refreshForcefieldPatterns()
{
	// Check to see if we need to update the list
	static Pattern *firstpattern = NULL;
	Model *m = master.currentModel();
	if (m->patterns() == NULL)
	{
		// No patterns defined for model. Clear list and disable.
		ui.FFPatternCombo->clear();
		ui.FFPatternCombo->setEnabled(FALSE);
		ui.AssignFFToPatternButton->setEnabled(FALSE);
	}
	else if (m->patterns() != firstpattern)
	{
		// First pattern pointer differs from model's first pattern, so clear and reload list (unless NULL)
		ui.FFPatternCombo->clear();
		for (Pattern *p = m->patterns(); p != NULL; p = p->next) ui.FFPatternCombo->addItem(p->name());
		ui.FFPatternCombo->setEnabled(TRUE);
		ui.AssignFFToPatternButton->setEnabled(TRUE);
	}
}

// Assign current forcefield to model
void AtenForm::on_AssignFFToCurrentButton_clicked(bool checked)
{
	master.currentModel()->setForcefield(master.currentForcefield());
}

// Assign current forcefield to all models
void AtenForm::on_AssignFFToAllButton_clicked(bool checked)
{
	for (Model *m = master.models(); m != NULL; m = m->next) m->setForcefield(master.currentForcefield());
}

// Assign current forcefield to pattern
void AtenForm::on_AssignFFToPatternButton_clicked(bool checked)
{
	printf("TODO\n");
}

// Perform automatic atom typing
void AtenForm::on_TypeModelButton_clicked(bool checked)
{
	if (master.currentModel()->typeAll()) gui.mainView.postRedisplay();
}

// Remove typing from model
void AtenForm::on_UntypeModelButton_clicked(bool checked)
{
	master.currentModel()->removeTyping();
	gui.mainView.postRedisplay();
}

// Set the selected atoms to have the specified forcefield type
void AtenForm::on_ManualTypeSetButton_clicked(bool checked)
{
	// Check selected forcefield against that assigned to the model
	Model *m = master.currentModel();
	Forcefield *ff = master.currentForcefield();
	if ((m == NULL) || (ff == NULL)) return;
	if (m->forcefield() != ff)
	{
		msg(Debug::None,"The type you are trying to assign is in a different forcefield to that assigned to the model.\n");
		return;
	}
	// Get the selected row in the FFTypeList
	int row = ui.FFTypeTable->currentRow();
	if (row == -1) return;
	QTableWidgetItem *item = ui.FFTypeTable->item(row,0);
	ForcefieldAtom *ffa = ff->findType(atoi(qPrintable(item->text())));
	if (ffa != NULL) m->selectionSetType(ffa, TRUE);
	gui.mainView.postRedisplay();
}

// Clear type definitions from the selected atoms
void AtenForm::on_ManualTypeClearButton_clicked(bool checked)
{
	master.currentModel()->selectionSetType(NULL, FALSE);
	gui.mainView.postRedisplay();
}

// Test selected atom type on current atom selection
void AtenForm::on_ManualTypeTestButton_clicked(bool checked)
{
	Forcefield *ff = master.currentForcefield();
	int row = ui.FFTypeTable->currentRow();
	if (row == -1) return;
	QTableWidgetItem *item = ui.FFTypeTable->item(row,0);
	ForcefieldAtom *ffa = ff->findType(atoi(qPrintable(item->text())));
	if (ffa != NULL)
	{
		Model *m = master.currentModel();
		Atomtype *at = ffa->atomType();
		if (m->autocreatePatterns())
		{
			msg(Debug::None,"Testing atom type '%s' (id = %i) from forcefield '%s' on current selection:\n", ffa->name(), ffa->typeId(), ff->name());
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
					msg(Debug::None,"Atom %i (%s) matched type with score %i.\n", i->id()+1, elements.symbol(i), matchscore);
				}
				else msg(Debug::None,"Atom %i (%s) is the wrong element for this type.\n", i->id()+1, elements.symbol(i));
			}
		}
	}
}

// Change target element in type list
void AtenForm::on_ManualTypeEdit_editingFinished()
{
	// Get the contents of the line edit and check that it is an element symbol
	int el = elements.find(qPrintable(ui.ManualTypeEdit->text()));
	if (el == -1)
	{
		msg(Debug::None,"Unknown element '%s'\n",qPrintable(ui.ManualTypeEdit->text()));
		ui.ManualTypeEdit->setText("H");
		typelist_element = 1;
	}
	else typelist_element = el;
	refreshForcefieldTypeList();
}
