/*
	*** Qt forcefield functions interface
	*** src/gui-qt/forcefield_funcs.cpp
	Copyright T. Youngs 2007

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
#include "gui/gui.h"
#include "gui-qt/mainwindow.h"
#include <QtGui/QListWidgetItem>
#include <QtGui/QTableWidgetItem>
#include <QtGui/QFileDialog>

// Local variables
int typelist_element = 1;

// Update the list of loaded forcefields
void AtenForm::refresh_forcefieldpage()
{
	ui.ForcefieldList->clear();
	QListWidgetItem *item;
	for (forcefield *ff = master.get_ffs(); ff != NULL; ff = ff->next)
	{
		item = new QListWidgetItem(ui.ForcefieldList);
		item->setText(ff->get_name());
	}
	// Select the current FF.
	if (master.get_currentff() == NULL) ui.ForcefieldList->setCurrentRow(0);
	else ui.ForcefieldList->setCurrentRow(master.get_currentff_id());
}

// Update list of forcefield types in typelist
void AtenForm::refresh_forcefieldtypelist()
{
	ui.FFTypeTable->clear();
	QTableWidgetItem *item;
	int count = 0;
	forcefield *ff = master.get_currentff();
	if (ff == NULL) return;
	for (ffatom *ffa = ff->get_atomtypes(); ffa != NULL; ffa = ffa->next)
	{
		if (ffa->get_atomtype()->el != typelist_element) continue;
		ui.FFTypeTable->setRowCount(count+1);
		item = new QTableWidgetItem(itoa(ffa->get_ffid()));
		ui.FFTypeTable->setItem(count, 0, item);
		item = new QTableWidgetItem(ffa->get_name());
		ui.FFTypeTable->setItem(count, 1, item);
		item = new QTableWidgetItem(ffa->get_description());
		ui.FFTypeTable->setItem(count, 2, item);
		count ++;
	}
	// Select the topmost item
	//if (master.get_currentff() == NULL) ui.ForcefieldList->setCurrentRow(0);
	//else ui.ForcefieldList->setCurrentRow(master.get_currentff_id());
}

// Set the current forcefield in master to reflect the list change
void AtenForm::on_ForcefieldList_currentRowChanged(int row)
{
	master.set_currentff_by_id(row);
	// Update type list
	refresh_forcefieldtypelist();
}

// Remove selected forcefield in list
void AtenForm::on_RemoveForcefieldButton_clicked(bool checked)
{
	master.remove_ff(master.get_currentff());
	refresh_forcefieldpage();
}

// Call forcefield editor
void AtenForm::on_EditForcefieldButton_clicked(bool checked)
{
	printf("Forcefield editor not yet implemented.\n");
}

// Update the list of model patterns
void AtenForm::refresh_forcefieldpatterns()
{
	// Check to see if we need to update the list
	static pattern *firstpattern = NULL;
	model *m = master.get_currentmodel();
	if (m->get_patterns() == NULL)
	{
		// No patterns defined for model. Clear list and disable.
		ui.FFPatternCombo->clear();
		ui.FFPatternCombo->setEnabled(FALSE);
		ui.AssignFFToPatternButton->setEnabled(FALSE);
	}
	else if (m->get_patterns() != firstpattern)
	{
		// First pattern pointer differs from model's first pattern, so clear and reload list (unless NULL)
		ui.FFPatternCombo->clear();
		for (pattern *p = m->get_patterns(); p != NULL; p = p->next) ui.FFPatternCombo->addItem(p->get_name());
		ui.FFPatternCombo->setEnabled(TRUE);
		ui.AssignFFToPatternButton->setEnabled(TRUE);
	}
}

// Assign current forcefield to model
void AtenForm::on_AssignFFToCurrentButton_clicked(bool checked)
{
	master.get_currentmodel()->set_ff(master.get_currentff());
}

// Assign current forcefield to all models
void AtenForm::on_AssignFFToAllButton_clicked(bool checked)
{
	for (model *m = master.get_models(); m != NULL; m = m->next) m->set_ff(master.get_currentff());
}

// Assign current forcefield to pattern
void AtenForm::on_AssignFFToPatternButton_clicked(bool checked)
{
	printf("TODO\n");
}

// Perform automatic atom typing
void AtenForm::on_TypeModelButton_clicked(bool checked)
{
	if (master.get_currentmodel()->type_all()) gui.mainview.postredisplay();
}

// Remove typing from model
void AtenForm::on_UntypeModelButton_clicked(bool checked)
{
	master.get_currentmodel()->remove_typing();
	gui.mainview.postredisplay();
}

// Set the selected atoms to have the specified forcefield type
void AtenForm::on_ManualTypeSetButton_clicked(bool checked)
{
	// Check selected forcefield against that assigned to the model
	model *m = master.get_currentmodel();
	forcefield *ff = master.get_currentff();
	if ((m == NULL) || (ff == NULL)) return;
	if (m->get_ff() != ff)
	{
		msg(DM_NONE,"The type you are trying to assign is in a different forcefield to that assigned to the model.\n");
		return;
	}
	// Get the selected row in the FFTypeList
	int row = ui.FFTypeTable->currentRow();
	if (row == -1) return;
	QTableWidgetItem *item = ui.FFTypeTable->item(row,0);
	ffatom *ffa = ff->find_type(atoi(qPrintable(item->text())));
	if (ffa != NULL) m->selection_set_type(ffa, TRUE);
	gui.mainview.postredisplay();
}

// Clear type definitions from the selected atoms
void AtenForm::on_ManualTypeClearButton_clicked(bool checked)
{
	master.get_currentmodel()->selection_set_type(NULL, FALSE);
	gui.mainview.postredisplay();
}

// Test selected atom type on current atom selection
void AtenForm::on_ManualTypeTestButton_clicked(bool checked)
{
	forcefield *ff = master.get_currentff();
	int row = ui.FFTypeTable->currentRow();
	if (row == -1) return;
	QTableWidgetItem *item = ui.FFTypeTable->item(row,0);
	ffatom *ffa = ff->find_type(atoi(qPrintable(item->text())));
	if (ffa != NULL)
	{
		model *m = master.get_currentmodel();
		atomtype *at = ffa->get_atomtype();
		if (m->autocreate_patterns())
		{
			msg(DM_NONE,"Testing atom type '%s' (id = %i) from forcefield '%s' on current selection:\n", ffa->get_name(), ffa->get_ffid(), ff->get_name());
			// Prepare for typing
			m->describe_atoms();
			int matchscore;
			for (atom *i = m->get_first_selected(); i != NULL; i = i->get_next_selected())
			{
				// Get the pattern in which the atom exists
				pattern *p = m->get_pattern(i);
				if (i->get_element() == at->el)
				{
					matchscore = at->match_atom(i, p->get_ringlist(), m, i);
					msg(DM_NONE,"Atom %i (%s) matched type with score %i.\n", i->get_id()+1, elements.symbol(i), matchscore);
				}
				else msg(DM_NONE,"Atom %i (%s) is the wrong element for this type.\n", i->get_id()+1, elements.symbol(i));
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
		msg(DM_NONE,"Unknown element '%s'\n",qPrintable(ui.ManualTypeEdit->text()));
		ui.ManualTypeEdit->setText("H");
		typelist_element = 1;
	}
	else typelist_element = el;
	refresh_forcefieldtypelist();
}
