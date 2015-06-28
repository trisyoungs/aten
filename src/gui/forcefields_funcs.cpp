/*
	*** FF/Energy Dock Widget
	*** src/gui/forcefields_funcs.cpp
	Copyright T. Youngs 2007-2015

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

#include <QCloseEvent>
#include <QtWidgets/QFileDialog>
#include "main/aten.h"
#include "methods/mc.h"
#include "methods/sd.h"
#include "methods/cg.h"
#include "ff/forcefield.h"
#include "base/pattern.h"
#include "base/sysfunc.h"
#include "base/forcefieldatom.h"
#include "gui/ffeditor.h"
#include "gui/selectpattern.h"
#include "gui/mainwindow.h"
#include "gui/forcefields.h"
#include "model/model.h"
#include "parser/commandnode.h"

ATEN_USING_NAMESPACE

// Constructor
ForcefieldsWidget::ForcefieldsWidget(AtenWindow& parent, Qt::WindowFlags flags) : QDockWidget(&parent, flags), parent_(parent)
{
	ui.setupUi(this);

	// Private variables
	typelistElement_ = -1;
	refreshing_ = false;
}

void ForcefieldsWidget::showWidget()
{
	show();
	refresh();
}

void ForcefieldsWidget::refresh()
{
	Messenger::enter("ForcefieldsWidget::refresh");
	
	// Update list of forcefields in the combo box
	refreshing_ = true;
// 	QStringList slist;
// 	int def = -1, n = 0;
// 	slist << "<No Forcefield>";
// 	for (Forcefield* ff = parent_.aten().forcefields(); ff != NULL; ff = ff->next)
// 	{
// 		n++;
// 		if (ff == parent_.aten().currentForcefield()) def = n;
// 		slist << ff->name();
// 	}
// 	ui.ForcefieldCombo->clear();
// 	ui.ForcefieldCombo->addItems(slist);
// 	ui.ForcefieldCombo->setEnabled( n == 0 ? false : true );
// 	
// 	// Select whichever forcefield is marked as the default
// 	if (def != -1) ui.ForcefieldCombo->setCurrentIndex(def);
// 	else ui.ForcefieldCombo->setCurrentIndex(0);
	

	refreshing_ = false;
	Messenger::exit("ForcefieldsWidget::refresh");
}

// Update list of forcefield types in typelist
void ForcefieldsWidget::refreshTypes()
{
	ui.FFTypeTable->clear();
	QTableWidgetItem *item;
	int count = 0;
	Forcefield* ff = parent_.aten().currentForcefield();
	if (ff == NULL) return;
	// Reset header labels
	ui.FFTypeTable->setHorizontalHeaderLabels(QStringList() << "TypeID" << "Name" << "Description");
	for (ForcefieldAtom* ffa = ff->types(); ffa != NULL; ffa = ffa->next)
	{
		if (ffa->neta()->characterElement() != typelistElement_) continue;
		ui.FFTypeTable->setRowCount(count+1);
		item = new QTableWidgetItem(QString::number(ffa->typeId()));
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

// Save forcefield 
void ForcefieldsWidget::on_SaveForcefieldButton_clicked(bool checked)
{
	// Get current forcefield
	Forcefield* ff = parent_.aten().currentForcefield();
	if (ff == NULL) return;

	// Does forcefield have a valid filename? If not, call the other routine....
	QString filename = ff->filename();
	if (filename.isEmpty()) ui.SaveForcefieldAsButton->click();
	else
	{
		// Save forcefield under filename currently in 'filenanme'
		Messenger::print("Saving forcefield '%s' to file '%s'...", qPrintable(ff->name()), qPrintable(ff->filename()));
		ff->save();
	}
}

// Save forcefield 
void ForcefieldsWidget::on_SaveForcefieldAsButton_clicked(bool checked)
{
	// Get current forcefield
	Forcefield* ff = parent_.aten().currentForcefield();
	if (ff == NULL) return;

	static QDir currentDirectory_(parent_.aten().dataDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Forcefield", currentDirectory_.path());
	if (filename.isEmpty()) return;
	ff->setFilename(qPrintable(filename));
	
	// Save forcefield under filename currently in 'filenanme'
	Messenger::print("Saving forcefield '%s' to file '%s'...", qPrintable(ff->name()), qPrintable(ff->filename()));
	ff->save();
}

/*
 * Manual Typing Tab
 */

// Set the selected atoms to have the specified forcefield type
void ForcefieldsWidget::on_ManualTypeSetButton_clicked(bool checked)
{
	// Check selected forcefield against that assigned to the model
	Model* m = parent_.aten().currentModel();
	Forcefield* ff = parent_.aten().currentForcefield();
	if ((m == NULL) || (ff == NULL)) return;
	if (m->forcefield() != ff)
	{
		Messenger::print("The type you are trying to assign is in a different forcefield to that assigned to the model.");
		return;
	}
	// Get the selected row in the FFTypeList
	int row = ui.FFTypeTable->currentRow();
	if (row == -1) return;
	QTableWidgetItem *item = ui.FFTypeTable->item(row,0);
	ForcefieldAtom* ffa = ff->findType(atoi(qPrintable(item->text())));
	if (ffa != NULL)
	{
		m->selectionSetType(ffa, true);
		Messenger::print("Manually set types of %i atoms.", parent_.aten().currentModel()->nSelected());
	}
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

// Clear type definitions from the selected atoms
void ForcefieldsWidget::on_ManualTypeClearButton_clicked(bool checked)
{
	parent_.aten().currentModel()->selectionSetType(NULL, false);
	Messenger::print("Cleared types of %i atoms.", parent_.aten().currentModel()->nSelected());
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

// Test selected atom type on current atom selection
void ForcefieldsWidget::on_ManualTypeTestButton_clicked(bool checked)
{
	Forcefield* ff = parent_.aten().currentForcefield();
	int row = ui.FFTypeTable->currentRow();
	if (row == -1) return;
	QTableWidgetItem *item = ui.FFTypeTable->item(row,0);
	ForcefieldAtom* ffa = ff->findType(atoi(qPrintable(item->text())));
	if (ffa != NULL)
	{
		Model* m = parent_.aten().currentModel();
		Neta* at = ffa->neta();
		if (m->createPatterns())
		{
			Messenger::print("Testing atom type '%s' (id = %i) from forcefield '%s' on current selection:", qPrintable(ffa->name()), ffa->typeId(), qPrintable(ff->name()));
			// Prepare for typing
			m->describeAtoms();
			int matchscore;
			for (RefListItem<Atom,int>* ri = m->selection(); ri != NULL; ri = ri->next)
			{
				// Get the pattern in which the atom exists
				Pattern* p = m->pattern(ri->item);
				if (ri->item->element() == at->characterElement())
				{
					matchscore = at->matchAtom(ri->item, p->ringList(), m);
					Messenger::print("Atom %i (%s) matched type with score %i.", ri->item->id()+1, Elements().symbol(ri->item), matchscore);
				}
				else Messenger::print("Atom %i (%s) is the wrong element for this type.", ri->item->id()+1, Elements().symbol(ri->item));
			}
		}
	}
}

// Change target element in type list
void ForcefieldsWidget::on_ManualTypeEdit_returnPressed()
{
	// Get the contents of the line edit and check that it is an element symbol
	int el = Elements().find(qPrintable(ui.ManualTypeEdit->text()), ElementMap::AlphaZMap);
	if (el == -1)
	{
		Messenger::print("Unknown element '%s'",qPrintable(ui.ManualTypeEdit->text()));
		ui.ManualTypeEdit->setText("H");
		typelistElement_ = 1;
	}
	else typelistElement_ = el;
	refreshTypes();
}

