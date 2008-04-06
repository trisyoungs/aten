/*
	*** Qt forcefield editor window functions
	*** src/gui/ffeditor_funcs.cpp
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

#include "gui/ffeditor.h"
#include "base/elements.h"
//#include <QtGui/QDialog>
#include "classes/forcefield.h"

// Constructor
AtenEdit::AtenEdit(QDialog *parent) : QDialog(parent)
{
	ui.setupUi(this);
}

// Finalise GUI
void AtenEdit::finaliseUi()
{
	dbgBegin(Debug::Calls,"AtenEdit::finaliseUi");
	dbgEnd(Debug::Calls,"AtenEdit::finaliseUi");
}

// Set controls
void AtenEdit::setControls()
{
	dbgBegin(Debug::Calls,"AtenEdit::setControls");
	dbgBegin(Debug::Calls,"AtenEdit::setControls");
}

// Populate widget with specified forcefield
void AtenEdit::populate(Forcefield *ff)
{
	// Clear tables
	ui.FFEditorTypesTable->clear();
	ui.FFEditorAtomsTable->clear();
	ui.FFEditorBondsTable->clear();
	ui.FFEditorAnglesTable->clear();
	ui.FFEditorTorsionsTable->clear();
	if (ff == NULL) return;
	QTableWidgetItem *item;
	int count;

	// Types List
	count = 0;
	ui.FFEditorTypesTable->setRowCount(ff->nTypes()+1);
	ui.FFEditorTypesTable->setHorizontalHeaderLabels(QStringList() << "TypeID" << "El" << "Name" << "Equiv" << "Description");
	for (ForcefieldAtom *ffa = ff->types(); ffa != NULL; ffa = ffa->next)
	{
		item = new QTableWidgetItem(itoa(ffa->typeId()));
		ui.FFEditorTypesTable->setItem(count, 0, item);
		item = new QTableWidgetItem(elements.symbol(ffa->atomtype()->characterElement()));
		ui.FFEditorTypesTable->setItem(count, 1, item);
		item = new QTableWidgetItem(ffa->name());
		ui.FFEditorTypesTable->setItem(count, 2, item);
		item = new QTableWidgetItem(ffa->equivalent());
		ui.FFEditorTypesTable->setItem(count, 3, item);
		item = new QTableWidgetItem(ffa->description());
		ui.FFEditorTypesTable->setItem(count, 4, item);
		count ++;
	}
	ui.FFEditorTypesTable->resizeColumnToContents(0);
	ui.FFEditorTypesTable->resizeColumnToContents(1);
	ui.FFEditorTypesTable->resizeColumnToContents(2);
	ui.FFEditorTypesTable->resizeColumnToContents(3);
	ui.FFEditorTypesTable->resizeColumnToContents(4);

	// Atoms List
	count = 0;
	ui.FFEditorAtomsTable->setRowCount(ff->nTypes()+1);
	ui.FFEditorAtomsTable->setHorizontalHeaderLabels(QStringList() << "TypeID" << "Element" << "Name" << "Equivalent" << "Description");
	for (ForcefieldAtom *ffa = ff->types(); ffa != NULL; ffa = ffa->next)
	{
		item = new QTableWidgetItem(itoa(ffa->typeId()));
		ui.FFEditorAtomsTable->setItem(count, 0, item);
		item = new QTableWidgetItem(elements.symbol(ffa->atomtype()->characterElement()));
		ui.FFEditorAtomsTable->setItem(count, 1, item);
		item = new QTableWidgetItem(ffa->name());
		ui.FFEditorAtomsTable->setItem(count, 2, item);
		item = new QTableWidgetItem(ffa->equivalent());
		ui.FFEditorAtomsTable->setItem(count, 3, item);
		item = new QTableWidgetItem(ffa->description());
		ui.FFEditorAtomsTable->setItem(count, 4, item);
		count ++;
	}
	ui.FFEditorAtomsTable->resizeColumnToContents(0);
	ui.FFEditorAtomsTable->resizeColumnToContents(1);
	ui.FFEditorAtomsTable->resizeColumnToContents(2);
	ui.FFEditorAtomsTable->resizeColumnToContents(3);
	ui.FFEditorAtomsTable->resizeColumnToContents(4);

	// Bonds List
	count = 0;
	ui.FFEditorBondsTable->setRowCount(ff->nBonds()+1);
	for (ForcefieldBound *ffb = ff->bonds(); ffb != NULL; ffb = ffb->next)
	{
		item = new QTableWidgetItem(ffb->typeName(0));
		ui.FFEditorBondsTable->setItem(count, 0, item);
		item = new QTableWidgetItem(ffb->typeName(1));
		ui.FFEditorBondsTable->setItem(count, 1, item);
		count ++;
	}
}

/*
// Atomtype Page
*/

void AtenEdit::on_FFEditorTestTypeButton_clicked(bool on)
{
	ui.FFEditorTypeScoreLabel->setText("Score : ");
	ui.FFEditorNumMatchedLabel->setText("Atoms Matched : ");
}

void AtenEdit::on_FFEditorGenerateTypeButton_clicked(bool on)
{
}

void AtenEdit::on_FFEditorAtomTable_currentRowChanged(int row)
{
}
