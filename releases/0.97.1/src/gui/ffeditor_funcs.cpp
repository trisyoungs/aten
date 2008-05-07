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
#include "classes/forcefield.h"

// Local enum variables
namespace TypeColumn
{
	enum TypeColumn { Id, Element, Name, Equivalent, TypeString, Description, nColumns };
}
namespace AtomColumn
{
	enum AtomColumn { Id, Name, Charge, Form, Data1, Data2, Data3, Data4, Data5, Data6, nColumns };
}
namespace BondColumn
{
	enum BondColumn { Type1, Type2, Form, Data1, Data2, Data3, Data4, Data5, Data6, nColumns };
}
namespace AngleColumn
{
	enum AngleColumn { Type1, Type2, Type3, Form, Data1, Data2, Data3, Data4, Data5, Data6, nColumns };
}
namespace TorsionColumn
{
	enum TorsionColumn { Type1, Type2, Type3, Type4, Form, Data1, Data2, Data3, Data4, Data5, Data6, nColumns };
}
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
	dbgEnd(Debug::Calls,"AtenEdit::setControls");
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
	int count, n;
	ForcefieldParams ffp;
	QStringList slist;

	// Main title
	ui.FFEditorForcefieldLabel1->setText(ff->name());
	ui.FFEditorForcefieldLabel2->setText(ff->filename());

	// Types List
	count = 0;
	ui.FFEditorTypesTable->setRowCount(ff->nTypes()-1);
	ui.FFEditorTypesTable->setHorizontalHeaderLabels(QStringList() << "TypeID" << "El" << "Name" << "Equiv" << "Atomtype" << "Description");
	for (ForcefieldAtom *ffa = ff->types()->next; ffa != NULL; ffa = ffa->next)
	{
		item = new QTableWidgetItem(itoa(ffa->typeId()));
		ui.FFEditorTypesTable->setItem(count, TypeColumn::Id, item);
		item = new QTableWidgetItem(elements.symbol(ffa->atomtype()->characterElement()));
		ui.FFEditorTypesTable->setItem(count, TypeColumn::Element, item);
		item = new QTableWidgetItem(ffa->name());
		ui.FFEditorTypesTable->setItem(count, TypeColumn::Name, item);
		item = new QTableWidgetItem(ffa->equivalent());
		ui.FFEditorTypesTable->setItem(count, TypeColumn::Equivalent, item);
		item = new QTableWidgetItem(ffa->atomtypeString());
		ui.FFEditorTypesTable->setItem(count, TypeColumn::TypeString, item);
		item = new QTableWidgetItem(ffa->description());
		ui.FFEditorTypesTable->setItem(count, TypeColumn::Description, item);
		count ++;
	}
	for (n=0; n<TypeColumn::nColumns; n++) ui.FFEditorTypesTable->resizeColumnToContents(n);

	// Atoms List
	count = 0;
	ui.FFEditorAtomsTable->setRowCount(ff->nTypes()-1);
	ui.FFEditorAtomsTable->setHorizontalHeaderLabels(QStringList() << "TypeID" << "Name" << "Charge" << "Form" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6");
	slist.clear();
	for (n=0; n<Forms::nVdwForms; n++) slist << Forms::vdwFunction( (Forms::VdwFunction) n);
	for (ForcefieldAtom *ffa = ff->types()->next; ffa != NULL; ffa = ffa->next)
	{
		ffp = ffa->params();
		item = new QTableWidgetItem(itoa(ffa->typeId()));
		ui.FFEditorAtomsTable->setItem(count, AtomColumn::Id, item);
		item = new QTableWidgetItem(ffa->name());
		ui.FFEditorAtomsTable->setItem(count, AtomColumn::Name, item);
		item = new QTableWidgetItem(ftoa(ffa->charge()));
		ui.FFEditorAtomsTable->setItem(count, AtomColumn::Charge, item);
		QComboBox *combo = new QComboBox(this);
		combo->setMinimumSize(64,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffa->vdwForm());
		ui.FFEditorAtomsTable->setCellWidget(count,AtomColumn::Form,combo);
		for (int n=0; n<6; n++)
		{
			item = new QTableWidgetItem(ftoa(ffp.data[n]));
			ui.FFEditorAtomsTable->setItem(count, AtomColumn::Data1+n, item);
		}
		count ++;
	}
	for (n=0; n<AtomColumn::nColumns; n++) ui.FFEditorAtomsTable->resizeColumnToContents(n);
	ui.FFEditorAtomsTable->setColumnWidth(AtomColumn::Form, 68);

	// Bonds List
	count = 0;
	ui.FFEditorBondsTable->setRowCount(ff->nBonds());
	ui.FFEditorBondsTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6");
	slist.clear();
	for (n=0; n<Forms::nBondFunctions; n++) slist << Forms::bondFunction( (Forms::BondFunction) n);
	for (ForcefieldBound *ffb = ff->bonds(); ffb != NULL; ffb = ffb->next)
	{
		ffp = ffb->params();
		item = new QTableWidgetItem(ffb->typeName(0));
		ui.FFEditorBondsTable->setItem(count, BondColumn::Type1, item);
		item = new QTableWidgetItem(ffb->typeName(1));
		ui.FFEditorBondsTable->setItem(count, BondColumn::Type2, item);
		QComboBox *combo = new QComboBox(this);
		combo->setMinimumSize(78,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffb->functionalForm().bondFunc);
		ui.FFEditorBondsTable->setCellWidget(count, BondColumn::Form, combo);
		for (int n=0; n<6; n++)
		{
			item = new QTableWidgetItem(ftoa(ffp.data[n]));
			ui.FFEditorBondsTable->setItem(count, BondColumn::Data1+n, item);
		}
		count ++;
	}
	for (n=0; n<BondColumn::nColumns; n++) ui.FFEditorBondsTable->resizeColumnToContents(n);
	ui.FFEditorBondsTable->setColumnWidth(BondColumn::Form, 82);

	// Angles List
	count = 0;
	ui.FFEditorAnglesTable->setRowCount(ff->nAngles());
	ui.FFEditorAnglesTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Type 3" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6");
	slist.clear();
	for (n=0; n<Forms::nAngleFunctions; n++) slist << Forms::angleFunction( (Forms::AngleFunction) n);
	for (ForcefieldBound *ffb = ff->angles(); ffb != NULL; ffb = ffb->next)
	{
		ffp = ffb->params();
		item = new QTableWidgetItem(ffb->typeName(0));
		ui.FFEditorAnglesTable->setItem(count, AngleColumn::Type1, item);
		item = new QTableWidgetItem(ffb->typeName(1));
		ui.FFEditorAnglesTable->setItem(count, AngleColumn::Type2, item);
		item = new QTableWidgetItem(ffb->typeName(2));
		ui.FFEditorAnglesTable->setItem(count, AngleColumn::Type3, item);
		QComboBox *combo = new QComboBox(this);
		combo->setMinimumSize(78,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffb->functionalForm().angleFunc);
		ui.FFEditorAnglesTable->setCellWidget(count, AngleColumn::Form, combo);
		for (int n=0; n<6; n++)
		{
			item = new QTableWidgetItem(ftoa(ffp.data[n]));
			ui.FFEditorAnglesTable->setItem(count, AngleColumn::Data1+n, item);
		}
		count ++;
	}
	for (n=0; n<AngleColumn::nColumns; n++) ui.FFEditorAnglesTable->resizeColumnToContents(n);
	ui.FFEditorAnglesTable->setColumnWidth(AngleColumn::Form, 82);

	// Torsions List
	count = 0;
	ui.FFEditorTorsionsTable->setRowCount(ff->nTorsions());
	ui.FFEditorTorsionsTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Type 3" << "Type 4" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6");
	slist.clear();
	for (n=0; n<Forms::nTorsionFunctions; n++) slist << Forms::torsionFunction( (Forms::TorsionFunction) n);
	for (ForcefieldBound *ffb = ff->torsions(); ffb != NULL; ffb = ffb->next)
	{
		ffp = ffb->params();
		item = new QTableWidgetItem(ffb->typeName(0));
		ui.FFEditorTorsionsTable->setItem(count, TorsionColumn::Type1, item);
		item = new QTableWidgetItem(ffb->typeName(1));
		ui.FFEditorTorsionsTable->setItem(count, TorsionColumn::Type2, item);
		item = new QTableWidgetItem(ffb->typeName(2));
		ui.FFEditorTorsionsTable->setItem(count, TorsionColumn::Type3, item);
		item = new QTableWidgetItem(ffb->typeName(3));
		ui.FFEditorTorsionsTable->setItem(count, TorsionColumn::Type4, item);
		QComboBox *combo = new QComboBox(this);
		combo->setMinimumSize(78,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffb->functionalForm().torsionFunc);
		ui.FFEditorTorsionsTable->setCellWidget(count, TorsionColumn::Form, combo);
		for (int n=0; n<6; n++)
		{
			item = new QTableWidgetItem(ftoa(ffp.data[n]));
			ui.FFEditorTorsionsTable->setItem(count, TorsionColumn::Data1+n, item);
		}
		count ++;
	}
	for (n=0; n<TorsionColumn::nColumns; n++) ui.FFEditorTorsionsTable->resizeColumnToContents(n);
	ui.FFEditorTorsionsTable->setColumnWidth(TorsionColumn::Form, 82);
}

/*
// Atomtype Page
*/

// Test entered atom type
void AtenEdit::on_FFEditorTestTypeButton_clicked(bool on)
{
	ui.FFEditorTypeScoreLabel->setText("Score : ");
	ui.FFEditorNumMatchedLabel->setText("Atoms Matched : ");
}

// Generate type
void AtenEdit::on_FFEditorGenerateTypeButton_clicked(bool on)
{
}

// Item in type table edited
void AtenEdit::on_FFEditorTypesTable_itemChanged(QTableWidgetItem *w)
{
	// Get position of changed item
	int row = ui.FFEditorTypesTable->row(w);
	int column = ui.FFEditorTypesTable->column(w);
}

/*
// Atom Page
*/

// Item in type table edited
void AtenEdit::on_FFEditorAtomsTable_itemChanged(QTableWidgetItem *w)
{
	// Get position of changed item
	int row = ui.FFEditorAtomsTable->row(w);
	int column = ui.FFEditorAtomsTable->column(w);
}

/*
// Bonds Page
*/

// Item in bonds table edited
void AtenEdit::on_FFEditorBondsTable_itemChanged(QTableWidgetItem *w)
{
	// Get position of changed item
	int row = ui.FFEditorBondsTable->row(w);
	int column = ui.FFEditorBondsTable->column(w);
}

/*
// Angles Page
*/

// Item in angles table edited
void AtenEdit::on_FFEditorAnglesTable_itemChanged(QTableWidgetItem *w)
{
	// Get position of changed item
	int row = ui.FFEditorAnglesTable->row(w);
	int column = ui.FFEditorAnglesTable->column(w);
}

/*
// Torsions Page
*/

// Item in torsions table edited
void AtenEdit::on_FFEditorTorsionsTable_itemChanged(QTableWidgetItem *w)
{
	// Get position of changed item
	int row = ui.FFEditorTorsionsTable->row(w);
	int column = ui.FFEditorTorsionsTable->column(w);
}
