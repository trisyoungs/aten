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
#include <QtGui/QMessageBox>
#include "gui/tcombobox.h"

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
AtenForcefieldEditor::AtenForcefieldEditor(QWidget *parent) : QDialog(parent)
{
	ui.setupUi(this);
	// Private Variables
	updating_ = FALSE;
	targetForcefield_ = NULL;
}

// Finalise GUI
void AtenForcefieldEditor::finaliseUi()
{
	dbgBegin(Debug::Calls,"AtenForcefieldEditor::finaliseUi");
	dbgEnd(Debug::Calls,"AtenForcefieldEditor::finaliseUi");
}

// Set controls
void AtenForcefieldEditor::setControls()
{
	dbgBegin(Debug::Calls,"AtenForcefieldEditor::setControls");
	dbgEnd(Debug::Calls,"AtenForcefieldEditor::setControls");
}

// Populate widget with specified forcefield
void AtenForcefieldEditor::populate(Forcefield *ff)
{
	updating_ = TRUE;
	// Clear tables
	ui.FFEditorTypesTable->clear();
	ui.FFEditorAtomsTable->clear();
	ui.FFEditorBondsTable->clear();
	ui.FFEditorAnglesTable->clear();
	ui.FFEditorTorsionsTable->clear();
	targetForcefield_ = ff;
	if (ff == NULL)
	{
		updating_ = FALSE;
		return;
	}
	QTableWidgetItem *item;
	int count, n;
	ForcefieldParams ffp;
	QStringList slist;
	TComboBox *combo;

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
	for (n=0; n<VdwFunctions::nVdwFunctions; n++) slist << VdwFunctions::VdwFunctions[n].keyword;
	for (ForcefieldAtom *ffa = ff->types()->next; ffa != NULL; ffa = ffa->next)
	{
		ffp = ffa->params();
		item = new QTableWidgetItem(itoa(ffa->typeId()));
		ui.FFEditorAtomsTable->setItem(count, AtomColumn::Id, item);
		item = new QTableWidgetItem(ffa->name());
		ui.FFEditorAtomsTable->setItem(count, AtomColumn::Name, item);
		item = new QTableWidgetItem(ftoa(ffa->charge()));
		ui.FFEditorAtomsTable->setItem(count, AtomColumn::Charge, item);
		combo = new TComboBox(this);
		combo->setMinimumSize(64,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffa->vdwForm());
		combo->setPointer(ffa);
		ui.FFEditorAtomsTable->setCellWidget(count,AtomColumn::Form,combo);
		QObject::connect(combo, SIGNAL(activated(int)), this, SLOT(VdwFunctionChanged(int)));
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
	ui.FFEditorBondsTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Form" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6");
	slist.clear();
	for (n=0; n<BondFunctions::nBondFunctions; n++) slist << BondFunctions::BondFunctions[n].keyword;
	for (ForcefieldBound *ffb = ff->bonds(); ffb != NULL; ffb = ffb->next)
	{
		ffp = ffb->params();
		item = new QTableWidgetItem(ffb->typeName(0));
		ui.FFEditorBondsTable->setItem(count, BondColumn::Type1, item);
		item = new QTableWidgetItem(ffb->typeName(1));
		ui.FFEditorBondsTable->setItem(count, BondColumn::Type2, item);
		combo = new TComboBox(this);
		combo->setMinimumSize(78,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffb->bondStyle());
		combo->setPointer(ffb);
		ui.FFEditorBondsTable->setCellWidget(count, BondColumn::Form, combo);
		QObject::connect(combo, SIGNAL(activated(int)), this, SLOT(BondFunctionChanged(int)));
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
	ui.FFEditorAnglesTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Type 3" << "Form" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6");
	slist.clear();
	for (n=0; n<AngleFunctions::nAngleFunctions; n++) slist << AngleFunctions::AngleFunctions[n].keyword;
	for (ForcefieldBound *ffb = ff->angles(); ffb != NULL; ffb = ffb->next)
	{
		ffp = ffb->params();
		item = new QTableWidgetItem(ffb->typeName(0));
		ui.FFEditorAnglesTable->setItem(count, AngleColumn::Type1, item);
		item = new QTableWidgetItem(ffb->typeName(1));
		ui.FFEditorAnglesTable->setItem(count, AngleColumn::Type2, item);
		item = new QTableWidgetItem(ffb->typeName(2));
		ui.FFEditorAnglesTable->setItem(count, AngleColumn::Type3, item);
		combo = new TComboBox(this);
		combo->setMinimumSize(78,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffb->angleStyle());
		combo->setPointer(ffb);
		ui.FFEditorAnglesTable->setCellWidget(count, AngleColumn::Form, combo);
		QObject::connect(combo, SIGNAL(activated(int)), this, SLOT(AngleFunctionChanged(int)));
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
	ui.FFEditorTorsionsTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Type 3" << "Type 4" << "Form" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6");
	slist.clear();
	for (n=0; n<TorsionFunctions::nTorsionFunctions; n++) slist << TorsionFunctions::TorsionFunctions[n].keyword;
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
		combo = new TComboBox(this);
		combo->setMinimumSize(78,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffb->torsionStyle());
		combo->setPointer(ffb);
		ui.FFEditorTorsionsTable->setCellWidget(count, TorsionColumn::Form, combo);
		QObject::connect(combo, SIGNAL(activated(int)), this, SLOT(TorsionFunctionChanged(int)));
		for (int n=0; n<6; n++)
		{
			item = new QTableWidgetItem(ftoa(ffp.data[n]));
			ui.FFEditorTorsionsTable->setItem(count, TorsionColumn::Data1+n, item);
		}
		count ++;
	}
	for (n=0; n<TorsionColumn::nColumns; n++) ui.FFEditorTorsionsTable->resizeColumnToContents(n);
	ui.FFEditorTorsionsTable->setColumnWidth(TorsionColumn::Form, 82);
	// Done
	updating_ = FALSE;
}

/*
// Atomtype Page
*/

// Test entered atom type
void AtenForcefieldEditor::on_FFEditorTestTypeButton_clicked(bool on)
{
	ui.FFEditorTypeScoreLabel->setText("Score : ");
	ui.FFEditorNumMatchedLabel->setText("Atoms Matched : ");
}

// Generate type
void AtenForcefieldEditor::on_FFEditorGenerateTypeButton_clicked(bool on)
{
}

// Item in type table edited
void AtenForcefieldEditor::on_FFEditorTypesTable_itemChanged(QTableWidgetItem *w)
{
	// Get position of changed item
	int row = ui.FFEditorTypesTable->row(w);
	int column = ui.FFEditorTypesTable->column(w);
}

/*
// Atom Page
*/

// Vdw interaction type changed
void AtenForcefieldEditor::VdwFunctionChanged(int index)
{
	// Cast sender
	TComboBox *combo = (TComboBox*) sender();
	if (!combo)
	{
		printf("AtenForcefieldEditor::VdwFunctionChanged - Sender could not be cast to a TComboBox.\n");
		return;
	}
	// Get ForcefieldAtom pointer and set data
	ForcefieldAtom *ffa = (ForcefieldAtom*) combo->pointer();
	ffa->setVdwForm( (VdwFunctions::VdwFunction) index);
}

// Item in type table edited
void AtenForcefieldEditor::on_FFEditorAtomsTable_itemChanged(QTableWidgetItem *w)
{
	if ((targetForcefield_ == NULL) || updating_) return;
	updating_ = TRUE;
	// Get position of changed item (skipping _NDEF_)
	int row = ui.FFEditorAtomsTable->row(w) + 1;
	int column = ui.FFEditorAtomsTable->column(w);
	// Get pointer to forcefield type from edited row
	ForcefieldAtom *ffa = targetForcefield_->type(row);
	// Set new data based on the column edited
	char text[512];
	ForcefieldAtom *old;
	int n, returnvalue;
	QComboBox *combo;
	switch (column)
	{
		// Forcefield TypeId
		case (AtomColumn::Id):
			// Must search FF to see if another type with this id already exists
			n = atoi(qPrintable(w->text()));
			old = targetForcefield_->findByTypeId(n, ffa);
			if (old != NULL)
			{
				sprintf(text, "Another type with id %i already exists (%s).\n", n, old->name());
				returnvalue = QMessageBox::warning(this, "Forcefield Editor", text, QMessageBox::Ok);
				// Set the table value item back to the old value
				w->setText(itoa(ffa->typeId()));
			}
			else ffa->setTypeId(n);
			break;
		// Type name
		case (AtomColumn::Name):
			ffa->setName(qPrintable(w->text()));
			break;
		// Atomic charge
		case (AtomColumn::Charge):
			ffa->setCharge(atof(qPrintable(w->text())));
			break;
		// VDW form
		case (AtomColumn::Form):
			combo = (QComboBox*) ui.FFEditorAtomsTable->cellWidget(row-1, column);
			n = combo->currentIndex();
			ffa->setVdwForm( (VdwFunctions::VdwFunction) n);
			break;
		// VDW data
		case (AtomColumn::Data1):
		case (AtomColumn::Data2):
		case (AtomColumn::Data3):
		case (AtomColumn::Data4):
		case (AtomColumn::Data5):
		case (AtomColumn::Data6):
			n = column - AtomColumn::Data1;
			ffa->params().data[n] = atof(qPrintable(w->text()));
			break;
	}
	updating_ = FALSE;
}

/*
// Bonds Page
*/

// Bond interaction type changed
void AtenForcefieldEditor::BondFunctionChanged(int index)
{
	// Cast sender
	TComboBox *combo = (TComboBox*) sender();
	if (!combo)
	{
		printf("AtenForcefieldEditor::BondFunctionChanged - Sender could not be cast to a TComboBox.\n");
		return;
	}
	// Get ForcefieldBound pointer and set data
	ForcefieldBound *ffb = (ForcefieldBound*) combo->pointer();
	ffb->setBondStyle( (BondFunctions::BondFunction) index);
}

// Item in bonds table edited
void AtenForcefieldEditor::on_FFEditorBondsTable_itemChanged(QTableWidgetItem *w)
{
	if ((targetForcefield_ == NULL) || updating_) return;
	updating_ = TRUE;
	// Get position of changed item
	int row = ui.FFEditorBondsTable->row(w);
	int column = ui.FFEditorBondsTable->column(w);
	// Get pointer to forcefield bound from edited row
	ForcefieldBound *ffb = targetForcefield_->bond(row);
	// Set new data based on the column edited
	int n;
	switch (column)
	{
		// Types involved in interaction
		case (BondColumn::Type1):
		case (BondColumn::Type2):
			n = column - BondColumn::Type1;
			ffb->setTypeName(n, qPrintable(w->text()));
			break;
		// Potential form
		case (BondColumn::Form):
			// Handled by AtenForcefieldEditor::BondsTableComboChanged
			break;
		// Parameter data
		case (BondColumn::Data1):
		case (BondColumn::Data2):
		case (BondColumn::Data3):
		case (BondColumn::Data4):
		case (BondColumn::Data5):
		case (BondColumn::Data6):
			n = column - BondColumn::Data1;
			ffb->params().data[n] = atof(qPrintable(w->text()));
			break;
	}
	updating_ = FALSE;
}

/*
// Angles Page
*/

// Angle interaction type changed
void AtenForcefieldEditor::AngleFunctionChanged(int index)
{
	// Cast sender
	TComboBox *combo = (TComboBox*) sender();
	if (!combo)
	{
		printf("AtenForcefieldEditor::AngleFunctionChanged - Sender could not be cast to a TComboBox.\n");
		return;
	}
	// Get ForcefieldBound pointer and set data
	ForcefieldBound *ffb = (ForcefieldBound*) combo->pointer();
	ffb->setAngleStyle( (AngleFunctions::AngleFunction) index);
}

// Item in angles table edited
void AtenForcefieldEditor::on_FFEditorAnglesTable_itemChanged(QTableWidgetItem *w)
{
	if ((targetForcefield_ == NULL) || updating_) return;
	updating_ = TRUE;
	// Get position of changed item
	int row = ui.FFEditorAnglesTable->row(w);
	int column = ui.FFEditorAnglesTable->column(w);
	// Get pointer to forcefield bound from edited row
	ForcefieldBound *ffb = targetForcefield_->angle(row);
	// Set new data based on the column edited
	int n;
	switch (column)
	{
		// Types involved in interaction
		case (AngleColumn::Type1):
		case (AngleColumn::Type2):
		case (AngleColumn::Type3):
			n = column - AngleColumn::Type1;
			ffb->setTypeName(n, qPrintable(w->text()));
			break;
		// Potential form
		case (AngleColumn::Form):
			// Handled by AtenForcefieldEditor::AnglesTableComboChanged
			break;
		// Parameter data
		case (AngleColumn::Data1):
		case (AngleColumn::Data2):
		case (AngleColumn::Data3):
		case (AngleColumn::Data4):
		case (AngleColumn::Data5):
		case (AngleColumn::Data6):
			n = column - AngleColumn::Data1;
			ffb->params().data[n] = atof(qPrintable(w->text()));
			break;
	}
	updating_ = FALSE;
}

/*
// Torsions Page
*/

// Torsion interaction type changed
void AtenForcefieldEditor::TorsionFunctionChanged(int index)
{
	// Cast sender
	TComboBox *combo = (TComboBox*) sender();
	if (!combo)
	{
		printf("AtenForcefieldEditor::TorsionFunctionChanged - Sender could not be cast to a TComboBox.\n");
		return;
	}
	// Get ForcefieldBound pointer and set data
	ForcefieldBound *ffb = (ForcefieldBound*) combo->pointer();
	ffb->setTorsionStyle( (TorsionFunctions::TorsionFunction) index);
}

// Item in torsions table edited
void AtenForcefieldEditor::on_FFEditorTorsionsTable_itemChanged(QTableWidgetItem *w)
{
	if ((targetForcefield_ == NULL) || updating_) return;
	updating_ = TRUE;
	// Get position of changed item
	int row = ui.FFEditorTorsionsTable->row(w);
	int column = ui.FFEditorTorsionsTable->column(w);
	// Get pointer to forcefield bound from edited row
	ForcefieldBound *ffb = targetForcefield_->torsion(row);
	// Set new data based on the column edited
	int n;
	switch (column)
	{
		// Types involved in interaction
		case (TorsionColumn::Type1):
		case (TorsionColumn::Type2):
		case (TorsionColumn::Type3):
		case (TorsionColumn::Type4):
			n = column - TorsionColumn::Type1;
			ffb->setTypeName(n, qPrintable(w->text()));
			break;
		// Potential form
		case (TorsionColumn::Form):
			// Handled by AtenForcefieldEditor::TorsionsTableComboChanged
			break;
		// Parameter data
		case (TorsionColumn::Data1):
		case (TorsionColumn::Data2):
		case (TorsionColumn::Data3):
		case (TorsionColumn::Data4):
		case (TorsionColumn::Data5):
		case (TorsionColumn::Data6):
			n = column - TorsionColumn::Data1;
			ffb->params().data[n] = atof(qPrintable(w->text()));
			break;
	}
	updating_ = FALSE;
}
