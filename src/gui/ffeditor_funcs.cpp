/*
	*** Qt forcefield editor window functions
	*** src/gui/ffeditor_funcs.cpp
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

#include <QtGui/QMessageBox>
#include "base/sysfunc.h"
#include "gui/ffeditor.h"
#include "gui/tcombobox.h"
#include "gui/tdoublespindelegate.uih"
#include "gui/tintegerspindelegate.uih"
#include "ff/forcefield.h"
#include "model/model.h"
#include "main/aten.h"
#include "base/forcefieldatom.h"
#include "base/forcefieldbound.h"

ATEN_USING_NAMESPACE

// Local enum variables
namespace TypeColumn
{
	enum TypeColumn { Id, Element, Name, Equivalent, NETAString, Description, nColumns };
}
namespace AtomColumn
{
	enum AtomColumn { Id, Name, Charge, Form, Data1, Data2, Data3, Data4, Data5, Data6, Data7, Data8, Data9, Data10, nColumns };
}
namespace BondColumn
{
	enum BondColumn { Type1, Type2, Form, Data1, Data2, Data3, Data4, Data5, Data6, Data7, Data8, Data9, Data10, nColumns };
}
namespace AngleColumn
{
	enum AngleColumn { Type1, Type2, Type3, Form, Data1, Data2, Data3, Data4, Data5, Data6, Data7, Data8, Data9, Data10, nColumns };
}
namespace TorsionColumn
{
	enum TorsionColumn { Type1, Type2, Type3, Type4, Form, ElecScale, VdwScale, Data1, Data2, Data3, Data4, Data5, Data6, Data7, Data8, Data9, Data10, nColumns };
}

// Constructor
AtenForcefieldEditor::AtenForcefieldEditor(QWidget *parent) : QDialog(parent)
{
	ui.setupUi(this);

	// Private Variables
	updating_ = FALSE;
	targetForcefield_ = NULL;
	
	// Set item delegates for columns
	int n;
	ui.FFEditorTypesTable->setItemDelegateForColumn(TypeColumn::Id, new TIntegerSpinDelegate(this, 1, Elements().nElements(), 1));
	ui.FFEditorAtomsTable->setItemDelegateForColumn(AtomColumn::Charge, new TDoubleSpinDelegate(this));
	for (n=AtomColumn::Data1; n<=AtomColumn::Data10; ++n) ui.FFEditorAtomsTable->setItemDelegateForColumn(n, new TDoubleSpinDelegate(this));
	for (n=BondColumn::Data1; n<=BondColumn::Data10; ++n) ui.FFEditorBondsTable->setItemDelegateForColumn(n, new TDoubleSpinDelegate(this));
	for (n=AngleColumn::Data1; n<=AngleColumn::Data10; ++n) ui.FFEditorAnglesTable->setItemDelegateForColumn(n, new TDoubleSpinDelegate(this));
	for (n=TorsionColumn::Data1; n<=TorsionColumn::Data10; ++n) ui.FFEditorTorsionsTable->setItemDelegateForColumn(n, new TDoubleSpinDelegate(this));
	ui.FFEditorTorsionsTable->setItemDelegateForColumn(TorsionColumn::ElecScale, new TDoubleSpinDelegate(this));
	ui.FFEditorTorsionsTable->setItemDelegateForColumn(TorsionColumn::VdwScale, new TDoubleSpinDelegate(this));
	for (n=TorsionColumn::Data1; n<=TorsionColumn::Data10; ++n) ui.FFEditorImpropersTable->setItemDelegateForColumn(n, new TDoubleSpinDelegate(this));
	ui.FFEditorImpropersTable->setItemDelegateForColumn(TorsionColumn::ElecScale, new TDoubleSpinDelegate(this));
	ui.FFEditorImpropersTable->setItemDelegateForColumn(TorsionColumn::VdwScale, new TDoubleSpinDelegate(this));
	for (n=BondColumn::Data1; n<=BondColumn::Data10; ++n) ui.FFEditorUreyBradleysTable->setItemDelegateForColumn(n, new TDoubleSpinDelegate(this));
}

// Populate widget with specified forcefield
void AtenForcefieldEditor::populate(Forcefield* ff)
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
	double* params;
	QStringList slist;
	TComboBox *combo;

	// Main title
	ui.FFEditorForcefieldLabel1->setText(ff->name());
	ui.FFEditorForcefieldLabel2->setText(ff->filename());

	// Types List
	count = 0;
	ui.FFEditorTypesTable->setRowCount(ff->nTypes()-1);
	ui.FFEditorTypesTable->setHorizontalHeaderLabels(QStringList() << "TypeID" << "El" << "Name" << "Equiv" << "NETA" << "Description");
	for (ForcefieldAtom* ffa = ff->types()->next; ffa != NULL; ffa = ffa->next)
	{
		item = new QTableWidgetItem(QString::number(ffa->typeId()));
		ui.FFEditorTypesTable->setItem(count, TypeColumn::Id, item);
		item = new QTableWidgetItem(Elements().symbol(ffa->neta()->characterElement()));
		ui.FFEditorTypesTable->setItem(count, TypeColumn::Element, item);
		item = new QTableWidgetItem(ffa->name());
		ui.FFEditorTypesTable->setItem(count, TypeColumn::Name, item);
		item = new QTableWidgetItem(ffa->equivalent());
		ui.FFEditorTypesTable->setItem(count, TypeColumn::Equivalent, item);
		item = new QTableWidgetItem(ffa->netaString());
		ui.FFEditorTypesTable->setItem(count, TypeColumn::NETAString, item);
		item = new QTableWidgetItem(ffa->description());
		ui.FFEditorTypesTable->setItem(count, TypeColumn::Description, item);
		count ++;
	}
	for (n=0; n<TypeColumn::nColumns; n++) ui.FFEditorTypesTable->resizeColumnToContents(n);

	// Atoms List
	count = 0;
	ui.FFEditorAtomsTable->setRowCount(ff->nTypes()-1);
	ui.FFEditorAtomsTable->setHorizontalHeaderLabels(QStringList() << "TypeID" << "Name" << "Charge" << "Form" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6" << "Data 7" << "Data 8" << "Data 9" << "Data 10" );
	slist.clear();
	for (n=0; n<VdwFunctions::nVdwFunctions; n++) slist << VdwFunctions::VdwFunctions[n].keyword;
	for (ForcefieldAtom* ffa = ff->types()->next; ffa != NULL; ffa = ffa->next)
	{
		params = ffa->parameters();
		item = new QTableWidgetItem(QString::number(ffa->typeId()));
		item->setFlags(Qt::ItemIsSelectable);
		ui.FFEditorAtomsTable->setItem(count, AtomColumn::Id, item);
		item = new QTableWidgetItem(ffa->name());
		item->setFlags(Qt::ItemIsSelectable);
		ui.FFEditorAtomsTable->setItem(count, AtomColumn::Name, item);
		item = new QTableWidgetItem(QString::number(ffa->charge()));
		ui.FFEditorAtomsTable->setItem(count, AtomColumn::Charge, item);
		combo = new TComboBox(this);
		combo->setMinimumSize(64,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffa->vdwForm());
		combo->data.set(VTypes::ForcefieldAtomData, ffa);
		ui.FFEditorAtomsTable->setCellWidget(count,AtomColumn::Form,combo);
		QObject::connect(combo, SIGNAL(activated(int)), this, SLOT(VdwFunctionChanged(int)));
		for (int n=0; n<MAXFFPARAMDATA; n++)
		{
			item = new QTableWidgetItem(QString::number(params[n]));
			ui.FFEditorAtomsTable->setItem(count, AtomColumn::Data1+n, item);
		}
		count ++;
	}
	for (n=0; n<AtomColumn::nColumns; n++) ui.FFEditorAtomsTable->resizeColumnToContents(n);
	ui.FFEditorAtomsTable->setColumnWidth(AtomColumn::Form, 68);
	ui.FFEditorAtomsTable->setSelectionMode(QAbstractItemView::SingleSelection);
	
	// Bonds List
	count = 0;
	ui.FFEditorBondsTable->setRowCount(ff->nBonds());
	ui.FFEditorBondsTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Form" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6" << "Data 7" << "Data 8" << "Data 9" << "Data 10");
	slist.clear();
	for (n=0; n<BondFunctions::nBondFunctions; n++) slist << BondFunctions::BondFunctions[n].keyword;
	for (ForcefieldBound* ffb = ff->bonds(); ffb != NULL; ffb = ffb->next)
	{
		params = ffb->parameters();
		item = new QTableWidgetItem(ffb->typeName(0));
		ui.FFEditorBondsTable->setItem(count, BondColumn::Type1, item);
		item = new QTableWidgetItem(ffb->typeName(1));
		ui.FFEditorBondsTable->setItem(count, BondColumn::Type2, item);
		combo = new TComboBox(this);
		combo->setMinimumSize(78,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffb->bondForm());
		combo->data.set(VTypes::ForcefieldBoundData, ffb);
		ui.FFEditorBondsTable->setCellWidget(count, BondColumn::Form, combo);
		QObject::connect(combo, SIGNAL(activated(int)), this, SLOT(BondFunctionChanged(int)));
		for (int n=0; n<MAXFFPARAMDATA; n++)
		{
			item = new QTableWidgetItem(QString::number(params[n]));
			ui.FFEditorBondsTable->setItem(count, BondColumn::Data1+n, item);
		}
		count ++;
	}
	for (n=0; n<BondColumn::nColumns; n++) ui.FFEditorBondsTable->resizeColumnToContents(n);
	ui.FFEditorBondsTable->setColumnWidth(BondColumn::Form, 82);
	ui.FFEditorBondsTable->setSelectionMode(QAbstractItemView::SingleSelection);

	// Angles List
	count = 0;
	ui.FFEditorAnglesTable->setRowCount(ff->nAngles());
	ui.FFEditorAnglesTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Type 3" << "Form" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6" << "Data 7" << "Data 8" << "Data 9" << "Data 10");
	slist.clear();
	for (n=0; n<AngleFunctions::nAngleFunctions; n++) slist << AngleFunctions::AngleFunctions[n].keyword;
	for (ForcefieldBound* ffb = ff->angles(); ffb != NULL; ffb = ffb->next)
	{
		params = ffb->parameters();
		item = new QTableWidgetItem(ffb->typeName(0));
		ui.FFEditorAnglesTable->setItem(count, AngleColumn::Type1, item);
		item = new QTableWidgetItem(ffb->typeName(1));
		ui.FFEditorAnglesTable->setItem(count, AngleColumn::Type2, item);
		item = new QTableWidgetItem(ffb->typeName(2));
		ui.FFEditorAnglesTable->setItem(count, AngleColumn::Type3, item);
		combo = new TComboBox(this);
		combo->setMinimumSize(78,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffb->angleForm());
		combo->data.set(VTypes::ForcefieldBoundData, ffb);
		ui.FFEditorAnglesTable->setCellWidget(count, AngleColumn::Form, combo);
		QObject::connect(combo, SIGNAL(activated(int)), this, SLOT(AngleFunctionChanged(int)));
		for (int n=0; n<MAXFFPARAMDATA; n++)
		{
			item = new QTableWidgetItem(QString::number(params[n]));
			ui.FFEditorAnglesTable->setItem(count, AngleColumn::Data1+n, item);
		}
		count ++;
	}
	for (n=0; n<AngleColumn::nColumns; n++) ui.FFEditorAnglesTable->resizeColumnToContents(n);
	ui.FFEditorAnglesTable->setColumnWidth(AngleColumn::Form, 82);
	ui.FFEditorAnglesTable->setSelectionMode(QAbstractItemView::SingleSelection);
	
	// Torsions List
	count = 0;
	ui.FFEditorTorsionsTable->setRowCount(ff->nTorsions());
	ui.FFEditorTorsionsTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Type 3" << "Type 4" << "Form" << "EScale" << "VScale" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6" << "Data 7" << "Data 8" << "Data 9" << "Data 10");
	slist.clear();
	for (n=0; n<TorsionFunctions::nTorsionFunctions; n++) slist << TorsionFunctions::TorsionFunctions[n].keyword;
	for (ForcefieldBound* ffb = ff->torsions(); ffb != NULL; ffb = ffb->next)
	{
		params = ffb->parameters();
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
		combo->setCurrentIndex(ffb->torsionForm());
		combo->data.set(VTypes::ForcefieldBoundData, ffb);
		ui.FFEditorTorsionsTable->setCellWidget(count, TorsionColumn::Form, combo);
		QObject::connect(combo, SIGNAL(activated(int)), this, SLOT(TorsionFunctionChanged(int)));
		item = new QTableWidgetItem(ffb->elecScale());
		ui.FFEditorTorsionsTable->setItem(count, TorsionColumn::ElecScale, item);
		item = new QTableWidgetItem(ffb->vdwScale());
		ui.FFEditorTorsionsTable->setItem(count, TorsionColumn::VdwScale, item);
		for (int n=0; n<MAXFFPARAMDATA; n++)
		{
			item = new QTableWidgetItem(QString::number(params[n]));
			ui.FFEditorTorsionsTable->setItem(count, TorsionColumn::Data1+n, item);
		}
		count ++;
	}
	for (n=0; n<TorsionColumn::nColumns; n++) ui.FFEditorTorsionsTable->resizeColumnToContents(n);
	ui.FFEditorTorsionsTable->setColumnWidth(TorsionColumn::Form, 82);
	ui.FFEditorTorsionsTable->setSelectionMode(QAbstractItemView::SingleSelection);
	
	// Impropers List
	count = 0;
	ui.FFEditorImpropersTable->setRowCount(ff->nImpropers());
	ui.FFEditorImpropersTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Type 3" << "Type 4" << "Form" << "EScale" << "VScale" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6" << "Data 7" << "Data 8" << "Data 9" << "Data 10");
	slist.clear();
	for (n=0; n<TorsionFunctions::nTorsionFunctions; n++) slist << TorsionFunctions::TorsionFunctions[n].keyword;
	for (ForcefieldBound* ffb = ff->impropers(); ffb != NULL; ffb = ffb->next)
	{
		params = ffb->parameters();
		item = new QTableWidgetItem(ffb->typeName(0));
		ui.FFEditorImpropersTable->setItem(count, TorsionColumn::Type1, item);
		item = new QTableWidgetItem(ffb->typeName(1));
		ui.FFEditorImpropersTable->setItem(count, TorsionColumn::Type2, item);
		item = new QTableWidgetItem(ffb->typeName(2));
		ui.FFEditorImpropersTable->setItem(count, TorsionColumn::Type3, item);
		item = new QTableWidgetItem(ffb->typeName(3));
		ui.FFEditorImpropersTable->setItem(count, TorsionColumn::Type4, item);
		combo = new TComboBox(this);
		combo->setMinimumSize(78,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffb->torsionForm());
		combo->data.set(VTypes::ForcefieldBoundData, ffb);
		ui.FFEditorImpropersTable->setCellWidget(count, TorsionColumn::Form, combo);
		QObject::connect(combo, SIGNAL(activated(int)), this, SLOT(TorsionFunctionChanged(int)));
		item = new QTableWidgetItem(ffb->elecScale());
		ui.FFEditorImpropersTable->setItem(count, TorsionColumn::ElecScale, item);
		item = new QTableWidgetItem(ffb->vdwScale());
		ui.FFEditorImpropersTable->setItem(count, TorsionColumn::VdwScale, item);
		for (int n=0; n<MAXFFPARAMDATA; n++)
		{
			item = new QTableWidgetItem(QString::number(params[n]));
			ui.FFEditorImpropersTable->setItem(count, TorsionColumn::Data1+n, item);
		}
		count ++;
	}
	for (n=0; n<TorsionColumn::nColumns; n++) ui.FFEditorImpropersTable->resizeColumnToContents(n);
	ui.FFEditorImpropersTable->setColumnWidth(TorsionColumn::Form, 82);
	ui.FFEditorImpropersTable->setSelectionMode(QAbstractItemView::SingleSelection);
	
	// UreyBradleys List
	count = 0;
	ui.FFEditorUreyBradleysTable->setRowCount(ff->nUreyBradleys());
	ui.FFEditorUreyBradleysTable->setHorizontalHeaderLabels(QStringList() << "Type 1" << "Type 2" << "Type 3" << "Form" << "Data 1" << "Data 2" << "Data 3" << "Data 4" << "Data 5" << "Data 6" << "Data 7" << "Data 8" << "Data 9" << "Data 10");
	slist.clear();
	for (n=0; n<BondFunctions::nBondFunctions; n++) slist << BondFunctions::BondFunctions[n].keyword;
	for (ForcefieldBound* ffb = ff->ureyBradleys(); ffb != NULL; ffb = ffb->next)
	{
		params = ffb->parameters();
		item = new QTableWidgetItem(ffb->typeName(0));
		ui.FFEditorUreyBradleysTable->setItem(count, AngleColumn::Type1, item);
		item = new QTableWidgetItem(ffb->typeName(1));
		ui.FFEditorUreyBradleysTable->setItem(count, AngleColumn::Type2, item);
		item = new QTableWidgetItem(ffb->typeName(2));
		ui.FFEditorUreyBradleysTable->setItem(count, AngleColumn::Type3, item);
		combo = new TComboBox(this);
		combo->setMinimumSize(78,24);
		combo->addItems(slist);
		combo->setCurrentIndex(ffb->bondForm());
		combo->data.set(VTypes::ForcefieldBoundData, ffb);
		ui.FFEditorUreyBradleysTable->setCellWidget(count, AngleColumn::Form, combo);
		QObject::connect(combo, SIGNAL(activated(int)), this, SLOT(BondFunctionChanged(int)));
		for (int n=0; n<MAXFFPARAMDATA; n++)
		{
			item = new QTableWidgetItem(QString::number(params[n]));
			ui.FFEditorUreyBradleysTable->setItem(count, AngleColumn::Data1+n, item);
		}
		count ++;
	}
	for (n=0; n<AngleColumn::nColumns; n++) ui.FFEditorUreyBradleysTable->resizeColumnToContents(n);
	ui.FFEditorUreyBradleysTable->setColumnWidth(AngleColumn::Form, 82);
	ui.FFEditorUreyBradleysTable->setSelectionMode(QAbstractItemView::SingleSelection);
	
	// Done
	updating_ = FALSE;
}

void AtenForcefieldEditor::boundFunctionChanged(TComboBox *sender, int i, ForcefieldBound::BoundType bt)
{
	// Check sender
	if (!sender)
	{
		printf("AtenForcefieldEditor::boundFunctionChanged - Sender could not be cast to a TComboBox.\n");
		return;
	}

	// Get ForcefieldBound pointer and set data
	ForcefieldBound* ffb = (ForcefieldBound*) sender->data.asPointer(VTypes::ForcefieldBoundData);
	switch (bt)
	{
		case (ForcefieldBound::BondInteraction):
			ffb->setBondForm( (BondFunctions::BondFunction) i);
			updateBondsLabels(ffb);
			break;
		case (ForcefieldBound::AngleInteraction):
			ffb->setAngleForm( (AngleFunctions::AngleFunction) i);
			if (ffb->type() == ForcefieldBound::UreyBradleyInteraction) updateUreyBradleysLabels(ffb);
			else updateAnglesLabels(ffb);
			break;
		case (ForcefieldBound::TorsionInteraction):
			ffb->setTorsionForm( (TorsionFunctions::TorsionFunction) i);
			if (ffb->type() == ForcefieldBound::UreyBradleyInteraction) updateImpropersLabels(ffb);
			else updateTorsionsLabels(ffb);
			break;
	}
}

/*
// Types Page
*/

// Test entered atom type
void AtenForcefieldEditor::on_FFEditorTestTypeButton_clicked(bool on)
{
// 	Model* m = parent_.aten().currentModelOrFrame();   ATEN2 TODO
// 	if (m == NULL) return;
// 	// Get position of changed item (skipping _NDEF_)
// 	int row = ui.FFEditorTypesTable->currentRow();
// 	if (row == -1) return;
// 	// Get pointer to forcefield type from edited row (skipping _NDEF_)
// 	ForcefieldAtom* ffa = targetForcefield_->type(row+1);
// 	m->selectNone(TRUE);
// 	m->selectType(ffa->element(), ffa->netaString(), TRUE, FALSE);
// 	Messenger::print("Type description matched %i atoms in current model.", m->nMarked());
}

// Item in type table edited
void AtenForcefieldEditor::on_FFEditorTypesTable_itemChanged(QTableWidgetItem *w)
{
	if ((targetForcefield_ == NULL) || updating_) return;
	updating_ = TRUE;

	// Get position of changed item (skipping _NDEF_)
	int row = ui.FFEditorTypesTable->row(w) + 1;
	int column = ui.FFEditorTypesTable->column(w);

	// Get pointer to forcefield type from edited row
	ForcefieldAtom* ffa = targetForcefield_->type(row);

	// Set new data based on the column edited
	QString text;
	ForcefieldAtom* old;
	int n, returnvalue;
	switch (column)
	{
		// Forcefield TypeId
		case (TypeColumn::Id):
			// Must search FF to see if another type with this id already exists
			n = atoi(qPrintable(w->text()));
			old = targetForcefield_->findByTypeId(n, ffa);
			if (old != NULL)
			{
				text.sprintf("Another type with id %i already exists (%s).", n, qPrintable(old->name()));
				returnvalue = QMessageBox::warning(this, "Forcefield Editor", qPrintable(text), QMessageBox::Ok);

				// Set the table value item back to the old value
				w->setText(QString::number(ffa->typeId()));
			}
			else ffa->setTypeId(n);
			break;
		// Character element
		case (TypeColumn::Element):
// 			ffa->setName(qPrintable(w->text()));
			break;
		// Type name
		case (TypeColumn::Name):
			ffa->setName(qPrintable(w->text()));
			break;
		// Equivalent name
		case (TypeColumn::Equivalent):
			ffa->setEquivalent(qPrintable(w->text()));
			break;
		// NETA description
		case (TypeColumn::NETAString):
			// 			ffa->setName(qPrintable(w->text()));   //TODO
			break;
		// Type (text) description
		case (TypeColumn::Description):
			ffa->setDescription(qPrintable(w->text()));
			break;
	}
	updating_ = FALSE;
}

/*
// Atom Page
*/

void AtenForcefieldEditor::updateVdwLabels(ForcefieldAtom* ffa)
{
	if (ffa == NULL)
	{
		ui.FFEditorAtomFormLabel->setText("None");
		ui.FFEditorAtomParametersLabel->setText("None");
		return;
	}
	VdwFunctions::VdwFunction vf = ffa->vdwForm();

	// Construct labels
	QString text;
	text.sprintf("%s (%s)", VdwFunctions::VdwFunctions[vf].name, VdwFunctions::VdwFunctions[vf].keyword);
	ui.FFEditorAtomFormLabel->setText(text);
	text.clear();
	for (int n=0; n<VdwFunctions::VdwFunctions[vf].nParameters; ++n)
	{
		if (n != 0) text += ", ";
		if (VdwFunctions::VdwFunctions[vf].isEnergyParameter[n]) text += VdwFunctions::VdwFunctions[vf].parameterKeywords[n];
		else text += VdwFunctions::VdwFunctions[vf].parameterKeywords[n];
	}
	ui.FFEditorAtomParametersLabel->setText(text);
}

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
	ForcefieldAtom* ffa = (ForcefieldAtom*) combo->data.asPointer(VTypes::ForcefieldAtomData);
	ffa->setVdwForm( (VdwFunctions::VdwFunction) index);
	updateVdwLabels(ffa);
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
	ForcefieldAtom* ffa = targetForcefield_->type(row);
	// Set new data based on the column edited
	int n;
	QComboBox* combo;
	switch (column)
	{
		// Forcefield TypeId
		case (AtomColumn::Id):
			// SHhuld never be called since these items have been set read-only
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
			ffa->setParameter(n, atof(qPrintable(w->text())));
			break;
	}
	updating_ = FALSE;
}

void AtenForcefieldEditor::on_FFEditorAtomsTable_itemSelectionChanged()
{
	int row = ui.FFEditorAtomsTable->currentRow();
	if (row == -1)
	{
		updateVdwLabels(NULL);
		return;
	}
	ForcefieldAtom* ffa = targetForcefield_->type(row+1);
	updateVdwLabels(ffa);
}

/*
// Bonds Page
*/

void AtenForcefieldEditor::updateBondsLabels(ForcefieldBound* ffb)
{
	if (ffb == NULL)
	{
		ui.FFEditorBondFormLabel->setText("None");
		ui.FFEditorBondParametersLabel->setText("None");
		return;
	}
	BondFunctions::BondFunction bf = ffb->bondForm();
	// Construct labels
	QString text;
	text.sprintf("%s (%s)", BondFunctions::BondFunctions[bf].name, BondFunctions::BondFunctions[bf].keyword);
	ui.FFEditorBondFormLabel->setText(text);
	text.clear();
	for (int n=0; n<BondFunctions::BondFunctions[bf].nParameters; ++n)
	{
		if (n != 0) text += ", ";
		if (BondFunctions::BondFunctions[bf].isEnergyParameter[n]) text += "<b>" + QString(BondFunctions::BondFunctions[bf].parameterKeywords[n]) + "</b>";
		else text += BondFunctions::BondFunctions[bf].parameterKeywords[n];
	}
	ui.FFEditorBondParametersLabel->setText(text);
}

// Bond interaction type changed
void AtenForcefieldEditor::BondFunctionChanged(int index)
{
	boundFunctionChanged((TComboBox*) sender(), index,  ForcefieldBound::BondInteraction);
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
	ForcefieldBound* ffb = targetForcefield_->bond(row);
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
			ffb->setParameter(n, atof(qPrintable(w->text())));
			break;
	}
	updating_ = FALSE;
}

void AtenForcefieldEditor::on_FFEditorBondsTable_itemSelectionChanged()
{
	int row = ui.FFEditorBondsTable->currentRow();
	if (row == -1)
	{
		updateBondsLabels(NULL);
		return;
	}
	ForcefieldBound* ffb = targetForcefield_->bond(row);
	updateBondsLabels(ffb);
}

/*
// Angles Page
*/

void AtenForcefieldEditor::updateAnglesLabels(ForcefieldBound* ffb)
{
	if (ffb == NULL)
	{
		ui.FFEditorAngleFormLabel->setText("None");
		ui.FFEditorAngleParametersLabel->setText("None");
		return;
	}
	AngleFunctions::AngleFunction bf = ffb->angleForm();
	// Construct labels
	QString text;
	text.sprintf("%s (%s)", AngleFunctions::AngleFunctions[bf].name, AngleFunctions::AngleFunctions[bf].keyword);
	ui.FFEditorAngleFormLabel->setText(text);
	text.clear();
	for (int n=0; n<AngleFunctions::AngleFunctions[bf].nParameters; ++n)
	{
		if (n != 0) text += ", ";
		if (AngleFunctions::AngleFunctions[bf].isEnergyParameter[n]) text += "<b>" + QString(AngleFunctions::AngleFunctions[bf].parameterKeywords[n]) + "</b>";
		else text += AngleFunctions::AngleFunctions[bf].parameterKeywords[n];
	}
	ui.FFEditorAngleParametersLabel->setText(text);
}

// Angle interaction type changed
void AtenForcefieldEditor::AngleFunctionChanged(int index)
{
	boundFunctionChanged((TComboBox*) sender(), index,  ForcefieldBound::AngleInteraction);
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
	ForcefieldBound* ffb = targetForcefield_->angle(row);
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
			ffb->setParameter(n, atof(qPrintable(w->text())));
			break;
	}
	updating_ = FALSE;
}

void AtenForcefieldEditor::on_FFEditorAnglesTable_itemSelectionChanged()
{
	int row = ui.FFEditorAnglesTable->currentRow();
	if (row == -1)
	{
		updateAnglesLabels(NULL);
		return;
	}
	ForcefieldBound* ffb = targetForcefield_->angle(row);
	updateAnglesLabels(ffb);
}

/*
// Torsions Page
*/

void AtenForcefieldEditor::updateTorsionsLabels(ForcefieldBound* ffb)
{
	if (ffb == NULL)
	{
		ui.FFEditorTorsionFormLabel->setText("None");
		ui.FFEditorTorsionParametersLabel->setText("None");
		return;
	}
	TorsionFunctions::TorsionFunction bf = ffb->torsionForm();
	// Construct labels
	QString text;
	text.sprintf("%s (%s)", TorsionFunctions::TorsionFunctions[bf].name, TorsionFunctions::TorsionFunctions[bf].keyword);
	ui.FFEditorTorsionFormLabel->setText(text);
	text.clear();
	for (int n=0; n<TorsionFunctions::TorsionFunctions[bf].nParameters; ++n)
	{
		if (n != 0) text += ", ";
		if (TorsionFunctions::TorsionFunctions[bf].isEnergyParameter[n]) text += "<b>" + QString(TorsionFunctions::TorsionFunctions[bf].parameterKeywords[n]) + "</b>";
		else text += TorsionFunctions::TorsionFunctions[bf].parameterKeywords[n];
	}
	ui.FFEditorTorsionParametersLabel->setText(text);
}

// Torsion interaction type changed
void AtenForcefieldEditor::TorsionFunctionChanged(int index)
{
	boundFunctionChanged((TComboBox*) sender(), index,  ForcefieldBound::TorsionInteraction);
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
	ForcefieldBound* ffb = targetForcefield_->torsion(row);
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
		// Electrostatic scaling factor
		case (TorsionColumn::ElecScale):
			ffb->setElecScale(atof(qPrintable(w->text())));
			break;
		// van der Waals scaling factor
		case (TorsionColumn::VdwScale):
			ffb->setVdwScale(atof(qPrintable(w->text())));
			break;
		// Parameter data
		case (TorsionColumn::Data1):
		case (TorsionColumn::Data2):
		case (TorsionColumn::Data3):
		case (TorsionColumn::Data4):
		case (TorsionColumn::Data5):
		case (TorsionColumn::Data6):
			n = column - TorsionColumn::Data1;
			ffb->setParameter(n, atof(qPrintable(w->text())));
			break;
	}
	updating_ = FALSE;
}

void AtenForcefieldEditor::on_FFEditorTorsionsTable_itemSelectionChanged()
{
	int row = ui.FFEditorTorsionsTable->currentRow();
	if (row == -1)
	{
		updateTorsionsLabels(NULL);
		return;
	}
	ForcefieldBound* ffb = targetForcefield_->torsion(row);
	updateTorsionsLabels(ffb);
}

/*
// Impropers Page
*/

void AtenForcefieldEditor::updateImpropersLabels(ForcefieldBound* ffb)
{
	if (ffb == NULL)
	{
		ui.FFEditorImproperFormLabel->setText("None");
		ui.FFEditorImproperParametersLabel->setText("None");
		return;
	}
	TorsionFunctions::TorsionFunction bf = ffb->torsionForm();
	// Construct labels
	QString text;
	text.sprintf("%s (%s)", TorsionFunctions::TorsionFunctions[bf].name, TorsionFunctions::TorsionFunctions[bf].keyword);
	ui.FFEditorImproperFormLabel->setText(text);
	text.clear();
	for (int n=0; n<TorsionFunctions::TorsionFunctions[bf].nParameters; ++n)
	{
		if (n != 0) text += ", ";
		if (TorsionFunctions::TorsionFunctions[bf].isEnergyParameter[n]) text += "<b>" + QString(TorsionFunctions::TorsionFunctions[bf].parameterKeywords[n]) + "</b>";
		else text += TorsionFunctions::TorsionFunctions[bf].parameterKeywords[n];
	}
	ui.FFEditorImproperParametersLabel->setText(text);
}

// Item in impropers table edited
void AtenForcefieldEditor::on_FFEditorImpropersTable_itemChanged(QTableWidgetItem *w)
{
	if ((targetForcefield_ == NULL) || updating_) return;
	updating_ = TRUE;
	// Get position of changed item
	int row = ui.FFEditorImpropersTable->row(w);
	int column = ui.FFEditorImpropersTable->column(w);
	// Get pointer to forcefield bound from edited row
	ForcefieldBound* ffb = targetForcefield_->torsion(row);
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
			ffb->setParameter(n, atof(qPrintable(w->text())));
			break;
	}
	updating_ = FALSE;
}

void AtenForcefieldEditor::on_FFEditorImpropersTable_itemSelectionChanged()
{
	int row = ui.FFEditorImpropersTable->currentRow();
	if (row == -1)
	{
		updateImpropersLabels(NULL);
		return;
	}
	ForcefieldBound* ffb = targetForcefield_->improper(row);
	updateImpropersLabels(ffb);
}

/*
// Urey-Bradleys Page
*/

void AtenForcefieldEditor::updateUreyBradleysLabels(ForcefieldBound* ffb)
{
	if (ffb == NULL)
	{
		ui.FFEditorUreyBradleyFormLabel->setText("None");
		ui.FFEditorUreyBradleyParametersLabel->setText("None");
		return;
	}
	BondFunctions::BondFunction bf = ffb->bondForm();
	// Construct labels
	QString text;
	text.sprintf("%s (%s)", BondFunctions::BondFunctions[bf].name, BondFunctions::BondFunctions[bf].keyword);
	ui.FFEditorUreyBradleyFormLabel->setText(text);
	text.clear();
	for (int n=0; n<BondFunctions::BondFunctions[bf].nParameters; ++n)
	{
		if (n != 0) text += ", ";
		if (BondFunctions::BondFunctions[bf].isEnergyParameter[n]) text += "<b>" + QString(BondFunctions::BondFunctions[bf].parameterKeywords[n]) + "</b>";
		else text += "%s", BondFunctions::BondFunctions[bf].parameterKeywords[n];
	}
	ui.FFEditorUreyBradleyParametersLabel->setText(text);
}

// Item in angles table edited
void AtenForcefieldEditor::on_FFEditorUreyBradleysTable_itemChanged(QTableWidgetItem *w)
{
	if ((targetForcefield_ == NULL) || updating_) return;
	updating_ = TRUE;
	// Get position of changed item
	int row = ui.FFEditorUreyBradleysTable->row(w);
	int column = ui.FFEditorUreyBradleysTable->column(w);
	// Get pointer to forcefield bound from edited row
	ForcefieldBound* ffb = targetForcefield_->ureyBradley(row);
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
			ffb->setParameter(n, atof(qPrintable(w->text())));
			break;
	}
	updating_ = FALSE;
}

void AtenForcefieldEditor::on_FFEditorUreyBradleysTable_itemSelectionChanged()
{
	int row = ui.FFEditorUreyBradleysTable->currentRow();
	if (row == -1)
	{
		updateUreyBradleysLabels(NULL);
		return;
	}
	ForcefieldBound* ffb = targetForcefield_->ureyBradley(row);
	updateUreyBradleysLabels(ffb);
}

/*
// Generate Page
*/

// Generate type
void AtenForcefieldEditor::on_FFEditorGenerateTypeButton_clicked(bool on)
{
}