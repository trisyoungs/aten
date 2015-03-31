/*
	*** Prefs Window
	*** src/gui/prefs.h
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

#ifndef ATEN_FFEDITWINDOW_H
#define ATEN_FFEDITWINDOW_H

#include "gui/ui_ffeditor.h"
#include "base/forcefieldbound.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class QComboBox;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Forcefield;
class ForcefieldAtom;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Forcefield editor window
class AtenForcefieldEditor : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions / Variables
	*/
	private:
	// Forcefield currently being edited
	Forcefield* targetForcefield_;
	// Whether the window is currently being repopulated
	bool updating_;

	/*
	// General Functions
	*/
	private:
	void boundFunctionChanged(QComboBox *sender, int i, ForcefieldBound::BoundType bt);

	public:
	void populate(Forcefield* target);

	/*
	// Types Tab
	*/
	private slots:
	void on_FFEditorGenerateTypeButton_clicked(bool on);
	void on_FFEditorTestTypeButton_clicked(bool on);
	void on_FFEditorTypesTable_itemChanged(QTableWidgetItem *w);

	/*
	// Atoms Tab
	*/
	private slots:
	void updateVdwLabels(ForcefieldAtom* ffa);
	void VdwFunctionChanged(int index);
	void on_FFEditorAtomsTable_itemChanged(QTableWidgetItem *w);
	void on_FFEditorAtomsTable_itemSelectionChanged();
	
	/*
	// Bonds Tab
	*/
	private slots:
	void updateBondsLabels(ForcefieldBound* ffb);
	void BondFunctionChanged(int index);
	void on_FFEditorBondsTable_itemChanged(QTableWidgetItem *w);
	void on_FFEditorBondsTable_itemSelectionChanged();
	
	/*
	// Angles Tab
	*/
	private slots:
	void updateAnglesLabels(ForcefieldBound* ffb);
	void AngleFunctionChanged(int index);
	void on_FFEditorAnglesTable_itemChanged(QTableWidgetItem *w);
	void on_FFEditorAnglesTable_itemSelectionChanged();

	/*
	// Torsions Tab
	*/
	private slots:
	void updateTorsionsLabels(ForcefieldBound* ffb);
	void TorsionFunctionChanged(int index);
	void on_FFEditorTorsionsTable_itemChanged(QTableWidgetItem *w);
	void on_FFEditorTorsionsTable_itemSelectionChanged();

	/*
	// Impropers Tab
	*/
	private slots:
	void updateImpropersLabels(ForcefieldBound* ffb);
	void on_FFEditorImpropersTable_itemChanged(QTableWidgetItem *w);
	void on_FFEditorImpropersTable_itemSelectionChanged();
		
	/*
	// UreyBradleys Tab
	*/
	private slots:
	void updateUreyBradleysLabels(ForcefieldBound* ffb);
	void on_FFEditorUreyBradleysTable_itemChanged(QTableWidgetItem *w);
	void on_FFEditorUreyBradleysTable_itemSelectionChanged();
	
	
	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenForcefieldEditor(QWidget* parent);
	// Main form declaration
	Ui::FFEditorDialog ui;
};

#endif
