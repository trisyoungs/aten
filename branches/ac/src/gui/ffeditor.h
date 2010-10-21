/*
	*** Qt prefs window declaration
	*** src/gui/prefs.h
	Copyright T. Youngs 2007-2010

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

#include "gui/gui.h"
#include "gui/ui_ffeditor.h"

// Forward Declarations
class Forcefield;
class ForcefieldAtom;

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
	Forcefield *targetForcefield_;
	// Whether the window is currently being repopulated
	bool updating_;

	public:
	void populate(Forcefield *target);

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
	void VdwFunctionChanged(int index);
	void on_FFEditorAtomsTable_itemChanged(QTableWidgetItem *w);

	/*
	// Bonds Tab
	*/
	private slots:
	void BondFunctionChanged(int index);
	void on_FFEditorBondsTable_itemChanged(QTableWidgetItem *w);

	/*
	// Angles Tab
	*/
	private slots:
	void AngleFunctionChanged(int index);
	void on_FFEditorAnglesTable_itemChanged(QTableWidgetItem *w);

	/*
	// Torsions Tab
	*/
	private slots:
	void TorsionFunctionChanged(int index);
	void on_FFEditorTorsionsTable_itemChanged(QTableWidgetItem *w);

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenForcefieldEditor(QWidget *parent = 0);
	// Main form declaration
	Ui::FFEditorDialog ui;
	// Finalise widgets (things that we couldn't do in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
