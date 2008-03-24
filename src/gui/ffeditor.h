/*
	*** Qt prefs window declaration
	*** src/gui/prefs.h
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

#ifndef ATEN_FFEDITWINDOW_H
#define ATEN_FFEDITWINDOW_H

#include "gui/gui.h"
#include "gui/ui_ffeditor.h"

// Forward Declarations
class Forcefield;

// Forcefield editor window
class AtenEdit : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void populate(Forcefield *);

	/*
	// Types Tab
	*/
	private slots:
	void on_FFEditorGenerateTypeButton_clicked(bool on);
	void on_FFEditorTestTypeButton_clicked(bool on);

	/*
	// Atoms Tab
	*/
	private:
	void setAtomData(ForcefieldAtom *ffa);
	private slots:
	void on_FFEditorAtomTable_currentRowChanged(int row);

	/*
	// Bonds Tab
	*/
	private slots:

	/*
	// Angles Tab
	*/
	private slots:

	/*
	// Torsions Tab
	*/
	private slots:

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenEdit(QDialog *parent = 0);
	// Main form declaration
	Ui::FFEditorDialog ui;
	// Finalise widgets (things that we couldn't do in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
