/*
	*** Qt loadmodel window declaration
	*** src/gui/loadmodel.h
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

#ifndef ATEN_LOADMODELWINDOW_H
#define ATEN_LOADMODELWINDOW_H

#include "base/dnchar.h"
#include "gui/gui.h"
#include "gui/ui_loadmodel.h"

// Forward Declarations
class Forcefield;
class Tree;

// Model Load Dialog
class AtenLoadModel : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Widget Functions
	*/
	private slots:
	void on_LoadModelBrowseButton_clicked(bool checked);
	void on_LoadModelEdit_editingFinished();
	void on_LoadModelEdit_returnPressed();
	void on_LoadModelRebondCombo_activated(int index);
	void on_LoadModelFoldCombo_activated(int index);
	void on_LoadModelPackCombo_activated(int index);
	void on_LoadModelCentreCombo_activated(int index);
	void on_LoadModelZMappingCombo_activated(int index);

	/*
	// Data
	*/
	private:
	// Selected filter
	Tree *selectedFilter_;
	// Filename in edit box
	Dnchar selectedFilename_;

	public:
	// Return the selected filter
	Tree *selectedFilter();
	// Return filename
	const char *selectedFilename();

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenLoadModel(QWidget *parent = 0);
	// Main form declaration
	Ui::LoadModelDialog ui;
	// Finalise widgets (things that we couldn't do in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
