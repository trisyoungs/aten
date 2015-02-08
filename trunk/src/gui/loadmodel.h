/*
	*** Load Model Window
	*** src/gui/loadmodel.h
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

#ifndef ATEN_LOADMODELWINDOW_H
#define ATEN_LOADMODELWINDOW_H

#include "base/dnchar.h"
#include "gui/ui_loadmodel.h"

// Forward Declarations
class Forcefield;
class Tree;
class AtenWindow;

// Model Load Dialog
class AtenLoadModel : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor
	AtenLoadModel(AtenWindow& parent);
	// Main form declaration
	Ui::LoadModelDialog ui;


	/*
	 * Reference to AtenWindow
	 */
	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	 * Widget Functions
	 */
	private slots:
	void on_BrowseButton_clicked(bool checked);
	void on_FilenameEdit_editingFinished();
	void on_FilenameEdit_returnPressed();
	void on_RebondCombo_activated(int index);
	void on_FoldCombo_activated(int index);
	void on_PackCombo_activated(int index);
	void on_CentreCombo_activated(int index);
	void on_ZMappingCombo_activated(int index);
	void on_BohrCheck_clicked(bool checked);
	void on_KeepNamesCheck_clicked(bool checked);
	void on_KeepTypesCheck_clicked(bool checked);

	public:
	// Update controls and show dialog
	void updateAndShow();


	/*
	 * Data
	 */
	private:
	// Whether the window is currently refreshing its controls
	bool refreshing_;
	// Filename in edit box
	Dnchar selectedFilename_;

	public:
	// Return the selected format (if any)
	Tree *selectedFormat();
	// Return filename
	const char *selectedFilename();
};

#endif
