/*
	*** Qt GUI: Element select dialog
	*** src/gui/selectelement.h
	Copyright T. Youngs 2007-2009

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

#ifndef ATEN_SELECTELEMENTDIALOG_H
#define ATEN_SELECTELEMENTDIALOG_H

#include "gui/gui.h"
#include "gui/ui_selectelement.h"
#include "base/elements.h"

// Select Element Dialog
class AtenSelectElement : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	private:
	QPushButton *addCommonButton(int el);
	private slots:
	void on_CancelButton_clicked(bool checked);
	void ElementButton_clicked(bool checked);
	void CommonElementButton_clicked(bool checked);
	public:
	void addCommonButtons(const char *buttonlist);

	/*
	// Local variables
	*/
	private:
	// Selected element
	int selectedElement_;
	// Reflist of common element buttons
	Reflist<QPushButton,int> commonButtons_;	
	// Array of element buttons
	QPushButton **elementButtons_;
	// Layout for common elements group
	QHBoxLayout *commonGroupLayout_;

	/*
	// Methods
	*/
	public:
	// Select an element
	int selectElement();

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenSelectElement(QWidget *parent = 0);
	// Main form declaration
	Ui::SelectElementDialog ui;
	// Finalise widgets (things that we couldn't do in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
