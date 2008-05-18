/*
	*** Qt GUI: Atomlist Window
	*** src/gui/atomlist.h
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

#ifndef ATEN_ATOMLISTWINDOW_H
#define ATEN_ATOMLISTWINDOW_H

#include "gui/ui_atomlist.h"

// Forward declarations
class Model;

// Program preferences window
class AtenAtomlist : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	void refresh();
	private:
	void peekScrollBar();
	void pokeScrollBar();
	private slots:
	void on_AtomTree_itemSelectionChanged();
	void on_ShiftUpButton_clicked(bool checked);
	void on_ShiftDownButton_clicked(bool checked);
	void on_MoveToStartButton_clicked(bool checked);
	void on_MoveToEndButton_clicked(bool checked);
	void dialogFinished(int result);

	/*
	// Local variables
	*/
	private:
	// Log points of model info displayed in list
	int listStructurePoint_, listSelectionPoint_;
	// Last model displayed in list
	Model *listLastModel_;
	// Whether the widget should refresh when it is next shown
	bool shouldRefresh_;
	// Whether the widget is currently refreshing
	bool refreshing_;
	// Position of list slider
	int listPosition_;

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtenAtomlist(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	~AtenAtomlist();
	// Main form declaration
	Ui::AtomlistDialog ui;
};

#endif
