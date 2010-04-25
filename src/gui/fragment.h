/*
	*** Qt GUI: Fragment Library Window
	*** src/gui/fragment.h
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

#ifndef ATEN_FRAGMENTWINDOW_H
#define ATEN_FRAGMENTWINDOW_H

#include "gui/ui_fragment.h"
#include "base/dnchar.h"

// Forward declarations
class Model;
class Fragment;
class TTreeWidgetItem;

// Atom list
class AtenFragment : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	void refresh();
	private slots:
	void on_FragmentTree_currentItemChanged(QTreeWidgetItem *current, QTreeWidgetItem *previous);
	void on_FragmentTree_doubleClicked(const QModelIndex &index);
	void on_FragmentTable_currentItemChanged(QTableWidgetItem *current, QTableWidgetItem *previous);
	void on_FragmentTable_doubleClicked(const QModelIndex &index);
	void on_FragmentFilterEdit_textChanged(const QString &text);
	void on_FragmentShowAllButton_clicked(bool checked);
	void on_ViewAsListCheck_clicked(bool checked);
	void on_ViewAsGridCheck_clicked(bool checked);
	void dialogFinished(int result);

	/*
	// Local variables
	*/
	private:
	// Current drawing fragment
	Fragment *currentFragment_;
	// Text string to filter fragments by
	Dnchar filterText_;

	public:
	// Return current drawing fragment
	Fragment *currentFragment();

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtenFragment(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	~AtenFragment();
	// Main form declaration
	Ui::FragmentDialog ui;
};

#endif
