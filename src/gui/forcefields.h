/*
	*** Qt GUI: Forcefields Window
	*** src/gui/forcefields.h
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

#ifndef ATEN_FORCEFIELDSWINDOW_H
#define ATEN_FORCEFIELDSWINDOW_H

#include "gui/ui_forcefields.h"

// Forward Declarations
class QFileDialog;
class TListWidgetItem;

// Forcefields window
class AtenForcefields : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	void refresh();
	void refreshTypes();
	void loadForcefield();
	private slots:
	void on_TypeModelButton_clicked(bool checked);
	void on_UntypeModelButton_clicked(bool checked);
	void on_ManualTypeSetButton_clicked(bool checked);
	void on_ManualTypeClearButton_clicked(bool checked);
	void on_ManualTypeTestButton_clicked(bool checked);
	void on_ManualTypeEdit_returnPressed();
	void on_LoadForcefieldButton_clicked(bool checked);
	void on_RemoveForcefieldButton_clicked(bool checked);
	void on_EditForcefieldButton_clicked(bool checked);
	void on_AssignFFToCurrentButton_clicked(bool checked);
	void on_AssignFFToAllButton_clicked(bool checked);
	void on_AssignFFToPatternButton_clicked(bool clicked);
	void on_ForcefieldList_currentRowChanged(int row);
	void on_ForcefieldList_itemClicked(QListWidgetItem *item);
	void dialogFinished(int result);

	/*
	// Local variables
	*/
	private:
	// Element selected in Type filter
	int typelistElement_;
	// Whether window contents should be refreshed when shown
	bool shouldRefresh_;
	// Current checked item (default forcefield) if any
	TListWidgetItem *checkedItem_;

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtenForcefields(QWidget *parent = 0);
	~AtenForcefields();
	// Main form declaration
	Ui::ForcefieldsDialog ui;
	// File dialogs for forcefields
	QFileDialog *openForcefieldDialog, *saveForcefieldDialog;
};

#endif
