/*
	*** Qt GUI: Grids Window
	*** src/gui/grids.h
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

#ifndef ATEN_GRIDSWINDOW_H
#define ATEN_GRIDSWINDOW_H

#include "gui/ui_grids.h"

// Program preferences window
class AtenGrids : public QWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void refresh();
	private:
	void refreshGridInfo();
	void gridOriginChanged(int component, double value);
	void gridAxisChanged(int row, int component, double value);
	private slots:
	void on_LoadGridButton_clicked(bool checked);
	void on_RemoveGridButton_clicked(bool checked);
	void on_SaveGridButton_clicked(bool checked);
	void on_GridList_currentRowChanged(int row);
	void on_GridStyleCombo_currentIndexChanged(int index);
	void on_GridList_itemClicked(QListWidgetItem *item);
	void on_GridCutoffSpin_valueChanged(double d);
	void on_GridOriginXSpin_valueChanged(double d);
	void on_GridOriginYSpin_valueChanged(double d);
	void on_GridOriginZSpin_valueChanged(double d);
	void on_GridAxesAXSpin_valueChanged(double d);
	void on_GridAxesAYSpin_valueChanged(double d);
	void on_GridAxesAZSpin_valueChanged(double d);
	void on_GridAxesBXSpin_valueChanged(double d);
	void on_GridAxesBYSpin_valueChanged(double d);
	void on_GridAxesBZSpin_valueChanged(double d);
	void on_GridAxesCXSpin_valueChanged(double d);
	void on_GridAxesCYSpin_valueChanged(double d);
	void on_GridAxesCZSpin_valueChanged(double d);
	void on_GridPositiveColourButton_clicked(bool checked);
	void on_GridNegativeColourButton_clicked(bool checked);
	void on_GridTransparencySpin_valueChanged(double d);
	void on_GridColourscaleSpin_valueChanged(int n);
	void on_GridSymmetricCheck_clicked(bool checked);

	/*
	// Local variables
	*/
	private:

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenGrids(QWidget *parent = 0);
	// Destructor
	~AtenGrids();
	// Main form declaration
	Ui::GridsWidget ui;
	// Finalise widgets (things that couldn't be done in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();
};

#endif
